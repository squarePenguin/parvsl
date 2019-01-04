// Big-number arithmetic.                                  A C Norman, 2019


// To do:
//    [I believe that all the following are just optimizations]
//    Lehmer-style gcd implementation.
//    Karatsuba-style multiplication when numbers are big enough.
//    review all fixnum and fixnum/bignum cases for optimisation.

/**************************************************************************
 * Copyright (C) 2019, Codemist.                         A C Norman       *
 *                                                                        *
 * Redistribution and use in source and binary forms, with or without     *
 * modification, are permitted provided that the following conditions are *
 * met:                                                                   *
 *                                                                        *
 *     * Redistributions of source code must retain the relevant          *
 *       copyright notice, this list of conditions and the following      *
 *       disclaimer.                                                      *
 *     * Redistributions in binary form must reproduce the above          *
 *       copyright notice, this list of conditions and the following      *
 *       disclaimer in the documentation and/or other materials provided  *
 *       with the distribution.                                           *
 *                                                                        *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS    *
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT      *
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS      *
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE         *
 * COPYRIGHT OWNERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,   *
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,   *
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS  *
 * OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND *
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR  *
 * TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF     *
 * THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH   *
 * DAMAGE.                                                                *
 *************************************************************************/



// There are quite a lot of bignumber packages out there on the web,
// but none of them seemed to be such that I could readily use them
// for arithmetic within a Lisp at all easily, for instance because of
// the storage management arangements used.
//
// This code is for use as a header-only library, so just "#include" it
// to be able to use it. All the use of the word "inline" is so that if
// you #include this from several source files there will not be trouble when
// you link them together.
//
// This code uses 64-bit digits and a 2s complement representation for
// negative numbers. This means it will work best on 64-bit platforms
// (which by now are by far the most important), and it provides bitwise
// logical operations (logand and logor) as well as arithmetic. It will work
// best where the C++ compiler supports a 128-bit integral type, but does not
// need that. It should work on 32-bit systems as well, although one should
// expect performance there to be lower.
//
// If "softfloat_h" is defined I will suppose that there is a type "float128"
// available and I will support conversions to and from that. In part because
// this type is not supported in any standard way I will be assuming that the
// version I use is as provided by the SoftFloat IEEE Floating-Point Arithmetic
// package released by John R. Hauser. That does everything in software so
// absolute speed may be modest, but it is nicely portable. For more
// information see http://www.jhauser.us/arithmetic/SoftFloat.html.
//
// I will check "#ifdef softfloat_h" to see if I will activate the 128-bit
// floating pint option. That should check whether softfloat.h has been
// included before this file.

//
// I will provide two levels of access and abstraction. At the low level
// a big number is represented as and array of uint64_t digits along with
// a size_t value that indicates the number of digits in use. The most
// significant digit in any number lives in memory with type uint64_t but
// is treated as signed (ie int64_t) in the arithmetic. For the purposes
// of the bitwise operations (and, or, xor and not) negative values are
// processed as if they had an infinite number of 1 bits above their
// most significant stored digit.
// If a positive value has a top digit whose unsigned value has its top bit
// set then an additional zero digit is used ahead of that, and equivalently
// for negative values.
//
// vectors to represent numbers are allocated using a function reserve()
// which takes an argument indicating how long the number might be. It will
// often be necessary to reserve memory in a conservative manner, ie to
// allocate more memory than will eventually prove to be needed.
// At the end of an arithmetic operation a reserved block of memory can be
// passed to abandon() when it is no longer required, or there can be a
// call to confirm_size() to establish the exact size that is to be retained.
// A variant call confirm_size_x() is used when the vector whose size is being
// confirmed is not the one that was most recently allocated. confirm_size()
// returns a handle for the vector, not the vector itself.
//
// In addition to numbers I will generate strings (I have code to make a
// string representation of a number, with hex, decimal, octal and binary
// variants). Space for the string will be allocated using reserve_string()
// and finalized using confirm_size_string(), with both of those indicating
// sizes in bytes. Note that when you reserve or confirm string size the
// length quoted is the number of characters that will be present excluding
// any header or termination markers - reserve_string() will allow for the
// needs of suchlike.
//
// A higher level packaging represents numbers using a class Bignum. This
// has just one field which will hold a potentially encoded version of a
// pointer to a vector that is the number. This vector will have a first
// item that is a header word containing length information and then the
// uint64_t values representing the numeric value. The representation of the
// header and the encoding of the pointer will be done in one of several
// ways, these being intended to provide models of the implementation intended
// for different use cases.
// In some cases (especially the NEW one) small integers are handled
// specially to improve efficiency.
// There are other support functions which will be described where they
// are defined or used.


//
//These cases are included in this file using conditional compilation:
//
// MALLOC:
//   A bignum with n digits is held in a vector of length n+1, and the
//   "encoded pointer" to it is a native pointer to the second element.
//   If this pointer is p then the number of words (n) is kept at p[-1]
//   and the least significant digit of the number is at p[0]. reserve()
//   uses malloc() to obtain space. confirm_size() uses realloc() to trim
//   the allocated space, and abandon() maps onto use of free(). This
//   uses C rather than C++ memory management because it wants to use realloc
//   which is not supported in the tidy C++ world. Performance of the code
//   as a whold will be sensitive to the malloc/realloc/free implementation
//   on the platform that is in use. To allow for a user who wished to
//   customize allocation, all calls to the basic memory allocation primitives
//   are made indirectly so that alternative equivalents can be plugged in.
//   Strings and allocated using malloc() and returned as simple nul-terminated
//   C strings. They must be released using free() after use.
// NEW:
//   A bignum with n digits will be stored in a vector whose size is the
//   next power of two strictly larger than n. As with the MALLOC case
//   the numeric data starts one word into this block and the initial word
//   of the block contains a header of length information. Here the header
//   is split into two 32-bit parts. One contains the length of the number
//   as before (but note that in general that will not fill up the entire
//   memory block), the other contains log2(block_size), ie it is a compact
//   indication of the size of the block. There will be free-chains for
//   blocks of size 2,4,8,... so that abandon() just pushes the released
//   memory onto one and reserve() can often merely retrieve a previously
//   used block. In most cases confirm_size just needs to write the actual
//   length of a number into the header word. When two large numbers are
//   subtracted the number of digits in the result may be much smaller than
//   the size of block that had to have been reserved. To allow for that sort
//   of situation confirm_size() reserves the right to notice cases where used
//   size in a block is much smaller than the capacity, and in such cases
//   allocate a fresh smaller block and copy information into it, allowing it
//   to abandon the overlarge chunk of memory.
//   The reference to the vector of digits is held using type intptr_t and
//   can be cast to obtain the address of the least significant digit of the
//   value. But so that this scheme as a whole provides better performance
//   for general users, small integer values will be handled specially. If
//   the "encoded pointer" has its bottom bit set than it represents a 63-bit
//   signed value. The intent here is that the class Bignum, by containing
//   just one integer-sized field, can be stored and passed around really
//   efficiently, and if in its use most arithmetic remains on values that
//   fit within 63 bits it will not do much storage allocation at all. If this
//   works well it should represent a reasonably convenient and tolerably
//   efficient C++ facility for general use.
//   Strings live in store allocated using "new char[nnn]" and are returned
//   as C style strings that must be disposed of using "delete". The use of
//   C rather than C++ style strings because I hope that makes storage
//   management interaction clearer.
// LISP:
//   The arrangements here are based on the arrangements I have in my VSL
//   and CSL Lisp implementations. I still hope that between the above options
//   and this one the code can be adapted reasonably easily. As before the
//   basic representation of a number with n digits is a vector of length
//   n+1, with the initial word containing a header. In VSL/CSL a header word
//   contains some tage bits identifying it as a header, then some type
//   bite that here will indicate that it is a header of a big number. Finally
//   it contains a length field. The exact bit-patterns and packing here will
//   be specific to the particular Lisp (obviously!). A reference to a big
//   number will be the address of the header word of this vector plus some
//   tag bits in the bottom 3 bits. This "low tagging" relies on all block
//   of memory being aligned to 8-byte boundaries (even on 32-bit platforms).
//   On a 32-bit system the header will only occupy the first 32-bits of the
//   initial 64-bit memory unit, and the second 32-bit region is spare and
//   would be best set to zero.
//   There are two expectations about memory management. The first is that
//   garbage collection has left a large block of contiguous memory within
//   which new material can be allocated linearly. Under this supposition the
//   most recently allocated block of memory can be shrunk or discarded by
//   simply resetting a heap-fringe pointer. The second is that it will
//   at least occasionally be desirable to perform linear scans of all memory.
//   To support that when a block that is not the most recently allocated one
//   is shrunk or discarded a header word is placed in the released space
//   leaving a valid but dummy Lisp item there.
//   Calls to memory allocation primitives are made without any special
//   concern for garbage collector safety of other pointers, and so in its
//   current form this code insistes on running in a context where the garbage
//   collector is conservative, so that for instance the untagged pointers to
//   raw vectors of digits are safe. This aspect of the code may well limit
//   its direct usefulness, but resolving the issue would involve an
//   understanding of how to arrange that particular items got treated as
//   list bases in some system.
//   In Lisp mode it is anticipated that as well as a tagged representation
//   of small integers that the generic arithemetic will need to support
//   floating point numbers (and possibly multiple widths of floating point
//   values, plus ratios and complex numbers) and so the dispatch on tagged
//   numbers needs to live at a higher level within the Lisp then just thise
//   code.
//   Strings are allocated using reserve_string() and finalized using
//   confirm_size_string. For Lisp purposes they will need to have a header
//   word containing the string length.

// I provide a default configuration, but by predefining one of the
// symbols allow other versions to be built.

#if !defined MALLOC && !defined NEW && !defined LISP
// If no explicit options have been set I will building using memory
// allocation via C++ "new".

#define NEW  1

#endif

#ifdef __cpp_inline_variables

// For versions of C++ up to C++17 I will put constant values in header
// files using something along the line of "static const int VAR = VAL;".
// This should give the compiler a chance to replace the name with its value
// throughout the compilation unit, and if the compiler is clever enough it
// will avoid leaving a word of memory with the value stored if all uses
// have been dealt with more directly. However it will tend to lead to a
// lot of "static variable defined but not used" warnings.
// From C++17 onwards (and C++ mandates the __cpp_inline_variables macro to
// indicate if the feature is in place) I will use
// "inline const int VAR = VAL;" and now if memory is allocated for the
// variable it will only be allocated once, and I hope that compilers will
// not feel entitled to moan about cases where there are no references.
//
#define INLINE_VAR inline
#else
#define INLINE_VAR static
#endif



#include <cstdio>
#include <cstring>
#include <cstdint>
#include <cctype>
#include <cinttypes>
#include <cassert>
#include <cstdlib>
#include <cstdarg>
#include <random>
#include <iostream>
#include <iomanip>
#include <thread>
#include <ctime>
#include <chrono>

namespace arith
{

// A scheme "my_assert" lets me write in my own code to print the
// diagnostics.

// my_abort() mainly exists so I can set a breakpoint on it! Setting one
// on the system abort() function sometimes does not give me as much help
// as I might have hoped on at least some platforms, while a break-point
// on my_abort() does what I expect.

static inline void my_abort(const char *msg)
{   std::cout << "About to abort: " << msg << std::endl;
    abort();
}

static inline void my_abort()
{   std::cout << "About to abort" << std::endl;
    abort();
}

#ifdef NEW
INLINE_VAR const bool debug_arith = true;
#else
INLINE_VAR const bool debug_arith = false;
#endif

template <typename F>
static inline void my_assert(bool ok, F&& action)
{
// Use this as in:
//     my_assert(predicate, [&]{...});
// where the "..." is an arbitrary sequence of actions to be taken
// if the assertion fails. The action will typically be to display
// extra information about what went wrong.
    if (debug_arith && !ok) { action(); my_abort(); }
}

static inline void my_assert(bool ok)
{
// For simple use where a customised message is not required:
//     my_assert(predicate);
    if (debug_arith && !ok) my_abort("failure reported via my_assert()");
}

// At times during development it is useful to be able to send messages
// to a log file.... This should not be used in final production code
// but still may help while debugging.

INLINE_VAR FILE *logfile = NULL;

// Making this "inline" avoids warning messages if it is not
// used. So even though this may somewhat waste space when it is used,
// I like this option.

static inline void logprintf(const char *fmt, ...)
{
// I use a fixed name for the log file. This is another respect in which
// this has to be seen as code only suitable for temporary use.
    if (logfile == NULL) logfile = std::fopen("arith.log", "w");
    std::va_list args;
    va_start(args, fmt);
    std::vfprintf(logfile, fmt, args);
    va_end(args);
    std::fflush(logfile);
}

// The C++ rules about shifts are not always very comfortable, in particular
// right shifts on signed values may or may not propagate the sign bit
// and a left shift on signed values might cause trouble in overflow cases.
// So here are versions that should behave consistently across all
// architectures.

static inline int32_t ASR(int32_t a, int n)
{   if (n<0 || n>=8*(int)sizeof(int32_t)) n=0;
    uint32_t r = ((uint32_t)a) >> n;
    uint32_t signbit = ((uint32_t)a) >> (8*sizeof(uint32_t)-1);
    if (n != 0) r |= ((-signbit) << (8*sizeof(uint32_t) - n));
    return (int32_t)r;
}

static inline int64_t ASR(int64_t a, int n)
{   if (n<0 || n>=8*(int)sizeof(int64_t)) n=0;
    uint64_t r = ((uint64_t)a) >> n;
    uint64_t signbit = ((uint64_t)a) >> (8*sizeof(uint64_t)-1);
    if (n != 0) r |= ((-signbit) << (8*sizeof(uint64_t) - n));
    return (int64_t)r;
}

static inline uint64_t ASR(uint64_t a, int n)
{   return ASR((int64_t)a, n);
}

// The behaviour of left shifts on negative (signed) values seems to be
// labelled as undefined in C/C++, so any time I am going to do a left shift
// I need to work in an unsigned type.

static inline int32_t ASL(int32_t a, int n)
{   if (n < 0 || n>=8*(int)sizeof(uint32_t)) n = 0;
    return (int32_t)(((uint32_t)a) << n);
}

static inline int64_t ASL(int64_t a, int n)
{   if (n < 0 || n>=8*(int)sizeof(uint64_t)) n = 0;
    return (int64_t)(((uint64_t)a) << n);
}

//=========================================================================
//=========================================================================
// I want to make C++ output using the "<<" operator on an ostream cope
// with big numbers. Doing so makes their use much smoother. The particular
// aspect of this addresses here is the provision of an IO manipulator
// called "std::bin" that sets for binary display of bignums (bit not of
// other integer types).
//=========================================================================
//=========================================================================

struct radix
{
public:
// I would like setting hex or oct or dec to disable this, but at present
// I do not know how to do that. So I will arrange that binary output is
// only generated if none of those flags are set, because I can clear
// them here. Then when I restore one of them I disable the test for binary.
// I will arrange that if nobody has ever asked for binary they do not get it,
// just so I feel safe.
    static void set_binary_output(std::ios_base &s)
    {   flag(s) = 1;
        s.unsetf(std::ios_base::dec);
        s.unsetf(std::ios_base::oct);
        s.unsetf(std::ios_base::hex);
    }
    static bool is_binary_output(std::ios_base &s)
    {   return flag(s) != 0;
    }
private:
    static long & flag(std::ios_base &s)
    {   static int n = std::ios_base::xalloc();
        return s.iword(n);
    }
};

} // temporary end of namespace arith

// I want a new io manipulator "std::bin" to select binary mode output.
// This will be used much as std::oct, std::dec and std::hex.

namespace std
{   std::ostream &bin(std::ostream &os)
    {   arith::radix::set_binary_output(os);
        return os;
    }
}

namespace arith
{

// declare a number of functions that maight usefully be used elsewhere. If
// I declare them "inline" then it will be OK even if I include this header
// from multiple source files because only one copy should end up in the
// eventually-linked executable.

inline uint64_t *reserve(size_t n);
inline intptr_t confirm_size(uint64_t *p, size_t n, size_t final);
inline intptr_t confirm_size_x(uint64_t *p, size_t n, size_t final);
inline void abandon(uint64_t *p);
inline void abandon(intptr_t h);

#ifdef LISP
typedef intptr_t string_handle;
#else
typedef char *string_handle;
#endif

inline string_handle confirm_size_string(char *p, size_t n, size_t final);
inline void abandon_string(string_handle);

inline intptr_t vector_to_handle(uint64_t *p);
inline uint64_t *vector_of_handle(intptr_t n);
inline size_t number_size(uint64_t *p);

inline bool fits_into_fixnum(int64_t n);
inline intptr_t int_to_handle(int64_t n);
constexpr inline int64_t int_of_handle(intptr_t n);

inline intptr_t string_to_bignum(const char *s);
inline intptr_t int_to_bignum(int64_t n);
inline intptr_t unsigned_int_to_bignum(uint64_t n);
inline intptr_t double_to_bignum(double d);
inline intptr_t double_to_floor(double d);
inline intptr_t double_to_ceiling(double d);
inline intptr_t uniform_positive(size_t n);
inline intptr_t uniform_signed(size_t n);
inline intptr_t uniform_upto(intptr_t a);
inline intptr_t random_upto_bits(size_t bits);
inline intptr_t fudge_distribution(intptr_t, int);

#if defined MALLOC

//=========================================================================
//=========================================================================
// The MALLOC option is perhaps the simplest! It uses C style
// malloc/realloc and free functions and the vector if turned into a
// handle by just casting it to an intptr_t value.
//=========================================================================
//=========================================================================


// The following are provided so that a user can update malloc_function,
// realloc_function and free_function to refer to their own choice of
// allocation technology.

typedef void *malloc_t(size_t);
typedef void *realloc_t(void *, size_t);
typedef void free_t(void *);

INLINE_VAR malloc_t  *malloc_function = malloc;
INLINE_VAR realloc_t *realloc_function = realloc;
INLINE_VAR free_t    *free_function   = free;

inline uint64_t *reserve(size_t n)
{   my_assert(n>0 && n<1000000); // Temporary upper limit on size
    uint64_t *r = (uint64_t *)(*malloc_function)((n+1)*sizeof(uint64_t));
    my_assert(r != NULL);
    return &r[1];
}

inline intptr_t confirm_size(uint64_t *p, size_t n, size_t final)
{   my_assert(final>0 && n>=final);
    p = (uint64_t *)
        (*realloc_function)((void *)&p[-1], (final_n+1)*sizeof(uint64_t));
    my_assert(p != NULL);
    p[0] = final_n;
    return vector_to_handle(&p[1]);
}

// In this model confirm_size_x() is just the same as confirm_size().

inline intptr_t confirm_size_x(uint64_t *p, size_t n, size_t final)
{   my_assert(final>0 && n>=final);
    confirm_size(p, n, final);
}

inline void abandon(uint64_t *p)
{   (*free_function)((void *)&p[-1]);
}

// Note that I allocate space for the string data plus a NUL terminating byte.

inline char *reserve_string(size_t n)
{   char *r = (char *)(*malloc_function)(n+1);
    my_assert(r != NULL);
    return r;
}

// When I confirm the size of a string all I do is to write a NUL byte
// to terminate it. I expect the code to have reserved an amount of memory
// that is not much longer than the amount that I will need, so using
// realloc to shrink things to the exact size that is filled would not
// be a good bargain.

inline char *confirm_size_string(char *p, size_t n, size_t final)
{   my_assert(final>0 && (n+9)>final);
    r[final] = 0;
    return r;
}

inline void abandon_string(char *s)
{   (*free_function)(s);
}

// In the C/malloc model I will represent a number by the intptr_t style
// integer that is obtained from a pointer to the first digit of the bignum.

inline intptr_t vector_to_handle(uint64_t *p)
{   return (intptr_t)p;
}

inline uint64_t *vector_of_handle(intptr_t n)
{   return (uint64_t *)n;
}

inline size_t number_size(uint64_t *p)
{   my_assert(p[-1]!=0 && p[-1]<1000000);
    return p[-1];
}

// When I use Bignums that are allocated using malloc() and operated on
// via C++ overloaded operators I often need to copy the data. However when
// memory management uses garbage collection I can allow multiple references
// to the same object and so copying is not needed as much. I have two
// copying functions. One only copies if the system is using MALLOC or
// NEW (but leaves data sharable on systems with garbage collection) while
// the other unconditionally copies.

inline intptr_t always_copy_bignum(uint64_t *p)
{   size_t n = p[-1];
    uint64_t *r = reserve(n);
    std::memcpy(r, p, n*sizeof(uint64_t));
    return confirm_size(r, n, n);
}

inline intptr_t copy_if_no_garbage_collector(uint64_t *p)
{   size_t n = p[-1];
    uint64_t *r = reserve(n);
    std::memcpy(r, p, n*sizeof(uint64_t));
    return confirm_size(r, n, n);
}

inline intptr_t copy_if_no_garbage_collector(intptr_t pp)
{   if (stored_as_fixnum(pp)) return pp;
    uint64_t *p = vector_of_handle(pp);
    size_t n = number_size(p);
    uint64_t *r = reserve(n);
    std::memcpy(r, p, n*sizeof(uint64_t));
    return confirm_size(r, n, n);
}

#elif defined NEW

//=========================================================================
//=========================================================================
// The NEW code is intended to be a reasonably sensible implementation for
// use of this code free-standing within C++. Memory is allocated in units
// whose size is a power of 2, and I keep track of memory that I have used
// and discarded so that I do not need to go back to the system-provided
// allocator too often.
//=========================================================================
//=========================================================================


inline unsigned int log_next_power_of_2(size_t n);

INLINE_VAR uint64_t *freechain_table[64];

// The intent is that for most purposes freechains.allocate() and
// freechains.abandon() behave rather like malloc, save that they REQUIRE
// the user of the memory that is returned to avoid overwriting the first
// 32 bit word of the block. Note that this is a 32-bit chunk even on
// 64-bit machines.

class freechains
{
public:
// I keep my freechain table within an object so that the object can
// be created as the program starts up and particularly so that it will
// then be deleted on program termination. Its destructor can free all the
// memory that it is keeping track of.
    freechains()
    {   for (size_t i=0; i<64; i++) freechain_table[i] = NULL;
    }
    ~freechains()
    {   for (size_t i=0; i<64; i++)
        {   uint64_t *f = freechain_table[i];
            if (debug_arith)
// Report how many entries there are in the freechain.
            {   size_t n = 0;
// To arrange that double-free mistakes are detected I arrange to put -1
// in the initial word of any deleted block, so that all blocks on the
// freechains here should show that. I set and test for that in the other
// bits of code that allocate or release memory.
                for (uint64_t *b=f; b!=NULL; b = (uint64_t *)b[1])
                {   my_assert(b[0] == -(uint64_t)1);
                    n++;
                }
                if (n != 0)
                    std::cout << "Freechain " << i << " length: "
                              << n << std::endl;
            }
            while (f != NULL)
            {   uint64_t w = f[1];
                delete f;
                f = (uint64_t *)w;
            }
            freechain_table[i] = NULL;
        }
    }
// Finding the number of bits in n is achieved by counting the leading
// zeros in the representation of n, and on many platforms an intrinsic
// function will compile this into a single machine code instruction.
    static uint64_t *allocate(size_t n)
    {
// If I am debugging I will always allocate an extra word that will lie
// just beyond what would have been the end of my block, and I will
// fill everything with a pattern that might let me spot some cases where
// I write beyond the proper size of data.
        int bits = log_next_power_of_2(debug_arith ? n+1 : n);
        my_assert(n<=(((size_t)1)<<bits) && n>0);
        uint64_t *r = freechain_table[bits];
        if (r == NULL)
        {   r = new uint64_t[((size_t)1)<<bits];
            my_assert(r != NULL);
            if (debug_arith)
            {   std::memset(r, 0xaa, (((size_t)1)<<bits)*sizeof(uint64_t));
                r[0] = 0;
            }
        }
        else
        {   freechain_table[bits] = (uint64_t *)r[1];
            if (debug_arith)
            {   my_assert(r[0] == -(uint64_t)1);
                std::memset(r, 0xaa, (((size_t)1)<<bits)*sizeof(uint64_t));
                r[0] = 0;
            }
        }
// Just the first 32-bits of the header word record the clock capacity.
// The casts here look (and indeed are) ugly, but when I store data into
// memory as a 32-bit value that is how I will read it later on, and the
// messy notation here does not correspond to complicated computation.
        ((uint32_t *)r)[0] = bits;
        return r;
    }
// When I abandon a memory block I will push it onto a relevant free chain.
    static void abandon(uint64_t *p)
    {   my_assert(p[0] != -(uint64_t)1);
        int bits = ((uint32_t *)p)[0];
        my_assert(bits>0 && bits<48);
// Here I assume that sizeof(uint64_t) >= sizeof(intptr_t) so I am not
// risking loss of information.
        if (debug_arith) p[0] = -(uint64_t)1;
        p[1] = (uint64_t)freechain_table[bits];
        freechain_table[bits] = p;
    }
};

// Set up the object that manages the freechains. At program startup this
// fills freechain_table with NULL entries, and at the end of execution
// it frees all the memory blocks mentioned there.

INLINE_VAR freechains fc;

inline uint64_t *reserve(size_t n)
{   my_assert(n>0 && n<1000000);
    return &(fc.allocate(n+1))[1];
}

inline intptr_t confirm_size(uint64_t *p, size_t n, size_t final)
{   my_assert(final>0 && n>=final);
// Verify that the word just beyond where anything should have been
// stored has not been clobbered.
    if (debug_arith) my_assert(p[n] == 0xaaaaaaaaaaaaaaaaU);
    if (final == 1 && fits_into_fixnum((int64_t)p[0]))
    {   intptr_t r = int_to_handle((int64_t)p[0]);
        fc.abandon(&p[-1]);
        return r;
    }
// I compare the final size with the capacity and if it is a LOT smaller
// I allocate a new smaller block and copy the data across.
// That situation can most plausibly arise when two similar-values big
// numbers are subtracted.
    int bits = ((uint32_t *)(&p[-1]))[0]; 
    size_t capacity = ((size_t)1)<<bits;
    if (capacity > 4*final)
    {   uint64_t *w = fc.allocate(((size_t)1)<<log_next_power_of_2(final+1));
        memcpy(&w[1], p, final*sizeof(uint64_t));
        fc.abandon(&p[-1]);
        p = &w[1];
    }
    ((uint32_t *)(&p[-1]))[1] = final;
    return vector_to_handle(p);
}

inline intptr_t confirm_size_x(uint64_t *p, size_t n, size_t final)
{   my_assert(final>0 && n>=final);
    return confirm_size(p, n, final);
}

inline intptr_t vector_to_handle(uint64_t *p)
{   return (intptr_t)p;
}

inline uint64_t *vector_of_handle(intptr_t n)
{   return (uint64_t *)n;
}

inline size_t number_size(uint64_t *p)
{   size_t r = ((uint32_t *)(&p[-1]))[1];
    my_assert(r>0 && r<1000000);
    return ((uint32_t *)(&p[-1]))[1];
}

inline bool stored_as_fixnum(intptr_t a)
{    return ((int)a & 1) != 0;
}

constexpr inline int64_t int_of_handle(intptr_t a)
{   return ((int64_t)a & ~(int64_t)1)/2;
}

// This function should only ever be called in situations where the
// arithmetic indicated will not overflow.

inline intptr_t int_to_handle(int64_t n)
{   return (intptr_t)(2*n + 1);
}

// The following two lines are slighly delicate in that INTPTR_MIN and _MAX
// may not have the low tage bits to be proper fixnums. But if I implement
// int_of_handle so that it ignores tag bits that will be OK.

INLINE_VAR const int64_t MIN_FIXNUM = int_of_handle(INTPTR_MIN);
INLINE_VAR const int64_t MAX_FIXNUM = int_of_handle(INTPTR_MAX);

inline bool fits_into_fixnum(int64_t a)
{   return a>=MIN_FIXNUM && a<=MAX_FIXNUM;
}

inline void abandon(uint64_t *p)
{   fc.abandon(&p[-1]);
}

inline void abandon(intptr_t p)
{   if (!stored_as_fixnum(p) && p!=0)
    {   uint64_t *pp = vector_of_handle(p);
        fc.abandon(&pp[-1]);
    }
}

inline char *reserve_string(size_t n)
{    return new char[n+1];
}

inline char *confirm_size_string(char *p, size_t n, size_t final)
{   my_assert(final>0 && (n+9)>final);
    p[final] = 0;
    return p;
}

inline void abandon_string(char *s)
{   delete s;
}

// In the NEW case I will want to make all operations cope with both
// shorter integers (up to 63 bits) stored as the "handle", or genuine
// big numbers where the handle yields a pointer to a vector of 64-bit
// words. I do this by having a dispatch scheme that can activate code
// for each of the mixtures of representations.
//

// Dispatch as between mixed bignum and fixnum representations using some
// template stuff and classes.

template <class OP,class RES>
inline RES op_dispatch1(intptr_t a1)
{   if (stored_as_fixnum(a1)) return OP::op(int_of_handle(a1));
    else return OP::op(vector_of_handle(a1));
}

template <class OP,class RES>
inline RES op_dispatch1(intptr_t a1, int64_t n)
{   if (stored_as_fixnum(a1)) return OP::op(int_of_handle(a1), n);
    else return OP::op(vector_of_handle(a1), n);
}

template <class OP,class RES>
inline RES op_dispatch1(intptr_t a1, int32_t n)
{   if (stored_as_fixnum(a1)) return OP::op(int_of_handle(a1), (int64_t)n);
    else return OP::op(vector_of_handle(a1), (int64_t)n);
}

template <class OP,class RES>
inline RES op_dispatch2(intptr_t a1, intptr_t a2)
{   if (stored_as_fixnum(a1))
    {   if (stored_as_fixnum(a2))
            return OP::op(int_of_handle(a1), int_of_handle(a2));
        else return OP::op(int_of_handle(a1), vector_of_handle(a2));
    }
    else
    {   if (stored_as_fixnum(a2))
            return OP::op(vector_of_handle(a1), int_of_handle(a2));
        else return OP::op(vector_of_handle(a1), vector_of_handle(a2));
    }
}

inline intptr_t always_copy_bignum(uint64_t *p)
{   size_t n = ((uint32_t *)(&p[-1]))[1];
    uint64_t *r = reserve(n);
    std::memcpy(r, p, n*sizeof(uint64_t));
    return confirm_size(r, n, n);
}

inline intptr_t copy_if_no_garbage_collector(uint64_t *p)
{   size_t n = number_size(p);
    uint64_t *r = reserve(n);
    std::memcpy(r, p, n*sizeof(uint64_t));
    return confirm_size(r, n, n);
}

inline intptr_t copy_if_no_garbage_collector(intptr_t pp)
{   if (stored_as_fixnum(pp)) return pp;
    uint64_t *p = vector_of_handle(pp);
    size_t n = number_size(p);
    uint64_t *r = reserve(n);
    std::memcpy(r, p, n*sizeof(uint64_t));
    return confirm_size(r, n, n);
}

#elif defined LISP

//=========================================================================
//=========================================================================
// The LISP code is for incorporation in VSL.
//=========================================================================
//=========================================================================

inline uint64_t *reserve(size_t n)
{   my_assert(n>0 && n<1000000);
// I must allow for alignment padding on 32-bit platforms.
    if (sizeof(LispObject)==4) n = n*sizeof(uint64_t) + 4;
    else n = n*sizeof(uint64_t);
    LispObject a = allocateatom(n);
    return (uint64_t *)(a + 8 - tagATOM);
}

inline intptr_t confirm_size(uint64_t *p, size_t n, size_t final)
{   my_assert(final>0 && n>=final);
    if (final == 1 && fits_into_fixnum((int64_t)p[0]))
    {   intptr_t r = int_to_handle((int64_t)p[0]);
        return r;
    }
// If I am on a 32-bit system the data for a bignum is 8 bit aligned and
// that leaves a 4-byte gat after the header. In such a case I will write
// in a zero just to keep memory tidy.
    if (sizeof(LispObject) == 4) p[-1] = 0;
    *((LispObject *)&p[-1]) =
        tagHDR + typeBIGNUM + packlength(final*sizeof(uint64_t));
// Here I should reset fringe down by (final-n) perhaps. Think about that
// later!
    return vector_to_handle(p);
}

inline intptr_t confirm_size_x(uint64_t *p, size_t n, size_t final)
{   my_assert(final>0 && n>=final);
// Here I might need to write a nice dummy object into the gap left by
// shrinking the object.
    return confirm_size(p, n, final);
}

inline intptr_t vector_to_handle(uint64_t *p)
{   return (intptr_t)p - 8 + tagATOM;
}

inline uint64_t *vector_of_handle(intptr_t n)
{   return (uint64_t *)(n + 8 - tagATOM);
}

inline size_t number_size(uint64_t *p)
{   uintptr_t h = *(uintptr_t *)&p[-1];
    size_t r = veclength(h);
    if (sizeof(LispObject) == 4) r -= 4;
    r = r/sizeof(uint64_t);
    my_assert(r>0 && r<1000000);
    return r;
}

inline bool stored_as_fixnum(intptr_t a)
{    return isFIXNUM(a);
}

constexpr inline int64_t int_of_handle(intptr_t a)
{   return qfixnum(a);
}

inline intptr_t int_to_handle(int64_t n)
{   return packfixnum(n);
}

INLINE_VAR const int64_t MIN_FIXNUM = int_of_handle(INTPTR_MIN);
INLINE_VAR const int64_t MAX_FIXNUM = int_of_handle(INTPTR_MAX);

inline bool fits_into_fixnum(int64_t a)
{   return a>=MIN_FIXNUM && a<=MAX_FIXNUM;
}

inline void abandon(uint64_t *p)
{   // No need to do anything! But MIGHT reset fringe pointer?
}

inline void abandon(intptr_t p)
{   if (!stored_as_fixnum(p) && p!=0)
    {   uint64_t *pp = vector_of_handle(p);
        abandon(pp);
    }
}

inline char *reserve_string(size_t n)
{    LispObject a = allocateatom(n);
     return (char *)(a - tagATOM + sizeof(LispObject));
}

inline LispObject confirm_size_string(char *p, size_t n, size_t final)
{   LispObject *a = (LispObject *)(p - sizeof(LispObject));
    *a = tagHDR + typeSTRING + packlength(final);
    return (LispObject)a +tagATOM;
}

inline void abandon_string(string_handle s)
{   // Do nothing.
}

template <class OP,class RES>
inline RES op_dispatch1(intptr_t a1)
{   if (stored_as_fixnum(a1)) return OP::op(int_of_handle(a1));
    else return OP::op(vector_of_handle(a1));
}

template <class OP,class RES>
inline RES op_dispatch1(intptr_t a1, int64_t n)
{   if (stored_as_fixnum(a1)) return OP::op(int_of_handle(a1), n);
    else return OP::op(vector_of_handle(a1), n);
}

template <class OP,class RES>
inline RES op_dispatch1(intptr_t a1, int32_t n)
{   if (stored_as_fixnum(a1)) return OP::op(int_of_handle(a1), (int64_t)n);
    else return OP::op(vector_of_handle(a1), (int64_t)n);
}

template <class OP,class RES>
inline RES op_dispatch2(intptr_t a1, intptr_t a2)
{   if (stored_as_fixnum(a1))
    {   if (stored_as_fixnum(a2))
            return OP::op(int_of_handle(a1), int_of_handle(a2));
        else return OP::op(int_of_handle(a1), vector_of_handle(a2));
    }
    else
    {   if (stored_as_fixnum(a2))
            return OP::op(vector_of_handle(a1), int_of_handle(a2));
        else return OP::op(vector_of_handle(a1), vector_of_handle(a2));
    }
}

inline intptr_t always_copy_bignum(uint64_t *p)
{   size_t n = number_size(p);
    uint64_t *r = reserve(n);
    std::memcpy(r, p, n*sizeof(uint64_t));
    return confirm_size(r, n, n);
}

inline intptr_t copy_if_no_garbage_collector(uint64_t *p)
{   size_t n = number_size(p);
    uint64_t *r = reserve(n);
    std::memcpy(r, p, n*sizeof(uint64_t));
    return confirm_size(r, n, n);
}

inline intptr_t copy_if_no_garbage_collector(intptr_t pp)
{   if (stored_as_fixnum(pp)) return pp;
    uint64_t *p = vector_of_handle(pp);
    size_t n = number_size(p);
    uint64_t *r = reserve(n);
    std::memcpy(r, p, n*sizeof(uint64_t));
    return confirm_size(r, n, n);
}


#else // none if MALLOC, LISP or NEW specified.
#error Unspecified memory model
#endif

// The main arithmetic operations are supported by code that can work on
// Bignums stored as vectors of digits or on Fixnums represented as (tagged)
// immediate data, or as mixtures. For each operation there is a class, and
// methods called "op" within it deal with the various cases via overloading.

class Plus
{   public:
    static intptr_t op(int64_t, int64_t);
    static intptr_t op(int64_t, uint64_t *);
    static intptr_t op(uint64_t *, int64_t);
    static intptr_t op(uint64_t *, uint64_t *);
};

inline intptr_t bigplus_small(intptr_t, int64_t);

class Difference
{   public:
    static intptr_t op(int64_t, int64_t);
    static intptr_t op(int64_t, uint64_t *);
    static intptr_t op(uint64_t *, int64_t);
    static intptr_t op(uint64_t *, uint64_t *);
};

class Revdifference
{   public:
    static intptr_t op(int64_t, int64_t);
    static intptr_t op(int64_t, uint64_t *);
    static intptr_t op(uint64_t *, int64_t);
    static intptr_t op(uint64_t *, uint64_t *);
};

class Times
{   public:
    static intptr_t op(int64_t, int64_t);
    static intptr_t op(int64_t, uint64_t *);
    static intptr_t op(uint64_t *, int64_t);
    static intptr_t op(uint64_t *, uint64_t *);
};

class Quotient
{   public:
    static intptr_t op(int64_t, int64_t);
    static intptr_t op(int64_t, uint64_t *);
    static intptr_t op(uint64_t *, int64_t);
    static intptr_t op(uint64_t *, uint64_t *);
};

class Remainder
{   public:
    static intptr_t op(int64_t, int64_t);
    static intptr_t op(int64_t, uint64_t *);
    static intptr_t op(uint64_t *, int64_t);
    static intptr_t op(uint64_t *, uint64_t *);
};

class Divide
{   public:
    static intptr_t op(int64_t, int64_t);
    static intptr_t op(int64_t, uint64_t *);
    static intptr_t op(uint64_t *, int64_t);
    static intptr_t op(uint64_t *, uint64_t *);
    static intptr_t op(int64_t, int64_t, intptr_t &);
    static intptr_t op(int64_t, uint64_t *, intptr_t &);
    static intptr_t op(uint64_t *, int64_t, intptr_t &);
    static intptr_t op(uint64_t *, uint64_t *, intptr_t &);
};

class Gcd
{   public:
    static intptr_t op(int64_t, int64_t);
    static intptr_t op(int64_t, uint64_t *);
    static intptr_t op(uint64_t *, int64_t);
    static intptr_t op(uint64_t *, uint64_t *);
};

class Lcm
{   public:
    static intptr_t op(int64_t, int64_t);
    static intptr_t op(int64_t, uint64_t *);
    static intptr_t op(uint64_t *, int64_t);
    static intptr_t op(uint64_t *, uint64_t *);
};

class Logand
{   public:
    static intptr_t op(int64_t, int64_t);
    static intptr_t op(int64_t, uint64_t *);
    static intptr_t op(uint64_t *, int64_t);
    static intptr_t op(uint64_t *, uint64_t *);
};

class Logor
{   public:
    static intptr_t op(int64_t, int64_t);
    static intptr_t op(int64_t, uint64_t *);
    static intptr_t op(uint64_t *, int64_t);
    static intptr_t op(uint64_t *, uint64_t *);
};

class Logxor
{   public:
    static intptr_t op(int64_t, int64_t);
    static intptr_t op(int64_t, uint64_t *);
    static intptr_t op(uint64_t *, int64_t);
    static intptr_t op(uint64_t *, uint64_t *);
};

class Zerop
{   public:
    static bool op(int64_t);
    static bool op(uint64_t *);
};

class Onep
{   public:
    static bool op(int64_t);
    static bool op(uint64_t *);
};

class Minusp
{   public:
    static bool op(int64_t);
    static bool op(uint64_t *);
};

class Evenp
{   public:
    static bool op(int64_t);
    static bool op(uint64_t *);
};

class Oddp
{   public:
    static bool op(int64_t);
    static bool op(uint64_t *);
};

class Eqn
{   public:
    static bool op(int64_t, int64_t);
    static bool op(int64_t, uint64_t *);
    static bool op(uint64_t *, int64_t);
    static bool op(uint64_t *, uint64_t *);
};

class Geq
{   public:
    static bool op(int64_t, int64_t);
    static bool op(int64_t, uint64_t *);
    static bool op(uint64_t *, int64_t);
    static bool op(uint64_t *, uint64_t *);
};

class Greaterp
{   public:
    static bool op(int64_t, int64_t);
    static bool op(int64_t, uint64_t *);
    static bool op(uint64_t *, int64_t);
    static bool op(uint64_t *, uint64_t *);
};

class Leq
{   public:
    static bool op(int64_t, int64_t);
    static bool op(int64_t, uint64_t *);
    static bool op(uint64_t *, int64_t);
    static bool op(uint64_t *, uint64_t *);
};

class Lessp
{   public:
    static bool op(int64_t, int64_t);
    static bool op(int64_t, uint64_t *);
    static bool op(uint64_t *, int64_t);
    static bool op(uint64_t *, uint64_t *);
};

class Add1
{   public:
    static intptr_t op(int64_t);
    static intptr_t op(uint64_t *);
};

class Sub1
{   public:
    static intptr_t op(int64_t);
    static intptr_t op(uint64_t *);
};

class Minus
{   public:
    static intptr_t op(int64_t);
    static intptr_t op(uint64_t *);
};

class Abs
{   public:
    static intptr_t op(int64_t);
    static intptr_t op(uint64_t *);
};

class Square
{   public:
    static intptr_t op(int64_t);
    static intptr_t op(uint64_t *);
};

class Isqrt
{   public:
    static intptr_t op(int64_t);
    static intptr_t op(uint64_t *);
};

class Lognot
{   public:
    static intptr_t op(int64_t);
    static intptr_t op(uint64_t *);
};

// Pow and the shifts have a second argument that must always be of
// type int64_t.

class Pow
{   public:
    static intptr_t op(int64_t, int64_t);
    static intptr_t op(uint64_t *, int64_t);
    static intptr_t op(int64_t, int32_t);
    static intptr_t op(uint64_t *, int32_t);
};

class Leftshift
{   public:
    static intptr_t op(int64_t, int64_t);
    static intptr_t op(uint64_t *, int64_t);
    static intptr_t op(int64_t, int32_t);
    static intptr_t op(uint64_t *, int32_t);
};

class Rightshift
{   public:
    static intptr_t op(int64_t, int64_t);
    static intptr_t op(uint64_t *, int64_t);
    static intptr_t op(int64_t, int32_t);
    static intptr_t op(uint64_t *, int32_t);
};

class Integer_length
{   public:
    static size_t op(int64_t);
    static size_t op(uint64_t *);
};

class Logbitp
{   public:
    static bool op(int64_t, size_t);
    static bool op(uint64_t *, size_t);
};

class Logcount
{   public:
    static size_t op(int64_t);
    static size_t op(uint64_t *);
};

class Float
{   public:
    static float op(int64_t);
    static float op(uint64_t *);
};

class Double
{   public:
    static double op(int64_t);
    static double op(uint64_t *);
};

#ifdef softfloat_h

class Float128
{   public:
    static float128_t op(int64_t);
    static float128_t op(uint64_t *);
};

#endif

inline string_handle bignum_to_string(intptr_t aa);
inline string_handle bignum_to_string_hex(intptr_t aa);
inline string_handle bignum_to_string_octal(intptr_t aa);
inline string_handle bignum_to_string_binary(intptr_t aa);

class Bignum;

inline void display(const char *label, const uint64_t *a, size_t lena);
inline void display(const char *label, intptr_t a);
inline void display(const char *label, const Bignum &a);


//=========================================================================
//=========================================================================
// I have a class Bignum that wraps up the representation of a number
// and then allows me to overload most operators so that big numbers can be
// used in C++ code anmost as if they were a natural proper type. The main
// big oddity will be that to denote a Bignum literal it will be necessary
// to use a constructor, with obvious constructors accepting integers of up
// to 64-bits and a perhaps less obvious one taking a string that is the
// decimal denotation of the integer concerned.
//=========================================================================
//=========================================================================

class Bignum
{
public:
// a Bignum only had one data field, and that is simple plain data.
    intptr_t val;


// A default constructor build a Bignum with no stored data.
    Bignum()
    {   val = 0;
    }
// In the next constructor the boolean argument is not used at run time but
// serves to indicate which constructure is wanted.
    Bignum(bool set_val, intptr_t v)
    {   val = v;
    }
    ~Bignum()
    {   abandon(val);
        val = 0;
    }
    Bignum(uint64_t *p)
    {   val = vector_to_handle(p);
    }
    Bignum(int32_t n)
    {   val = int_to_bignum((int64_t)n);
    }
    Bignum(int64_t n)
    {   val = int_to_bignum(n);
    }
    Bignum(double d)
    {   val = double_to_bignum(d);
    }
    Bignum(float d)
    {   val = double_to_bignum((double)d);
    }
    Bignum(const char *s)
    {   val = string_to_bignum(s);
    }
    Bignum(const Bignum &a)
    {
// On the next line in the NEW case (at least) the handle might be a handle
// of an immediate fixnum, and in that case a.vec() should be considered
// an invalid operation and this code will need to be adjusted!
        val = copy_if_no_garbage_collector(a.val);
    }

    uint64_t *vec() const
    {   return vector_of_handle(val);
    }

    inline void operator = (const Bignum &x)
    {   if (this == &x) return; // assign to self - a silly case!
        abandon(val);
// See comment in the copy constructor.
        val = copy_if_no_garbage_collector(x.val);
    }

    inline void operator = (const int64_t x)
    {   abandon(val);
        val = int_to_bignum(x);
    }

    inline void operator = (const uint64_t x)
    {   abandon(val);
        val = unsigned_int_to_bignum(x);
    }

    inline void operator = (const int32_t x)
    {   abandon(val);
        val = int_to_bignum((int64_t)x);
    }

    inline void operator = (const uint32_t x)
    {   abandon(val);
        val = int_to_bignum((int64_t)x);
    }

    inline void operator = (const char *x)
    {   abandon(val);
        val = string_to_bignum(x);
    }

    inline Bignum operator +(const Bignum &x) const
    {   return Bignum(true, op_dispatch2<Plus,intptr_t>(val, x.val));
    }

    inline Bignum operator -(const Bignum &x) const
    {   return Bignum(true, op_dispatch2<Difference,intptr_t>(val, x.val));
    }

    inline Bignum operator *(const Bignum &x) const
    {   return Bignum(true, op_dispatch2<Times,intptr_t>(val, x.val));
    }

    inline Bignum operator /(const Bignum &x) const
    {   return Bignum(true, op_dispatch2<Quotient,intptr_t>(val, x.val));
    }

    inline Bignum operator %(const Bignum &x) const
    {   return Bignum(true, op_dispatch2<Remainder,intptr_t>(val, x.val));
    }

    inline Bignum operator -() const
    {   return Bignum(true, op_dispatch1<Minus,intptr_t>(val));
    }

    inline Bignum operator &(const Bignum &x) const
    {   return Bignum(true, op_dispatch2<Logand,intptr_t>(val, x.val));
    }

    inline Bignum operator |(const Bignum &x) const
    {   return Bignum(true, op_dispatch2<Logor,intptr_t>(val, x.val));
    }

    inline Bignum operator ^(const Bignum &x) const
    {   return Bignum(true, op_dispatch2<Logxor,intptr_t>(val, x.val));
    }

    inline Bignum operator <<(int n) const
    {   return Bignum(true, op_dispatch1<Leftshift,intptr_t>(val, n));
    }

    inline Bignum operator >>(int n) const
    {   return Bignum(true, op_dispatch1<Rightshift,intptr_t>(val, n));
    }

    inline Bignum operator ~() const
    {   return Bignum(true, op_dispatch1<Lognot,intptr_t>(val));
    }

    inline bool operator ==(const Bignum &x) const
    {   return op_dispatch2<Eqn,bool>(val, x.val);
    }
    inline bool operator !=(const Bignum &x) const
    {   return !op_dispatch2<Eqn,bool>(val, x.val);
    }

    inline bool operator >(const Bignum &x) const
    {   return op_dispatch2<Greaterp,bool>(val, x.val);
    }

    inline bool operator >=(const Bignum &x) const
    {   return op_dispatch2<Geq,bool>(val, x.val);
    }

    inline bool operator <(const Bignum &x) const
    {   return op_dispatch2<Lessp,bool>(val, x.val);
    }

    inline bool operator <=(const Bignum &x) const
    {   return op_dispatch2<Leq,bool>(val, x.val);
    }

    inline void operator +=(const Bignum &x)
    {   intptr_t r = op_dispatch2<Plus,intptr_t>(val, x.val);
        abandon(val);
        val = r;
    }

    inline void operator -=(const Bignum &x)
    {   intptr_t r = op_dispatch2<Difference,intptr_t>(val, x.val);
        abandon(val);
        val = r;
    }

    inline void operator *=(const Bignum &x)
    {   intptr_t r = op_dispatch2<Times,intptr_t>(val, x.val);
        abandon(val);
        val = r;
    }

    inline void operator /=(const Bignum &x)
    {   intptr_t r = op_dispatch2<Quotient,intptr_t>(val, x.val);
        abandon(val);
        val = r;
    }

    inline void operator %=(const Bignum &x)
    {   intptr_t r = op_dispatch2<Remainder,intptr_t>(val, x.val);
        abandon(val);
        val = r;
    } 

    inline void operator &=(const Bignum &x)
    {   intptr_t r = op_dispatch2<Logand,intptr_t>(val, x.val);
        abandon(val);
        val = r;
    } 

    inline void operator |=(const Bignum &x)
    {   intptr_t r = op_dispatch2<Logor,intptr_t>(val, x.val);
        abandon(val);
        val = r;
    } 

    inline void operator ^=(const Bignum &x)
    {   intptr_t r = op_dispatch2<Logxor,intptr_t>(val, x.val);
        abandon(val);
        val = r;
    } 

    inline void operator <<=(int n)
    {   intptr_t r = op_dispatch1<Leftshift,intptr_t>(val, n);
        abandon(val);
        val = r;
    }

    inline void operator >>=(int n)
    {   intptr_t r = op_dispatch1<Rightshift,intptr_t>(val, n);
        abandon(val);
        val = r;
    }

    inline Bignum operator ++()
    {   intptr_t r = bigplus_small(val, 1);
        abandon(val);
        val = r;
        return *this;
    }

    inline Bignum operator ++(int)
    {   intptr_t r = bigplus_small(val, 1);
// I assign explicitly to oldval.val because trying to use a constructor
// of Bignum or assigning to one would so things more complicated than I want!
        Bignum oldval;
        oldval.val = val;
        val = r;
        return oldval;
    }

    inline Bignum operator --()
    {   intptr_t r = bigplus_small(val, -1);
        abandon(val);
        val = r;
        return *this;
    }

    inline Bignum operator --(int)
    {   intptr_t r = bigplus_small(val, -1);
        Bignum oldval;
        oldval.val = val;
        val = r;
        return oldval;
    }

    friend std::ostream & operator << (std::ostream &out, const Bignum &a)
    {   std::ios_base::fmtflags fg = out.flags();
#ifdef LISP
        LispObject s;
#else
        char *s;
#endif
        if ((fg & std::ios_base::hex) != 0)
            s = bignum_to_string_hex(a.val);
        else if ((fg & std::ios_base::oct) != 0)
            s = bignum_to_string_octal(a.val);
        else if ((fg & std::ios_base::dec) != 0)
            s = bignum_to_string(a.val);
        else if (radix::is_binary_output(out))
            s = bignum_to_string_binary(a.val);
        else s = bignum_to_string(a.val);
#ifdef LISP
        std::string ss(s, veclength(qheader(s)));
        out << ss;
#else
        out << s;
#endif
        abandon_string(s);
        return out;
    }
    friend std::istream & operator >> (std::istream &in, Bignum &a)
    {   int64_t n;
// What I really want to do is to read in a string of digits and then
// use string_to_bignum().
        in >> n;
        abandon(a.val);
        a.val = int_to_bignum(n);
        return in;
    }
};

inline const string_handle to_string(Bignum x)
{   return bignum_to_string(x.val);
}

inline Bignum uniform_positive_bignum(size_t n)
{   return Bignum(true, uniform_positive(n));
}

inline Bignum uniform_signed_bignum(size_t n)
{   return Bignum(true, uniform_signed(n));
}

inline Bignum uniform_upto_bignum(Bignum a)
{   return Bignum(true, uniform_upto(a.val));
}

inline Bignum fudge_distribution_bignum(Bignum a, int n)
{   return Bignum(true, fudge_distribution(a.val, n));
}

inline Bignum random_upto_bits_bignum(size_t n)
{   return Bignum(true, random_upto_bits(n));
}

inline Bignum square(const Bignum &x)
{   return Bignum(true, op_dispatch1<Square,intptr_t>(x.val));
}

inline Bignum isqrt(const Bignum &x)
{   return Bignum(true, op_dispatch1<Isqrt,intptr_t>(x.val));
}

inline Bignum abs(const Bignum &x)
{   return Bignum(true, op_dispatch1<Abs,intptr_t>(x.val));
}

inline bool zerop(const Bignum &x)
{   return op_dispatch1<Zerop,bool>(x.val);
}

inline bool onep(const Bignum &x)
{   return op_dispatch1<Onep,bool>(x.val);
}

inline bool minusp(const Bignum &x)
{   return op_dispatch1<Minusp,bool>(x.val);
}

inline bool evenp(const Bignum &x)
{   return op_dispatch1<Evenp,bool>(x.val);
}

inline bool oddp(const Bignum &x)
{   return op_dispatch1<Oddp,bool>(x.val);
}

inline Bignum pow(const Bignum &x, int64_t n)
{   if (n == 0) return Bignum(true, int_to_bignum(1));
    else if (n == 1) return Bignum(true, copy_if_no_garbage_collector(x.val));
    else if (n == 2) return square(x);
    else return Bignum(true, op_dispatch1<Pow,intptr_t>(x.val, n));
}

inline Bignum pow(const Bignum &x, int32_t n)
{   return pow(x, (int64_t)n);
}

inline Bignum gcd(const Bignum &x, const Bignum &y)
{   return Bignum(true, op_dispatch2<Gcd,intptr_t>(x.val, y.val));
}

inline Bignum lcm(const Bignum &x, const Bignum &y)
{   return Bignum(true, op_dispatch2<Lcm,intptr_t>(x.val, y.val));
}

inline Bignum fix_bignum(double d)
{   return Bignum(true, double_to_bignum(d));
}

inline Bignum floor_bignum(double d)
{   return Bignum(true, double_to_floor(d));
}

inline Bignum ceil_bignum(double d)
{   return Bignum(true, double_to_ceiling(d));
}

inline Bignum fix_bignum(float d)
{   return fix_bignum((double)d);
}

inline Bignum floor_bignum(float d)
{   return floor_bignum((double)d);
}

inline Bignum ceil_bignum(float d)
{   return ceil_bignum((double)d);
}

inline double float_bignum(const Bignum &x)
{   return op_dispatch1<Float,float>(x.val);
}

inline double double_bignum(const Bignum &x)
{   return op_dispatch1<Double,double>(x.val);
}

#ifdef softfloat_h

inline double float128_bignum(const Bignum &x)
{   return op_dispatch1<Float128,float128_t>(x.val);
}

#endif

//=========================================================================
//=========================================================================
// display() will show the internal representation of a bignum as a
// sequence of hex values. This is obviously useful while debugging!
// I make this inline solely because that gets rid of warnings about an
// unused static function!
//=========================================================================
//=========================================================================

inline void display(const char *label, const uint64_t *a, size_t lena)
{   std::cout << label << " [" << (int)lena << "]";
    for (size_t i=0; i<lena; i++)
        std::cout << " "
                  << std::hex << std::setfill('0')
                  << std::setw(16) << a[lena-i-1]
                  << std::dec << std::setw(0);
    std::cout << std::endl;
}

inline void display(const char *label, intptr_t a)
{   if (stored_as_fixnum(a))
    {   std::cout << label << " [fixnum] " << std::hex
                  << a << std::dec << " = "
                  << int_of_handle(a) << std::endl;
        return;
    }
    uint64_t *d = vector_of_handle(a);
    size_t len = number_size(d);
    std::cout << label << " [" << (int)len << "]";
    for (size_t i=0; i<len; i++)
        std::cout << " "
                  << std::hex << std::setfill('0')
                  << std::setw(16) << d[len-i-1]
                  << std::dec << std::setw(0);
    std::cout << std::endl;
}

inline void display(const char *label, const Bignum &a)
{   display(label, a.val);
}


//=========================================================================
//=========================================================================
// I will have a collection of low level functions that support the
// fundamental operations needed for implementing big-number arithmetic:
// add-with-carry, multiplication and division.
//=========================================================================
//=========================================================================

#ifdef __GNUC__

// Note that __GNUC__ also gets defined by clang on the Macintosh, so
// this code is probably optimized there too. This must NEVER be called
// with a zero argument.

// Count the leading zeros in a 64-bit word.

inline int nlz(uint64_t x)
{   return __builtin_clzll(x);  // Must use the 64-bit version of clz.
}

inline int popcount(uint64_t x)
{   return __builtin_popcountll(x);
}

#else // __GNUC__

inline int nlz(uint64_t x)
{   int n = 0;
    if (x <= 0x00000000FFFFFFFFU) {n = n +32; x = x <<32;}
    if (x <= 0x0000FFFFFFFFFFFFU) {n = n +16; x = x <<16;}
    if (x <= 0x00FFFFFFFFFFFFFFU) {n = n + 8; x = x << 8;}
    if (x <= 0x0FFFFFFFFFFFFFFFU) {n = n + 4; x = x << 4;}
    if (x <= 0x3FFFFFFFFFFFFFFFU) {n = n + 2; x = x << 2;}
    if (x <= 0x7FFFFFFFFFFFFFFFU) {n = n + 1;}
    return n;
}

inline int popcount(uint64_t x)
{   x = (x & 0x5555555555555555U) + (x >> 1 & 0x5555555555555555U);
    x = (x & 0x3333333333333333U) + (x >> 2 & 0x3333333333333333U);
    x = x + (x >> 4) & 0x0f0f0f0f0f0f0f0fU;
    x = x + (x >> 8);
    x = x + (x >> 16);
    x = x + (x >> 32) & 0x7f;
}

#endif // __GNUC__

// Round a size_t integer up to the next higher power of 2.
// I do this based on counting the number of leading zeros in the
// binary representation of n-1.

inline size_t next_power_of_2(size_t n)
{   return ((size_t)1) << (64-nlz((uint64_t)(n-1)));
}

inline unsigned int log_next_power_of_2(size_t n)
{   return (64-nlz((uint64_t)(n-1)));
}

// I am going to represent bignums as arrays of 64-bit digits.
// Overall the representation will use 2s complement, and so all but the
// top digit will be treated as unsigned, while the top one is signed
// and the whole number must act as if it had its sign bit propagated
// indefinitely to the left. When I pass numbers to the low level
// code I will pass references to the input arrays and lengths. I will
// pass an arrange that will certainly be large enough to hold the result
// and the arithmetic functions will return the length in it that is used.
// This length will be such that the overall number does not have any
// extraneous leading zeros or leading 0xffffffffffffffff words, save that
// the value zero will be returned as a single word value not a no-word
// one. Note the word "extraneous", because the positive value 2^64-1
// will be represented as a 2-word item with 0 in the higher digit and
// 0xffffffffffffffff in the lower one - the leading zero is needed so
// that it is clear that the value is positive. A consequence of all this
// is that any bignum with length 1 can be extracted as an int64_t without
// loss.

// I want "add-with-carry" operations, and so I provide a function here to
// implement it. If the C++ compiler had a nice intrinsic I would like
// to use that! Well Intel compilers have an _addcarry_u64 that passes and
// returns the carry in an unsigned char and uses a pointer not a reference
// argument for passing back the result.

// a1 and a2 are 64-bit unsigned integers. While c_in is also that type it
// must only have one of the values 0 or 1. The effect will be to set r to
// the low 64-bits of a1+a2+c_in and return any carry that is generated.

inline uint64_t add_with_carry(uint64_t a1, uint64_t a2,
                               uint64_t c_in, uint64_t &r)
{   uint64_t w = a1 + c_in;
    if (w < c_in) // carry generated here. In this case the only possibility
    {             // was that a1 was allbits and c_in was 1, and so we have
                  // ended up with w==0. So the result sum is just a2 and the
                  // carry out will be 1
        r = a2;
        return 1;
    }
    r = w = w + a2;
    return (w < a2 ? 1 : 0);
}

// I have an overload of add_with_carry for use where it is known that
// the input carry is zero. That cases saves a small amount of work.

inline uint64_t add_with_carry(uint64_t a1, uint64_t a2, uint64_t &r)
{   uint64_t w;
    r = w = a1 + a2;
    return (w < a1 ? 1 : 0);
}

#ifdef __SIZEOF_INT128__

// Well it seems that g++ and clang have different views about how to
// ask for unsigned 128-bit integers! So I abstract that away via a typedef
// called UNIT128.

#ifdef __CLANG__
typedef __int128  INT128;
typedef __uint128 UINT128;
#else // __CLANG__
typedef __int128  INT128;
typedef unsigned __int128 UINT128;
#endif // __CLANG__

// At least for debugging I may wish to display 128-bit integers. Here I
// only do hex printing. I could do decimal and octal if I really wanted
// but just for debugging that does not seem vital. If some C++ compiler
// already supported printing of 128-bit ints this definition might clash
// and would need commenting out.

inline std::ostream & operator << (std::ostream &out, UINT128 a)
{   out << std::hex << std::setw(16) << std::setfill('0') <<(uint64_t)(a>>64)
        << " "
        << (uint64_t)a << std::dec << std::setw(0) << std::setfill(' '); 
    return out;
}

inline UINT128 pack128(uint64_t hi, uint64_t lo)
{   return (((UINT128)hi)<<64) | lo;
}

// I want code that will multiply two 64-bit values and yield a 128-bit
// result. The result must be expressed as a pair of 64-bit integers.
// If I have a type "__int128", as will often be the case when using gcc,
// this is very easy to express. Otherwise I split the two inputs into
// 32-bit halves, do 4 multiplications and some additions to construct
// the result. At least I can keep the code portable, even if I can then
// worry about performance a bit.


inline void multiply64(uint64_t a, uint64_t b,
                       uint64_t &hi, uint64_t &lo)
{   UINT128 r = (UINT128)a*(UINT128)b;
    hi = (uint64_t)(r >> 64);
    lo = (uint64_t)r;
}

// Now much the same but forming a*b+c. Note that this can not overflow
// the 128-bit result. Both hi and lo are only updated at the end
// of this, and so they are allowed to be the same as other arguments.

inline void multiplyadd64(uint64_t a, uint64_t b, uint64_t c,
                          uint64_t &hi, uint64_t &lo)
{   UINT128 r = (UINT128)a*(UINT128)b +
                          (UINT128)c;
    hi = (uint64_t)(r >> 64);
    lo = (uint64_t)r;
}

// divide (hi,lo) by divisor and generate a quotient and a remainder. The
// version of the code that is able to use __int128 can serve as clean
// documentation of the intent.

inline void divide64(uint64_t hi, uint64_t lo, uint64_t divisor,
                     uint64_t &q, uint64_t &r)
{   my_assert(divisor != 0 && hi < divisor);
    UINT128 dividend = pack128(hi, lo);
    q = dividend / divisor;
    r = dividend % divisor;
}

#else // __SIZEOF_INT128__

// If the C++ system I am using does not support and 128-bit integer
// type or if I have not detected it everything can still be done using
// lots of 64-bit operations, with each 64-bit value often treated as
// two 32-bit halves.

inline void multiply64(uint64_t a, uint64_t b,
                       uint64_t &hi, uint64_t &lo)
{   uint64_t a1 = a >> 32,           // top half
             a0 = a & 0xFFFFFFFFU;   // low half
    uint64_t b1 = b >> 32,           // top half
             b0 = b & 0xFFFFFFFFU;   // low half
    uint64_t u1 = a1*b1,             // top of result
             u0 = a0*b0;             // bottom of result
// Now I need to add in the two "middle" bits a0*b1 and a1*b0
    uint64_t w = a0*b1;
    u1 += w >> 32;
    w <<= 32;
    u0 += w;
    if (u0 < w) u1++;
// a0*b1 done
    w = a1*b0;
    u1 += w >> 32;
    w <<= 32;
    u0 += w;
    if (u0 < w) u1++;
    hi = u1;
    lo = u0;
}

// Now much the same but forming a*b+c. Note that this can not overflow
// the 128-bit result. Both hi and lo are only updated at the end
// of this, and so they are allowed to be the same as other arguments.

inline void multiplyadd64(uint64_t a, uint64_t b, uint64_t c,
                          uint64_t &hi, uint64_t &lo)
{   uint64_t a1 = a >> 32,           // top half
             a0 = a & 0xFFFFFFFFU;   // low half
    uint64_t b1 = b >> 32,           // top half
             b0 = b & 0xFFFFFFFFU;   // low half
    uint64_t u1 = a1*b1,             // top of result
             u0 = a0*b0;             // bottom of result
// Now I need to add in the two "middle" bits a0*b1 and a1*b0
    uint64_t w = a0*b1;
    u1 += w >> 32;
    w <<= 32;
    u0 += w;
    if (u0 < w) u1++;
// a0*b1 done
    w = a1*b0;
    u1 += w >> 32;
    w <<= 32;
    u0 += w;
    if (u0 < w) u1++;
    u0 += c;                         // add in C.
    if (u0 < c) u1++; 
    hi = u1;
    lo = u0;
}

inline void divide64(uint64_t hi, uint64_t lo, uint64_t divisor,
                     uint64_t &q, uint64_t &r)
{   my_assert(divisor != 0 && hi < divisor);
    uint64_t u1 = hi;
    uint64_t u0 = lo;
    uint64_t c = divisor;
// See the Hacker's Delight for commentary about what follows. The associated
// web-site explains usage rights:
// "You are free to use, copy, and distribute any of the code on this web
// site (www.hackersdelight.org) , whether modified by you or not. You need
// not give attribution. This includes the algorithms (some of which appear
// in Hacker's Delight), the Hacker's Assistant, and any code submitted by
// readers. Submitters implicitly agree to this." and then "The author has
// taken care in the preparation of this material, but makes no expressed
// or implied warranty of any kind and assumes no responsibility for errors
// or omissions. No liability is assumed for incidental or consequential
// damages in connection with or arising out of the use of the information
// or programs contained herein."
// I may not be obliged to give attribution, but I view it as polite to!
// Any error that have crept in in my adapaptation of the original code
// will be my fault, but you see in the BSD license at the top of this
// file that I disclaim any possible liability for consequent loss or damage.
    const uint64_t base = 0x100000000U; // Number base (32 bits).
    uint64_t un1, un0,        // Norm. dividend LSD's.
             vn1, vn0,        // Norm. divisor digits.
             q1, q0,          // Quotient digits.
             un32, un21, un10,// Dividend digit pairs.
             rhat;            // A remainder.
// I am going to shift both operands left until the divisor has its
// most significant bit set.
    int s = nlz(c);           // Shift amount for norm. 0 <= s <= 63.
    c = c << s;               // Normalize divisor.
// Now I split the divisor from a single 64-bit number into a pair
// of 32-vit values.
    vn1 = c >> 32;            // Break divisor up into
    vn0 = c & 0xFFFFFFFFU;    // two 32-bit digits.
// Shift the dividend... and split it into parts.
    if (s == 0) un32 = u1;
    else un32 = (u1 << s) | (u0 >> (64 - s));
    un10 = u0 << s;           // Shift dividend left.
    un1 = un10 >> 32;         // Break right half of
    un0 = un10 & 0xFFFFFFFFU; // dividend into two digits.
// Predict a 32-bit quotient digit...
    q1 = un32/vn1;            // Compute the first
    rhat = un32 - q1*vn1;     // quotient digit, q1.
again1:
    if (q1 >= base || q1*vn0 > base*rhat + un1)
    {   q1 = q1 - 1;
        rhat = rhat + vn1;
        if (rhat < base) goto again1;
    }
    un21 = un32*base + un1 - q1*c;  // Multiply and subtract.
    q0 = un21/vn1;            // Compute the second
    rhat = un21 - q0*vn1;     // quotient digit, q0.
again2:
    if (q0 >= base || q0*vn0 > base*rhat + un0)
    {   q0 = q0 - 1;
        rhat = rhat + vn1;
        if (rhat < base) goto again2;
    }
    q = (q1 << 32) | q0;      // assemble and return quotient & remainder
    r = (un21*base + un0 - q0*c) >> s;
}

#endif // __SIZEOF_INT128__

// While my arithmetic is all done in uint64_t (and that is important so
// that in C++ the consequences of overflow are defined) I need to treat
// some top-digits as signed: here are values and tests relating to that.

INLINE_VAR const uint64_t allbits   = ~(uint64_t)0;
INLINE_VAR const uint64_t topbit    = ((uint64_t)1)<<63;
INLINE_VAR const uint64_t allbuttop = topbit - 1;

inline bool positive(uint64_t a)
{   return ((int64_t)a) >= 0;
}

inline bool negative(uint64_t a)
{   return ((int64_t)a) < 0;
}

inline void internal_copy(const uint64_t *a, size_t lena, uint64_t *b)
{   memcpy(b, a, lena*sizeof(uint64_t));
}

// This internal functions sets b to be -a without altering its length.
// Because length is not changed it does not need a length for the
// destination passed to it.

inline void internal_negate(const uint64_t *a, size_t lena, uint64_t *b)
{   uint64_t carry = 1;
    for (size_t i=0; i<lena; i++)
    {   uint64_t w = b[i] = ~a[i] + carry;
        carry = (w < carry ? 1 : 0);
    }
}

// When printing numbers in octal it will be handy to be able treat the
// data as an array of 3-bit digits, so here is an access function that
// does that. There is a messy issue about the top of a number, where it
// may not be a whole number of 3-bit octal digits. I pass in v, a vector
// of 64-bit values, n which is the length of that vector and i which
// is the index of the octal digit that I wish to extract. To help with
// that I have a function virtual_digit64() which lets me read from a
// bignum as if it has been usefully sign-extended.

inline uint64_t virtual_digit64(const uint64_t *v, size_t n, size_t j)
{   if (j < n) return v[j];
    else if (positive(v[n-1])) return 0;
    else return UINT64_C(0xffffffffffffffff);
}

// This function reads a 3-bit digit from a bignum, and is for use when
// printing in octal.

inline int read_u3(const uint64_t *v, size_t n, size_t i)
{   size_t bits = 3*i;
    size_t n0 = bits/64;   // word with lowest bit of the 3
    size_t s0 = bits%64;   // amount to shift right to align it properly
    uint64_t w = virtual_digit64(v, n, n0) >> s0;
// If I needed to shift by 62 or 63 bits then the octal digit I am interested
// in needs some bits from the next word up.
    if (s0 >= 62) w |= (virtual_digit64(v, n, n0+1) << (64-s0));
    return (int)(w & 0x7);
}

//=========================================================================
//=========================================================================
// Some support for two models of memory layout. If VSL is set a number
// will be represented as an intptr_t value with its low 3 bits used as
// a tag. When the tag is removed and the intptr_t is cast to (uint64_t *)
// it points at a block of words. The first word holds a header value
// that includes (in packed form) the length of the block. Beyond that
// is the row of uint64_t values making up the bignum itself.
//
// For more direct C++ use the type is just (uint64_t *) and it refers
// directly to the row of digits of the bignum. However at the address
// just ahead of that (ie at v[-1]) there is a header word giving the
// length of the bignum.
// Sometime soon this header word will be structured as two 32-bit
// parts. One will give the number of 64-bit elements of the vector that
// are actually in ise. The other will be a small integer indicating
// a power of two that is the size of memory block that was allocated.
// Such a scheme always rounds allocated sizes up using next_power_of_2()
// and then when the actual number of digits a number occupies turns out
// to be less than it might have there is no need to recycle memory - the
// "actual length" field is just updates. Furthermore a modest sized
// table can keep freelists of discarded blocks on each size, so allocation
// is potentially speeded up.
//=========================================================================
//=========================================================================


// For a free-standing bignum application (including my test code for the
// stuff here, bignums are represented as blocks of memory (allocated using
// malloc) where the pointer that is used points to the start of the
// array of bignum digits, and the word in memory before that contains
// the length (in words) of the block.
// Strings are returned to the user as freshly malloced memory holding a
// native-style C++ string with a terminating NUL character at the end.


//=========================================================================
//=========================================================================
// Random number support
//=========================================================================
//=========================================================================


// It is useful to be able to generate random values. C++11 is simultaneously
// very helpful and rather unhelpful. The class std::random_device is
// expected to yield genuine unpredictable values, but it is not guaranteed
// to and it fails to on some platforms, so despite the fact that when it
// works it is a really good thing I can not rely solely on it. Each time I
// use a random_device it gives me just 32 bits. For my real generator that
// is not really enough.
// So here I create 3 notionally unpredictable units and then merge in the
// identity of the current thread and two measurements related to time.
// To avoid thread safety issues with random_device I make calls to it
// global, and then the thread identifier and time of day information stands
// a prospect of arranging that each thread gets its own mersenne-twister
// with its own seeding.
// Note that Wikipedia explains "Multiple instances that differ only in
// seed value (but not other parameters) are not generally appropriate
// for Monte-Carlo simulations that require independent random number
// generators" and here even the independence of my thread-specific
// seed values is questionable.

// I perform all this setup at initialization time, but by wrapping the
// same sequence of steps as a critical region I could use it to re-seed
// generators whenever I felt the need to.
//

INLINE_VAR std::random_device basic_randomness;
INLINE_VAR uint64_t thread_local threadid =
    (uint64_t)(std::hash<std::thread::id>()(std::this_thread::get_id()));
INLINE_VAR uint64_t seed_component_1 = (uint64_t)basic_randomness();
INLINE_VAR uint64_t seed_component_2 = (uint64_t)basic_randomness();
INLINE_VAR uint64_t seed_component_3 = (uint64_t)basic_randomness();
INLINE_VAR thread_local uint64_t time_now = (uint64_t)time(NULL);
INLINE_VAR thread_local uint64_t chrono_now = (uint64_t)
    (std::chrono::high_resolution_clock::now().time_since_epoch().count());

// Observe the thread_local status here.
// In my first draft of this library I had made the random seed directly
// from uint64_t values. However when testing on a Raspberry Pi that
// triggered a messages about bugs in gcc before gcc7 (relating to the
// alignment of some values passed as arguments in obscure cases). Building
// the seed sequence using 32-bit values avoids that issue, and since this
// is only done during initialization it is not time-critical.
//
INLINE_VAR thread_local std::seed_seq random_seed{
    (uint32_t)threadid,
    (uint32_t)seed_component_1,
    (uint32_t)seed_component_2,
    (uint32_t)seed_component_3,
    (uint32_t)time_now,
    (uint32_t)chrono_now,
    (uint32_t)(threadid>>32),
    (uint32_t)(seed_component_1>>32),
    (uint32_t)(seed_component_2>>32),
    (uint32_t)(seed_component_3>>32),
    (uint32_t)(time_now>>32),
    (uint32_t)(chrono_now>>32)};

INLINE_VAR thread_local std::mt19937_64 mersenne_twister(random_seed);
// mersenne_twister() now generates 64-bit unsigned integers.

// To re-seed I can just call this. I think that when I re-seed it will be
// to gain more repeatable behaviour, and so I am fairly happy about
// limiting the amount of input entropy here to 64-bits.

inline void reseed(uint64_t n)
{   mersenne_twister.seed(n);
}

// Now a number of functions for setting up random bignums. These may be
// useful for users, but they will also be very useful while testing this
// code.

// Return a random integer in the range 0 ... n-1.
// Given that the largest n that can be passed is UINT64_MAX the biggest
// rangs that can be generated here is 1 less than the full range of 64-bit
// values. To get a full 64-bit range merely call mersenne_twister()
// directly.

inline uint64_t uniform_uint64(uint64_t n)
{   if (n <= 1) return 0;
// I I want the remainder operation on the last line of this function to
// return a uniformly distributed result. To ensure that I want r to be
// drawn uniformly from a range that is a multiple of n.
    uint64_t q = UINT64_MAX/n;
    uint64_t w = n*q;
    uint64_t r;
// In the worst case here n was just over UINT64_MAX/2 and q came out
// as 1. In that case on average I will need to call mersenne_twister
// twice. Either larger or smaller inputs will behave better, and rather
// small inputs will mean I hardly ever need to re-try.
    do
    {   r = mersenne_twister();
    } while (r >= w);
    return r%n;
}

// A uniform distribution across the range [0 .. (2^bits)-1], ie
// a bignum using (up to) the given number of bits. So eg uniform_positive(3)
// should return 0,1,2,3,4,5,6 or 7 each with equal probability.

inline void uniform_positive(uint64_t *r, size_t &lenr, size_t bits)
{   if (bits == 0)
    {   r[0] = 0;
        lenr = 1;
    }
    lenr = (bits+63)/64;
    for (size_t i=0; i<lenr; i++)
        r[i] = mersenne_twister();
    if (bits%64 == 0) r[lenr-1] = 0;
    else r[lenr-1] &= UINT64_C(0xffffffffffffffff) >> (64-bits%64);
    while (lenr!=1 && r[lenr-1] == 0 && positive(r[lenr-2])) lenr--;
}

inline intptr_t uniform_positive(size_t n)
{   size_t lenr = (n + 63)/64;
    if (lenr == 0) lenr = 1; // special case!
    size_t save = lenr;
    uint64_t *r = reserve(lenr);
    uniform_positive(r, lenr, n);
    return confirm_size(r, save, lenr);
}

// As above but returning a value that may be negative. uniform_signed(3)
// could return -8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6 or 7.
// Note that while uniform_unsigned(0) can only return the value 0,
// uniform_signed(0) can return -1 or 0.

inline void uniform_signed(uint64_t *r, size_t &lenr, size_t bits)
{   lenr = 1 + bits/64;
    for (size_t i=0; i<lenr; i++)
        r[i] = mersenne_twister();
// Now if the "extra" bit is zero my number will end up positive.
    if ((r[lenr-1] & (UINT64_C(1) << (bits%64))) == 0)
    {   r[lenr-1] &= UINT64_C(0xffffffffffffffff) >> (63-bits%64);
        while (lenr!=1 && r[lenr-1] == 0) lenr--;
    }
// Otherwise the result will end up negative.
    else
    {   r[lenr-1] |= UINT64_C(0xffffffffffffffff) << (bits%64);
        while (lenr!=1 && r[lenr-1] == UINT64_C(0xffffffffffffffff)) lenr--;
    }
}

inline intptr_t uniform_signed(size_t n)
{   size_t lenr = n/64+1;
    size_t save = lenr;
    uint64_t *r = reserve(lenr);
    uniform_signed(r, lenr, n);
    return confirm_size(r, save, lenr);
}

inline size_t bignum_bits(const uint64_t *a, size_t lena);

// Generate a a value in the range 0 .. a-1 using a uniform distribution

inline void uniform_upto(const uint64_t *a, size_t lena, uint64_t *r, size_t &lenr)
{   size_t n = bignum_bits(a, lena);
// I will repeatedly generate numbers that have as many bits as a until
// I get one that has a value less than a has. On average that should only
// take two tries.
    for (;;)
    {   uniform_positive(r, lenr, n);
        if (lena > lenr) return;
        for (size_t len=lena;;)
        {   len--;
            if (a[len] > r[len]) return;
            if (a[len] < r[len] || len == 0) break;
        }
    }
}

inline intptr_t uniform_upto(intptr_t aa)
{   uint64_t *a;
    size_t lena;
    uint64_t w[2];
    if (stored_as_fixnum(aa))
    {   w[1] = (uint64_t)int_of_handle(aa);
        lena = 1;
        a = &w[1];
    }
    else
    {   a = vector_of_handle(aa);
        lena = number_size(a);
    }
    uint64_t *r = reserve(lena);
    size_t lenr;
    uniform_upto(a, lena, r, lenr);
    return confirm_size(r, lena, lenr);
}

inline void truncate_positive(const uint64_t *r, size_t &n)
{   while (r[n-1]==0 && n>1 && positive(r[n-2])) n--;
}

inline void truncate_negative(const uint64_t *r, size_t &n)
{   while (r[n-1]==allbits && n>1 && negative(r[n-2])) n--;
}

// The following is a rather strange function. It looks at the 4 bit number n.
// It then processes its input a in accordance with the following table, where
// A is the (positive) input value and X is A rounded down to the nearest
// power of 2 less than it (ie keeping just the top bit of A):
//
//    0   X-1                     8   -(X-1)    
//    1   X                       9   -X        
//    2   X+1                    10   -(X+1)    
//    3   A                      11   -A        
//    4   A                      12   -A
//    5   A                      13   -A
//    6   A                      14   -A
//    7   A                      15   -A

// The idea behind this is that the input A will be a random value from a
// reasonably smooth distribution, and n will be a random 4 bit value. The
// output will still be random, but now half the time it will be negative.
// And a significant proportion of the time it will be a power of 2 (or one
// either side of being a power of 2). This last is something I want because
// with an internal representation that is based on 2s complement values
// close to powers of 2 can easily be "edge cases" that deserve extra attention
// during testing.

inline void fudge_distribution(const uint64_t *a, size_t lena,
                        uint64_t *r, size_t &lenr, int n)
{   lenr = lena;
    switch (n&7)
    {
    case 0:
    case 1:
    case 2:
        for (size_t i=0; i<lena+1; i++) r[i] = 0;
        if (a[lena-1] == 0)
        {   if (lena>1) r[lena-2] = ((uint64_t)1)<<63;
        }
        else r[lena-1] = ((uint64_t)1) << (63-nlz(a[lena-1]));
        if ((n&7) == 0) // decrement it
        {   if (lena!=1 || a[0]!=0) // avoid decrementing zero.
            {   uint64_t *p = r;
                while (*p == 0) *p++ = (uint64_t)(-1);
                (*p)--;
            }
        }
        else if ((n&7) == 2) // increment it
        {   uint64_t *p = r;
            while (*p == (uint64_t)(-1)) *p++ = 0;
            (*p)++;
        }
        break;
    default:
        for (size_t i=0; i<lena; i++) r[i] = a[i];
        break;
    }
    if ((n&8) != 0)
    {   uint64_t carry = 1;
        for (size_t i=0; i<lena+1; i++)
        {   carry = add_with_carry(~r[i], carry, r[i]);
        }
        truncate_negative(r, lenr);
    }
    else truncate_positive(r, lenr);
}

inline intptr_t fudge_distribution(intptr_t aa, int n)
{   uint64_t *a;
    size_t lena;
    uint64_t w[2];
    if (stored_as_fixnum(aa))
    {   w[1] = (uint64_t)int_of_handle(aa);
        lena = 1;
        a = &w[1];
    }
    else
    {   a = vector_of_handle(aa);
        lena = number_size(a);
    }
    uint64_t *r = reserve(lena+1);
    size_t lenr;
    fudge_distribution(a, lena, r, lenr, n);
    return confirm_size(r, lena+1, lenr);
}

// Generate a value in the range 0 .. 2^bits-1 using a distribution such
// numbers with each bit-length are equally probable. This works by
// selecting a big-length uniformly and then creating a number uniformly
// distributed across all those with that exact bit-width. This is perhaps
// not a very nice distribution from a mathematical perspective, but is is
// nevertheless a useful one to have in some test code.

inline void random_upto_bits(uint64_t *r, size_t &lenr, size_t n)
{   size_t bits = (size_t)uniform_uint64(n);
    if (bits == 0)
    {   r[0] = 0;
        lenr = 1;
        return;
    }
// The number will have from 1 to 64 bits in its top digit.
    lenr = (bits+63)/64;
    for (size_t i=0; i<lenr; i++)
        r[i] = mersenne_twister();
    if (n%64 != 0)
        r[lenr-1] &= UINT64_C(0xffffffffffffffff) >> (64-bits%64);
    r[lenr-1] |= UINT64_C(1) << ((bits-1)%64);
    if (bits%64 == 0) r[lenr++] = 0;
    my_assert(!negative(r[lenr-1]));
}

inline intptr_t random_upto_bits(size_t bits)
{   size_t m = 1+bits/64;
    if (m == 0) m = 1;
    uint64_t *r = reserve(m);
    size_t lenr;
    random_upto_bits(r, lenr, bits);
    return confirm_size(r, m, lenr);
}

//=========================================================================
//=========================================================================
// Here I have a few tiny conversion functions followed by code for
// conversion between big numbers and strings. All of these are rather
// important for getting data in and out of the big number format and so
// deserve to be shown early.
//=========================================================================
//=========================================================================


// Convert a 64-bit integer to a bignum.
// This can be useful when there is no special-purpose code to
// perform arithmetic between a bignum and a native int64_t integer
// directly.

inline void int_to_bignum(int64_t n, uint64_t *r)
{   r[0] = (uint64_t)n;
}

inline intptr_t int_to_bignum(int64_t n)
{   if (fits_into_fixnum(n)) return int_to_handle(n);
    uint64_t *r = reserve(1);
    int_to_bignum(n, r);
    return confirm_size(r, 1, 1);
}

inline void unsigned_int_to_bignum(uint64_t n, uint64_t *r, size_t &lenr)
{   r[0] = n;
    if (negative(n))
    {   r[1] = 0;
        lenr = 2;
    }
    else lenr = 1;
}

inline intptr_t unsigned_int_to_bignum(uint64_t n)
{   size_t w = (negative(n) ? 2 : 1);
    uint64_t *r = reserve(w);
    size_t lenr;
    unsigned_int_to_bignum(n, r, lenr);
    return confirm_size(r, w, lenr);
}

inline intptr_t double_to_bignum(double d)
{
// I return 0 if the input is a NaN or either +infinity or -infinity.
// This is somewhat arbitrary, but right now I am not minded to raise an
// exception.
    if (!std::isfinite(d) || d==0.0) return int_to_handle(0);
    int x;
    d = std::frexp(d, &x);
    d = std::ldexp(d, 53);
    intptr_t i = int_to_bignum((int64_t)d);
    intptr_t r = op_dispatch1<Leftshift,intptr_t>(i, x-53);
    abandon(i);
    return r;
}

inline intptr_t double_to_floor(double d)
{   if (!std::isfinite(d) || d==0.0) return int_to_handle(0);
    int x;
    d = std::floor(d);
    d = std::frexp(d, &x);
    d = std::ldexp(d, 53);
    intptr_t i = int_to_bignum((int64_t)d);
    intptr_t r = op_dispatch1<Leftshift,intptr_t>(i, x-53);
    abandon(i);
    return r;
}

inline intptr_t double_to_ceiling(double d)
{   if (!std::isfinite(d) || d==0.0) return int_to_handle(0);
    int x;
    d = std::ceil(d);
    d = std::frexp(d, &x);
    d = std::ldexp(d, 53);
    intptr_t i = int_to_bignum((int64_t)d);
    intptr_t r = op_dispatch1<Leftshift,intptr_t>(i, x-53);
    abandon(i);
    return r;
}

// On Cygwin (at least) the std::ldexpf function that is part of C++11
// is hidden in the header file perhaps because of issues about thread
// safety in its implementation. I reason here that converting from a
// float to a double will never lose anything, then ldexp() can be used.
// The case back to a float can not introduxce rounding, but might notice
// overflow leading to a result that is an IEEE infinity.

inline float ldexpf(float a, int n)
{   return (float)ldexp((double)a, n);
}

inline float Float::op(int64_t a)
{
// The bad news here is that I am not confident that C++ will guarantee
// to round large integer values in any particular way when it converts
// them to floating point. So I will take careful action so that the
// conversions that I do are ones that will be exact, and I will perform
// rounding in IEEE style myself.
// First I will see if the value is small enough that I can work directly.
// In fact I would still be safe with the simple case without the "-1" in
// the netx line, but that would slightly complicate the code lower down.
    static const int64_t range = ((int64_t)1)<<24;
    if (a >= -range && a <= range) return (float)a;
// I will now drop down to a sign and magnitude representation
    bool sign = a < 0;
    uint64_t top24 = sign ? -(uint64_t)a : a;
// Because top24 > 2^24 the number of leading zeros in its representation is
// at most 39. Ha ha. That guaranteed that the shift below will not overflow
// and is why I chose my range as I did. 
    int lz = nlz(top24);
    uint64_t low = top24 << (lz+24);
    top24 = top24 >> (64-24-lz);
    if (low > 0x8000000000000000U) top24++;
    else if (low == 0x8000000000000000U) top24 += (top24 & 1); // round to even
    my_assert(top24 >= ((int64_t)1)<<23 &&
              top24 <= ((int64_t)1)<<24);
// The next line should never introduce any rounding at all.
    float d = (float)top24;
    my_assert(top24 == (uint64_t)d);
    d = ldexpf(d, (int)(64-24-lz));
    if (sign) return -d;
    else return d;
}   

inline float Float::op(uint64_t *a)
{   size_t lena = number_size(a);
    if (lena == 1) return Float::op((int64_t)a[0]);
// Now I need to do something similar to that done for the int64_t case
// but written larger. Specifically I want to split my input number into
// its top 24 bits and then all the rest. I will take separate paths
// for the positive and negative cases.
    uint64_t top24;
    int lz;
    bool sign = false;
    uint64_t top, next;
    bool carried = true;
    for (size_t i=0; i<lena-2; i++)
    {   if (a[i] != 0)
        {   carried = false;
            break;
        }
    }
// Grap the top 128 bits of the number as {top,next}.
    top = a[lena-1];
    next = a[lena-2];    // lena >= 2 here
// Take its absolute value.
    if (negative(top))
    {   sign = true;
        top = ~top;
        next = ~next;
        if (carried)
        {   next++;
            if (next == 0) top++;
        }
    }
    if (!carried) next |= 1;
// Now I need to do something very much like the code for the int64_t case.
    if (top == 0) lz = nlz(next) + 64;
    else lz = nlz(top);
//
//  uint64_t top24 = {top,next} >> (128-24-lz);
    int sh = 128-24-lz;
// Note that sh can never be zero here.
    if (sh < 64) top24 = (next >> sh) | (top << (64-sh));
    else top24 = top >> (sh-64);
//
//  {top,next} = {top,next} << lz+24; // keep only the fraction bits
    sh = lz+24;
    if (sh < 64)
    {   top = (top << sh) | (next >> (64-sh));
        next = next << sh;
    }
    else
    {   top = next << (sh - 64);
        next = 0;
    }
//
//  if ({top,next} > 0x80000000000000000000000000000000U) top24++;
//  else if ({top,next} == 0x80000000000000000000000000000000U)
//      top24 += (top24 & 1);
    if (top > 0x8000000000000000U) top24++;
    else if (top == 0x8000000000000000U)
    {   if (next != 0) top24++;
        else top24 += (top24&1);
    }
    my_assert(top24 >= ((int64_t)1)<<23 &&
              top24 <= ((int64_t)1)<<24);
    double d = (float)top24;
    my_assert(top24 == (uint64_t)d);
    if (sign) d = -d;
    return ldexpf(d, (int)(128-24-lz+64*(lena-2)));
}

inline double Double::op(int64_t a)
{
// The bad news here is that I am not confident that C++ will guarantee
// to round large integer values in any particular way when it converts
// them to floating point. So I will take careful action so that the
// conversions that I do are ones that will be exact, and I will perform
// rounding in IEEE style myself.
// First I will see if the value is small enough that I can work directly.
    static const int64_t range = ((int64_t)1)<<53;
    if (a >= -range && a <= range) return (double)a;
// I will now drop down to a sign and magnitude representation
    bool sign = a < 0;
    uint64_t top53 = sign ? -(uint64_t)a : a;
// Because top53 >= 2^53 the number of leading zeros in its representation is
// at most 10. Ha ha. That guaranteed that the shift below will not overflow
// and is why I chose my range as I did. 
    int lz = nlz(top53);
    uint64_t low = top53 << (lz+53);
    top53 = top53 >> (64-53-lz);
    if (low > 0x8000000000000000U) top53++;
    else if (low == 0x8000000000000000U) top53 += (top53 & 1); // round to even
    my_assert(top53 >= ((int64_t)1)<<52 &&
              top53 <= ((int64_t)1)<<53);
// The next line should never introduce any rounding at all.
    double d = (double)top53;
    my_assert(top53 == (uint64_t)d);
    d = std::ldexp(d, (int)(64-53-lz));
    if (sign) return -d;
    else return d;
}   

inline double Double::op(uint64_t *a)
{   size_t lena = number_size(a);
    if (lena == 1) return Float::op((int64_t)a[0]);
// Now I need to do something similar to that done for the int64_t case
// but written larger. Specifically I want to split my input number into
// its top 53 bits and then all the rest. I will take separate paths
// for the positive and negative cases.
    uint64_t top53;
    int lz;
    bool sign = false;
    uint64_t top, next;
    bool carried = true;
    for (size_t i=0; i<lena-2; i++)
    {   if (a[i] != 0)
        {   carried = false;
            break;
        }
    }
// Grap the top 128 bits of the number as {top,next}.
    top = a[lena-1];
    next = a[lena-2];    // lena >= 2 here
// Take its absolute value.
    if (negative(top))
    {   sign = true;
        top = ~top;
        next = ~next;
        if (carried)
        {   next++;
            if (next == 0) top++;
        }
    }
    if (!carried) next |= 1;
// Now I need to do something very much like the code for the int64_t case.
    if (top == 0) lz = nlz(next) + 64;
    else lz = nlz(top);
//
//  uint64_t top53 = {top,next} >> (128-53-lz);
    int sh = 128-53-lz;
// Note that sh can never be zero here.
    if (sh < 64) top53 = (next >> sh) | (top << (64-sh));
    else top53 = top >> (sh-64);
//
//  {top,next} = {top,next} << lz+53; // keep only the fraction bits
    sh = lz+53;
    if (sh < 64)
    {   top = (top << sh) | (next >> (64-sh));
        next = next << sh;
    }
    else
    {   top = next << (sh - 64);
        next = 0;
    }
//
//  if ({top,next} > 0x80000000000000000000000000000000U) top53++;
//  else if ({top,next} == 0x80000000000000000000000000000000U)
//      top53 += (top53 & 1);
    if (top > 0x8000000000000000U) top53++;
    else if (top == 0x8000000000000000U)
    {   if (next != 0) top53++;
        else top53 += (top53&1);
    }
    my_assert(top53 >= ((int64_t)1)<<52 &&
              top53 <= ((int64_t)1)<<53);
    double d = (double)top53;
    my_assert(top53 == (uint64_t)d);
    if (sign) d = -d;
    return std::ldexp(d, (int)(128-53-lz+64*(lena-2)));
}

#ifdef softfloat_h

inline float128_t Float128::op(int64_t a)
{   return i64_to_f128(a);
}   

inline float128_t Double::op(uint64_t *a)
{   size_t lena = number_size(a);
    if (lena == 1) return Float128::op((int64_t)a[0]);
    uint64_t top113, top113a;
    int lz;
    bool sign = false;
    uint64_t top, next1, next2;
    bool carried = true;
    for (size_t i=0; i<lena-3; i++)
    {   if (a[i] != 0)
        {   carried = false;
            break;
        }
    }
// Grap the top 192 bits of the number as {top,next}.
    top = a[lena-1];
    next1 = a[lena-2];
    next2 = len1==2 ? 0 : a[lena-3];
// Take its absolute value.
    if (negative(top))
    {   sign = true;
        top = ~top;
        next1 = ~next1;
        next2 = ~next2;
        if (carried)
        {   next2++;
            if (next2 == 0)
            {   next1++;
                if (next1 == 0) top++;
            }
        }
    }
    if (!carried) next2 |= 1;
// I now have {top,next1,next2} the top 192 bits of my integer. top may be
// zero, but if it is then next1 will have its top bit set, and so within
// these bits I certainly have the 113 that I need to obtain an accurate
// floating point value.
    if (top == 0) lz = nlz(next1) + 64;
    else lz = nlz(top);
//
//  uint64_t {top113,top112a} = {top,next1,next2} >> (128-113-lz);
    int sh = 192-113-lz;
// Note that sh can never be zero here.
    if (sh < 64)
    {   top113 = (next >> sh) | (top << (64-sh));
        top113a = (next2 >> sh) | (next1 << (64-sh));
    }
    else
    {   top113 = top >> (sh-64);
        top113a = (next1 >> (sh-64)) | (top << (128-sh));
    }
//
//  {top,next} = {top,next} << lz+113; // keep only the fraction bits
    sh = lz+113;
    if (sh < 64)
    {   top = (top << sh) | (next1 >> (64-sh));
        next1 = (next1 << sh) | (next2 >> (64-sh));
        next2 = next2 << sh;
    }
    else
    {   top = next1 << (sh - 64);
        next1 = (next1 << (sh-64)) | (next2 >> (129-sh));
        next2 = 0;
    }
//
//  if ({top,next1,next2} > 0x80000000000000000000000000000000U) top113++;
//  else if ({top,next1, next2} == 0x80000000000000000000000000000000U)
//      top113 += (top113 & 1);
    if (top > 0x8000000000000000U)
    {   top113a++;
        if (top113a == 0) top113++;
    }
    else if (top == 0x8000000000000000U)
    {   if (next1 != 0 || (next1==0 && next2!=0))
        {   top113a++;
            if (top113a == 0) top113++;
        }
        else top113 += add_with_carry(top113a, top113a&1, top113a);
    }
//  float128_t d = i64_to_f128({top113, top113a});
    float128_t d = i64_to_f128(top113);
    d = f128_add(f128_multiply(2.0^64, d), u64_to_f128(top113a));
    if (sign) d = f128_negate(d);
// There is an implementation of ldexp() for 128-bit floats in
// the CSL source file arith14.cpp.
    f128M_ldexp(&d, (int)(192-113-lz+64*(lena-2)));
    return d;
}

#endif // softfloat_t

INLINE_VAR const uint64_t ten19 = UINT64_C(10000000000000000000);

inline intptr_t string_to_bignum(const char *s)
{   bool sign = false;
    if (*s == '-')
    {   sign = true;
        s++;
    }
    size_t chars = strlen(s);
    size_t words = 1 + (108853*(uint64_t)chars)/0x200000;
// I have predicted the number of 64-bit digits that will be needed to
// represent an s-digit (decimal) number based an approximation
// 108853/2^21 for log(10)/log(2^64). In 64-bit arithmetic the numerator
// here will not overflow until you have an improbable string of length
// 2^47 as input! The division by a power of 2 should be done very
// rapidly as a shift. I rather expect this calculation to give a rather
// good measure of how many 64-bit words will be needed! It must never be an
// overestimate so that the vector that I allocate never overflows. Somewhat
// rarely it will be and overestimate and it will be necessary to trim the
// vector at the end.
    uint64_t *r = reserve(words);
    for (size_t i=0; i<words; i++) r[i] = 0;
// Now for each chunk of digits NNNN in the input I want to go in effect
//     r = 10^19*r + NNNN;
// where the number 19 is used because 10^19 is the largest power of 10
// that fits in a 64-bit word.
    size_t next = 19*((chars-1)/19);
    while (chars != 0)
    {   uint64_t d = 0;
// assemble 19 digit blocks from the input into a value (d).
        while (chars != next)
        {   my_assert(std::isdigit(*s));
            d = 10*d + (*s++ - '0');
            chars--;
        }
        next -= 19;
// now perform r = 10^19*r + d to consolidate into the eventual result.
        for (size_t i=0; i<words; i++)
            multiplyadd64(r[i], ten19, d, d, r[i]);
    }
    size_t n1 = words;
// Here I may be negating a positive number, and in 2s complement that
// can never lead to a number growing in length.
    if (sign)
    {   internal_negate(r, words, r);
        truncate_negative(r, n1);
    }
// However I could not have been precisely certain how many 64-bit words were
// needed and I arranged that any error was conservative - ie allocating
// more that would eventually be used.
    else truncate_positive(r, n1);
    return confirm_size(r, words, n1);
}

// The next functions are a key one for printing values. They convert a
// bignum so that it is still stored as a sequence of digits each within
// a 64-bit work, but now each digit will be be in the range 0 - (10^19-1)
// so that the value is in effect represented base 10^19. From that state
// printing it in decimal becomes easy!


// This first one takes a number represented base 2^64 with digits
// 0 to n-1 and divides it by 10^19, returning the remainder and
// setting both the digits and its length suitably to be the quotient.
// The number is POSITIVE here. Note that the function overwrites its input
// with the quotient.


inline uint64_t short_divide_ten_19(uint64_t *r, size_t &n)
{   uint64_t hi = 0;
    size_t i=n-1;
    for (;;)
    {   divide64(hi, r[i], ten19, r[i], hi);
        if (i == 0) break;
        i--;
    }
    if (r[n-1] == 0) n--;
    return hi;
}

// How many bits are there in a bignum?

// Note that if a bignum occupies over 1/8 of your total memory that
// the number of bits it uses might overflow size_t. On a 32-bit system
// this might happen if the number occupies over 512 Mbytes and I view
// that as a situation I will accept as a limit for 32-bit platforms.

inline size_t bignum_bits(const uint64_t *a, size_t lena)
{   if (lena == 0 && a[0] == 0) return 1; // say that 0 has 1 bit.
    uint64_t top = a[lena-1];  // top digit.
// The exact interpretation of "the length in bits of a negative number"
// is something I need to think through. Well Common Lisp counts the
// number of bits apart from the sign bit, so we have
//      n      bignum_bits(n)   bignum_bits(-n)
//      0           0                0
 //     1           1    1           0 ..11111:
//      2           2   10           1 ..1111:0
//      3           2   11           2 ..111:01
//      4           3  100           2 ..111:00
//      7           3  111           3 ..11:001
//      8           4 1000           3 ..11:000
    if (negative(top))
    {   uint64_t carry = 1;
        for (size_t i=0; i<lena; i++)
        {   top = ~a[i] + carry;
            carry = (top < carry ? 1 : 0);
        }
        top--;
    }
    return 64*(lena-1) + (top==0 ? 0 : 64-nlz(top));
}

// I want an estimate of the number of bytes that it will take to
// represent a number when I convert it to a string.
//
// I will work through an example. Consider the input 12024932 = 0xb77c64.
// [I use this value because at one time it revealed a mistake I had made!]
// This value uses 24 bits, ie its value is at least 2^23 (8388608) and
// it is less than 2^26 (16777216). log10(2^24) is 7.2247... so in decimal
// the number will use 7.2 digits, well that must be rounded up to 8.
// log10(2^24) = 24*log10(2) = 24*0.301030.. < 24*(617/2048) [because that
// fraction = 0.30127.. > log10(2)]. So if one the number of decimal digits
// that can be generated will be ceil(24*617/2048). I will compute that by
// forming a quotient that is truncated towards zero and then adding 1, and
// in this case this yields 8 as required. For negative numbers I will add 1
// to allow for a "-" sign.

inline size_t predict_size_in_bytes(const uint64_t *a, size_t lena)
{
// I am first going to estimate the size in BITS and then I will
// see how that maps onto bytes.
    size_t r = bignum_bits(a, lena);
    r = 1 + (size_t)((617*(uint64_t)r)/2048);
    if (negative(a[lena-1])) r += 2; // allow space for a "-" sign.
    return r;
} 

// The "as_unsigned" option here is not for general use - it is JUST for
// internal debugging because at times I work with values that are known
// to be positive and so where the top digit must be treated as unsigned...

inline string_handle bignum_to_string(const uint64_t *a, size_t lena,
                                      bool as_unsigned=false)
{
// Making one-word numbers a special case simplifies things later on! It may
// also make this case go just slightly faster.
    if (lena == 1)
    {   uint64_t v = a[0];
        bool sign;
        if (negative(v) && !as_unsigned)
        {   sign = true;
            v = -v;
        }
        else sign = false;
        char buffer[24];
        int len = 0;
        while (v != 0)
        {   buffer[len++] = '0' + v%10;
            v = v/10;
        }
// Now I have the decimal digits on the number in my buffer, with the
// least significant first and the most significant last. Insert the sign bit
// if needed (and deal with the special case of zero).
        if (sign) buffer[len++] = '-';
        else if (len == 0) buffer[len++] = '0';
        char *r = reserve_string(len);
        for (int i=0; i<len; i++) r[i] = buffer[len-i-1];
        return confirm_size_string(r, len, len);
    }
// The size (m) for the block of memory that I put my result in is
// such that it could hold the string representation of my input, and
// I estimate that via predict_size_in_bytes(). Well the smallest bignum
// that will need 2 words will be {0,0x8000000000000000}, ie 2^63. That
// will need 19 decimal digits plus space for a sign bit, so there will be
// at least 20 bytes allocated for the printed representation of any 2-word
// bignum, and at least 40 for a 3-word value, at least 59 for a 4-word one
// etc. This means that the space I will allocate here for the result
// leaves me with plenty of workspace to use while constructing the
// output string. The case liable to be tightest will be that of the
// smallest 2-woed bignum, so if I ensure that is OK all the rest will
// certainly be safe.
    uint64_t m = predict_size_in_bytes(a, lena);
// I am going to build up (decimal) digits of the converted number by
// repeatedly dividing by 10^19. Each time I do that the remainder I
// amd left with is the next low 19 decimal digits of my number. Doing the
// divisions needs a vector to store the number I am dividing by 10^19 and
// to put the quotient, and I do not want to corrupt my original input, so
// I will copy my input into a fresh vector. And I will force it to be
// positive. The made-positive version might have a leading digit with
// its top bit set - that will not worry me because I view it as unsigned.
    char *rc = reserve_string(m);
// I have allocated the space that will be needed for the eventual string of
// characters. I will use that space to save numeric values along the way, so
// here I cast so I can use that same memory as a vector of 64-bit integers.
// I will only ever access data in the format that it was placed into memory!
// Note that this will assume that the string data was allocated so as to
// be aligned suitably for uint64_t values.
    uint64_t *r = (uint64_t *)rc;
    size_t i;
// For the edge case lena==2 and m==20. I copy 2 words across. That will leave
// 4 bytes unused.
    for (i=0; i<lena; i++) r[i] = a[i];
    for (; i<m/sizeof(uint64_t); i++) r[i] = 0;
// Make the number positive
    bool sign = false;
    if (negative(r[lena-1]) && !as_unsigned)
    {   sign = true;
        internal_negate(r, lena, r);
    }
// Now my number is positive and is of length lena, but the vector it is
// stored in is length m with m usefully larger than lena. I will repeatedly
// divide by 10^19 and each time I do that I can store the remainder working
// down from the top of the vector. That should JUST keep up so that I
// never overwrite digits of the reducing part! I will stop when the
// number I have been working with end up < 10^19.
    size_t p = m/sizeof(uint64_t)-1; // where to put next output digit
// Each value written into the vector here will stand for 19 decimal
// digits, and will use 8 bytes. So here the nastiest case will be when the
// number of decimal digits to end up with is 7 mod 8 (so that I lose as
// much space as possible) and the number is as large as possible. My
// belief is that numbers from 10^16 upwards will lead to there being enough
// space.
    while (lena > 1 || r[0] > ten19)
    {   uint64_t d = short_divide_ten_19(r, lena);
        r[p--] = d;
    }
    r[p] = r[0];
// Now I have the data that has to go into my result as a sequence of
// digits base 10^19, with the most significant one first. Convert
// to character data. I write in the string data just over what has been
// digits data, and I have arranged to position everything to (just)
// avoid overwriting myself.
    uint64_t top = r[p++];
    if (top == 0) top = r[p++]; // discard potential leading zero!
// Get a pointer into the buffer as character data...
    char *p1 = (char *)rc;
    size_t len = 0;
    if (sign)
    {   *p1++ = '-';
        len = 1;
    }
// I am going to convert my numbers to decimal using explicit code here.
// in an earlier draft I used sprintf(), however that adds unnecessary
// overhead.
    char buffer[24];
    int bp = 0;
// The first part of the number is printed naturally so that it only
// uses as many bytes of output as it needs.
    do
    {   buffer[bp++] = '0' + top%10;
        top = top/10;
    } while (top != 0);
    do
    {   *p1++ = buffer[--bp];
        len++;
    } while (bp != 0);
    my_assert(len + 19*(m/sizeof(uint64_t)-p)<= m);
    while (p < m/sizeof(uint64_t))
    {
// I will always pick up the number I am going to expand before writing any
// digits into the buffer.
        top = r[p++];
// For subsequent chunks I want to print exactly 19 decimal digits.
        for (int i=0; i<18; i++)
        {   p1[18-i] = '0' + top%10;
            top = top/10;
        }
        *p1 = '0' + (int)top;
        p1 += 19;
        len += 19;
        my_assert(len <= m);
    }
// To convince myself that this is safe consider when I pick up the final
// chunk. It will turn into 19 bytes of output, so where it comes from must
// be no more than 19 bytes before the length (m) of the final string, because
// otherwise it would have got clobbered when I unpacked the previous chunk.
// But this final chunk is itself 8 bytes wide and there can be up to 7 bytes
// beyond it that are there to support proper alignment - so that last chunk
// lives within the final 15 bytes of the buffer and that is a fortiori within
// the last 19 as required.
    return confirm_size_string(rc, m, len);
}

inline string_handle bignum_to_string(intptr_t aa)
{   uint64_t *a, v[1];
    size_t lena;
    if (stored_as_fixnum(aa))
    {   v[0] = int_of_handle(aa);
        a = v;
        lena = 1;
    }
    else
    {   a = vector_of_handle(aa);
        lena = number_size(a);
    }
    return bignum_to_string(a, lena);
}

// As well as converting to decimal I can do hex, octal or binary!

inline string_handle bignum_to_string_hex(intptr_t aa)
{   uint64_t *a, v[1];
    size_t n;
    if (stored_as_fixnum(aa))
    {   v[0] = int_of_handle(aa);
        a = v;
        n = 1;
    }
    else
    {   a = vector_of_handle(aa);
        n = number_size(a);
    }
// Making the value zero a special case simplifies things later on!
    if (n == 1 && a[0] == 0)
    {   char *r = reserve_string(1);
        strcpy(r, "0");
        return confirm_size_string(r, 1, 1);
    }
// printing in hexadecimal should be way easier!
    size_t m = 16*n;
    uint64_t top = a[n-1];
    bool sign = negative(top);
    if (sign)
    {   m += 2; // for "~f"
        while ((top>>60) == 0xf)
        {   top = top << 4;
            m--;
        }
    }
    else
    {   while (top == 0)
        {   n--;
            top = a[n-1];
        }
        while ((top>>60) == 0)
        {   top = top << 4;
            m--;
        }
    }
    char *r = reserve_string(m);
    char *p = (char *)r;
    top = a[n-1];
    if (sign)
    {   *p++ = '~';
        *p++ = 'f';
    }
    bool started = false;
    for (size_t i=0; i<n; i++)
    {   uint64_t v = a[n-i-1];
        for (int j=0; j<16; j++)
        {   int d = (int)(v >> (60-4*j)) & 0xf;
            if (!started)
            {   if ((sign && d==0xf) ||
                    (!sign && d==0)) continue;
                started = true;
            }
            *p++ = "0123456789abcdef"[d];
        }
    }   
    return confirm_size_string(r, m, m);
}

inline string_handle bignum_to_string_octal(intptr_t aa)
{   uint64_t *a, v[1];
    size_t n;
    if (stored_as_fixnum(aa))
    {   v[0] = int_of_handle(aa);
        a = v;
        n = 1;
    }
    else
    {   a = vector_of_handle(aa);
        n = number_size(a);
    }
    size_t width = (64*n + 2)/3; // raw number of octal digits needed.
    uint64_t top = a[n-1];
    bool sign = negative(top);
// There is a slight misery in that 64 is not a multiple of 3 (!) and so
// the octal representation of a value has some digits that depend on a pair
// of adjacent words from the bignum.
    size_t nn;  // will be the number of characters used in the output
    if (sign)
    {   while (read_u3(a, n, width-1) == 7 && width > 1) width--;
        nn = width+2;
    }
    else
    {   while (read_u3(a, n, width-1) == 0 && width > 1) width--;
        nn = width;
    }
    char *r = reserve_string(nn);
    char *p = (char *)r;
    if (sign)
    {   *p++ = '~';
        *p++ = '7';
    }
    for (size_t i=0; i<width; i++)
        *p++ = '0' + read_u3(a, n, width-i-1);
    return confirm_size_string(r, nn, width);
}

inline string_handle bignum_to_string_binary(intptr_t aa)
{   uint64_t *a, v[1];
    size_t n;
    if (stored_as_fixnum(aa))
    {   v[0] = int_of_handle(aa);
        a = v;
        n = 1;
    }
    else
    {   a = vector_of_handle(aa);
        n = number_size(a);
    }
// Making the value zero a special case simplifies things later on!
    if (n == 1 && a[0] == 0)
    {   char *r = reserve_string(1);
        strcpy(r, "0");
        return confirm_size_string(r, 1, 1);
    }
    size_t m = 64*n;
    uint64_t top = a[n-1];
    bool sign = negative(top);
    if (sign)
    {   m += 2; // for "~1"
        while ((top>>63) == 1)
        {   top = top << 1;
            m--;
        }
    }
    else
    {   my_assert(top != 0);
        while ((top>>63) == 0)
        {   top = top << 1;
            m--;
        }
    }
    char *r = reserve_string(m);
    char *p = (char *)r;
    top = a[n-1];
    if (sign)
    {   *p++ = '~';
        *p++ = '1';
    }
    bool started = false;
    for (size_t i=0; i<n; i++)
    {   uint64_t v = a[n-i-1];
        for (int j=0; j<64; j++)
        {   int d = (int)(v >> (63-j)) & 0x1;
            if (!started)
            {   if ((sign && d==1) ||
                    (!sign && d==0)) continue;
                started = true;
            }
            *p++ = '0' + d;
        }
    }   
    return confirm_size_string(r, m, m);
}

//=========================================================================
//=========================================================================
// Big number comparisons.
//=========================================================================
//=========================================================================


inline bool Zerop::op(uint64_t *a)
{   return number_size(a) == 1 && a[0] == 0;
}

inline bool Zerop::op(int64_t a)
{   return a == 0;
}

inline bool Onep::op(uint64_t *a)
{   return number_size(a) == 1 && a[0] == 1;
}

inline bool Onep::op(int64_t a)
{   return a == 1;
}

inline bool Minusp::op(uint64_t *a)
{   return negative(a[number_size(a)-1]);
}

inline bool Minusp::op(int64_t a)
{   return a < 0;
}

inline bool Evenp::op(uint64_t *a)
{   return (a[number_size(a)-1] & 1) == 0;
}

inline bool Evenp::op(int64_t a)
{   return (a & 1) == 0;
}

inline bool Oddp::op(uint64_t *a)
{   return (a[number_size(a)-1] & 1) != 0;
}

inline bool Oddp::op(int64_t a)
{   return (a & 1) != 0;
}

// eqn

inline bool bigeqn(const uint64_t *a, size_t lena,
            const uint64_t *b, size_t lenb)
{   if (lena != lenb) return false;
    return std::memcmp(a, b, lena*sizeof(uint64_t)) == 0;   
}


inline bool Eqn::op(uint64_t *a, uint64_t *b)
{   size_t lena = number_size(a);
    size_t lenb = number_size(b);
    return bigeqn(a, lena, b, lenb);
}

inline bool Eqn::op(uint64_t *a, int64_t b)
{   size_t lena = number_size(a);
    return lena==1 && (int64_t)a[0]==b;
}

inline bool Eqn::op(int64_t a, uint64_t *b)
{   size_t lenb = number_size(b);
    return lenb==1 && a==(int64_t)b[0];
}

inline bool Eqn::op(int64_t a, int64_t b)
{   return (a == b);
}

// greaterp

inline bool biggreaterp(const uint64_t *a, size_t lena,
                        const uint64_t *b, size_t lenb)
{   uint64_t a0 = a[lena-1], b0 = b[lenb-1];
// If one of the numbers has more digits than the other then the sign of
// the longer one gives my the answer.
    if (lena > lenb) return positive(a0);
    else if (lenb > lena) return negative(b0);
// When the two numbers are the same length but the top digits differ
// then comparing those digits tells me all I need to know.
    if ((int64_t)a0 > (int64_t)b0) return true;
    if ((int64_t)a0 < (int64_t)b0) return false;
// Otherwise I need to scan down through digits...
    lena--;
    while (lena != 0)
    {   lena--;
        a0 = a[lena];
        b0 = b[lena];
        if (a0 > b0) return true;
        if (a0 < b0) return false;
    }
    return false;
}

inline bool Greaterp::op(uint64_t *a, uint64_t *b)
{   size_t lena = number_size(a);
    size_t lenb = number_size(b);
    return biggreaterp(a, lena, b, lenb);
}

inline bool Greaterp::op(uint64_t *a, int64_t bb)
{   uint64_t b[1] = {(uint64_t)bb};
    size_t lena = number_size(a);
    return biggreaterp(a, lena, b, 1);
}

inline bool Greaterp::op(int64_t aa, uint64_t *b)
{   uint64_t a[1] = {(uint64_t)aa};
    size_t lenb = number_size(b);
    return biggreaterp(a, 1, b, lenb);
}

inline bool Greaterp::op(int64_t a, int64_t b)
{   return a > b;
}

// geq

inline bool Geq::op(uint64_t *a, uint64_t *b)
{   return !Greaterp::op(b, a);
}

inline bool Geq::op(uint64_t *a, int64_t b)
{   return !Greaterp::op(b, a);
}

inline bool Geq::op(int64_t a, uint64_t *b)
{   return !Greaterp::op(b, a);
}

inline bool Geq::op(int64_t a, int64_t b)
{   return a >= b;
}

// lessp

inline bool Lessp::op(uint64_t *a, uint64_t *b)
{   return Greaterp::op(b, a);
}

inline bool Lessp::op(uint64_t *a, int64_t b)
{   return Greaterp::op(b, a);
}

inline bool Lessp::op(int64_t a, uint64_t *b)
{   return Greaterp::op(b, a);
}

inline bool Lessp::op(int64_t a, int64_t b)
{   return a < b;
}


// leq

inline bool Leq::op(uint64_t *a, uint64_t *b)
{   return !Greaterp::op(a, b);
}

inline bool Leq::op(uint64_t *a, int64_t b)
{   return !Greaterp::op(a, b);
}

inline bool Leq::op(int64_t a, uint64_t *b)
{   return !Greaterp::op(a, b);
}

inline bool Leq::op(int64_t a, int64_t b)
{   return a <= b;
}


// Negation, addition and subtraction. These are easy apart from a mess
// concerning the representation of positive numbers that risk having the
// most significant bit of their top word a 1, and the equivalent for
// negative numbers.
// Boolean operations all treat negative numbers as if there had been an
// unending string of 1 bits before the stop bit that is stored.
//=========================================================================
//=========================================================================


// Negation. Note that because I am using 2s complement the result could be
// one word longer or shorter than the input. For instance if you negate
// [0x8000000000000000] (a negative value) you get [0,0x8000000000000000],
// and vice versa.

inline void bignegate(const uint64_t *a, size_t lena, uint64_t *r, size_t &lenr)
{   internal_negate(a, lena, r);
// When I negate (-(2^(64n-1))) I will need to place a zero work ahead of the
// value that is mow positive, making the bignum one digit longer.
// If I have 2^(64n-1) it will have been represented with that padding zero
// ahead of it, but when negated the bignum can shrink.
    if (r[lena-1]==topbit) r[lena++] = 0;
    else if (r[lena-1]==UINT64_C(0xffffffffffffffff) && lena>1 &&
        negative(r[lena-2])) lena--; 
    lenr = lena;
}

intptr_t Minus::op(uint64_t *a)
{   size_t n = number_size(a);
    uint64_t *p = reserve(n+1);
    size_t final_n;
    bignegate(a, n, p, final_n);
    return confirm_size(p, n+1, final_n);
}

// The following can only be called via op_dispatch1(), and in that
// case the argument has to have started off as a fixnum. In such cases
// the result will also be a fixnum except when negating MIN_FIXNUM. But
// even in that case (-a) can not overflow 64-bit arithmetic because
// the fixnum will have had at least one tag bit.

intptr_t Minus::op(int64_t a)
{   if (a == MIN_FIXNUM) return int_to_bignum(-a);
    else return int_to_handle(-a);
}

intptr_t Add1::op(uint64_t *a)
{   return Plus::op(a, 1);
}

intptr_t Add1::op(int64_t a)
{   return int_to_bignum(a+1);
}

intptr_t Sub1::op(uint64_t *a)
{   return Plus::op(a, -1);
}

intptr_t Sub1::op(int64_t a)
{   return int_to_bignum(a-1);
}

intptr_t Abs::op(uint64_t *a)
{   size_t n = number_size(a);
    if (!negative(a[n-1]))
    {   uint64_t *r = reserve(n);
        std::memcpy(r, a, n*sizeof(uint64_t));
        return confirm_size(r, n, n);
    }
    uint64_t *r = reserve(n+1);
    size_t final_n;
    bignegate(a, n, r, final_n);
    return confirm_size(r, n+1, final_n);
}

// The following can only be called via op_dispatch1(), and in that
// case the argument has to have started off as a fixnum. In such cases
// the result will also be a fixnum except when negating MIN_FIXNUM. But
// even in that case (-a) can not overflow 64-bit arithmetic because
// the fixnum will have had at least one tag bit.

intptr_t Abs::op(int64_t a)
{   if (a == MIN_FIXNUM) return unsigned_int_to_bignum(-a);
    else return int_to_handle(a<0 ? -a : a);
}

// The "bitnot" operation is simple and length can not change.

inline void biglognot(const uint64_t *a, size_t lena, uint64_t *r, size_t &lenr)
{   for (size_t i=0; i<lena; i++)
    {   r[i] = ~a[i];
    }
    lenr = lena;
}

intptr_t Lognot::op(uint64_t *a)
{   size_t n = number_size(a);
    uint64_t *p = reserve(n+1);
    size_t final_n;
    biglognot(a, n, p, final_n);
    return confirm_size(p, n+1, final_n);
}

intptr_t Lognot::op(int64_t a)
{   return int_to_handle(~a);
}

// logand

inline void ordered_biglogand(const uint64_t *a, size_t lena,
                              const uint64_t *b, size_t lenb,
                              uint64_t *r, size_t &lenr)
{   for (size_t i=0; i<lenb; i++)
        r[i] = a[i] & b[i];
    if (negative(b[lenb-1]))
    {   for (size_t i=lenb; i<lena; i++) r[i] = a[i];
        lenr = lena;
    }
    else lenr = lenb;
    truncate_positive(r, lenr);
}

inline void biglogand(const uint64_t *a, size_t lena,
                      const uint64_t *b, size_t lenb,
                      uint64_t *r, size_t &lenr)
{   if (lena >= lenb) return ordered_biglogand(a, lena, b, lenb, r, lenr);
    else return ordered_biglogand(b, lenb, a, lena, r, lenr);
}

intptr_t Logand::op(uint64_t *a, uint64_t *b)
{   size_t lena = number_size(a);
    size_t lenb = number_size(b);
    size_t n;
    if (lena >= lenb) n = lena;
    else n = lenb;
    uint64_t *p = reserve(n);
    size_t final_n;
    biglogand(a, lena, b, lenb, p, final_n);
    return confirm_size(p, n, final_n);
}

// The next two are not optimised - a case of (logand bignum positive-fixnum)
// is guaranteed to end up a fixnum spo can be done more slickly.

intptr_t Logand::op(uint64_t *a, int64_t b)
{   size_t lena = number_size(a);
    uint64_t *p = reserve(lena);
    size_t final_n;
    uint64_t bb[1] = {(uint64_t)b};
    biglogand(a, lena, bb, 1, p, final_n);
    return confirm_size(p, lena, final_n);
}

intptr_t Logand::op(int64_t a, uint64_t *b)
{   size_t lenb = number_size(b);
    uint64_t *p = reserve(lenb);
    size_t final_n;
    uint64_t aa[1] = {(uint64_t)a};
    biglogand(aa, 1, b, lenb, p, final_n);
    return confirm_size(p, lenb, final_n);
}

intptr_t Logand::op(int64_t a, int64_t b)
{   return int_to_handle(a & b);
}

// logor

inline void ordered_biglogor(const uint64_t *a, size_t lena,
                             const uint64_t *b, size_t lenb,
                             uint64_t *r, size_t &lenr)
{   for (size_t i=0; i<lenb; i++)
        r[i] = a[i] | b[i];
    if (negative(b[lenb-1])) lenr = lenb;
    else
    {   for (size_t i=lenb; i<lena; i++) r[i] = a[i];
        lenr = lena;
    }
    truncate_negative(r, lenr);
}

inline void biglogor(const uint64_t *a, size_t lena,
                     const uint64_t *b, size_t lenb,
                     uint64_t *r, size_t &lenr)
{   if (lena >= lenb) return ordered_biglogor(a, lena, b, lenb, r, lenr);
    else return ordered_biglogor(b, lenb, a, lena, r, lenr);
}

intptr_t Logor::op(uint64_t *a, uint64_t *b)
{   size_t lena = number_size(a);
    size_t lenb = number_size(b);
    size_t n;
    if (lena >= lenb) n = lena;
    else n = lenb;
    uint64_t *p = reserve(n);
    size_t final_n;
    biglogor(a, lena, b, lenb, p, final_n);
    return confirm_size(p, n, final_n);
}

intptr_t Logor::op(uint64_t *a, int64_t b)
{   size_t lena = number_size(a);
    uint64_t *p = reserve(lena);
    size_t final_n;
    uint64_t bb[1] = {(uint64_t)b};
    biglogor(a, lena, bb, 1, p, final_n);
    return confirm_size(p, lena, final_n);
}

intptr_t Logor::op(int64_t a, uint64_t *b)
{   size_t lenb = number_size(b);
    uint64_t *p = reserve(lenb);
    size_t final_n;
    uint64_t aa[1] = {(uint64_t)a};
    biglogor(aa, 1, b, lenb, p, final_n);
    return confirm_size(p, lenb, final_n);
}

intptr_t Logor::op(int64_t a, int64_t b)
{   return int_to_handle(a | b);
}

// logxor

inline void ordered_biglogxor(const uint64_t *a, size_t lena,
                              const uint64_t *b, size_t lenb,
                              uint64_t *r, size_t &lenr)
{   size_t i;
    for (i=0; i<lenb; i++)
        r[i] = a[i] ^ b[i];
    if (negative(b[lenb-1]))
    {   for (; i<lena; i++)
            r[i] = ~a[i];
    }
    else
    {   for (; i<lena; i++)
            r[i] = a[i];
    }
    lenr = lena;
// The logxor operation can cause the inputs to shrink.
    truncate_positive(r, lenr);
    truncate_negative(r, lenr);
}

inline void biglogxor(const uint64_t *a, size_t lena,
                      const uint64_t *b, size_t lenb,
                      uint64_t *r, size_t &lenr)
{   if (lena >= lenb) return ordered_biglogxor(a, lena, b, lenb, r, lenr);
    else return ordered_biglogxor(b, lenb, a, lena, r, lenr);
}

intptr_t Logxor::op(uint64_t *a, uint64_t *b)
{   size_t lena = number_size(a);
    size_t lenb = number_size(b);
    size_t n;
    if (lena >= lenb) n = lena;
    else n = lenb;
    uint64_t *p = reserve(n);
    size_t final_n;
    biglogxor(a, lena, b, lenb, p, final_n);
    return confirm_size(p, n, final_n);
}

intptr_t Logxor::op(uint64_t *a, int64_t b)
{   size_t lena = number_size(a);
    uint64_t *p = reserve(lena);
    size_t final_n;
    uint64_t bb[1] = {(uint64_t)b};
    biglogxor(a, lena, bb, 1, p, final_n);
    return confirm_size(p, lena, final_n);
}

intptr_t Logxor::op(int64_t a, uint64_t *b)
{   size_t lenb = number_size(b);
    uint64_t *p = reserve(lenb);
    size_t final_n;
    uint64_t aa[1] = {(uint64_t)a};
    biglogxor(aa, 1, b, lenb, p, final_n);
    return confirm_size(p, lenb, final_n);
}

intptr_t Logxor::op(int64_t a, int64_t b)
{   return int_to_handle(a ^ b);
}


inline void bigrightshift(const uint64_t *a, size_t lena,
                          int n,
                          uint64_t *r, size_t &lenr);

inline void bigleftshift(const uint64_t *a, size_t lena,
                         int n,
                         uint64_t *r, size_t &lenr)
{   if (n == 0)
    {   internal_copy(a, lena, r);
        lenr = lena;
        return;
    }
    else if (n < 0)
    {   bigrightshift(a, lena, -n, r, lenr);
        return;
    }
    size_t words = n/64;
    size_t bits = n % 64;
    for (size_t i=0; i<words; i++) r[i] = 0;
    if (bits == 0)
    {   for (size_t i=0; i<lena; i++)
           r[i+words] = a[i];
        lenr = lena+words;
    }
    else
    {   r[words] = a[0]<<bits;
        for (size_t i=1; i<lena; i++)
           r[i+words] = (a[i]<<bits) |
                        (a[i-1]>>(64-bits));
        r[words+lena] = (negative(a[lena-1]) ? ((uint64_t)(-1))<<bits : 0) |
                        (a[lena-1]>>(64-bits));
        lenr = lena+words+1;
    }
    truncate_positive(r, lenr);
    truncate_negative(r, lenr);
    
}

inline intptr_t rightshift_b(uint64_t *a, int n);

intptr_t Leftshift::op(uint64_t *a, int64_t n)
{   if (n == 0) return copy_if_no_garbage_collector(a);
    else if (n < 0) return Rightshift::op(a, -n);
    size_t lena = number_size(a);
    size_t nr = lena + (n/64) + 1;
    uint64_t *p = reserve(nr);
    size_t final_n;
    bigleftshift(a, lena, n, p, final_n);
    return confirm_size(p, nr, final_n);
}

intptr_t Leftshift::op(uint64_t *a, int32_t n)
{   return Leftshift::op(a, (int64_t)n);
}

intptr_t Leftshift::op(int64_t aa, int64_t n)
{   if (n == 0) return int_to_handle(aa);
    else if (n < 0) return Rightshift::op(aa, -n);
    size_t nr = (n/64) + 2;
    uint64_t *p = reserve(nr);
    size_t final_n;
    uint64_t a[1] = {(uint64_t)aa};
    bigleftshift(a, 1, n, p, final_n);
    return confirm_size(p, nr, final_n);
}

intptr_t Leftshift::op(int64_t a, int32_t n)
{   return Leftshift::op(a, (int64_t)n);
}

inline void bigrightshift(const uint64_t *a, size_t lena,
                          int n,
                          uint64_t *r, size_t &lenr)
{   if (n == 0)
    {   internal_copy(a, lena, r);
        lenr = lena;
        return;
    }
    else if (n < 0)
    {   bigleftshift(a, lena, -n, r, lenr);
        return;
    }
    size_t words = n/64;
    size_t bits = n % 64;
    if (words >= lena)
    {   r[0] = negative(a[lena-1]) ? -(uint64_t)1 : 0;
        lenr = 1;
    }
    else if (bits == 0)
    {   for (size_t i=0; i<lena-words; i++)
           r[i] = a[i+words];
        lenr = lena-words;
    }
    else
    {   for (size_t i=0; i<lena-words-1; i++)
           r[i] = (a[i+words]>>bits) |
                  (a[i+words+1]<<(64-bits));
        r[lena-words-1] = ASR(a[lena-1], bits);
        lenr = lena-words;
    }
    truncate_positive(r, lenr);
    truncate_negative(r, lenr);
}

intptr_t Rightshift::op(uint64_t *a, int64_t n)
{   if (n == 0) return copy_if_no_garbage_collector(a);
    else if (n < 0) return Leftshift::op(a, -n);
    size_t lena = number_size(a);
    size_t nr;
    if (lena > (size_t)n/64) nr = lena - n/64;
    else nr = 1;
    uint64_t *p = reserve(nr);
    size_t final_n;
    bigrightshift(a, lena, n, p, final_n);
    return confirm_size(p, nr, final_n);
}

intptr_t Rightshift::op(uint64_t *a, int32_t n)
{   return Rightshift::op(a, (int64_t)n);
}

intptr_t Rightshift::op(int64_t a, int64_t n)
{   if (n == 0) return int_to_handle(a);
    else if (n < 0) return Leftshift::op(a, -n);
// Shifts of 64 and up obviously lose all the input data apart from its
// sign, but so does a shift by 63.
    if (n >= 63) return int_to_handle(a>=0 ? 0 : -1);
// Because C++ does not guarantee that right shifts on signed values
// duplicate the sign bit I perform the "shift" here using division by
// a power of 2. Because I have n <= 62 here I will not get overflow.
    int64_t q = ((int64_t)1)<<n; 
    return int_to_handle((a & ~(q-1))/q);
}

intptr_t Rightshift::op(int64_t a, int32_t n)
{   return Rightshift::op(a, (int64_t)n);
}

size_t Integer_length::op(uint64_t *a)
{   return bignum_bits(a, number_size(a));
}

size_t Integer_length::op(int64_t aa)
{   uint64_t a;
    if (aa == 0 || aa == -1) return 0;
    else if (aa < 0) a = -(uint64_t)aa - 1;
    else a = aa;
    return (size_t)(64-nlz(a));

}

size_t Logcount::op(uint64_t *a)
{   size_t lena = number_size(a);
    size_t r = 0;
    if (negative(a[lena-1]))
    {   for (size_t i=0; i<lena; i++) r += popcount(~a[i]);
    }
    else for (size_t i=0; i<lena; i++) r += popcount(a[i]);
    return r;
}

size_t Logcount::op(int64_t a)
{   if (a < 0) return (size_t)popcount(~a);
    else return (size_t)popcount(a);
}

bool Logbitp::op(uint64_t *a, size_t n)
{   size_t lena = number_size(a);
    if (n >= 64*lena) return negative(a[lena-1]);
    return (a[n/64] & (((uint64_t)1) << (n%64))) != 0;
}

bool Logbitp::op(int64_t a, size_t n)
{   if (n >= 64) return (a < 0);
    else return (a & (((uint64_t)1) << n)) != 0;
}

// Addition when the length of a is art least than that of b.

inline void ordered_bigplus(const uint64_t *a, size_t lena,
                            const uint64_t *b, size_t lenb,
                            uint64_t *r, size_t &lenr)
{   my_assert(lena >= lenb);
    uint64_t carry = 0;
    size_t i = 0;
// The lowest digits can be added without there being any carry-in.
    carry = add_with_carry(a[0], b[0], r[0]);
// Add the digits that (a) and (b) have in common
    for (i=1; i<lenb; i++)
        carry = add_with_carry(a[i], b[i], carry, r[i]);
// From there on up treat (b) as if it had its sign bit extended to the
// left.
    uint64_t topb = negative(b[lenb-1]) ? allbits : 0;
    for (; i<lena; i++)
        carry = add_with_carry(a[i], topb, carry, r[i]);
// And of course (a) must also be treated as being extended by its sign bit.
    uint64_t topa = negative(a[lena-1]) ? allbits : 0;
// The result calculated here is 1 word longer than (a), and addition
// can never carry further than that.
    r[i] = topa + topb + carry;
// However because I am using (2s complement) signed arithmetic the result
// could be shorter, so I will check for that and return the length that
// is actually needed.
    while (r[i]==0 && i>0 && positive(r[i-1])) i--;
    while (r[i]==allbits && i>0 && negative(r[i-1])) i--;
    lenr = i+1;
}

// Add a small number to a bignum

inline void bigplus_small(const uint64_t *a, size_t lena,
                          int64_t n,
                          uint64_t *r, size_t &lenr)
{   uint64_t w[1];
    w[0] = (uint64_t)n;
    ordered_bigplus(a, lena, w, 1, r, lenr);
}

// When I do a general addition I will not know which input is longer.

inline void bigplus(const uint64_t *a, size_t lena,
                    const uint64_t *b, size_t lenb,
                    uint64_t *r, size_t &lenr)
{   if (lena >= lenb) return ordered_bigplus(a, lena, b, lenb, r, lenr);
    else return ordered_bigplus(b, lenb, a, lena, r, lenr);
}

intptr_t Plus::op(uint64_t *a, uint64_t *b)
{   size_t lena = number_size(a);
    size_t lenb = number_size(b);
    size_t n;
    if (lena >= lenb) n = lena+1;
    else n = lenb+1;
    uint64_t *p = reserve(n);
    size_t final_n;
    bigplus(a, lena, b, lenb, p, final_n);
    return confirm_size(p, n, final_n);
}

// At present I implement the op_ii, opt_ib and opt_bi operations
// by converting the integer argument to a 1-word bignum and dropping into
// the general bignum code. This will generally be a long way from the
// most efficient implementation, so at a later stage I will want to hone
// the code to make it better!

intptr_t Plus::op(int64_t a, int64_t b)
{
// The two integer arguments will in fact each have been derived from a
// tagged representation, and a consequence of that is that I can add
// them and be certain I will not get arithmetic overflow. However the
// resulting value may no longer be representable as a fixnum.
    int64_t c = a + b;
    if (fits_into_fixnum(c)) return int_to_handle(c);
// Now because there had not been overflow I know that the bignum will
// only need one word.
    uint64_t *r = reserve(1);
    r[0] = c;
    return confirm_size(r, 1, 1);
}

intptr_t Plus::op(int64_t a, uint64_t *b)
{   uint64_t aa[1];
    aa[0] = a;
    size_t lenb = number_size(b);
    uint64_t *r = reserve(lenb+1);
    size_t final_n;
    bigplus(aa, 1, b, lenb, r, final_n);
    return confirm_size(r, lenb+1, final_n);
}

intptr_t Plus::op(uint64_t *a, int64_t b)
{   size_t lena = number_size(a);
    uint64_t bb[1];
    bb[0] = b;
    uint64_t *r = reserve(lena+1);
    size_t final_n;
    bigplus(a, lena, bb, 1, r, final_n);
    return confirm_size(r, lena+1, final_n);
}

inline intptr_t bigplus_small(intptr_t aa, int64_t b)
{   uint64_t *a = vector_of_handle(aa);
    size_t lena = number_size(a);
    uint64_t *p = reserve(lena+1);
    size_t final_n;
    bigplus_small(a, lena, b, p, final_n);
    return confirm_size(p, lena+1, final_n);
}

// For subtraction I implement both a-b and b-a. These work by
// computing a + (~b) + 1 and (~a) + b + 1 respectively.

inline void ordered_bigsubtract(const uint64_t *a, size_t lena,
                                const uint64_t *b, size_t lenb,
                                uint64_t *r, size_t &lenr)
{   my_assert(lena >= lenb);
    uint64_t carry = 1;
    size_t i;
// Add the digits that (a) and (b) have in common
    for (i=0; i<lenb; i++)
        carry = add_with_carry(a[i], ~b[i], carry, r[i]);
// From there on up treat (b) as if it had its sign bit extended to the
// left.
    uint64_t topb = negative(~b[lenb-1]) ? allbits : 0;
    for (; i<lena; i++)
        carry = add_with_carry(a[i], topb, carry, r[i]);
// And of course (a) must also be treated as being extended by its sign bit.
    uint64_t topa = negative(a[lena-1]) ? allbits : 0;
// The result calculated here is 1 word longer than (a), and addition
// can never carry further than that.
    r[i] = topa + topb + carry;
// However because I am using (2s complement) signed arithmetic the result
// could be shorter, so I will check for that and return the length that
// is actually needed.
    while (r[i]==0 && i>0 && positive(r[i-1])) i--;
    while (r[i]==allbits && i>0 && negative(r[i-1])) i--;
    lenr = i+1;
}

inline void ordered_bigrevsubtract(const uint64_t *a, size_t lena,
                                   const uint64_t *b, size_t lenb,
                                   uint64_t *r, size_t &lenr)
{   my_assert(lena >= lenb);
    uint64_t carry = 1;
    size_t i;
// Add the digits that (a) and (b) have in common
    for (i=0; i<lenb; i++)
        carry = add_with_carry(~a[i], b[i], carry, r[i]);
// From there on up treat (b) as if it had its sign bit extended to the
// left.
    uint64_t topb = negative(b[lenb-1]) ? allbits : 0;
    for (; i<lena; i++)
        carry = add_with_carry(~a[i], topb, carry, r[i]);
// And of course (a) must also be treated as being extended by its sign bit.
    uint64_t topa = negative(~a[lena-1]) ? allbits : 0;
// The result calculated here is 1 word longer than (a), and addition
// can never carry further than that.
    r[i] = topa + topb + carry;
// However because I am using (2s complement) signed arithmetic the result
// could be shorter, so I will check for that and return the length that
// is actually needed.
    while (r[i]==0 && i>0 && positive(r[i-1])) i--;
    while (r[i]==allbits && i>0 && negative(r[i-1])) i--;
    lenr = i+1;
}

// Subtract a small number from a bignum

inline void bigsubtract_small(const uint64_t *a, size_t lena,
                              int64_t n,
                              uint64_t *r, size_t &lenr)
{   uint64_t w[1];
    w[0] = (uint64_t)n;
    ordered_bigsubtract(a, lena, w, 1, r, lenr);
}

// subtract a bignum from a small number

inline void bigrevsubtract_small(const uint64_t *a, size_t lena,
                                 int64_t n,
                                 uint64_t *r, size_t &lenr)
{   uint64_t w[1];
    w[0] = (uint64_t)n;
    ordered_bigrevsubtract(a, lena, w, 1, r, lenr);
}


inline void bigsubtract(const uint64_t *a, size_t lena,
                        const uint64_t *b, size_t lenb,
                        uint64_t *r, size_t &lenr)
{   if (lena >= lenb) return ordered_bigsubtract(a, lena, b, lenb, r, lenr);
    else return ordered_bigrevsubtract(b, lenb, a, lena, r, lenr);
}

intptr_t Difference::op(uint64_t *a, uint64_t *b)
{   size_t lena = number_size(a);
    size_t lenb = number_size(b);
    size_t n;
    if (lena >= lenb) n = lena+1;
    else n = lenb+1;
    uint64_t *p = reserve(n);
    size_t final_n;
    bigsubtract(a, lena, b, lenb, p, final_n);
    return confirm_size(p, n, final_n);
}

intptr_t Difference::op(int64_t a, int64_t b)
{   uint64_t aa[1], bb[1];
    aa[0] = a;
    bb[0] = b;
    uint64_t *r = reserve(2);
    size_t final_n;
    bigsubtract(aa, 1, bb, 1, r, final_n);
    return confirm_size(r, 2, final_n);
}

intptr_t Difference::op(int64_t a, uint64_t *b)
{   uint64_t aa[1];
    aa[0] = a;
    size_t lenb = number_size(b);
    uint64_t *r = reserve(lenb+1);
    size_t final_n;
    bigsubtract(aa, 1, b, lenb, r, final_n);
    return confirm_size(r, lenb+1, final_n);
}

intptr_t Difference::op(uint64_t *a, int64_t b)
{   size_t lena = number_size(a);
    uint64_t bb[1];
    bb[0] = b;
    uint64_t *r = reserve(lena+1);
    size_t final_n;
    bigsubtract(a, lena, bb, 1, r, final_n);
    return confirm_size(r, lena+1, final_n);
}


intptr_t Revdifference::op(uint64_t *a, uint64_t *b)
{   size_t lena = number_size(a);
    size_t lenb = number_size(b);
    size_t n;
    if (lena >= lenb) n = lena+1;
    else n = lenb+1;
    uint64_t *p = reserve(n);
    size_t final_n;
    bigsubtract(b, lenb, a, lena, p, final_n);
    return confirm_size(p, n, final_n);
}

intptr_t Revdifference::op(int64_t a, int64_t b)
{   uint64_t aa[1], bb[1];
    aa[0] = a;
    bb[0] = b;
    uint64_t *r = reserve(2);
    size_t final_n;
    bigsubtract(bb, 1, aa, 1, r, final_n);
    return confirm_size(r, 2, final_n);
}

intptr_t Revdifference::op(int64_t a, uint64_t *b)
{   uint64_t aa[1];
    aa[0] = a;
    size_t lenb = number_size(b);
    uint64_t *r = reserve(lenb+1);
    size_t final_n;
    bigsubtract(b, lenb, aa, 1, r, final_n);
    return confirm_size(r, lenb+1, final_n);
}

intptr_t Revdifference::op(uint64_t *a, int64_t b)
{   size_t lena = number_size(a);
    uint64_t bb[1];
    bb[0] = b;
    uint64_t *r = reserve(lena+1);
    size_t final_n;
    bigsubtract(bb, 1, a, lena, r, final_n);
    return confirm_size(r, lena+1, final_n);
}

//=========================================================================
//=========================================================================
// multiplication, squaring and exponentiation.
//=========================================================================
//=========================================================================

inline void bigmultiply(const uint64_t *a, size_t lena,
                        const uint64_t *b, size_t lenb,
                        uint64_t *r, size_t &lenr)
{   for (size_t i=0; i<lena+lenb; i++) r[i] = 0;
// If a and/or be are negative then I can treat their true values as
//    a = sa + va      b = sb + vb
// where sa and sb and the signs - represented here as 0 for a positive
// number and -2^(64*len) for a negative one. va and vb are then the simple
// bit-patterns for a and b but now interpreted as unsigned values. So if
// instead of using 64-bit digits I was using 8 bit ones, the value -3
// would be stored as 0xfd and that would be spit up as -128 + 253.
// Then a*b = sa*sb + sa*vb + sb*va + va*vb.
// The last item there is just the product of a and b when treated as
// unsigned values, and so is what I compute first here rather simply.
// If sa and/or sb is non-zero it is just the negative of a power of 2^64,
// and so I can correct the unsigned product into a signed one by (sometimes)
// subtracting a shifted version of a or b from it.
    for (size_t i=0; i<lena; i++)
    {   uint64_t hi = 0;
        for (size_t j=0; j<lenb; j++)
        {   uint64_t lo;
// The largest possible value if (hi,lo) here is (0xffffffffffffffff, 0)
// which arises if a[1], b[i] and prev_hi are all at their maximum. That
// means that in all other cases (and in particular unless lo==0) hi ends
// up LESS than the maximum, and so adding one to it can happen without
// overflow.
            multiplyadd64(a[i], b[j], hi, hi, lo);
            hi += add_with_carry(lo, r[i+j], r[i+j]);
        }
        r[i+lenb] = hi;
    }
    if (negative(a[lena-1]))
    {   uint64_t carry = 1;
        for (size_t i=0; i<lenb; i++)
            carry = add_with_carry(r[i+lena], ~b[i], carry, r[i+lena]);
    }
    if (negative(b[lenb-1]))
    {   uint64_t carry = 1;
        for (size_t i=0; i<lena; i++)
            carry = add_with_carry(r[i+lenb], ~a[i], carry, r[i+lenb]);
    }
    lenr = lena + lenb;
// The actual value may be 1 word shorter than this.
//  test top digit or r and if necessary reduce lenr.
    truncate_positive(r, lenr);
    truncate_negative(r, lenr);
}

intptr_t Times::op(uint64_t *a, uint64_t *b)
{   size_t lena = number_size(a);
    size_t lenb = number_size(b);
    size_t n = lena+lenb;
    uint64_t *p = reserve(n);
    size_t final_n;
    bigmultiply(a, lena, b, lenb, p, final_n);
    return confirm_size(p, n, final_n);
}

intptr_t Times::op(int64_t a, int64_t b)
{   uint64_t aa[1], bb[1];
    aa[0] = a;
    bb[0] = b;
    uint64_t *r = reserve(2);
    size_t final_n;
    bigmultiply(aa, 1, bb, 1, r, final_n);
    return confirm_size(r, 2, final_n);
}

intptr_t Times::op(int64_t a, uint64_t *b)
{   uint64_t aa[1];
    aa[0] = a;
    size_t lenb = number_size(b);
    uint64_t *r = reserve(lenb+1);
    size_t final_n;
    bigmultiply(aa, 1, b, lenb, r, final_n);
    return confirm_size(r, lenb+1, final_n);
}

intptr_t Times::op(uint64_t *a, int64_t b)
{   size_t lena = number_size(a);
    uint64_t bb[1];
    bb[0] = b;
    uint64_t *r = reserve(lena+1);
    size_t final_n;
    bigmultiply(a, lena, bb, 1, r, final_n);
    return confirm_size(r, lena+1, final_n);
}

// For big multi-digit numbers squaring can be done almost twice as fast
// as general multiplication. 
// eg (a0,a1,a2,a3)^2 can be expressed as
// a0^2+a1^2+a2^2+a3^2 + 2*(a0*a1+a0*a2+a0*a3+a1*a2+a1*a3+a2*a3)
// where the part that has been doubled uses symmetry to reduce the work.
//
// For negative inputs I can form the product first treating the inputs
// as if they had been unsigned, and then subtract 2*2^w*a from the result.

inline void bigsquare(const uint64_t *a, size_t lena,
                      uint64_t *r, size_t &lenr)
{   for (size_t i=0; i<2*lena; i++) r[i] = 0;
    uint64_t carry;
    lenr = 2*lena;
    for (size_t i=0; i<lena; i++)
    {   uint64_t hi = 0;
// Note that all the terms I add in here will need to be doubled in the
// final accounting.
        for (size_t j=i+1; j<lena; j++)
        {   uint64_t lo;
            multiplyadd64(a[i], a[j], hi, hi, lo);
            hi += add_with_carry(lo, r[i+j], r[i+j]);
        }
        r[i+lena] = hi;
    }
// Double the part that has been computed so far.
    carry = 0;
    for (size_t i=0; i<2*lena; i++)
    {   uint64_t w = r[i];
        r[i] = (w << 1) | carry;
        carry = w >> 63;
    }
// Now add in the bits that do not get doubled.
    carry = 0;
    uint64_t hi = 0;
    for (size_t i=0; i<lena; i++)
    {   uint64_t lo;
        multiplyadd64(a[i], a[i], r[2*i], hi, lo);
        carry = add_with_carry(lo, carry, r[2*i]);
        carry = add_with_carry(hi, r[2*i+1], carry, r[2*i+1]);
    }
// Now if the input had been negative I have a correction to apply...
// I subtract 2a from the top half of the result.
    if (negative(a[lena-1]))
    {   uint64_t carry = 1;
        int fromprev = 0;
        for (size_t i=0; i<lena; i++)
        {   uint64_t d = a[i];
            uint64_t w = (d<<1) | fromprev;
            fromprev = (int)(d>>63);
            carry = add_with_carry(r[lena+i], ~w, carry, r[lena+i]);
        }
    }
// The actual value may be 1 word shorter than this.
//  test top digit or r and if necessary reduce lenr.
    truncate_positive(r, lenr);
    truncate_negative(r, lenr);
}

intptr_t Square::op(uint64_t *a)
{   size_t lena = number_size(a);
    size_t n = 2*lena;
    uint64_t *p = reserve(n);
    size_t final_n;
    bigsquare(a, lena, p, final_n);
    return confirm_size(p, n, final_n);
}

intptr_t Square::op(int64_t a)
{   uint64_t hi, lo;
    multiply64(a, a, hi, lo);
    if (a < 0) hi -= 2u*(uint64_t)a;
// Now I have a 128-bit product of the inputs
    if ((hi == 0 && positive(lo)) ||
        (hi == (uintptr_t)(-1) && negative(lo)))
    {   if (fits_into_fixnum((int64_t)lo)) return int_to_handle((int64_t)lo);
        else
        {   uint64_t *p = reserve(1);
            p[0] = lo;
            return confirm_size(p, 1, 1);
        }
    }
    uint64_t *p = reserve(2);
    p[0] = lo;
    p[1] = hi;
    return confirm_size(p, 2, 2);
}

intptr_t Isqrt::op(uint64_t *a)
{   size_t lena = number_size(a);
    if (lena == 1) return Isqrt::op((int64_t)a[0]);
    size_t lenx = (lena+1)/2;
    uint64_t *x = reserve(lenx);
    for (size_t i=0; i<lenx; i++) x[i] = 0;
    size_t bitstop = a[lena-1]==0 ? 0 : 64 - nlz(a[lena-1]);
    bitstop /= 2;
    if ((lena%2) == 0) bitstop += 32;
    x[lenx-1] = ((uint64_t)1) << bitstop;
    if (bitstop == 63) x[lenx-1]--; // ensure it is still positive!
// I now have a first approximation to the square root as a number that is
// a power of 2 with about half the bit-length of a. I will degenerate into
// using generic arithmetic here even though that may have extra costs.
    Bignum biga(true, vector_to_handle(a));
    Bignum bigx(true, confirm_size(x, lenx, lenx));
// I will do the first step outside the loop to guarantee that my
// approximation is an over-estimate before I try the end-test.
    bigx = (bigx + biga/bigx) >> 1;
    for (;;)
    {   Bignum y = (bigx + biga/bigx) >> 1;
        if (y >= bigx) break;
        bigx = y;
    }
// The Bignum "biga" encapsulated my argument: when its destructor is called
// I do not want the input vector "a" to be clobbered, so I clobber the
// bignum first to break the link. Ditto bigx.
    biga.val = 0;
    intptr_t r = bigx.val;
    bigx.val = 0;
    return r;
}

intptr_t Isqrt::op(int64_t aa)
{   if (aa <= 0) return int_to_bignum(0);
    uint64_t a = (uint64_t)aa;
    size_t w = 64 - nlz(a);
    uint64_t x0 = a >> (w/2);
// The iteration here converges to sqrt(a) from above, but I believe that
// when the value stops changing it will be at floor(sqrt(a)). There are
// some cases where the sequence ends up alternating between two adjacent
// values. Because my input is at most 2^63-1 the number of iterations
// written here will always suffice.
    uint64_t x1 = (x0 + a/x0)/2;
    uint64_t x2 = (x1 + a/x1)/2;
    if (x2 >= x1) return unsigned_int_to_bignum(x1);
    uint64_t x3 = (x2 + a/x2)/2;
    if (x3 >= x2) return unsigned_int_to_bignum(x2);
    uint64_t x4 = (x3 + a/x3)/2;
    if (x4 >= x3) return unsigned_int_to_bignum(x3);
    uint64_t x5 = (x4 + a/x4)/2;
    if (x5 >= x4) return unsigned_int_to_bignum(x4);
    return unsigned_int_to_bignum(x5);
}

// This raises a bignum to a positive integer power. If the power is n then
// the size of the output may be n*lena. The two vectors v and w are workspace
// and must both be of size (at least) the size that the result could end
// up as. Well with greater subtlty we can see that the sum of their sizes
// must be at least the size of the result, but it is not clear that any
// useful saving spece saving can be found down that path.

inline void bigpow(const uint64_t *a, size_t lena, uint64_t n,
                   uint64_t *v,
                   uint64_t *w,
                   uint64_t *r, size_t &lenr, size_t maxlenr)
{   if (n == 0)
    {   r[0] = 0;
        lenr = 1;
        return;
    }
    internal_copy(a, lena, v);
    size_t lenv = lena;
    w[0] = 1;
    size_t lenw = 1;
    while (n > 1)
    {   if (n%2 == 0)
        {   bigsquare(v, lenv, r, lenr);
            my_assert(lenr <= maxlenr);
            internal_copy(r, lenr, v);
            lenv = lenr;
            n = n/2;
        }
        else
        {   bigmultiply(v, lenv, w, lenw, r, lenr);
            my_assert(lenr <= maxlenr);
            internal_copy(r, lenr, w);
            lenw = lenr;
            bigsquare(v, lenv, r, lenr);
            my_assert(lenr <= maxlenr);
            internal_copy(r, lenr, v);
            lenv = lenr;
            n = (n-1)/2;
        }
    }
    bigmultiply(v, lenv, w, lenw, r, lenr);
}

// In cases where n is too large this can fail. At present I deal with that
// with my_assert() statements rather than any comfortable scheme for reporting
// the trouble.

// The code that dispatches into here should have filtered cases such that
// the exponent n is not 0, 1 or 2 here.

intptr_t Pow::op(uint64_t *a, int64_t n)
{   size_t lena = number_size(a);
//  1^(-n) == 1,
//  (-1)^(-n) == 1 if n is even or -1 if n is odd.
//  a^(-n) == 0 when a is not -1, 0 or 1.
    if (n < 0)
    {   int z = 0;
        if (lena == 0)
        {   if ((int64_t)a[0]==1) z = 1;
            else if ((int64_t)a[0]==-1) z = (n%1==0 ? 1 : -1);
            else my_assert(a[0] != 0u);
        }
// 0^(-n) is an error
// 1^(-n) = 1
// (-1)^(-n) = +1 or -1 depending on whether n is odd or even
// x^(-n) = 0 otherwise.
        return int_to_bignum(z);
    }
// 6^n = 0
    size_t bitsa = bignum_bits(a, lena);
    uint64_t hi, bitsr;
    multiply64(n, bitsa, hi, bitsr);
    my_assert(hi == 0); // Check that size is at least somewhat sane!
// I estimate the largest size that my result could be, but then add
// an extra word because the internal working of multiplication can
// write a zero beyond its true result - eg if you are multiplying a pair
// of 1-word numbers together it will believe that the result could be 2
// words wide even if in fact you know it will not be.
    uint64_t lenr1 = 2 + bitsr/64;
    size_t lenr = (size_t)lenr1;
// if size_t was more narrow than 64-bits I could lose information in
// truncating from uint64_t to size_t.
    my_assert(lenr == lenr1);
    uint64_t olenr = lenr;
    uint64_t *r = reserve(lenr);
    uint64_t *v = reserve(lenr);
    uint64_t *w = reserve(lenr);
    bigpow(a, lena, (uint64_t)n, v, w, r, lenr, lenr);
    my_assert(lenr <= olenr);
    abandon(w);
    abandon(v);
    return confirm_size(r, olenr, lenr);
}

intptr_t Pow::op(uint64_t *a, int32_t n)
{   return Pow::op(a, (int64_t)n);
}

// Again the cases n = 0, 1 and 2 have been filtered out

intptr_t Pow::op(int64_t a, int64_t n)
{   if (n < 0)
    {   int z = 0;
        if (a == 1) z = 1;
        else if (a == -1) z = (n%1==0 ? 1 : 0);
        else my_assert(a != 0);
        return int_to_handle(z);
    }
    if (a == 0) return int_to_handle(0);
    else if (a == 1) return int_to_handle(a);
    else if (n == 0) return int_to_handle(1);
    uint64_t absa = (a < 0 ? -(uint64_t)a : (uint64_t)a);
    size_t bitsa = 64 - nlz(absa);
    uint64_t hi, bitsr;
    multiply64(n, bitsa, hi, bitsr);
    my_assert(hi == 0); // Check that size is at least somewhat sane!
    uint64_t lenr1 = 2 + bitsr/64;
    size_t lenr = (size_t)lenr1;
// if size_t was more narrow than 64-bits I could lose information in
// truncating from uint64_t to size_t.
    my_assert(lenr == lenr1);
    uint64_t olenr = lenr;
    uint64_t *r = reserve(lenr);
    uint64_t *v = reserve(lenr);
    uint64_t *w = reserve(lenr);
    uint64_t aa[1] = {(uint64_t)a};
    bigpow(aa, 1, (uint64_t)n, v, w, r, lenr, lenr);
    my_assert(lenr <= olenr);
    abandon(w);
    abandon(v);
    return confirm_size(r, olenr, lenr);
}

intptr_t Pow::op(int64_t a, int32_t n)
{   return Pow::op(a, (int64_t)n);
}

//=========================================================================
//=========================================================================


//
// Division with the main number representation being 2s complement
// turns out to have many messy special cases! Here are some of the
// underlying issues:
// . Inputs may have had initial 0 or -1 digits pre-pended to allow
//   for positive values with their top bit set and negative ones with
//   it clear. So if I had 8-bit words the number 128 would have an
//   unsigned representation of [0x80], but it has to be stored as a
//   two digit number [0x00,0x80]. Similarly some negative numbers
//   need an initial 0xff attached just so that it can be seen that they
//   are negative.
// . If a result (quotient or remainder) is as above then space can be
//   needed for the prefix digit.
// . Long division needs to have a dividend with at least 3 digits
//   (after scaling) and a divisor with at least 2. That means that various
//   small cases have to be treated specially.
// . An operation as basic as taking the absolute value of an integer
//   generally involves allocating memory, and I would like to avoid that
//   as much as I can.
// . quotient and remainder operations are very similar indeed, but I ought
//   to be able to safe memory allocation in one or the other. Specifically
//   if I am computing a remainder I can discard quotient digits as I go
//   rather than having anywhere to put them.
// . On many useful platforms I will have an integer type that is 128 bits
//   wide and I can use that for a 128-by-64 division operation that is
//   really helpful when working with 64-bit digits. It is possible that
//   if I do not have 128-bit arithmetic available it would be best to
//   treat my numbers as being in base 2^32 so that 64-by-32 division is
//   the order of the day as a basic primitive.
// . For Lisp purposes I will have "fixnums" as well as "bignums" so special
//   paths for arithmetic that involves values -2^59 to 2^59-1 will be
//   required.
//
// Well perhaps I am fussing about all the above. But my first drafts of this
// code has not thought through all the cases carefully enough!


// Divide the bignum a by the b, returning a quotient or a remainder or
// both. Note that at this stage a may still be negative! The value b is
// passed in sign and magnitide form as {b, b_negative}

inline void unsigned_short_division(const uint64_t *a, size_t lena,
                                    uint64_t b, bool b_negative,
                                    bool want_q, uint64_t *&q,
                                    size_t &olenq, size_t &lenq,
                                    bool want_r, uint64_t *&r,
                                    size_t &olenr, size_t &lenr)
{   uint64_t hi = 0;
    bool a_negative = false;
    uint64_t *aa;
    if (negative(a[lena-1]))
    {   a_negative = true;
// Take absolute value of a if necessary.
        aa = reserve(lena);
        internal_negate(a, lena, aa);
        a = (const uint64_t *)aa;
    }
// Now both a and b are positive so I can do the division fairly simply.
// Allocate space for the quotient if I need that, and then do standard
// short division.
    size_t i=lena-1;
    if (want_q)
    {   olenq = lena;
        q = reserve(olenq);
    }
    for (;;)
    {   uint64_t d;
        divide64(hi, a[i], b, d, hi);
        if (want_q) q[i] = d;
        if (i == 0) break;
        i--;
    }
// If the original a had been negative I allocated space to store its
// absolute value, and I can discard that now.
    if (a_negative) abandon(aa);
    if (want_q)
    {   lenq = lena;
// The quotient will be negative if divisor and dividend had different signs.
        if (a_negative != b_negative)
        {   internal_negate(q, lenq, q);
            truncate_negative(q, lenq);
        }
        else truncate_positive(q, lenq);
    }
    if (want_r)
    {
// The remainder is now present as an unsigned value in hi. The sign it
// must end up having must match the sign of a (the dividend). Furthermore
// the remainder will be strictly smaller then b, and the largest possible
// value for b is 0xffffffffffffffff. The remainder may need to be returned
// as a 2-digit bignum.
        if (a_negative)
        {   hi = -hi;
            if (positive(hi) && hi!=0)
            {   olenr = lenr = 2;
                r = reserve(olenr);
                r[0] = hi;
                r[1] = -1;
            }
            else
            {   olenr = lenr = 1;
                r = reserve(olenr);
                r[0] = hi;
            }
        }
        else
        {   if (negative(hi))
            {   olenr = lenr = 2;
                r = reserve(olenr);
                r[0] = hi;
                r[1] = 0;
            }
            else
            {   olenr = lenr = 1;
                r = reserve(olenr);
                r[0] = hi;
            }
        }
    }
}

inline void signed_short_division(const uint64_t *a, size_t lena,
                                  int64_t b,
                                  bool want_q, uint64_t *&q,
                                  size_t &olenq, size_t &lenq,
                                  bool want_r, uint64_t *&r,
                                  size_t &olenr, size_t &lenr)
{   if (b > 0) unsigned_short_division(a, lena, (uint64_t)b, false,
                                       want_q, q, olenq, lenq,
                                       want_r, r, olenr, lenr);
    else unsigned_short_division(a, lena, -(uint64_t)b, true,
                                 want_q, q, olenq, lenq,
                                 want_r, r, olenr, lenr);
}

inline void unsigned_long_division(uint64_t *a, size_t &lena,
                                   uint64_t *b, size_t &lenb,
                                   bool want_q, uint64_t *q,
                                   size_t &olenq, size_t &lenq);

// The following is a major entrypoint to the division code. (a) and (b) are
// vectors of digits such that the top digit of a number is treated as signed
// and the lower ones as unsigned. To cope with this there will sometimes
// be a sort of initial padder digit. The two boolean values indicate whether
// either or both of quotient and remainder are required. if want_q is set
// then this creates a new vector for q and return it via q/lenq. Similarly
// for want_r. The inputs a and b can be bignums of any size and are allowed
// to be positive or negative - this sorts everything out.

// Divide a by b to obtain a quotient q and a remainder r. 

inline void division(const uint64_t *a, size_t lena,
                     const uint64_t *b, size_t lenb,
                     bool want_q, uint64_t *&q, size_t &olenq, size_t &lenq,
                     bool want_r, uint64_t *&r, size_t &olenr, size_t &lenr)
{   my_assert(want_q || want_r);
// First I will filter out a number of cases where the divisor is "small".
// I only want to proceed into the general case code if it is a "genuine"
// big number with at least two digits. This bit of the code is messier
// than one might have imagined because of the 2s complement representation
// I use and the fact that extreme values that almost fit in a single
// digit can ends up as 2-digit values with a degenerate top digit.
//
// The first case is when the single digit if b is a signed value in the
// range -2^63 to 2^63-1.
    if (lenb == 1)
    {   my_assert(b[0] != 0); // would be division by zero
        signed_short_division(a, lena, (int64_t)b[0],
                              want_q, q, olenq, lenq,
                              want_r, r, olenr, lenr);
        return;
    }
// Next I have b in the range 2^63 to 2^64-1. Such values can be represented
// in uint64_t. 
    else if (lenb == 2 && b[1]==0)
    {   unsigned_short_division(a, lena, b[0], false,
                                want_q, q, olenq, lenq,
                                want_r, r, olenr, lenr);
        return;
    }
// Now for b in -2^64 to -2^63-1. The 2s complement representetation will be
// of the form (-1,nnn) with nnn an unsigned 64-bit value.
    else if (lenb == 2 && b[1]==(uint64_t)(-1))
    {
// -b(0) is an unsigned representation of the absolute value of b. There is
// one special case when -b(0) is zero, and that corresponds to division
// by -2^64, so I will need to detect that and turn the division into a
// combination of shift and negate operations.
        if (b[0] == 0)
        {   if (want_q)
            {   lenq = lena;
                olenq = lena;
                q = reserve(lena);
// The next line took me some while to arrive at!
                uint64_t carry = !negative(a[lena-1]) || a[0]==0 ? 1 : 0;
                for (size_t i=1; i<lena; i++)
                    carry = add_with_carry(~a[i], carry, q[i-1]);
                q[lena-1] = negative(a[lena-1]) ? 0 : -(uint64_t)1;
                truncate_positive(q, lenq);
                truncate_negative(q, lenq);
            }
            if (want_r)
            {   uint64_t rr = a[0], padr = 0;
                lenr = 1;
                if (negative(a[lena-1]) && positive(rr))
                {   padr = -1;
                    lenr++;
                }
                else if (positive(a[lena-1]) && negative(rr))
                {   padr = 0;
                    lenr++;
                }
                r = reserve(lenr);
                olenr = lenr;
                r[0] = rr;
                if (lenr != 1) r[1] = padr;
            }
            return;
        }
        unsigned_short_division(a, lena, -b[0], true,
                                want_q, q, olenq, lenq,
                                want_r, r, olenr, lenr);
        return;
    }
// Now the absolute value of b will be at least 2 digits of 64-bits with the
// high digit non-zero. I need to make a copy of it because I will scale
// it during long division.
    uint64_t *bb = NULL;
    size_t lenbb = lenb;
//@@    olenr = lenb;
    bool b_negative = negative(b[lenb-1]);
    if (b_negative)
    {
// In the case that b is negative I will want its absolute value. Especially
// in a multi-thread world I must not disturb or overwrite the input vector,
// so a create a temporary copy of b to negate. In my full 2s complement
// representation negating -2^(64*n-1) would lead to what was supposed to be
// a positive value but it would have its top bit set so it would require
// and extra leading 0. Because the value I generate here is to be treated
// as unsigned this leading top bit does not matter and so the absolute value
// of b fits in the same amount of space that b did with no risk of overflow.  
        bb = reserve(lenb);
        olenr = lenb;
        internal_negate(b, lenb, bb);
        if (bb[lenbb-1] == 0) lenbb--;
    }
    else if (b[lenb-1] == 0) lenbb--;
    my_assert(lenbb >= 2);
// Now I should look at the dividend. If it is shorter than the divisor
// then I know that the quotient will be zero and the dividend will be the
// remainder. If I had made this test before normalizing the divisor I could
// have needed to worry about the case of (-2^(64n-1))/(2^(64n-1)) where the
// divisor would have had an initial padding zero so would have shown up
// as longer than the dividend but the quotient would have needed to come out
// as 1. But here with the divisor made tidy this test is safe!
    if (lena < lenbb)
    {   if (want_q)
        {   q = reserve(1);
            olenq = 1;
            q[0] = 0;
            lenq = 1;
        }
        if (want_r)
        {   r = reserve(lena);
            olenr = lena;
            internal_copy(a, lena, r);
            lenr = lena;
        }
        if (b_negative) abandon(bb);
        return;
    }
// Now lena >= lenb >= 2 and I will need to do a genuine long division. This
// will need me to allocate some workspace.
//
// Because I will scale the divisor I need that to be a copy of the
// original data even if that has been positive and so I had not copied
// it yet. I delay creation of that copy until now because that lets my
// avoid a spurious allocation in the various small cases.
    if (!b_negative)
    {   bb = reserve(lenb);
        olenr = lenb;
        internal_copy(b, lenbb, bb);
    }
    if (debug_arith) my_assert(bb[olenr] == 0xaaaaaaaaaaaaaaaa);
// If I actually return the quotient I may need to add a leading 0 or -1 to
// make its 2s complement representation valid. Hence the "+2" rather than
// the more obvious "+1" here.
    if (want_q)
    {   lenq = lena - lenb + 2;
        q = reserve(lenq);
        olenq = lenq;
    }
// I will need space where I store something that starts off as a scaled
// copy of the dividend and gradually have values subtracted from it until
// it ends up as the remainder.
    lenr = lena;
    r = reserve(lenr+1);
    bool a_negative = negative(a[lena-1]);
    if (a_negative) internal_negate(a, lena, r);
    else internal_copy(a, lena, r);
    unsigned_long_division(r, lenr, bb, lenbb, want_q, q, olenq, lenq);
    if (debug_arith) my_assert(r[lena+1] == 0xaaaaaaaaaaaaaaaa);
// While performing the long division I will have had three vectors that
// were newly allocated. r starts off containing a copy of a but ends up
// holding the remainder. It is rather probable that this remainder will
// often be a distinctly shorter vector than a was. The vector q is only
// created and used if want_q was set, and it ends up holding the quotient.
// finally bb holds the absolute value of the divisor but scaled up by a
// power of 2 so that its leading digit has its top bit set. Well the actual
// remainder is smaller than the divisor and so it will be a closer fit into
// bb than r. So copy it into there so that the allocate/abandon and
// size confirmation code is given less extreme things to cope with.
    my_assert(lenr<=lenb);
    if (want_r) internal_copy(r, lenr, bb); 
    abandon(r);
    if (want_q)
    {   if (a_negative != b_negative)
        {   internal_negate(q, lenq, q);
            truncate_negative(q, lenq);
        }
        else truncate_positive(q, lenq);
    }
//  else abandon(q);
    if (want_r)
    {   r = bb;
        if (a_negative)
        {   internal_negate(r, lenr, r);
            truncate_negative(r, lenr);
        }
        else truncate_positive(r, lenr);
    }
    else abandon(bb);
}

// During long division I will scale my numbers by shifting left by an
// amount s. I do that in place. The shift amount will be such that
// the divisor ends up with the top bit of its top digit set. The
// dividend will need to extend into an extra digit, and I deal with that
// by returning the overflow word as a result of the scaling function. Note
// that the shift amount will be in the range 0-63.


inline uint64_t scale_for_division(uint64_t *r, size_t lenr, int s)
{
// There are two reasons for me to treat a shift by zero specially. The
// first is that it is cheap because no data needs moving at all. But the
// more subtle reason is that if I tried using the general code as below
// that would execute a right shift by 64, which is out of the proper range
// for C++ right shifts.
    if (s == 0) return 0;
    uint64_t carry = 0;
    for (size_t i=0; i<lenr; i++)
    {   uint64_t w = r[i];
        r[i] = (w << s) | carry;
        carry = w >> (64-s);
    }
    return carry;
}

// r = r - b*q*base^(lena-lenb-1).

inline void multiply_and_subtract(uint64_t *r, size_t lenr,
                                  uint64_t q0,
                                  uint64_t *b, size_t lenb)
{   my_assert(lenr > lenb);
    uint64_t hi = 0, lo, carry = 1;
    for (size_t i=0; i<lenb; i++)
    {   multiplyadd64(b[i], q0, hi, hi, lo);
// lo is now the next digit of b*q, and hi needs to be carried up to the
// next one.
        carry = add_with_carry(r[i+lenr-lenb-1], ~lo, carry, r[i+lenr-lenb-1]);
    }
    r[lenr-1] = r[lenr-1] + ~hi + carry;
}

// add_back_correction() is used when a quotient digit was mis-predicted by
// 1 and I detect that when I calculate r = r - b*q and end up with r negative
// result. I fix things up by decrementing q and going
//         r = r + (b<<(lenr-lenb-1))
 
inline void add_back_correction(uint64_t *r, size_t lenr,
                                uint64_t *b, size_t lenb)
{   my_assert(lenr > lenb);
    uint64_t carry = 0;
    for (size_t i=0; i<lenb; i++)
        carry = add_with_carry(r[i+lenr-lenb-1], b[i], carry, r[i+lenr-lenb-1]);
    r[lenr-1] += carry;
}

inline uint64_t next_quotient_digit(uint64_t *r, size_t &lenr,
                                    uint64_t *b, size_t lenb)
{   my_assert(lenr > lenb);
    my_assert(lenb >= 2);
    my_assert(b[lenb-1] != 0);
    uint64_t q0, r0;
    if (r[lenr-1] == b[lenb-1])
    {   q0 = (uint64_t)(-1);
        r0 = r[lenr-2] + b[lenb-1];
// Here perhaps q0 is still an over-estimate by 1?
    }
    else
   {   divide64(r[lenr-1], r[lenr-2], b[lenb-1], q0, r0);
// At this stage q0 may be correct or it may be an over-estimate by 1 or 2,
// but never any worse than that.
//
// The tests on the next lines should detect all case where q0 was in error
// by 2 and most when it was in error by 1.
        uint64_t hi, lo;
        multiply64(q0, b[lenb-2], hi, lo);
        if (hi > r0 ||
            (hi == r0 && lo > r[lenr-3])) q0--;
    }
//
// Now I want to go "r = r - b*q0*2^(64*(lenr-lenb));" so that r
// is set to an accurate remainder after using q0 as (part of) the
// quotient. This may carry an overshoot into atop and if so I will need
// to reduce q0 again and compensate.
//
    multiply_and_subtract(r, lenr, q0, b, lenb);
    if (negative(r[lenr-1]))
    {   q0--;
        add_back_correction(r, lenr, b, lenb);
    }
    lenr--;  // a is now one digit shorter.
    return q0;
}

// r is an unsigned number. Shift right (in place) by s bits, where s
// is in the range 0 - 63. The bits shifted out to the right should all
// be zero.

inline void unscale_for_division(uint64_t *r, size_t &lenr, int s)
{   if (s != 0)
    {   uint64_t carry = 0;
        size_t i = lenr-1;
        for (;;)
        {   uint64_t w = r[i];
            r[i] = (w >> s) | carry;
            carry = w << (64-s);
            if (i == 0) break;
            i--;
        }
        my_assert(carry==0);
    }
    truncate_positive(r, lenr);
}

// This function does long division on unsigned values, computing the
// quotient (a/b). In doing so it updates (a) so that at the end it holds
// the remainder. It only fills in a value for the quotient q if want_q is
// true. Note also that this code will scale (b) so that the top bit of its
// highest digit is a "1", so b must be an array that can be overwritten
// without disturbing code elsewhere.

inline void unsigned_long_division(uint64_t *a, size_t &lena,
                                   uint64_t *b, size_t &lenb,
                                   bool want_q, uint64_t *q,
                                   size_t &olenq, size_t &lenq)
{   my_assert(lenb >= 2);
    my_assert(lena >= lenb);
// I will multiply a and b by a scale factor that gets the top digit of "b"
// reasonably large. The value stored in "a" can become one digit longer,
// but there is space to store that.
//
// The scaling is done here using a shift, which seems cheaper to sort out
// then multiplication by a single-digit value.
    my_assert(b[lenb-1] != 0);
    int ss = nlz(b[lenb-1]);
// When I scale the dividend expands into an extra digit but the scale
// factor has been chosen so that the divisor does not.
    a[lena] = scale_for_division(a, lena, ss);
    lena++;
    my_assert(scale_for_division(b, lenb, ss) == 0);
    lenq = lena-lenb; // potential length of quotient.
    size_t m = lenq-1;
    for (;;)
    {   uint64_t qd = next_quotient_digit(a, lena, b, lenb);
// If I am only computing the remainder I do not need to store the quotient
// digit that I have just found.
        if (want_q) q[m] = qd;
        if (m == 0) break;
        m--;
    }
    unscale_for_division(a, lena, ss);
// The quotient is OK correct now but has been computed as an unsigned value
// so if its top digit has its top bit set I need to prepend a zero;
    if (want_q)
    {   if (negative(q[lenq-1])) q[lenq++] = 0;
        else truncate_positive(q, lenq);
    }
    if (negative(a[lena-1])) a[lena++] = 0;
    else truncate_positive(a, lena);
}

// Use unsigned_long_division when all that is required is the remainder.
// Here a>b and b is at least 2 words. The code corrupts b and replaces
// a with remainder(a, b).

inline void unsigned_long_remainder(uint64_t *a, size_t &lena,
                                    uint64_t *b, size_t &lenb)
{   size_t w;
    unsigned_long_division(a, lena, b, lenb,
                           false, NULL, w, w);
}

intptr_t Quotient::op(uint64_t *a, uint64_t *b)
{   size_t lena = number_size(a);
    size_t lenb = number_size(b);
    uint64_t *q, *r;
    size_t olenq, olenr, lenq, lenr;
    division(a, lena, b, lenb,
             true, q, olenq, lenq,
             false, r, olenr, lenr);
    return confirm_size(q, olenq, lenq);
}

intptr_t Quotient::op(uint64_t *a, int64_t b)
{   size_t lena = number_size(a);
    uint64_t *q, *r;
    size_t olenq, olenr, lenq, lenr;
    uint64_t bb[1] = {(uint64_t)b};
    division(a, lena, bb, 1,
             true, q, olenq, lenq,
             false, r, olenr, lenr);
    return confirm_size(q, olenq, lenq);
}

// A fixnum divided by a bignum ought always to yield 0, except that
// maybe -0x8000000000000000} / {0,0x8000000000000000) => -1

intptr_t Quotient::op(int64_t a, uint64_t *b)
{   if (number_size(b)==1 &&
        b[0]==-(uint64_t)a) return int_to_handle(-1);
    return int_to_handle(0); 
}

// unpleasantly -0x8000000000000000 / -1 => a bignum

intptr_t Quotient::op(int64_t a, int64_t b)
{   if (b==-1 && a == MIN_FIXNUM) return int_to_bignum(-a);
    else return int_to_handle(a / b);
}

intptr_t Remainder::op(uint64_t *a, uint64_t *b)
{   size_t lena = number_size(a);
    size_t lenb = number_size(b);
    uint64_t *q, *r;
    size_t olenq, olenr, lenq, lenr;
    division(a, lena, b, lenb,
             false, q, olenq, lenq,
             true, r, olenr, lenr);
    return confirm_size(r, olenr, lenr);
}

intptr_t Remainder::op(uint64_t *a, int64_t b)
{   size_t lena = number_size(a);
    uint64_t *q, *r;
    size_t olenq, olenr, lenq, lenr;
    uint64_t bb[1] = {(uint64_t)b};
    division(a, lena, bb, 1,
             false, q, olenq, lenq,
             true, r, olenr, lenr);
    return confirm_size(r, olenr, lenr);
}

intptr_t Remainder::op(int64_t a, uint64_t *b)
{   if (number_size(b)==1 &&
        b[0]==-(uint64_t)a) return int_to_handle(0);
    return int_to_handle(a); 
}

intptr_t Remainder::op(int64_t a, int64_t b)
{   return int_to_handle(a % b);
}



#ifdef LISP

// In LISP mode I provide a function that returns both quotient and
// remainder. In the other two modes I support the same idea but
// as a function that delivers the quotient as its result and saves
// the remainder via an additional argument.

}

static inline LispObject cons(LispObject a, LispObject b);

namespace arith
{

intptr_t Divide::op(uint64_t *a, uint64_t *b)
{   size_t lena = number_size(a);
    size_t lenb = number_size(b);
    uint64_t *q, *r;
    size_t olenq, olenr, lenq, lenr;
    division(a, lena, b, lenb,
             true, q, olenq, lenq,
             true, r, olenr, lenr);
    intptr_t rr = confirm_size(r, olenr, lenr);
    intptr_t qq = confirm_size_x(q, olenq, lenq);
    return cons(qq, rr);
}

intptr_t Divide::op(uint64_t *a, int64_t bb)
{   size_t lena = number_size(a);
    uint64_t *q, *r;
    size_t olenq, olenr, lenq, lenr;
    uint64_t b[1] = {(uint64_t)bb};
    division(a, lena, b, 1,
             true, q, olenq, lenq,
             true, r, olenr, lenr);
    intptr_t rr = confirm_size(r, olenr, lenr);
    intptr_t qq = confirm_size_x(q, olenq, lenq);
    return cons(qq, rr);
}

intptr_t Divide::op(int64_t aa, uint64_t *b)
{   size_t lenb = number_size(b);
    uint64_t *q, *r;
    size_t olenq, olenr, lenq, lenr;
    uint64_t a[1] = {(uint64_t)aa};
    division(a, 1, b, lenb,
             true, q, olenq, lenq,
             true, r, olenr, lenr);
    intptr_t rr = confirm_size(r, olenr, lenr);
    intptr_t qq = confirm_size_x(q, olenq, lenq);
    return cons(qq, rr);
}

intptr_t Divide::op(int64_t aa, int64_t bb)
{   uint64_t *q, *r;
    size_t olenq, olenr, lenq, lenr;
    uint64_t a[1] = {(uint64_t)aa};
    uint64_t b[1] = {(uint64_t)bb};
    division(a, 1, b, 1,
             true, q, olenq, lenq,
             true, r, olenr, lenr);
    intptr_t rr = confirm_size(r, olenr, lenr);
    intptr_t qq = confirm_size_x(q, olenq, lenq);
    return cons(qq, rr);
}

#else

intptr_t Divide::op(uint64_t *a, uint64_t *b, intptr_t &rem)
{   size_t lena = number_size(a);
    size_t lenb = number_size(b);
    uint64_t *q, *r;
    size_t olenq, olenr, lenq, lenr;
    division(a, lena, b, lenb,
             true, q, olenq, lenq,
             true, r, olenr, lenr);
    rem = confirm_size(r, olenr, lenr);
    return confirm_size_x(q, olenq, lenq);
}

#endif

inline void biggcd(uint64_t *a, size_t lena, uint64_t *b, size_t lenb,
                   uint64_t *r, size_t &lenr)
{
    my_abort("not implented yet");
}

intptr_t Gcd::op(uint64_t *a, uint64_t *b)
{   if (number_size(b) == 1) return Gcd::op(a, (int64_t)b[0]);
// Here I am going to have an initial implementation that will be crude
// and much less efficient than would be possible, just so I have something
// that might work. Specifically I will implement a straightforward
// Euclidean GCD in a way that may allocate (and release) memory for
// intermediate results over and over again.
    intptr_t absa = Abs::op(a);
    intptr_t absb = Abs::op(b);
    if (op_dispatch2<Greaterp,bool>(absb, absa))
    {   intptr_t w = absa;
        absa = absb;
        absb = w;
    }
// Now absa >= absb
    while (!stored_as_fixnum(absb) && number_size(vector_of_handle(absb))!=1)
    {   intptr_t w = op_dispatch2<Remainder,intptr_t>(absa, absb);
        absa = absb;
        absb = w;
    }
    if (stored_as_fixnum(absa))
        return Gcd::op(int_of_handle(absa), int_of_handle(absb));
    else return Gcd::op(vector_of_handle(absa), int_of_handle(absb));
}

intptr_t Gcd::op(uint64_t *a, int64_t bb)
{
// This case involved doing a long-by-short remainder operation and then
// it reduces to the small-small case. The main problem is the handling of
// negative inputs.
    uint64_t b = bb < 0 ? -bb : bb;
    size_t lena = number_size(a);
    bool signa = negative(a[lena-1]);
    uint64_t hi = 0, q;
    for (size_t i=lena-1;;i--)
        divide64(hi, (signa ? ~a[i] : a[i]), b, q, hi);
// Now if a had been positive we have hi=a%b. If a had been negative we
// have (~a)%b == (-a-1)%b which is about |a|%b -1
    if (signa) hi = (hi+1)%b;
    return Gcd::op(b, hi);
}

intptr_t Gcd::op(int64_t a, uint64_t *b)
{   return Gcd::op(b, a);
}

intptr_t Gcd::op(int64_t a, int64_t b)
{
// Take absolute values of both arguments.
    uint64_t aa = a < 0 ? -(uint64_t)a : a;
    uint64_t bb = b < 0 ? -(uint64_t)b : b;
// Ensure that aa >= bb
    if (bb > aa)
    {   uint64_t cc = aa;
        aa = bb;
        bb = cc;
    }
// Do simple Euclidean algorithm
    while (bb != 0)
    {   uint64_t cc = aa % bb;
        aa = bb;
        bb = cc;
    }
// A messy case is gcd(-MIX_FIXNUM, MIN_FIXNUM) which yields -MIN_FIXNUM
// which is liable to be MAX_FIXNUM+1 and so has to be returned as a bignum.
    return unsigned_int_to_bignum(aa);
}

intptr_t Lcm::op(uint64_t *a, uint64_t *b)
{   intptr_t g = Gcd::op(a, b);
    if (stored_as_fixnum(g))
    {   int64_t gg = int_of_handle(g);
        if (gg == 1) return Times::op(a, b);
        intptr_t q = Quotient::op(b, gg);
        return Times::op(vector_to_handle(a), q);
    }
    else
    {   intptr_t q = Quotient::op(b, vector_of_handle(g));
        if (stored_as_fixnum(q)) return Times::op(a, int_of_handle(q));
        else return Times::op(a, vector_of_handle(q));
    }
}

intptr_t Lcm::op(uint64_t *a, int64_t b)
{   intptr_t g = Gcd::op(a, b);
// The GCD can be a bignum if b = MIN_FIXNUM.
    if (stored_as_fixnum(g))
    {   int64_t gg = int_of_handle(g);
        if (gg == 1) return Times::op(a, b);
        intptr_t q = Quotient::op(b, gg);
        return Times::op(vector_to_handle(a), q);
    }
    else
    {   intptr_t q = Quotient::op(b, vector_of_handle(g));
        if (stored_as_fixnum(q)) return Times::op(a, int_of_handle(q));
        else return Times::op(a, vector_of_handle(q));
    }
}

intptr_t Lcm::op(int64_t a, uint64_t *b)
{   return Lcm::op(b, a);
}

intptr_t Lcm::op(int64_t a, int64_t b)
{   intptr_t g = Gcd::op(a, b);
// The GCD can be a bignum if b = MIN_FIXNUM.
    if (stored_as_fixnum(g))
    {   int64_t gg = int_of_handle(g);
        if (gg == 1) return Times::op(a, b);
        intptr_t q = Quotient::op(b, gg);
        if (stored_as_fixnum(q)) return Times::op(a, int_of_handle(q));
        else return Times::op(a, vector_of_handle(q));
    }
    else
    {   intptr_t q = Quotient::op(b, vector_of_handle(g));
        if (stored_as_fixnum(q)) return Times::op(a, int_of_handle(q));
        else return Times::op(a, vector_of_handle(q));
    }
}

} // end of namespace arith

//=========================================================================
//=========================================================================
// End of everything.
//=========================================================================
//=========================================================================

// end of arith.hpp
