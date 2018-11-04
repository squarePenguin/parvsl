// Big-number arithmetic.                                  A C Norman, 2018

// There are quite a lot of bignumber packages out there on the web,
// but none of them seemed to be such that I could readily use them
// for arithmetic within a Lisp at all easily. The code here tries to
// support "big-numbers as objects" where the exact structure of the object
// and storage management for it is done elsewhere, and also "big-numbers
// as block of memory managed by malloc and free".
//
// The code uses 64-bit digits and a 2s complement representation for
// negative numbers. This means it will work best on 64-bit platforms
// (which by now are by far the most important), and it provides bitwise
// logical operations (logand and logor) as well as arithmetic.
//
// If VSL is defined when this is compiled it uses Lisp-style object
// representation, otherwise malloc(). Well I use malloc() and free()
// rather than the C++ equivalents because that way I can provide an easy
// way for the user to plug in replacements....
//
// If TEST is defined then this file becomes a self-contained one with
// a few demonstration and test examples at the end.


/**************************************************************************
 * Copyright (C) 2018, Codemist.                         A C Norman       *
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



// TODO:
//   gcdn, lcmn
//   leftshift & rightshift
//   float, floor, ceil, fix
//   isqrt
//   random bignum generation
//   bitlength, findfirst-bit, findlast-bit, bit-is-set, bit-is-clear
//   check support where int128 is not available
// a LOT of testing. Some profiling and performance tuning.


#define __STDC_FORMAT_MACROS 1
#define __STDC_CONST_MACROS 1
#define __STDC_LIMIT_MACROS 1

#include <cstdio>
#include <cstring>
#include <cstdint>
#include <cinttypes>
#include <cassert>
#include <cstdlib>
#include <cstdarg>
#include <random>
#include <iostream>
#include <iomanip>
#include <thread>
#include <ctime>

static FILE *logfile = NULL;

static void logprintf(const char *fmt, ...)
{   if (logfile == NULL) logfile = std::fopen("arith.log", "w");
    std::va_list args;
    va_start(args, fmt);
    std::vfprintf(logfile, fmt, args);
    va_end(args);
    std::fflush(logfile);
}

#ifdef VSL

// A Lisp item is represented as an integer and the low 3 bits
// contain tag information that specify how the rest will be used.

typedef intptr_t LispObject;

#define TAGBITS    0x7

#define tagCONS    0     // Traditional Lisp "cons" item.
#define tagSYMBOL  1     // a symbol.
#define tagFIXNUM  2     // An immediate integer value (29 or 61 bits).
#define tagFLOAT   3     // A double-precision number.
#define tagATOM    4     // Something else that will have a header word.
#define tagFORWARD 5     // Used during garbage collection.
#define tagHDR     6     // the header word at the start of an atom .
#define tagSPARE   7     // not used!

// Note that in the above I could have used tagATOM to include the case
// of symbols (aka identifiers) but as an optimisation I choose to make that
// a special case. I still have one spare code (tagSPARE) that could be
// used to extend the system.

// Now I provide macros that test the tag bits. These are all rather obvious!

#define isCONS(x)    (((x) & TAGBITS) == tagCONS)
#define isSYMBOL(x)  (((x) & TAGBITS) == tagSYMBOL)
#define isFIXNUM(x)  (((x) & TAGBITS) == tagFIXNUM)
#define isFLOAT(x)   (((x) & TAGBITS) == tagFLOAT)
#define isATOM(x)    (((x) & TAGBITS) == tagATOM)
#define isFORWARD(x) (((x) & TAGBITS) == tagFORWARD)
#define isHDR(x)     (((x) & TAGBITS) == tagHDR)

// In memory CONS cells and FLOATS exist as just 2-word items with
// all their bits in use. All other sorts of data have a header word
// at their start.
// This contains extra information about the exact form of data present.

#define TYPEBITS    0x78

#define typeSYM     0x00
#define typeSTRING  0x08
#define typeVEC     0x10
#define typeBIGNUM  0x18
#define typeEQHASH  0x20
#define typeEQHASHX 0x28
#define typeGAP     0x30
// Codes 0x38, 0x40, 0x48, 0x50, 0x58, 0x60, 0x68,
// 0x70 and 0x78 spare!

#define veclength(h)  (((uintptr_t)(h)) >> 7)
#define packlength(n) (((LispObject)(n)) << 7)

static inline LispObject *heapaddr(LispObject x)
{
    return (LispObject *)x;
}

// General indirection

#define qind(x)     (*((LispObject *)(x)))

// For all other types I must remove the tagging information before I
// can use the item as a pointer.

#define flagSPECFORM  0x100
#define flagMACRO     0x200
#define flagGLOBAL    0x400
#define flagFLUID     0x800
// There are LOTS more bits available for flags etc here if needbe!

// Other atoms have a header that gives info about them. Well as a special
// case I will allow that something tagged with tagATOM but with zero as
// its address is a special marker value...

#define NULLATOM   (tagATOM + 0)
#define qheader(x) ((heapaddr((x)-tagATOM))[0])

// Fixnums and Floating point numbers are rather easy!

#define qfixnum(x)     (((intptr_t)(x)) >> 3)
// NB that C++ makes this undefined if there is overflow!
#define packfixnum(n)  ((((LispObject)(n)) << 3) + tagFIXNUM)

#define MIN_FIXNUM     qfixnum(INTPTR_MIN)
#define MAX_FIXNUM     qfixnum(INTPTR_MAX)

#define qfloat(x)      (((double *)((x)-tagFLOAT))[0])

#define isBIGNUM(x) (isATOM(x) && ((qheader(x) & TYPEBITS) == typeBIGNUM))
#define qbignum(x) ((char *)((x) - tagATOM + sizeof(uint64_t)))

#define isSTRING(x) (isATOM(x) && ((qheader(x) & TYPEBITS) == typeSTRING))
#define qstring(x) ((char *)((x) - tagATOM + sizeof(LispObject)))

#define isVEC(x) (isATOM(x) && ((qheader(x) & TYPEBITS) == typeVEC))
#define isEQHASH(x) (isATOM(x) && ((qheader(x) & TYPEBITS) == typeEQHASH))
#define isEQHASHX(x) (isATOM(x) && ((qheader(x) & TYPEBITS) == typeEQHASHX))


static inline LispObject boxfloat(double a)
{  // LispObject r;
//    check_space(8, __LINE__);
//    setheapstartsandfp(fringe1);
//    r = fringe1 + tagFLOAT;
//    qfloat(r) = a;
//    fringe1 += 8;
//    return r;
    return 0;
}

#endif // VSL

// Storage management for bignums is sort of delicate because at the
// start of an operation one normally has just an upper bound on how much
// space will be needed - the exact number of words to be used only emerges
// at the end. So the protocol I support is based on the calls:
//   uint64_t *preallocate(size_t n)
//      This returns a pointer to n 64-bit words (and it may have allocated
//      a space for a header in front of that).
//   number_representation_t confirm_size(uint64_t *p, size_t n, size_t final_n)
//      The pointer p was as returned by preallocate, but it is now known
//      that just final_n words will be needed (this must be no larger than
//      the number originally given. The effect can be somewhat as if a call
//      to realloc was given. Very often this will also write the final_n
//      into a header tha preceeds the stored digits of the number. It would
//      be able to detect (eg) a special case of small numbers are return
//      them in a different way, and add any other tag or marker bits that
//      are required. When confirm_size() is called the system can be
//      confident that no other calls to preallocate() have been made - this
//      can help because it then "knows" that the memory involved is right
//      at the fringe of active memory.
//   number_representation_t confirm_size_x(uint64_t *p, size_t n, size_t final_n)
//      This behave just like confirm_size apart from the fact that it is to
//      used in a context where two (or more) regions of memory have been
//      preallocated. This must be used on all but the last allocated one
//      when their length is confirmed. The thought behind this is that
//      when a block shrinks or is discarded and it is not the most recent one
//      allocated it may be necessary to put some padding material in the
//      gap that is left, or it may be possible to place that released
//      space on a free-chain for future re-use.
//      As a special case if two blocks are preallocated and the second
//      has its size confirmed as zero then the first one may use
//      confirm_size rather than needing confirm_size_x.
//   void free_bignum(number_representation p)
//      Release memory. In a Lisp system this will be a no-op because garbage
//      collection will do the job. In a freestanding application this
//      can use free().
//
// For strings (as returned when I want to prepare a bignum for printing)
// I will use preallocate as before and an effect is that the string will
// be built up within a block of memory that is a multiple of 8 bytes long.
// Again there MAY be space left for a header. When I know exactly how many
// characters are present I will confirm the exact size required:
//
//   string_representation confirm_size_string(uint64_t *p, size_t n, size_t final_n)
//       In this call n is the original size as used with preallocate, so it
//       measures in units of sizeof(uint64_t), while final_size is measured
//       in bytes and is the number of characters present. For native style
//       strings this will add a '\0' as a terminator and use realloc to
//       trim the size down to include just that.
//   void free_string(string_representation s)
//       Release memory for the string.


#ifdef VSL

// Within Lisp all values are kept using type "LispObject" which involves
// all values having a tag in their low 3 bits and all non-trivial objects
// (apart from cons cells) in memory having a header field that contains
// detailed type information plus the length of the object. The code here
// maps this representation onto the one needed within the bignum code.

typedef LispObject number_representation;
typedef LispObject string_representation;

size_t number_size(number_representation a)
{   return veclength(qheader(a))/sizeof(uint64_t);
}

// Here strings DO NOT have a terminating nul character - their end is
// indicated by the leader word that preceeds them having a length
// field in it.

size_t string_size(number_representation a)
{   return veclength(qheader(a));
}

uint64_t *number_data(number_representation a)
{   return (uint64_t)qbignum(a);
}

const char *string_data(string_representation a)
{   return (const char *)qstring(a);
}

// My representation has a header word that is an intptr_t stored 8 bytes
// ahead of the start of the data that represents bignum digits. On a 64-bit
// machine this is thoroughly natural. On a 32-bit machine it will leave a
// 32-bit gap so that everything end up nicely aligned.

uintptr_t &bignum_header(uint64_t *a)
{   return *(uintptr_t)((char *)a - sizeof(uint64_t)));
}

// For strings the data does not need to end up doubleword aligned, and so
// the string data starts just an uintptr_t size along.

uintptr_t &string_header(uint64_t *a)
{   return *(uintptr_t)((char *)a - sizeof(uintptr_t));
}

#define MEMORY_SIZE 1000000
static uint64_t memory[MEMORY_SIZE];
static size_t memory_used = 0;

uint64_t *preallocate(size_t n)
{   uint64_t *r = &memory[memory_used+1];
    memory_used += n + 1;
// No attempt at garbage collection here - I will just allocate
// bignums linearly in memory until I run out of space. And I do not
// provide any scheme for the user to release them.
    assert(memory_used <= MEMORY_SIZE);
}

LispObject confirm_size(uint64_t *p, size_t n, size_t final_n)
{
#ifdef SUPPORT_FIXNUMS
// WHile doing some initial testing I will represent ALL integers as
// bignums, but for real use within the Lisp I will have a cheaper way
// of representing small values.
    if (final_n == 1)
    {   memory_used =- (n+1);
        int64_t v = (int64_t)p[0];
        if (v >= SMALLEST_FIXNUM && v <= LARGEST_FIXNUM)
            return packfixnum(v);
    }
#endif
    memory_used -= (n - final_n);
    bignum_header(p) = tagHDR + typeBIGNUM + packlength(n*sizeof(uint64_t));
    return (LispObject)&bignum_header(p) + tagATOM;
}

LispObject confirm_size_x(uint64_t *p, size_t n, size_t final_n)
{
#ifdef SUPPORT_FIXNUMS
    if (final_n == 1)
    {   p[-1] = tagHDR + typeGAP + packlength(n*sizeof(uint64_t));
        int64_t v = (int64_t)p[0];
        if (v >= SMALLEST_FIXNUM && v <= LARGEST_FIXNUM)
            return packfixnum(v);
    }
#endif
    bignum_header(p) = tagHDR + typeBIGNUM + packlength(final_n*sizeof(uint64_t));
// I insert an item with typeGAP as a filler...
    p[final_n] = tagHDR + typeGAP + packlength((n-final_n)*sizeof(uint64_t));
    return (LispObject)&bignum_header(p) + tagATOM;
}

LispObject confirm_size_string(uint64_t *p, size_t n, size_t final_n)
{
// The array (p), whose size (n) is expressed in 64-bit chunks, was allocated
// and laid out as for a bignum. That means it has a header in memory just
// ahead of it. Both p and the address of the header were kept 8-byte aligned.
// The size of the header is sizeof(uintptr_t). That means that on a 32-bit
// platform there is an unused 4-byte gap.
// When the data is to be arranged as a string there is no longer any need
// for the string data to remain 8-byte alogned, and so the gap can be
// filled by shuffling data down. This then lets me reduce the final size
// a little (typically by 4 bytes).
    if (sizeof(uint64_t) != sizeof(intptr_t))
    {   char *p1 = (char *)&p[-1];
        std::memmove(p1+sizeof(uintptr_t), p, final_n);
        final_n -= (sizeof(uint64_t) - sizeof(intptr_t));
    }
// In my Lisp world I allocate memory in units of 8 bytes and when a string
// does not completely fill the final unit I like to pad with NULs. Doing so
// makes it possible to compare strings by doing word-at-a-time operations
// rather than byte-at-a-time and similarly compute hash values fast. And
// because I am using byte access here that should not lead to strict aliasing
// worried when I then access the data using wider data types. I think!
    size_t n1 = final_n;
    while ((n1%8) != 0) (char *)p[n1++] = 0;
    memory_used -= (n - n1/sizeof(uint64_t));
// The next two lines may look as if they should use string_header, but
// in fact they are still needing to recognize that p had been a pointer
// into a bignum not a string!
    bignum_header(p) = tagHDR + typeSTRING + packlength(final_n);
    return (LispObject)&bignum_header(p) + tagATOM;
}

number_representation sometimes_copy_bignum(number_representation p)
{
    return p;
}

void free_bignum(number_representation p)
{
}

void free_string(string_representation p)
{
}

#else // VSL

// For a free-standing bignum application (including my test code for the
// stuff here, bignums are represented as blocks of memory (allocated using
// malloc) where the pointer that is used points to the start of the
// array of bignum digits, and the word in memory before that contains
// the length (in words) of the block.
// Strings are returned to the user as freshly malloced memory holding a
// native-style C++ string with a terminating NUL character at the end.


// As a first try I am using malloc() to allocate each memory block every
// time I might want one, realloc() to adjust its size so it is precisely as
// large as is needed and free() to release it. Doing things this way is
// straightforward but may put severe strain on the memory management
// subsystem! An alternative scheme that might be worth considering would
// still represent each bignum by a block of uint64_t values with a header
// word, but now the header word would hold two values packed together.
// One would be the length of data used for the bignum and the second would
// indicate the total size of the memory block. I thinnk it would be reasonable
// to use two uint32_t values in the space that could hold a uint64_t. If the
// "length used" field counted in digits that could cope with up to 2^32
// digits, each 8-bytes long, i.e. it could cope with individual bignums each
// using up to 32 Gbytes of  memory. That seems sufficient for now and for all
// rational use of this package. I would then keep every bignum in a block
// of memory whose size was a power of 2, and so I would only need 5 bits in
// the other half of the header.
// Then where I now perform calls to realloc() to shorten a vector I might
// either always leave the vector with its existing length and just mark it
// as having a lesser number of digits in use, or I could perform more
// enthusiastic adjustment when the size had changed significantly.
// I could also look at all uses of free_bignum() and see if I was about to
// perform a preallocate() that could use the space released. This would
// work particularly well with some cases of Bignum::operator= where the
// bignum presently in the variable is at present discarded and could
// instead often be recycled. If I make all memory blocks a power of 2 in
// size I might consider a "buddy" scheme for managing them...

typedef uint64_t *number_representation;
typedef const char *string_representation;

size_t number_size(number_representation a)
{   return a[-1];
}

size_t string_size(string_representation a)
{   return strlen(a);
}

uint64_t *number_data(number_representation a)
{   return a;
}

const char *string_data(string_representation a)
{   return a;
}

// The following are provided so that a user can update malloc_function,
// realloc_function and free_function to refer to their own choice of
// allocation technology.

typedef void *malloc_t(size_t);
typedef void *realloc_t(void *, size_t);
typedef void free_t(void *);

malloc_t  *malloc_function = malloc;
realloc_t *realloc_function = realloc;
free_t    *free_function   = free;

uint64_t *preallocate(size_t n)
{   uint64_t *r = (uint64_t *)(*malloc_function)((n+1)*sizeof(uint64_t));
    assert(r != NULL);
    return &r[1];
}

number_representation confirm_size(uint64_t *p, size_t n, size_t final_n)
{   p = (uint64_t *)(*realloc_function)((void *)&p[-1], (final_n+1)*sizeof(uint64_t));
    assert(p != NULL);
    p[0] = final_n;
    return &p[1];
}

number_representation confirm_size_x(uint64_t *p, size_t n, size_t final_n)
{   p = (uint64_t *)(*realloc_function)((void *)&p[-1], (final_n+1)*sizeof(uint64_t));
    assert(p != NULL);
    p[0] = final_n;
    return &p[1];
}

// The string data has been built up in a block of memory that was suited
// to be a bignum, so it was preceeded by a header word that could have
// been used to store its length. I copy the characters down to start at the
// very start of the allocated block. Remember agaiin that n measures
// the number of data words in a bignum (not including the header) and
// final_n measures the string in characters.

string_representation confirm_size_string(uint64_t *p, size_t n, size_t final_n)
{   char *c = (char *)&p[-1];
// When a string is returned it will not need a preceeding header word, so
// I memmove() the data down to the start of the memory block. This has
// a cost and perhaps keeping the length information elsewhere would have
// avoided that, but the scheme used here keeps the representation of
// big numbers nice and tidy ans self-contained, and the linear cost of this
// memort move tends to be associated with a quadratic cost convertion from
// internal to character form so it is really not liable to dominate overall
// timings.
    std::memmove(c, (char *)p, final_n);
    c[final_n] = 0; // write in terminator
// Remember to allow for the terminator when adjusting the size! Well
// there will always have been space for it because we are losing the header
// word.
    const char *cc = (const char *)(*realloc_function)(c, final_n+1);
    assert(cc != NULL);
    return cc;
}

number_representation sometimes_copy_bignum(number_representation p)
{
    size_t n = number_size(p);
    uint64_t *d = number_data(p);
    uint64_t *r = preallocate(n);
    std::memcpy(&r[-1], &d[-1], (n+1)*sizeof(uint64_t));
    return confirm_size(r, n, n);
}

void free_bignum(number_representation p)
{   if (p != (number_representation)0)
        (*free_function)((void *)&p[-1]);
}

void free_string(string_representation p)
{   if ((number_representation)p != (number_representation)0)
       (*free_function)((void *)p);
}

#endif // VSL

// When I use Bignums that are allocated using malloc() and operated on
// via C++ overloaded operators I often need to copy the data. However when
// memory management uses garbage collection I can allow multiple references
// to the same object and so copying is not needed as much. This copies
// in the cases where that is relevant.

// This version ALWAYS creates a fresh copy of the Bignum.

number_representation always_copy_bignum(number_representation p)
{   size_t n = number_size(p);
    uint64_t *d = number_data(p);
    uint64_t *r = preallocate(n);
    std::memcpy(&r[-1], &d[-1], (n+1)*sizeof(uint64_t));
    return confirm_size(r, n, n);
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

static inline uint64_t add_with_carry(uint64_t a1, uint64_t a2,
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

static inline uint64_t add_with_carry(uint64_t a1, uint64_t a2, uint64_t &r)
{   uint64_t w;
    r = w = a1 + a2;
    return (w < a1 ? 1 : 0);
}

// I want code that will multiply two 64-bit values and yield a 128-bit
// result. The result must be expressed as a pair of 64-bit integers.
// If I have a type "__int128", as will often be the case when using gcc,
// this is very easy to express. Otherwise I split the two inputs into
// 32-bit halves, do 4 multiplications and some additions to construct
// the result. At least I can keep the code portable, even if I can then
// worry about performance a bit.

// Well it seems that g++ and clang have different views about how to
// ask for unsigned 128-bit integers!

#ifdef __SIZEOF_INT128__
#ifdef __CLANG__
typedef __int128  INT128;
typedef __uint128 UINT128;
#else // __CLANG__
typedef __int128  INT128;
typedef unsigned __int128 UINT128;
#endif // __CLANG__

std::ostream & operator << (std::ostream &out, UINT128 a)
{   out << std::hex << std::setw(16) << std::setfill('0') <<(uint64_t)(a>>64)
        << " "
        << (uint64_t)a << std::dec << std::setw(0) << std::setfill(' '); 
    return out;
}

#endif // __SIZEOF_INT128__

static inline void multiply64(uint64_t a, uint64_t b,
                              uint64_t &hi, uint64_t &lo)
{
#ifdef __SIZEOF_INT128__
    UINT128 r = (UINT128)a*(UINT128)b;
    hi = (uint64_t)(r >> 64);
    lo = (uint64_t)r;
#else
    uint64_t a1 = a >> 32,           // top half
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
#endif
}

// Now much the same but forming a*b+c. Note that this can not overflow
// the 128-bit result. Both hi and lo are only updated at the end
// of this, and so they are allowed to be the same as other arguments.

static inline void multiply64(uint64_t a, uint64_t b, uint64_t c,
                              uint64_t &hi, uint64_t &lo)
{
#ifdef __SIZEOF_INT128__
    UINT128 r = (UINT128)a*(UINT128)b +
                          (UINT128)c;
    hi = (uint64_t)(r >> 64);
    lo = (uint64_t)r;
#else
    uint64_t a1 = a >> 32,           // top half
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
#endif
}

// While my arithmetic is all done in uint64_t (and that is important so
// that in C++ the consequences of overflow are defined) I need to treat
// some top-digits as signed: here are values and tests relating to that.

static const uint64_t allbits   = ~(uint64_t)0;
static const uint64_t topbit    = ((uint64_t)1)<<63;
static const uint64_t allbuttop = topbit - 1;

static inline bool positive(uint64_t a)
{   return ((int64_t)a) >= 0;
}

static inline bool negative(uint64_t a)
{   return ((int64_t)a) < 0;
}

// At times it may be helpful to treat the array of digits as
// being a row of 32-bit values rather than 64-bit ones. gcc at least
// has predefined symbols that can tell me when I am little-endian and
// that helps. But to be secure against the strict aliasing rules I need
// to access memory using (nominally) character-at-a-time operations.
//
// The behaviour here depends on byte-ordering: on some systems I will
// have predefined macros that let me know what happens on the platform
// I am on. Specifically gcc and clang seem to define symbols as tested
// for here... which let me use code that is pretty clean and fast.

#if defined __BYTE_ORDER__ && \
    defined __ORDER_LITTLE_ENDIAN__ && \
    __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__

static inline uint32_t read_u32(const uint64_t *v, size_t n)
{   uint32_t r;
    std::memcpy(&r, (const char *)v + 4*n, sizeof(uint32_t));
    return r;
}

static inline void write_u32(uint64_t *v, size_t n, uint32_t r)
{   std::memcpy((char *)v + 4*n, &r, sizeof(uint32_t));
}

#elif defined __BYTE_ORDER__ && \
    defined __ORDER_BIG_ENDIAN__ && \
    __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__

static inline uint32_t read_u32(const uint64_t *v, size_t n)
{   uint32_t r;
    std::memcpy(&r, (const char *)v + 4*(n^1), sizeof(uint32_t));
    return r;
}

static inline void write_u32(uint64_t *v, size_t n, uint32_t r)
{   std::memcpy((char *)v + 4*(n^1), &r, sizeof(uint32_t));
}

#else // endianness not known at compile time

// If I am uncertain about endianness I can extract or insert data
// using shift operations. It is not actually TOO bad. I am mainly providing
// these in case the nicest implementation of long division when I do not
// have int128_t available will involve working with 32-bit rather than
// 64-bit digits.

static inline uint32_t read_u32(const uint64_t *v, size_t n)
{   uint64_t r = v[n/2];
    if ((n & 1) != 0) r >>= 32;
    return (uint32_t)r;
}

static inline void write_u32(uint64_t *v, size_t n, uint32_t r)
{   uint64_t w = v[n/2];
    if ((n & 1) != 0) w = ((uint32_t)w) | ((uint64_t)r << 32);
    else w = (w & ((uint64_t)(-1)<<32)) | r;
    v[n/2] = w;
}

#endif // endianness

// When printing numbers in octal it will be handy to be able treat the
// data as an array of 3-bit digits, so here is an access function that
// does that. There is a messy issue about the top of a number, where it
// may not be a whole number of 3-bit octal digits. I pass in v, a vector
// of 64-bit values, n which is the length of that vector and i which
// is the index of the octal digit that I wish to extract. To help with
// that I have a function virtual_digit64() which lets me read from a
// bignum as if it has been usefully sign-extended.

static inline uint64_t virtual_digit64(const uint64_t *v, size_t n, size_t j)
{   if (j < n) return v[j];
    else if (positive(v[n-1])) return 0;
    else return UINT64_C(0xffffffffffffffff);
}

static inline int read_u3(const uint64_t *v, size_t n, size_t i)
{   size_t bits = 3*i;
    size_t n0 = bits/64;   // word with lowest bit of the 3
    size_t s0 = bits%64;   // amount to shift right to align it properly
    uint64_t w = virtual_digit64(v, n, n0) >> s0;
// If I needed to shift by 62 or 63 bits then the octal digit I am interested
// in needs some bits from the next word up.
    if (s0 >= 62) w |= (virtual_digit64(v, n, n0+1) << (64-s0));
logprintf("digit %d (of %d words) = %d\n", (int)i, (int)n, (int)(w & 0x7));
    return (int)(w & 0x7);
}
    

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
// same sequence of steps as a crifical region I could use it to re-seed
// generators whenever I felt the need to.
//

static std::random_device basic_randomness;
static unsigned int seed_component_1 = basic_randomness();
static unsigned int seed_component_2 = basic_randomness();
static unsigned int seed_component_3 = basic_randomness();
static thread_local std::seed_seq random_seed
{   (unsigned int)
        std::hash<std::thread::id>()(std::this_thread::get_id()),
    seed_component_1,
    seed_component_2,
    seed_component_3,
    (unsigned int)time(NULL),
    (unsigned int)
        std::chrono::high_resolution_clock::now().time_since_epoch().count()
};
static thread_local std::mt19937_64 mersenne_twister(random_seed);
// mersenne_twister() now generates 64-bit unsigned integers.


// Convert a 64-bit integer to a bignum.

static inline void int_to_bignum(int64_t n, uint64_t *r)
{   r[0] = (uint64_t)n;
}

// The functions that are not static and that return an number_representation
// are the ones intended for external use.

number_representation int_to_bignum(int64_t n)
{   uint64_t *r = preallocate(1);
    int_to_bignum(n, r);
    return confirm_size(r, 1, 1);
}

static const uint64_t ten19 = UINT64_C(10000000000000000000);

static inline void truncate_positive(const uint64_t *r, size_t &n)
{   while (r[n-1]==0 && n>1 && positive(r[n-2])) n--;
}

static inline void truncate_negative(const uint64_t *r, size_t &n)
{   while (r[n-1]==allbits && n>1 && negative(r[n-2])) n--;
}

number_representation string_to_bignum(const char *s)
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
    uint64_t *r = preallocate(words);
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
        {   d = 10*d + (*s++ - '0');
            chars--;
        }
        next -= 19;
// now perform r = 10^19*r + d to consolidate into the evential result.
        for (size_t i=0; i<words; i++)
        {   uint64_t hi, lo;
            multiply64(r[i], ten19, d, hi, lo);
            r[i] = lo;
            d = hi;
        }
    }
    size_t n1 = words;
// Here I may be negating a positive number, and in 2s complement that
// can never lead to a number growing in length.
    if (sign)
    {   uint64_t carry = 1;
        for (size_t i=0; i<words; i++)
        {   uint64_t w = r[i] = ~r[i] + carry;
            carry = (w < carry ? 1 : 0);  
        }
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
// The number is POSITIVE here.

#ifdef __SIZEOF_INT128__

static uint64_t short_divide_ten_19(uint64_t *r, size_t &n)
{   uint64_t hi = 0;
    for (size_t i = n-1; i!=0; i--)
    {   UINT128 p = ((UINT128)hi << 64) | r[i];
        uint64_t q = (uint64_t)(p / ten19);
        hi = (uint64_t)(p % ten19);
        r[i] = q;
    }
    UINT128 p = ((UINT128)hi << 64) | r[0];
    uint64_t q = (uint64_t)(p / ten19);
    hi = (uint64_t)(p % ten19);
    r[0] = q;
    if (r[n-1] == 0) n--;
    return hi;
}

#else // __SIZEOF_INT128__

static uint64_t short_divide_ten_19(uint64_t *r, size_t &n)
{   std::cout << "short_divide_ten_19 without int128_t" << std::endl;
    abort();
}

#endif // __SIZEOF__INT128__

#ifdef __GNUC__

// Note that __GNUC__ also gets defined by clang on the Macintosh, so
// this code is probably optimized there too. This must NEVER be called
// with a zero argument.

static inline int nlz(uint64_t x)
{   return __builtin_clzll(x);  // Must use the 64-bit version of clz.
}

#else // __GNUC__

static inline int nlz(uint64_t x)
{   int n = 0;
    if (x <= 0x00000000FFFFFFFFU) {n = n +32; x = x <<32;}
    if (x <= 0x0000FFFFFFFFFFFFU) {n = n +16; x = x <<16;}
    if (x <= 0x00FFFFFFFFFFFFFFU) {n = n + 8; x = x << 8;}
    if (x <= 0x0FFFFFFFFFFFFFFFU) {n = n + 4; x = x << 4;}
    if (x <= 0x3FFFFFFFFFFFFFFFU) {n = n + 2; x = x << 2;}
    if (x <= 0x7FFFFFFFFFFFFFFFU) {n = n + 1;}
    return n;
}

#endif // __GNUC__

// I want an estimate of the number of bytes that it will take to
// represent a number when I convert it to a string.

static size_t predict_size_in_bytes(const uint64_t *a, size_t n)
{   uint64_t r;
// I am first going to estimate the size in BITS and then I will
// see how that maps onto bytes.
    uint64_t top = a[n-1];  // top digit.
    if (negative(top))
    {   top = -top;
        r = 8;       // 8 bits for a "-" sign.
    }
    else r = 0;
    r = r + 64*(n-1) + (top==0 ? 0 : 64-nlz(top));
// This risks overflow if the string was going to be over 2^51 bytes long!
    return 1+(617*r)/2048;
} 

// The "as_unsigned" option here is not for general use - it is JUST for
// internal debugging because at times I work with values that are known
// to be positive and so where the top digit must be treated as unsigned...

string_representation bignum_to_string(const uint64_t *a, size_t n,
                                       bool as_unsigned=false)
{
// Making the value zero a special case simplifies things later on!
    if (n == 1 && a[0] == 0)
    {   uint64_t *r = preallocate(1);
        strcpy((char *)r, "0");
        return confirm_size_string(r, 1, 1);
    }
// The size (m) for the block of memory that I put my result in is
// such that it could hold the string representation of my input, and
// I estimate that via predict_size_in_bytes().
    uint64_t m = (7 + predict_size_in_bytes(a, n))/8;
// I am going to build up (decimal) digits of the converted number by
// repeatedly dividing by 10^19. Each time I do that the remainder I
// amd left with is the next low 19 decimal digits of my number. Doing the
// divisions needs a vector to store the number I am dividing by 10^19 and
// to put the quotient, and I do not want to corrupt my original input, so
// I will copy my input into a fresh vector. And I will force it to be
// positive. The made-positive version might have a leading digit with
// its top bit set - that will not worry me because I view it as unsigned.
    uint64_t *r = preallocate(m);
    size_t i;
    for (i=0; i<n; i++) r[i] = a[i];
    for (; i<m; i++) r[i] = 0;
// Make it positive
    bool sign = false;
    if (negative(r[n-1]) && !as_unsigned)
    {   sign = true;
        uint64_t carry = 1;
        for (i=0; i<n; i++)
        {   uint64_t w = r[i] = ~r[i] + carry;
            carry = (w < carry ? 1 : 0);
        }
    }
// Now my number is positive and is of length n, but the vector it is
// stored in is length m with m usefully larger than n. I will repeatedly
// divide by 10^19 and each time I do that I can store the remainder working
// down from the top of the vector. That should JUST keep up so that I
// never overwrite digits of the reducing part! I will stop when the
// number I have been working with end up < 10^19.
    size_t p = m-1; // indicates where to put next output digit
    while (n > 1 || r[0] > ten19)
    {   uint64_t d = short_divide_ten_19(r, n);
        r[p--] = d;
    }
    r[p] = r[0];
// Now I have the data that has to go into my result as a sequence of
// digits base 10^19, with the most significant one first. Convert
// to character data. I write in the string data just over what has been
// digits data, and I have arranged to position everything to (just)
// avoid overwriting myself.
    char *p1 = (char *)r;
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
    uint64_t top = r[p++];
    do
    {   buffer[bp++] = '0' + top%10;
        top = top/10;
    } while (top != 0);
    do
    {   *p1++ = buffer[--bp];
        len++;
    } while (bp != 0);
    assert(len < m*sizeof(uint64_t));
    while (p < m)
    {   top = r[p++];
// Here I want to print exactly 19 decimal digits.
        for (int i=0; i<18; i++)
        {   p1[18-i] = '0' + top%10;
            top = top/10;
        }
        *p1 = '0' + (int)top;
        p1 += 19;
        len += 19;
        assert(len <= m*sizeof(uint64_t));
    }
    return confirm_size_string(r, m, len);
}

string_representation bignum_to_string(number_representation aa)
{   size_t n = number_size(aa);
    uint64_t *a = number_data(aa);
    return bignum_to_string(a, n);
}

string_representation bignum_to_string_hex(number_representation aa)
{   size_t n = number_size(aa);
    uint64_t *a = number_data(aa);
// Making the value zero a special case simplifies things later on!
    if (n == 1 && a[0] == 0)
    {   uint64_t *r = preallocate(1);
        strcpy((char *)r, "0");
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
    {   while ((top>>60) == 0)
        {   top = top << 4;
            m--;
        }
    }
    size_t nn = (m + 7)/8;
    uint64_t *r = preallocate(nn);
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
    return confirm_size_string(r, nn, m);
}

string_representation bignum_to_string_octal(number_representation aa)
{   size_t n = number_size(aa);
    uint64_t *a = number_data(aa);
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
    nn = (nn + 7)/8;   // words needed for string result
    uint64_t *r = preallocate(nn);
    char *p = (char *)r;
    if (sign)
    {   *p++ = '~';
        *p++ = '7';
    }
    for (size_t i=0; i<width; i++)
        *p++ = '0' + read_u3(a, n, width-i-1);
    return confirm_size_string(r, nn, width);
}

string_representation bignum_to_string_binary(number_representation aa)
{   size_t n = number_size(aa);
    uint64_t *a = number_data(aa);
// Making the value zero a special case simplifies things later on!
    if (n == 1 && a[0] == 0)
    {   uint64_t *r = preallocate(1);
        strcpy((char *)r, "0");
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
    {   while ((top>>63) == 0)
        {   top = top << 1;
            m--;
        }
    }
    size_t nn = (m + 7)/8;
    uint64_t *r = preallocate(nn);
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
    return confirm_size_string(r, nn, m);
}

// eqn

bool bigeqn(const uint64_t *a, size_t lena,
            const uint64_t *b, size_t lenb)
{   if (lena != lenb) return false;
    return std::memcmp(a, b, lena*sizeof(uint64_t)) == 0;   
}

bool bigeqn(number_representation a, number_representation b)
{   size_t na = number_size(a);
    size_t nb = number_size(b);
    return bigeqn(number_data(a), na, number_data(b), nb);
}

// greaterp

bool biggreaterp(const uint64_t *a, size_t lena,
                 const uint64_t *b, size_t lenb)
{   uint64_t a0, b0;
// If one of the numbers has more digits than the other then the sign of
// the longer one gives my the answer.
    if (lena > lenb) return positive(a0 = a[lena-1]);
    else if (lenb > lena) return negative(b0 = b[lenb-1]);
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
// If the two values are the same return false
    return false;
}

bool biggreaterp(number_representation a, number_representation b)
{   size_t na = number_size(a);
    size_t nb = number_size(b);
    return biggreaterp(number_data(a), na, number_data(b), nb);
}

// geq

bool biggeq(const uint64_t *a, size_t lena,
            const uint64_t *b, size_t lenb)
{   uint64_t a0, b0;
    if (lena > lenb) return positive(a0 = a[lena-1]);
    else if (lenb > lena) return negative(b0 = b[lenb-1]);
    if ((int64_t)a0 > (int64_t)b0) return true;
    if ((int64_t)a0 < (int64_t)b0) return false;
    lena--;
    while (lena != 0)
    {   lena--;
        a0 = a[lena];
        b0 = b[lena];
        if (a0 > b0) return true;
        if (a0 < b0) return false;
    }
    return true;
}

bool biggeq(number_representation a, number_representation b)
{   size_t na = number_size(a);
    size_t nb = number_size(b);
    return biggeq(number_data(a), na, number_data(b), nb);
}

// lessp

bool biglessp(const uint64_t *a, size_t lena,
              const uint64_t *b, size_t lenb)
{   return biggreaterp(b, lenb, a, lena);
}

bool biglessp(number_representation a, number_representation b)
{   size_t na = number_size(a);
    size_t nb = number_size(b);
    return biglessp(number_data(a), na, number_data(b), nb);
}

// leq

bool bigleq(const uint64_t *a, size_t lena,
            const uint64_t *b, size_t lenb)
{   return biggeq(b, lenb, a, lena);
}

bool bigleq(number_representation a, number_representation b)
{   size_t na = number_size(a);
    size_t nb = number_size(b);
    return bigleq(number_data(a), na, number_data(b), nb);
}

// Negation. Note that because I am using 2s complement the result could be
// one word longer or shorter than the input. For instance if you negate
// [0x8000000000000000] (a negative value) you get [0,0x8000000000000000],
// and vice versa.

static void bignegate(const uint64_t *a, size_t lena, uint64_t *r, size_t &lenr)
{   uint64_t carry = 1;
    for (size_t i=0; i<lena; i++)
    {   carry = ~a[i] + carry;
        r[i] = carry;
        carry = (carry == 0 ? 1 : 0);
    }
// The next digit up is really now (carry + (negative(a[i-1]) ? allbits : 0))
// but because the length-changing cases are very special here I can simplify
// the tests that I do to detect them. If the top digit of the result is now
// 0x8000000000000000 then it must now be positive and I must extend the
// number, while if it is 0 and the next digit down now looks positive
// I can shrink.
    if (r[lena-1]==topbit) r[lena++] = 0;
    else if (r[lena-1]==0 && lena>1 && positive(r[lena-2])) lena--; 
    lenr = lena;
}

// bigabsval() is used internally and leaves a number where every digit is
// to be treated as unsigned. So if the top digit of a number starts off
// as 0x8000000000000000 (the largest negative value possible) a normal
// negation to give a 2s complement accurate result would have to put an
// extra zero word ahead of the negative of that (which has the same bit
// pattern). Here where the output is treated as unsigned the length remains
// unchanged. It can also be the case that the input started off as positive
// but its value had required a leading zero. I remove that zero.

static void bigabsval(const uint64_t *a, size_t lena, uint64_t *r, size_t &lenr)
{   if (positive(a[lena-1]))
    {   if (lenr > 1 && a[lenr-1] == 0) lena--;
        memcpy(r, a, lena*sizeof(uint64_t));
    }
    else
    {   uint64_t carry = 1;
        for (size_t i=0; i<lena; i++)
        {   carry = ~a[i] + carry;
            r[i] = carry;
            carry = (carry == 0 ? 1 : 0);
        }
    }
    lenr = lena;
}

number_representation bignegate(number_representation a)
{   size_t n = number_size(a);
    uint64_t *p = preallocate(n+1);
    size_t final_n;
    bignegate(number_data(a), n, p, final_n);
    return confirm_size(p, n, final_n);
}

// The "bitnot" operation is simple and length can not change.

static void biglognot(const uint64_t *a, size_t lena, uint64_t *r, size_t &lenr)
{   for (size_t i=0; i<lena; i++)
    {   r[i] = ~a[i];
    }
    lenr = lena;
}

number_representation biglognot(number_representation a)
{   size_t n = number_size(a);
    uint64_t *p = preallocate(n+1);
    size_t final_n;
    biglognot(number_data(a), n, p, final_n);
    return confirm_size(p, n, final_n);
}

// logand

void ordered_biglogand(const uint64_t *a, size_t lena,
                       const uint64_t *b, size_t lenb,
                       uint64_t *r, size_t &lenr)
{   for (size_t i=0; i<lenb; i++)
        r[i] = a[i] & b[i];
    if (negative(b[lenb-1])) lenr = lena;
    else lenr = lenb;
}

void biglogand(const uint64_t *a, size_t lena,
               const uint64_t *b, size_t lenb,
               uint64_t *r, size_t &lenr)
{   if (lena >= lenb) return ordered_biglogand(a, lena, b, lenb, r, lenr);
    else return ordered_biglogand(b, lenb, a, lena, r, lenr);
}

number_representation biglogand(number_representation a, number_representation b)
{   size_t na = number_size(a);
    size_t nb = number_size(b);
    size_t n;
    if (na >= nb) n = na;
    else n = nb;
    uint64_t *p = preallocate(n);
    size_t final_n;
    biglogand(number_data(a), na, number_data(b), nb, p, final_n);
    return confirm_size(p, n, final_n);
}

// logor

void ordered_biglogor(const uint64_t *a, size_t lena,
                      const uint64_t *b, size_t lenb,
                      uint64_t *r, size_t &lenr)
{   for (size_t i=0; i<lenb; i++)
        r[i] = a[i] | b[i];
    if (negative(b[lenb-1])) lenr = lenb;
    else lenr = lena;
}

void biglogor(const uint64_t *a, size_t lena,
              const uint64_t *b, size_t lenb,
              uint64_t *r, size_t &lenr)
{   if (lena >= lenb) return ordered_biglogor(a, lena, b, lenb, r, lenr);
    else return ordered_biglogor(b, lenb, a, lena, r, lenr);
}

number_representation biglogor(number_representation a, number_representation b)
{   size_t na = number_size(a);
    size_t nb = number_size(b);
    size_t n;
    if (na >= nb) n = na;
    else n = nb;
    uint64_t *p = preallocate(n);
    size_t final_n;
    biglogor(number_data(a), na, number_data(b), nb, p, final_n);
    return confirm_size(p, n, final_n);
}

// logxor

void ordered_biglogxor(const uint64_t *a, size_t lena,
                       const uint64_t *b, size_t lenb,
                       uint64_t *r, size_t &lenr)
{   size_t i;
    for (i=0; i<lenb; i++)
        r[i] = a[i] ^ b[i];
    if (negative(b[lenb-1]))
    {   for (; i<lena; i++)
            r[i] = ~a[i];
    }
    lenr = lena;
// The logxor operation can cause the inputs to shrink.
    truncate_positive(r, lenr);
    truncate_negative(r, lenr);
}

void biglogxor(const uint64_t *a, size_t lena,
               const uint64_t *b, size_t lenb,
               uint64_t *r, size_t &lenr)
{   if (lena >= lenb) return ordered_biglogxor(a, lena, b, lenb, r, lenr);
    else return ordered_biglogxor(b, lenb, a, lena, r, lenr);
}

number_representation biglogxor(number_representation a, number_representation b)
{   size_t na = number_size(a);
    size_t nb = number_size(b);
    size_t n;
    if (na >= nb) n = na;
    else n = nb;
    uint64_t *p = preallocate(n);
    size_t final_n;
    biglogxor(number_data(a), na, number_data(b), nb, p, final_n);
    return confirm_size(p, n, final_n);
}

extern void bigrightshift(const uint64_t *a, size_t lena,
                          int n,
                          uint64_t *r, size_t &lenr);

void bigleftshift(const uint64_t *a, size_t lena,
                  int n,
                  uint64_t *r, size_t &lenr)
{   if (n == 0)
    {   std::memcpy(r, a, lena*sizeof(uint64_t));
        lenr = lena;
        return;
    }
    else if (n < 0)
    {   bigrightshift(a, lena, -b, r, lenr);
        return;
    }
// @@@    
    abort();
}

extern number_representation bigrightshift(number_representation a, int n);

number_representation bigleftshift(number_representation a, int n)
{   if (n == 0) return a;
    else if (n < 0) return bigrightshift(a, -n);
    size_t na = number_size(a);
    size_t nr = na + (n/64) + 1;
    uint64_t *p = preallocate(nr);
    size_t final_n;
    bigleftshift(number_data(a), na, n, p, final_n);
    return confirm_size(p, nr, final_n);
}

void bigrightshift(const uint64_t *a, size_t lena,
                   int n,
                   uint64_t *r, size_t &lenr)
{   if (n == 0)
    {   std::memcpy(r, a, lena*sizeof(uint64_t));
        lenr = lena;
        return;
    }
    else if (n < 0)
    {   bigleftshift(a, lena, -b, r, lenr);
        return;
    }
    size_t words = n/64;
    size_t bits = n % 64;
    if (bits == 0)
    {   for (int i=0; i<lena-words; i++)
           r[i] = a[i+words];
    }
    else
    {   for (int i=0; i<lena-words-1; i++)
           r[i] = (a[i+words]>>bits) |
                  (a[i+words+1]<<(64-bits);
        r[lena-words-1] = (uint64_t)((int64_t)a[lena-1]>>bits);
    }
    lenr = lena-words;
    truncate_positive(r, lenr);
    truncate_negative(r, lenr);
}

number_representation bigrightshift(number_representation a, int n)
{   if (n == 0) return a;
    else if (n < 0) return bigleftshift(a, -n);
    size_t na = number_size(a);
    size_t nr = na - (n/64);
    uint64_t *p = preallocate(nr);
    size_t final_n;
    bigrightshift(number_data(a), na, n, p, final_n);
    return confirm_size(p, nr, final_n);
}

// Add when the length of a is greater than that of b.

static inline void ordered_bigadd(const uint64_t *a, size_t lena,
                                  const uint64_t *b, size_t lenb,
                                  uint64_t *r, size_t &lenr)
{   assert(lena >= lenb);
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

void bigadd_small(const uint64_t *a, size_t lena,
                  int64_t n,
                  uint64_t *r, size_t &lenr)
{   uint64_t w[1];
    w[0] = (uint64_t)n;
    ordered_bigadd(a, lena, w, 1, r, lenr);
}

// When I do a general addition I will not know which input is longer.

void bigadd(const uint64_t *a, size_t lena,
            const uint64_t *b, size_t lenb,
            uint64_t *r, size_t &lenr)
{   if (lena >= lenb) return ordered_bigadd(a, lena, b, lenb, r, lenr);
    else return ordered_bigadd(b, lenb, a, lena, r, lenr);
}

number_representation bigadd(number_representation a, number_representation b)
{   size_t na = number_size(a);
    size_t nb = number_size(b);
    size_t n;
    if (na >= nb) n = na+1;
    else n = nb+1;
    uint64_t *p = preallocate(n);
    size_t final_n;
    bigadd(number_data(a), na, number_data(b), nb, p, final_n);
    return confirm_size(p, n, final_n);
}

number_representation bigadd_small(number_representation a, int64_t b)
{   size_t na = number_size(a);
    uint64_t *p = preallocate(na+1);
    size_t final_n;
    bigadd_small(number_data(a), na, b, p, final_n);
    return confirm_size(p, na+1, final_n);
}

// For subtraction I implement both a-b and b-a. These work by
// computing a + (~b) + 1 and (~a) + b + 1 respectively.

static inline void ordered_bigsubtract(const uint64_t *a, size_t lena,
                                       const uint64_t *b, size_t lenb,
                                       uint64_t *r, size_t &lenr)
{   assert(lena >= lenb);
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

static inline void ordered_bigrevsubtract(const uint64_t *a, size_t lena,
                                          const uint64_t *b, size_t lenb,
                                          uint64_t *r, size_t &lenr)
{   assert(lena >= lenb);
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

void bigsubtract_small(const uint64_t *a, size_t lena,
                       int64_t n,
                       uint64_t *r, size_t &lenr)
{   uint64_t w[1];
    w[0] = (uint64_t)n;
    ordered_bigsubtract(a, lena, w, 1, r, lenr);
}

// subtract a bignum from a small number

void bigrevsubtract_small(const uint64_t *a, size_t lena,
                          int64_t n,
                          uint64_t *r, size_t &lenr)
{   uint64_t w[1];
    w[0] = (uint64_t)n;
    ordered_bigrevsubtract(a, lena, w, 1, r, lenr);
}


void bigsubtract(const uint64_t *a, size_t lena,
                 const uint64_t *b, size_t lenb,
                 uint64_t *r, size_t &lenr)
{   if (lena >= lenb) return ordered_bigsubtract(a, lena, b, lenb, r, lenr);
    else return ordered_bigrevsubtract(b, lenb, a, lena, r, lenr);
}

number_representation bigsubtract(number_representation a, number_representation b)
{   size_t na = number_size(a);
    size_t nb = number_size(b);
    size_t n;
    if (na >= nb) n = na+1;
    else n = nb+1;
    uint64_t *p = preallocate(n);
    size_t final_n;
    bigsubtract(number_data(a), na, number_data(b), nb, p, final_n);
    return confirm_size(p, n, final_n);
}

number_representation bigrevsubtract(number_representation a, number_representation b)
{   size_t na = number_size(a);
    size_t nb = number_size(b);
    size_t n;
    if (a >= b) n = na+1;
    else n = nb+1;
    uint64_t *p = preallocate(n);
    size_t final_n;
    bigsubtract(number_data(b), nb, number_data(a), na, p, final_n);
    return confirm_size(p, n, final_n);
}

extern void display(const char *label, const uint64_t *a, size_t lena); // @@@

// The next is temporary and is for debugging!
static void temp(const char *label, const uint64_t *a, size_t lena)
{   display(label, a, lena);
    std::cout << string_data(bignum_to_string(a, lena, true)) << std::endl;
}

void bigmultiply(const uint64_t *a, size_t lena,
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
    {   uint64_t prev_hi = 0, carry = 0;
        for (size_t j=0; j<lenb; j++)
        {   uint64_t hi, lo, w;
            multiply64(a[i], b[j], hi, lo);
            uint64_t c1 = add_with_carry(lo, r[i+j], w);
            uint64_t c2 = add_with_carry(w, prev_hi, r[i+j]);
            prev_hi = hi + c1;  // can never overflow
            carry = c2;
        }
        r[i+lenb] = prev_hi + carry;
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

number_representation bigmultiply(number_representation a, number_representation b)
{   size_t na = number_size(a);
    size_t nb = number_size(b);
    size_t n = na+nb;
    uint64_t *p = preallocate(n);
    size_t final_n;
    bigmultiply(number_data(a), na, number_data(b), nb, p, final_n);
    return confirm_size(p, n, final_n);
}

// For big multi-digit numbers squaring can be done almost twice as fast
// as general multiplication. 
// eg (a0,a1,a2,a3)^2 can be expressed as
// a0^2+a1^2+a2^2+a3^2 + 2*(a0*a1+a0*a2+a0*a3+a1*a2+a1*a3+a2*a3)
// where the part that has been doubled uses symmetry to reduce the work.

void bigsquare(const uint64_t *a, size_t lena,
               uint64_t *r, size_t &lenr)
{   for (size_t i=0; i<2*lena; i++) r[i] = 0;
// If a is negative then I can treat its true values as
//    a = sign(a) + unsigned(a)
// where a negative value is indicated by having sign(a)=2^(64*len).
// Then the signed product is just a*a [ - 2*a*2^(64*len) ].
    uint64_t carry;
    for (size_t i=0; i<lena; i++)
    {   uint64_t prev_hi = 0;
        carry = 0;
// Note that all the terms I add in here will need to be doubled in the
// final accounting.
        for (size_t j=i+1; j<lena; j++)
        {   uint64_t hi, lo, w;
            multiply64(a[i], a[j], hi, lo);
            uint64_t c1 = add_with_carry(lo, r[i+j], w);
            uint64_t c2 = add_with_carry(w, prev_hi, r[i+j]);
            prev_hi = hi + c1;  // can never overflow
            carry = c2;
        }
        r[i+lena] = prev_hi + carry;
    }
// Double the part that has been computed so far.
    carry = 0;
    for (size_t i=0; i<lenr; i++)
    {   uint64_t w = r[i];
        r[i] = (w >> 1) | (carry << 63);
        carry = w & 1;
    }
// Now add in the bits that do not get doubled.
    carry = 0;
    for (size_t i=0; i<lena; i++)
    {   uint64_t hi, lo, w;
        multiply64(a[i], a[i], hi, lo);
// Add (hi,lo) + carry in at r[2*i+1], r[2*i]
        carry = add_with_carry(lo, r[2*i], carry, r[2*i]);
        carry = add_with_carry(hi, r[2*i+1], carry, r[2*i+1]);
    }
// Now if the original a was negative I must subtract 2*a from the high
// half of my result.
    if (negative(a[lena-1]))
    {   carry = 1;
        uint64_t shiftbit = 0;
        for (size_t i=0; i<lena; i++)
        {   uint64_t w = ~a[i];
            carry = add_with_carry(r[i+lena], 2*w+shiftbit, carry, r[i+lena]);
            shiftbit = w>>63;
        }
    }
    lenr = 2*lena;
// The actual value may be 1 word shorter than this.
//  test top digit or r and if necessary reduce lenr.
    truncate_positive(r, lenr);
    truncate_negative(r, lenr);
}

number_representation bigsquare(number_representation a)
{   size_t na = number_size(a);
    size_t n = 2*na;
    uint64_t *p = preallocate(n);
    size_t final_n;
    bigsquare(number_data(a), na, p, final_n);
    return confirm_size(p, n, final_n);
}

// This raises a bignum to a positive integer power. If the power is n then
// the size of the output may be n*lena. The two vectors v and w are workspace
// and must both be of size (at least) the size that the result could end
// up as.

void bigpower(uint64_t *a, size_t lena, uint64_t n,
              uint64_t *v,
              uint64_t *w,
              uint64_t *r, size_t &lenr)
{   if (n == 0)
    {   r[0] = 0;
        lenr = 1;
        return;
    }
    memcpy((void *)v, (void *)a, lena*sizeof(uint64_t));
    size_t lenv = lena;
    w[0] = 1;
    size_t lenw = 1;
    while (n > 1)
    {   if (n%2 == 0)
        {   bigsquare(v, lenv, r, lenr);
            memcpy((void *)v, (void *)r, lenr*sizeof(uint64_t));
            lenv = lenr;
            n = n / 2;
        }
        else
        {   bigmultiply(v, lenv, w, lenw, r, lenr);
            memcpy((void *)w, (void *)r, lenr*sizeof(uint64_t));
            lenw = lenr;
            bigsquare(v, lenv, r, lenr);
            memcpy((void *)v, (void *)r, lenr*sizeof(uint64_t));
            lenv = lenr;
            n = (n-1) / 2;
        }
    }
    bigmultiply(v, lenv, w, lenw, r, lenr);
}

//
// The next section of code will become the division stuff for bignums.
// At present it is very very much work in progress, with a sketch that
// shows the structure I intend to use but with some functions only present
// as stubs and MANY things just plain incorrect.
//

static void bigmultiply_by_int_in_place(uint64_t *r, size_t lenr,
                                        uint64_t scale, uint64_t &extra)
{   uint64_t carry = 0;
    for (size_t i=0; i<lenr; i++)
    {   uint64_t hi, lo;
        multiply64(r[i], scale, hi, lo);
        uint64_t w = r[i] = lo + carry;
// NB that adding 1 to hi here can never overflow.
        if (w < carry) carry = hi + 1;
        else carry = hi;
    }
    extra = carry;
}

// divide (hi,lo) by divisor and generate a quotient and a remainder. The
// version of the code that is able to use __int128 can serve as clean
// documentation of the intent.

#ifdef __SIZEOF_INT128__

static inline void divide64(uint64_t hi, uint64_t lo, uint64_t divisor,
                            uint64_t &q, uint64_t &r)
{   UINT128 num = ((UINT128)hi << 64) | lo;
    q = num / divisor;
    r = num % divisor;
}

#else // __SIZEOF_INT128__

static uint64_t divide64(uint64_t hi, uint64_t low, uint64_t divisor,
                         uint64_t &q, uint64_t &r)
{   uint64_t u1 = hi;
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

#endif // __SIZE_OF_INT128__

static uint64_t multiply_and_subtract(uint64_t *a, size_t lena,
                                      uint64_t q0,
                                      uint64_t *b, size_t lenb)
{   return 0;
}

static uint64_t add_back_correction(uint64_t *a, size_t lena,
                                    uint64_t *b, size_t lenb)
{   return 0;
}

static inline uint64_t next_quotient_digit(uint64_t atop,
                                           uint64_t *a, size_t &lena,
                                           uint64_t *b, size_t lenb)
{   UINT128 p0 = (UINT128)atop<<64 | a[lena-1];
    uint64_t q0 =  (uint64_t)(p0 / (UINT128)b[lenb-1]);
    uint64_t r0 =  (uint64_t)(p0 % (UINT128)b[lenb-1]);
// At this stage q0 may be correct or it may be an over-estimate by 1 or 2,
// but never any worse than that.
//
// The test on the next line should detect all case where q0 was in error
// by 2 and most when it was in error by 1.
//
    std::cout << "p0 = " << p0 << " / " << (UINT128)b[lenb-1] << std::endl;
    std::cout << "q0 = " << q0 << "  r0 = " << r0 << std::endl;
    if (q0 == UINT64_C(0x8000000000000000) ||
        (UINT128)q0*(UINT128)b[lenb-2] >
        ((UINT128)r0<<64 | a[lena-2]))
        q0--;
    std::cout << "Leading quotient digit = " << q0 << std::endl;
//
// Now I want to go "a = a - b*q0*2^(31*(lena-lenb));" so that a
// is set to an accurate remainder after using q0 as (part of) the
// quotient. This may carry an overshoot into atop and if so I will need
// to reduce q0 again and compensate.
//
    atop += multiply_and_subtract(a, lena, q0, b, lenb);
    temp("mul & sub by q0: ", a, lena);
    std::cout << "sets a to " << atop << std::endl;
    if (negative(atop))
    {   q0--;
        std::cout << "need to add back correction" << std::endl;
        atop = add_back_correction(a, lena, b, lenb);
// When I add back b I ought to get a carry...
        assert(atop == 1);
    }
    lena--;  // a is now one digit shorter.
    return q0;
}

static void negate_in_place(uint64_t *r, size_t &lenr)
{
}

static void unscale(uint64_t *r, size_t &lenr, uint64_t scale)
{
}

static void fix_up_bignum_length(uint64_t *q, size_t &lenq)
{
}

// I need to make copies of both numerator and denominator here because
// both forced positive and both get scaled. So w is passed as temporary
// workspace, and q and r as places where the quotient and remainder will
// end up - note r has to start of one word longer than the numerator a
// even though by the end it will be shorter than b.

void bigquotrem(uint64_t *a, size_t lena,
                uint64_t *b, size_t lenb,
                uint64_t *w, size_t lenw,   // temp - size lenb
                uint64_t *q, size_t &lenq,  // quotient - size lena-lenb+1
                uint64_t *r, size_t &lenr)  // remainder - size lena+1
{
// I copy the absolute values of a and b to places where it will be
// OK to overwrite them, taking their absolute values as I go. I record
// whether the eventual quotient and/or remainder will need to be negated
// at the end. This leaves the two inputs as rows of unsigned 64-bit digits
// with a[alen] and b[blen] both non-zero.
    temp("quotrem a: ", a, lena);
    temp("quotrem b: ", b, lenb);
    bigabsval(a, lena, r, lenr);
    bigabsval(b, lenb, w, lenw);
    bool quot_sign = false, rem_sign = false;
    if (negative(a[lena-1]))
        quot_sign = rem_sign = true;
    if (negative(b[lenb-1])) quot_sign = !quot_sign;
// Now I want to compute the quotient of r by w, and both are positive.
// Taking absolute values might have changed the lengths, so this is a good
// place to check for a division where the quotient is unquestionably zero.
    if (lenr < lenw ||
        (lenr == lenw && r[lenr-1] < w[lenw-1]))
    {   q[0] = 0;
        lenq = 1;
        memcpy((void *)r, (void *)a, lena*sizeof(uint64_t));
        lenr = lena;
        temp("quotient is zero, remainder: ", r, lenr);
        return;
    } 
// By now a and b both have strictly positive leading digits.
    temp("quotrem r: ", a, lenr);
    temp("quotrem w: ", b, lenw);
    lenq = lena-lenb; // potential length of quotient.
// I will multiply a and b by a scale factor that gets the top digit of "b"
// reasonably large. The value stored in "a" can become one digit longer,
// but there is space to store that.
//
// The scale factor used here is as per Knuth II edition II. Edition III
// proposed 0x7fffffffU/bignum_digits(b)[lenb] and if you look at just the
// leading digit of b alone that seems OK, but I am concerned that when you
// take lower digits of b into account that multiplying b by it can overflow.
    uint64_t scale = UINT64_C(0x8000000000000000) / (w[lenw-1] + 1);
// When I scale the dividend expands into an extra digit but the scale
// factor has been chosen so that the divisor does not. So beware that
// r now has digits running from 0 to lenr rather than 0 to lenr-1.
    std::cout << "scale by " << scale << std::endl;
    bigmultiply_by_int_in_place(r, lenr, scale, r[lenr]);
    uint64_t wtop;
    bigmultiply_by_int_in_place(w, lenw, scale, wtop);
    assert(wtop == 0);
    temp("scaled r: ", r, lenr);
    temp("scaled w: ", w, lenw);
    size_t m = lenq-1;
    for (;;)
    {   uint64_t qd = next_quotient_digit(
            r[lenr], r, lenr,
            w, lenw);
        std::cout << "next quotient digit = " << qd << std::endl;
        q[m] = qd;
        if (m == 0) break;
        m--;
    }
// Unscale and correct the signs.
    unscale(r, lenr, scale);
    if (rem_sign) negate_in_place(r, lenr);
// Ensure that the quotient has r prefix zero digit if needbe.
    fix_up_bignum_length(q, lenq);
    if (quot_sign) negate_in_place(q, lenq);
// Now I need to pack the results so that they are suitable for use
// elsewhere in the system. 
//@@        mv_2 = pack_up_result(r, lenr);
//@@        return pack_up_result(q, lenq);
}

number_representation bigquotient(number_representation a, number_representation b)
{   size_t na = number_size(a);
    size_t nb = number_size(b);
    size_t n1 = na-nb+1;           // for the quotient
    uint64_t *q = preallocate(n1);
    size_t n2 = na+1;              // for workspace and the remainder
    uint64_t *r = preallocate(n2);
    size_t n3 = nb;                // temp workspace
    uint64_t *w = preallocate(n3);
    bigquotrem(a, na, b, nb,
               w, n3,
               q, n1, r, n2);
// here w and r are no longer needed.
    return confirm_size(q, na+nb+1, n1);
}

void bigremainder(const uint64_t *a, size_t lena,
                  const uint64_t *b, size_t lenb,
                  uint64_t *r, size_t &lenr)
{   std::cout << "Division not dcoded yet\n" << std::endl;
    abort();
}

number_representation bigremainder(number_representation a, number_representation b)
{   size_t na = number_size(a);
    size_t nb = number_size(b);
    size_t n = na+nb;
    uint64_t *p = preallocate(n);
    size_t final_n;
    bigremainder(number_data(a), na, number_data(b), nb, p, final_n);
    return confirm_size(p, n, final_n);
}

void bigdivide(const uint64_t *a, size_t lena,
               const uint64_t *b, size_t lenb,
               uint64_t *r, size_t &lenr)
{   std::cout << "Division not dcoded yet" << std::endl;
    abort();
}

number_representation bigdivide(number_representation a, number_representation b)
{   size_t na = number_size(a);
    size_t nb = number_size(b);
    size_t n = na+nb;
    uint64_t *p = preallocate(n);
    size_t final_n;
    bigdivide(number_data(a), na, number_data(b), nb, p, final_n);
    return confirm_size(p, n, final_n);
}




// If this code is going to be used within a Lisp system (or indeed
// some similar system) then I think that the rest of the code will
// with to call bigadd etc directly and the "convenience" layer will emerge
// when arithmetic gets reflected up unto the Lisp world, eg as (plus 2 3).
// However for other users it may be handy to provide operator overloading
// so that C++ code can use buf arithmetic integrated in with the rest of
// their code and using infix operators.

#ifndef VSL

class Bignum
{
public:
    number_representation val;
// A default constructor build a Bignum with no stored data.
    Bignum()
    {   val = (number_representation)0;
    }
    ~Bignum()
    {   free_bignum(val);
    }
    Bignum(int32_t n)
    {   val = int_to_bignum((int64_t)n);
    }
    Bignum(int64_t n)
    {   val = int_to_bignum(n);
    }
    Bignum(const char *s)
    {   val = string_to_bignum(s);
    }

    void operator = (const Bignum &x)
    {   if (this == &x) return; // assign to self - a silly case!
        free_bignum(val);
        val = sometimes_copy_bignum(x.val);
    }

    void operator = (const int64_t x)
    {   free_bignum(val);
        val = int_to_bignum(x);
    }

    void operator = (const int32_t x)
    {   free_bignum(val);
        val = int_to_bignum((int64_t)x);
    }

    void operator = (const char *x)
    {   free_bignum(val);
        val = string_to_bignum(x);
    }

    bool operator ==(const Bignum &x) const;
    bool operator > (const Bignum &x) const;
    bool operator >=(const Bignum &x) const;
    bool operator < (const Bignum &x) const;
    bool operator <=(const Bignum &x) const;

    Bignum operator + (const Bignum &x) const;
    Bignum operator - (const Bignum &x) const;
    Bignum operator * (const Bignum &x) const;
    Bignum operator / (const Bignum &x) const;
    Bignum operator % (const Bignum &x) const;
    Bignum operator - () const;

    Bignum operator & (const Bignum &x) const;
    Bignum operator | (const Bignum &x) const;
    Bignum operator ^ (const Bignum &x) const;
    Bignum operator ~ () const;

    Bignum operator >> (int n) const;
    Bignum operator << (int n) const;

    void operator += (const Bignum &x);
    void operator -= (const Bignum &x);
    void operator *= (const Bignum &x);
    void operator /= (const Bignum &x);
    void operator %= (const Bignum &x);

    void operator &= (const Bignum &x);
    void operator |= (const Bignum &x);
    void operator ^= (const Bignum &x);

    void operator >>= (int n);
    void operator <<= (int n);

    Bignum operator ++();
    Bignum operator ++(int);
    Bignum operator --();
    Bignum operator --(int);

    friend std::ostream & operator << (std::ostream &out, const Bignum &a)
    {   std::ios_base::fmtflags fg = out.flags();
        string_representation s;
        if ((fg & std::ios_base::hex) != 0)
            s = bignum_to_string_hex(a.val);
        else if ((fg & std::ios_base::oct) != 0)
            s = bignum_to_string_octal(a.val);
        else s = bignum_to_string(a.val); // decimal
        out << std::setw(string_size(s)) << string_data(s);
        free_string(s);
        return out;
    }
    friend std::istream & operator >> (std::istream &in, Bignum &a)
    {   int64_t n;
// What I really want to do is to read in a string of digits and then
// use string_to_bignum().
        in >> n;
        a.val = int_to_bignum(n);
        return in;
    }
};


const char *to_string(Bignum x)
{   return bignum_to_string(x.val);
}

inline Bignum Bignum::operator +(const Bignum &x) const
{   Bignum ans;
    ans.val = bigadd(this->val, x.val);
    return ans;
}

inline Bignum Bignum::operator -(const Bignum &x) const
{   Bignum ans;
    ans.val = bigsubtract(this->val, x.val);
    return ans;
}

inline Bignum Bignum::operator *(const Bignum &x) const
{   Bignum ans;
    ans.val = bigmultiply(this->val, x.val);
    return ans;
}

inline Bignum Bignum::operator /(const Bignum &x) const
{   Bignum ans;
    ans.val = bigquotient(this->val, x.val);
    return ans;
}

inline Bignum Bignum::operator %(const Bignum &x) const
{   Bignum ans;
    ans.val = bigremainder(this->val, x.val);
    return ans;
}

inline Bignum Bignum::operator -() const
{   Bignum ans;
    ans.val = bignegate(this->val);
    return ans;
}

inline Bignum Bignum::operator &(const Bignum &x) const
{   Bignum ans;
    ans.val = biglogand(this->val, x.val);
    return ans;
}

inline Bignum Bignum::operator |(const Bignum &x) const
{   Bignum ans;
    ans.val = biglogor(this->val, x.val);
    return ans;
}

inline Bignum Bignum::operator ^(const Bignum &x) const
{   Bignum ans;
    ans.val = biglogxor(this->val, x.val);
    return ans;
}

inline Bignum Bignum::operator <<(int n) const
{   Bignum ans;
    ans.val = bigleftshift(this->val, n);
    return ans;
}

inline Bignum Bignum::operator >>(int n) const
{   Bignum ans;
    ans.val = bigrightshift(this->val, n);
    return ans;
}

inline Bignum Bignum::operator ~() const
{   Bignum ans;
    ans.val = biglognot(this->val);
    return ans;
}

inline bool Bignum::operator ==(const Bignum &x) const
{   return bigeqn(this->val, x.val);
}

inline bool Bignum::operator >(const Bignum &x) const
{   return biggreaterp(this->val, x.val);
}

inline bool Bignum::operator >=(const Bignum &x) const
{   return biggeq(this->val, x.val);
}

inline bool Bignum::operator <(const Bignum &x) const
{   return biglessp(this->val, x.val);
}

inline bool Bignum::operator <=(const Bignum &x) const
{   return bigleq(this->val, x.val);
}




inline void Bignum::operator +=(const Bignum &x)
{   number_representation r = bigadd(val, x.val);
    free_bignum(val);
    val = r;
}

inline void Bignum::operator -=(const Bignum &x)
{   number_representation r = bigsubtract(val, x.val);
    free_bignum(val);
    val = r;
}

inline void Bignum::operator *=(const Bignum &x)
{   number_representation r = bigmultiply(val, x.val);
    free_bignum(val);
    val = r;
}

inline void Bignum::operator /=(const Bignum &x)
{   number_representation r = bigquotient(val, x.val);
    free_bignum(val);
    val = r;
}

inline void Bignum::operator %=(const Bignum &x)
{   number_representation r = bigremainder(val, x.val);
    free_bignum(val);
    val = r;
} 

inline void Bignum::operator &=(const Bignum &x)
{   number_representation r = biglogand(val, x.val);
    free_bignum(val);
    val = r;
} 

inline void Bignum::operator |=(const Bignum &x)
{   number_representation r = biglogor(val, x.val);
    free_bignum(val);
    val = r;
} 

inline void Bignum::operator ^=(const Bignum &x)
{   number_representation r = biglogxor(val, x.val);
    free_bignum(val);
    val = r;
} 

inline void Bignum::operator <<=(int n)
{   number_representation r = bigleftshift(val, n);
    free_bignum(val);
    val = r;
}

inline void Bignum::operator >>=(int n)
{   number_representation r = bigrightshift(val, n);
    free_bignum(val);
    val = r;
}

inline Bignum Bignum::operator ++()
{   number_representation r = bigadd_small(val, 1);
    free_bignum(val);
    val = r;
    return *this;
}

inline Bignum Bignum::operator ++(int)
{   number_representation r = bigadd_small(val, 1);
    Bignum oldval;
    oldval.val = val;
    val = r;
    return oldval;
}

inline Bignum Bignum::operator --()
{   number_representation r = bigadd_small(val, -1);
    free_bignum(val);
    val = r;
    return *this;
}

inline Bignum Bignum::operator --(int)
{   number_representation r = bigadd_small(val, -1);
    Bignum oldval;
    oldval.val = val;
    val = r;
    return oldval;
}

#endif


#ifdef TEST

// I need some test code for all of this!

// display() will show the internal representation of a bignum as a
// sequence of hex values. This is obviously useful while debugging!

void display(const char *label, const uint64_t *a, size_t lena)
{   std::cout << label << " [" << (int)lena << "]";
    for (size_t i=0; i<lena; i++)
        std::cout << " "
                  << std::hex << std::setfill('0')
                  << std::setw(16) << a[lena-i-1]
                  << std::dec << std::setw(0);
    std::cout << std::endl;
}

void display(const char *label, number_representation a)
{   
    const uint64_t *d = number_data(a);
    size_t len = number_size(a);
    std::cout << label << " [" << (int)len << "]";
    for (size_t i=0; i<len; i++)
        std::cout << " "
                  << std::hex << std::setfill('0')
                  << std::setw(16) << d[len-i-1]
                  << std::dec << std::setw(0);
    std::cout << std::endl;
}

void display(const char *label, const Bignum &a)
{   display(label, a.val);
}

int main(int argc, char *argv[])
{
    Bignum a, b, c;
    a = "100000000000000000000000000";
    b = "100000000000000000000000000000000";
    c = b / a;
    display("c", c);
    std::cout << c << std::endl;

//    std::cout << "a*a - b*b  =   " << (a*a - b*b) << std::endl;
//    std::cout << "(a+b)*(a-b)  = " << (a + b)*(a - b) << std::endl;
//    std::cout << "a*a - b*b  =   " << std::hex << (a*a - b*b) << std::endl;
//    std::cout << "(a+b)*(a-b)  = " << std::hex << (a + b)*(a - b) << std::endl;
//    std::cout << "a*a - b*b  =   " << std::oct << (a*a - b*b) << std::endl;
//    std::cout << "(a+b)*(a-b)  = " << std::oct << (a + b)*(a - b) << std::endl;

    return 0;    
}

#endif // TEST

// end of arith.cpp
