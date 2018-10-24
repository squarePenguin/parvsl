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
// representation, otherwise malloc().
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
//   eqn, greaterp, geq, lessp, leq
//   gcdn, lcmn
//   float, floor, ceil, fix
//   quotient, remainder
//   expt, isqrt
//   bitlength, findfirst-bit, findlast-bit


#define __STDC_FORMAT_MACROS 1
#define __STDC_CONST_MACROS 1
#define __STDC_LIMIT_MACROS 1

#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <inttypes.h>
#include <assert.h>
#include <stdlib.h>

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

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

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
        memmove(p1+sizeof(uintptr_t), p, final_n);
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

uint64_t *preallocate(size_t n)
{   uint64_t *r = (uint64_t *)malloc((n+1)*sizeof(uint64_t));
    assert(r != NULL);
    return &r[1];
}

number_representation confirm_size(uint64_t *p, size_t n, size_t final_n)
{   p = (uint64_t *)realloc((void *)&p[-1], (final_n+1)*sizeof(uint64_t));
    assert(p != NULL);
    p[0] = final_n;
    return &p[1];
}

// In this implementation I just let malloc sort itself out.

number_representation confirm_size_x(uint64_t *p, size_t n, size_t final_n)
{   p = (uint64_t *)realloc((void *)&p[-1], (final_n+1)*sizeof(uint64_t));
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
    memmove(c, (char *)p, final_n);
    c[final_n] = 0; // write in terminator
// Remember to allow for the terminator when adjusting the size! Well
// there will always have been space for it because we are losing the header
// word.
    const char *cc = (const char *)realloc(c, final_n+1);
    assert(cc != NULL);
    return cc;
}

void free_bignum(number_representation p)
{   free((void *)&p[-1]);
}

void free_string(string_representation p)
{   free((void *)p);
}

#endif // VSL

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
// one. A consequence of all this is that any bignum with length 1 can be
// extracted as an int64_t without loss.

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


static inline void multiply64(uint64_t a, uint64_t b,
                              uint64_t &hi, uint64_t &lo)
{
#ifdef __SIZEOF_INT128__
    unsigned __int128 r = (unsigned __int128)a*(unsigned __int128)b;
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
    unsigned __int128 r = (unsigned __int128)a*(unsigned __int128)b +
                          (unsigned __int128_t)c;
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
    memcpy(&r, (const char *)v + 4*n, sizeof(uint32_t));
    return r;
}

static inline void write_u32(uint64_t *v, size_t n, uint32_t r)
{   memcpy((char *)v + 4*n, &r, sizeof(uint32_t));
}

#elif defined __BYTE_ORDER__ && \
    defined __ORDER_BIG_ENDIAN__ && \
    __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__

static inline uint32_t read_u32(const uint64_t *v, size_t n)
{   uint32_t r;
    memcpy(&r, (const char *)v + 4*(n^1), sizeof(uint32_t));
    return r;
}

static inline void write_u32(uint64_t *v, size_t n, uint32_t r)
{   memcpy((char *)v + 4*(n^1), &r, sizeof(uint32_t));
}

#else // endianness not known at compile time

// If I am uncertain about endianness I can extract or insert data
// using shift operations. It is not actually TOO bad.

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
        while (r[n1-1]==allbits && n1>1 && negative(r[n1-2])) n1--;
    }
// However I could not have been precisely certain how many 64-bit words were
// needed and I arranged that any error was conservative - ie allocating
// more that would eventually be used.
    else while (r[n1-1]==0 && n1>1 && positive(r[n1-2])) n1--; 
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

static uint64_t short_divide_ten_19(uint64_t *r, size_t &n)
{   uint64_t hi = 0;
    for (size_t i = n-1; i!=0; i--)
    {   unsigned __int128 p = ((unsigned __int128)hi << 64) | r[i];
        uint64_t q = (uint64_t)(p / ten19);
        hi = (uint64_t)(p % ten19);
        r[i] = q;
    }
    unsigned __int128 p = ((unsigned __int128)hi << 64) | r[0];
    uint64_t q = (uint64_t)(p / ten19);
    hi = (uint64_t)(p % ten19);
    r[0] = q;
    if (r[n-1] == 0) n--;
    return hi;
}

#ifdef __GNUC__

// Note that __GNUC__ also gets defined by clang on the Macintosh, so
// this code is probably optimized there too. Because bignums never have
// a zero leading digit I should never call this with a zero argument,
// which is just as well because its behaviour in that case is undefined!

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

static size_t predict_size_in_bytes(uint64_t *a, size_t n)
{   uint64_t r;
// I am first going to estimate the size in BITS and then I will
// see how that maps onto bytes.
    uint64_t top = a[n-1];  // top digit.
    if (negative(top))
    {   top = -top;
        r = 8;       // 8 bits for a "-" sign.
    }
    else r = 0;
    r = r + 64*(n-1) + (64-nlz(top));
// This risks overflow if the string was going to be over 2^51 bytes long!
    return (617*r)/2048;
} 

string_representation bignum_to_string(number_representation aa)
{   size_t n = number_size(aa);
    uint64_t *a = number_data(aa);
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
    if (negative(r[n-1]))
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
    int len = 0;
    if (sign)
    {   *p1++ = '-';
        len = 1;
    }
    len += sprintf(p1, "%" PRId64, r[p++]);
    p1 += len;
    assert(len < m*sizeof(uint64_t));
    while (p < m)
    {   sprintf(p1, "%.19" PRId64, r[p++]);
        p1 += 19;
        len += 19;
        assert(len <= m*sizeof(uint64_t));
    }
    return confirm_size_string(r, m, len);
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

// The "bitnot" operation is simple and length can not change.

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
// The logxor operation can cause the inputs to shrink.
    while (r[lena-1]==0 && lena>1 && positive(r[lena-2])) lena--;
    while (r[lena-1]==allbits && lena>1 && negative(r[lena-2])) lena--;
    lenr = lena;
}

void biglogxor(const uint64_t *a, size_t lena,
               const uint64_t *b, size_t lenb,
               uint64_t *r, size_t &lenr)
{   if (lena >= lenb) return ordered_biglogxor(a, lena, b, lenb, r, lenr);
    else return ordered_biglogxor(b, lenb, a, lena, r, lenr);
}

// Add when the length of a is greater than that of b.

static inline void ordered_bigadd(const uint64_t *a, size_t lena,
                                  const uint64_t *b, size_t lenb,
                                  uint64_t *r, size_t &lenr)
{   uint64_t carry = 0;
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

// For subtraction I implement both a-b and b-a. These work by
// computing a + (~b) + 1 and (~a) + b + 1 respectively.

static inline void ordered_bigsubtract(const uint64_t *a, size_t lena,
                                       const uint64_t *b, size_t lenb,
                                       uint64_t *r, size_t &lenr)
{   uint64_t carry = 1;
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
{   uint64_t carry = 1;
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

void bigmultiply(const uint64_t *a, size_t lena,
                 const uint64_t *b, size_t lenb,
                 uint64_t *r, size_t &lenr)
{   for (size_t i=0; i<lena+lenb; i++)
        r[i] = 0;
// As coded at present I only cope with +ve inputs.
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
    lenr = lena + lenb;
// The actual value may be 1 word shorter than this.
}

#ifdef TEST

// I need some test code for all of this!

// display() will show the internal representation of a bignum as a
// sequence of hex values. This is obviously useful while debugging!

void display(const char *label, uint64_t *a, size_t lena)
{   printf("%s: [%d]", label, (int)lena);
    for (size_t i=0; i<lena; i++)
        printf(" %.16" PRIx64, a[lena-i-1]);
    printf("\n");
}

void display(const char *label, number_representation a)
{   printf("%s: [%d]", label, (int)number_size(a));
    uint64_t *d = number_data(a);
    size_t len = number_size(a);
    for (size_t i=0; i<len; i++)
        printf(" %.16" PRIx64, d[len-i-1]);
    printf("\n");
}

int main(int argc, char *argv[])
{
    number_representation ten = string_to_bignum("100000000000000000000");
    display("ten", ten);
    const char *s = bignum_to_string(ten);
    printf("ten = <%s>\n", s);
    ten = string_to_bignum("123456789012345678901234567890123456789012345");
    display("ten", ten);
    s = bignum_to_string(ten);
    printf("ten = <%s>\n", s);
    for (int i=0; i<20; i++)
    {   char ti[30];
        sprintf(ti, "%d", -i);
        number_representation k = string_to_bignum(ti);
        const char *l = bignum_to_string_hex(k);
        printf("%d : %s\n", i, l);
    }

    return 0;    
}

#endif // TEST

// end of arith.cpp

