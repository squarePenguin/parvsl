// A sketch of simple bignum arithmetic using 64-bit digits


#define __STDC_FORMAT_MACROS 1
#define __STDC_CONST_MACROS 1
#define __STDC_LIMIT_MACROS 1

#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <inttypes.h>
#include <assert.h>
#include <stdlib.h>

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
#define qint64(x) (*(int64_t *)((x) - tagATOM + 8))

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

//static LispObject error0(const char *msg)
//{   return 0;
//}

//static LispObject error1(const char *msg, LispObject a)
//{   return 0;
//}

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

// Storage management for bignums is sort of delicate because at the
// start of an operation one normally has just an upper bound on how much
// space will be needed - the exact number of words to be used only emerges
// at the end. So the protocol I support is based on thee calls:
//   uint64_t *preallocate(size_t n)
//      This returns a pointer to n words (and it may have allocated a
//      space for a header in front of that).
//   uintptr_t confirm_size(uintptr_t *p, size_t n, size_t final_n)
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
//   uintptr_t confirm_size_x(uintptr_t *p, size_t n, size_t final_n)
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

#ifdef FOR_VSL

typedef LispObject item_representation;

size_t item_size(item_representation a)
{   return veclength(qheader(a))/sizeof(uint64_t);
}

uint64_t *item_data(item_representation a)
{   return (uint64_t)qstring(a);
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
    if (final_n == 1)
    {   memory_used =- (n+1);
        int64_t v = (int64_t)p[0];
        if (v >= SMALLEST_FIXNUM && v <= LARGEST_FIXNUM)
            return packfixnum(v);
    }
#endif
    memory_used -= (n - final_n);
    p[-1] = tagHDR + typeBIGNUM + packlength(n*sizeof(uint64_t));
    return (LispObject)&p[-1] + tagATOM;
}

// I insert an item with typeGAP as a filler...

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
    p[-1] = tagHDR + typeBIGNUM + packlength(n*sizeof(uint64_t));
    p[final_n] = tagHDR + typeGAP + packlength((n-final_n)*sizeof(uint64_t));
    return (LispObject)&p[-1] + tagATOM;
}

#else

typedef uint64_t *item_representation;

size_t item_size(item_representation a)
{   return a[-1];
}

uint64_t *item_data(item_representation a)
{   return a;
}

uint64_t *preallocate(size_t n)
{   uint64_t *r = (uint64_t *)malloc((n+1)*sizeof(uint64_t));
    assert(r != NULL);
    return &r[1];
}

item_representation confirm_size(uint64_t *p, size_t n, size_t final_n)
{   p = (uint64_t *)realloc((void *)&p[-1], (final_n+1)*sizeof(uint64_t));
    assert(p != NULL);
    p[0] = final_n;
    return p;
}

// In this implementationm I just let malloc sort itself out.

item_representation confirm_size_x(uint64_t *p, size_t n, size_t final_n)
{   p = (uint64_t *)realloc((void *)&p[-1], (final_n+1)*sizeof(uint64_t));
    assert(p != NULL);
    p[0] = final_n;
    return p;
}

#endif



//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

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
    unsigned __int128 r = (__int128)a*(__int128)b;
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
    unsigned __int128 r = (__int128)a*(__int128)b + (__int128_t)c;
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

// The functions that are not static and that return an item_representation
// are the ones intended for external use.

item_representation int_to_bignum(int64_t n)
{   uint64_t *r = preallocate(1);
    int_to_bignum(n, r);
    return confirm_size(r, 1, 1);
}

static const uint64_t ten19 = UINT64_C(10000000000000000000);

item_representation string_to_bignum(const char *s)
{   bool sign = false;
    if (*s == '-')
    {   sign = true;
        s++;
    }
    size_t chars = strlen(s);
    size_t words = 1 + (267*chars+5143)/5144;
// I have predicted the number of 64-bit digits that will be needed to
// represent an s-digit (decimal) number based on rounding up from a
// rational approximation 267/5144 for log(10)/log(2^64). The rational
// approximation is an over-estimate. In a rather small number of cases
// this will lead to the bignum ending up with one more 64-bit word than
// is needed, but I can trim that off at the end.
    uint64_t *r = preallocate(words);
    for (size_t i=0; i<words; i++) r[i] = 0;
// Now for each chunk of digits NNNN in the input I want to go in effect
//     r = 10^19*r + NNNN;
// where the number 19 is used because 10^19 is the largest power of 10
// that fits in a 64-bit word.
    size_t next = 19*((chars-1)/19);
    while (chars != 0)
    {   uint64_t d = 0;
        while (chars != next)
        {   d = 10*d + (*s++ - '0');
            chars--;
        }
        next -= 19;
// now try r = 10^19*r + d.
        for (size_t i=0; i<words; i++)
        {   uint64_t hi, lo;
            multiply64(r[i], ten19, d, hi, lo);
            r[i] = lo;
            d = hi;
        }
    }
    size_t n1 = words;
// Here I will be negating a positive number, and in 2s complement that
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

item_representation bignegate(item_representation a)
{   size_t n = item_size(a);
    uint64_t *p = preallocate(n+1);
    size_t final_n;
    bignegate(item_data(a), n, p, final_n);
    return confirm_size(p, n, final_n);
}

// The "bitnot" operation is simple and length can not change.

static void biglognot(const uint64_t *a, size_t lena, uint64_t *r, size_t &lenr)
{   for (size_t i=0; i<lena; i++)
    {   r[i] = ~a[i];
    }
    lenr = lena;
}

item_representation biglognot(item_representation a)
{   size_t n = item_size(a);
    uint64_t *p = preallocate(n+1);
    size_t final_n;
    biglognot(item_data(a), n, p, final_n);
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

// @@@@@@@@@@@@@@@@@@@@@@@@@@@@

// I need some test code for all of this!

void display(const char *label, uint64_t *a, size_t lena)
{   printf("%s: [%d]", label, (int)lena);
    for (size_t i=0; i<lena; i++)
        printf(" %.16" PRIx64, a[lena-i-1]);
    printf("\n");
}

void display(const char *label, item_representation a)
{   printf("%s: [%d]", label, (int)item_size(a));
    uint64_t *d = item_data(a);
    size_t len = item_size(a);
    for (size_t i=0; i<len; i++)
        printf(" %.16" PRIx64, d[len-i-1]);
    printf("\n");
}

uint64_t a[100], b[100], c[100];
size_t lena, lenb, lenc;

int main(int argc, char *argv[])
{
    item_representation ten = string_to_bignum("100");
    display("a", ten);
    return 0;    
}


// end of arith.cpp
