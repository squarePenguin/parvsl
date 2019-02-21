#ifndef COMMON_HPP
#define COMMON_HPP

#include <cstdint>

#ifdef __WIN32__
#include <windows.h>
#else
#include <sys/mman.h>

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
#define INLINE inline
#else
#define INLINE static
#endif

// There is a portability issue about MAP_ANONYMOUS: I hope that
// the adjustments made here will leave everything workable
// everywhere.
#ifdef MAP_ANONYMOUS
#define MMAP_FLAGS (MAP_PRIVATE|MAP_ANONYMOUS)
#define MMAP_FD    -1
#else 
#ifdef MAP_ANON
#define MMAP_FLAGS (MAP_PRIVATE|MAP_ANON)
#define MMAP_FD    -1
#else
int devzero_fd = 0;
#define MMAP_FLAGS MAP_PRIVATE
#define MMAP_FD                                                   \
    (devzero_fd == 0 ? (devzero_fd = open("/dev/zero", O_RDWR)) : \
                       devzero_fd)
#endif // MMAP_ANON
#endif // MMAP_ANONYMOUS
#endif // __WIN32__

#ifdef WIN32
#define popen _popen
#endif


// This version is an extension of the minimal vsl system. It uses
// a conservative garbage collector and will support a fuller and
// higher performance Lisp.


// A Lisp item is represented as an integer and the low 3 bits
// contain tag information that specify how the rest will be used.

typedef intptr_t LispObject;

const intptr_t TAGBITS    = 0x7;

const intptr_t tagCONS    = 0;     // Traditional Lisp "cons" item.
const intptr_t tagSYMBOL  = 1;     // a symbol.
const intptr_t tagFIXNUM  = 2;     // An immediate integer value (29 or 61 bits).
const intptr_t tagFLOAT   = 3;     // A double-precision number.
const intptr_t tagATOM    = 4;     // Something else that will have a header word.
const intptr_t tagFORWARD = 5;     // Used during garbage collection.
const intptr_t tagHDR     = 6;     // the header word at the start of an atom .
const intptr_t tagSPARE   = 7;     // not used!

// Note that in the above I could have used tagATOM to include the case
// of symbols (aka identifiers) but as an optimisation I choose to make that
// a special case. I still have one spare code (tagSPARE) that could be
// used to extend the system.

// Now I provide functions that test the tag bits. These are all rather obvious!

static inline bool isCONS(LispObject x)
{   return (x & TAGBITS) == tagCONS;
}

static inline bool isSYMBOL(LispObject x)
{   return (x & TAGBITS) == tagSYMBOL;
}

static inline bool isFIXNUM(LispObject x)
{   return (x & TAGBITS) == tagFIXNUM;
}

static inline bool isFLOAT(LispObject x)
{   return (x & TAGBITS) == tagFLOAT;
}

static inline bool isATOM(LispObject x)
{   return (x & TAGBITS) == tagATOM;
}

static inline bool isFORWARD(LispObject x)
{   return (x & TAGBITS) == tagFORWARD;
}

static inline bool isHDR(LispObject x)
{   return (x & TAGBITS) == tagHDR;
}


// In memory CONS cells and FLOATS exist as just 2-word items with
// all their bits in use. All other sorts of data have a header word
// at their start.
// This contains extra information about the exact form of data present.

const intptr_t TYPEBITS    = 0x78;
const intptr_t TYPEBITSX   = 0x70;

const intptr_t typeSYM     = 0x00;
const intptr_t typeSTRING  = 0x08;
const intptr_t typeVEC     = 0x10;
// In a first version a BIGNUM only uses typeBIGNUM and the payload it
// carries is an int64_t.
const intptr_t typeBIGNUM  = 0x20;
// EQHASH is a hash table that is in a good state and that is ready
// for use. EQHASHX is one that contains all the correct data but that
// needs re-hashing before it is used. This case arises because garbage
// collection can rearrange memory and thus leave hash-codes out of date.
const intptr_t typeEQHASH  = 0x30;
const intptr_t typeEQHASHX = 0x38;
// Codes 0x28, 0x40, 0x48, 0x50, 0x58, 0x60, 0x68,
// 0x70 and 0x78 spare!

static inline size_t veclength(LispObject h)
{   return ((uintptr_t)h) >> 7;
}

static inline constexpr LispObject packlength(size_t n)
{   return (LispObject)(n << 7);
}

static inline LispObject *heapaddr(LispObject x)
{   return (LispObject *)x;
}

// General indirection. Hmm I think this is an UGLY thing to have because
// the issue of types for it are unclear. I will leave it as a macro for
// now but try to remove it later on.

#define qind(x)     (*((LispObject *)(x)))

// Accessor macros the extract fields from LispObjects ...

static inline LispObject &qcar(LispObject x)
{   return (heapaddr(x))[0];
}

static inline LispObject &qcdr(LispObject x)
{   return (heapaddr(x))[1];
}

// For all other types I must remove the tagging information before I
// can use the item as a pointer.

// An especially important case is that of Symbols. These are the fields that
// they provide.

typedef LispObject SpecialForm(LispObject lits, LispObject a1);
// I will have separate entried for functions with from 0-4 args. For 5
// or more arguments the first 4 will be passed individually with the rest
// in a list.
typedef LispObject LispFn0(LispObject lits);
typedef LispObject LispFn1(LispObject lits, LispObject a1);
typedef LispObject LispFn2(LispObject lits, LispObject a1, LispObject a2);
typedef LispObject LispFn3(LispObject lits, LispObject a1,
                           LispObject a2, LispObject a3);
typedef LispObject LispFn4(LispObject lits, LispObject a1, LispObject a2,
                           LispObject a3, LispObject a4);
typedef LispObject LispFn5up(LispObject lits, LispObject a1, LispObject a2,
                             LispObject a3, LispObject a4, LispObject a5up);

static inline LispObject &qflags(LispObject x)
{   return (heapaddr(x-tagSYMBOL))[0];
}

static inline LispObject &qvalue(LispObject x)
{   return (heapaddr(x-tagSYMBOL))[1];
}

static inline LispObject &qplist(LispObject x)
{   return (heapaddr(x-tagSYMBOL))[2];
}

static inline LispObject &qpname(LispObject x)
{   return (heapaddr(x-tagSYMBOL))[3];
}

static inline LispObject &qlits(LispObject x)
{   return (heapaddr(x-tagSYMBOL))[4];
}

static inline LispObject &qspare(LispObject x)
{   return (heapaddr(x-tagSYMBOL))[5];
}

static inline LispFn0 *&qdefn0(LispObject x)
{   return ((LispFn0 **)(heapaddr(x-tagSYMBOL)))[6];
}

static inline LispFn1 *&qdefn1(LispObject x)
{   return ((LispFn1 **)(heapaddr(x-tagSYMBOL)))[7];
}

static inline LispFn2 *&qdefn2(LispObject x)
{   return ((LispFn2 **)(heapaddr(x-tagSYMBOL)))[8];
}

static inline LispFn3 *&qdefn3(LispObject x)
{   return ((LispFn3 **)(heapaddr(x-tagSYMBOL)))[9];
}

static inline LispFn4 *&qdefn4(LispObject x)
{   return ((LispFn4 **)(heapaddr(x-tagSYMBOL)))[10];
}

static inline LispFn5up *&qdefn5up(LispObject x)
{   return ((LispFn5up **)(heapaddr(x-tagSYMBOL)))[11];
}

INLINE const size_t SYMSIZE = 12;

// Bits within the flags field of a symbol. Uses explained later on.

INLINE const LispObject flagTRACED    = 0x080;
INLINE const LispObject flagSPECFORM  = 0x100;
INLINE const LispObject flagMACRO     = 0x200;
INLINE const LispObject flagGLOBAL    = 0x400;
INLINE const LispObject flagFLUID     = 0x800;
// There are LOTS more bits available for flags etc here if needbe!

// Other atoms have a header that gives info about them. Well as a special
// case I will allow that something tagged with tagATOM but with zero as
// its address is a special marker value...

// I will INSIAT that the definition of tagATOM s in the same compilation
// unit as this is.
INLINE const LispObject NULLATOM = tagATOM + 0;

static inline LispObject &qheader(LispObject x)
{   return (heapaddr((x)-tagATOM))[0];
}

// Fixnums and Floating point numbers are rather easy!

// The behaviour of signed shifts is not nicely defined in C++, so what I
// really expect to compile as (x>>3) needs to be expressed as division.
//
// Also note use of the C++11 "constexpr" annotation to encourage the idea
// that when this is used ini initialization it can be processed at compile
// time.

static inline constexpr intptr_t qfixnum(LispObject x)
{   return ((intptr_t)(x & ~(uintptr_t)7)) / 8;
}

static inline LispObject packfixnum(intptr_t n)
{   return (LispObject)((uintptr_t)n << 3) + tagFIXNUM;
}

INLINE const intptr_t MIN_FIXNUM = qfixnum(INTPTR_MIN);
INLINE const intptr_t MAX_FIXNUM = qfixnum(INTPTR_MAX);

static inline double &qfloat(LispObject x)
{   return ((double *)(x-tagFLOAT))[0];
}

static inline bool isBIGNUM(LispObject x)
{   return isATOM(x) && ((qheader(x) & TYPEBITSX) == typeBIGNUM);
}

static inline int64_t &qint64(LispObject x)
{   return *(int64_t *)(x - tagATOM + 8);
}

static inline bool isSTRING(LispObject x)
{   return isATOM(x) && ((qheader(x) & TYPEBITS) == typeSTRING);
}

static inline char *qstring(LispObject x)
{   return (char *)(x - tagATOM + sizeof(LispObject));
}

static inline bool isVEC(LispObject x)
{   return isATOM(x) && ((qheader(x) & TYPEBITS) == typeVEC);
}

static inline bool isEQHASH(LispObject x)
{   return isATOM(x) && ((qheader(x) & TYPEBITS) == typeEQHASH);
}

static inline bool isEQHASHX(LispObject x)
{   return isATOM(x) && ((qheader(x) & TYPEBITS) == typeEQHASHX);
}

// The Lisp heap will have fixed size.

#ifndef MEM
INLINE constexpr size_t MEM = 1024;
#endif // MEM

INLINE constexpr size_t HALFBITMAPSIZE = (uintptr_t)MEM*1024*(1024/128);
// Each byte in the bitmap will allow marking for 8 entities, and each
// entity is 8 bytes wide (both on 32 and 64-bit systems), hence each
// bitmap uses 1/64th of the memory used by the region it maps.

LispObject *C_stackbase;

// This sets the size of the hash table used to store all the symbols
// that Lisp knows about. I note that if I built a serious application
// such as the Reduce algebra system (reduce-algebra.sourceforge.net) I would
// end up with around 7000 symbols in a basic installation! So the size
// table I use here intended to give decent performance out to that scale.
// This is (of course) utterly over the top for the purpose of toy and
// demonstration applications! I make the table size a prime in the hope that
// that will help keep hashed distribution even across it.
// Hmm - a full copy of everything that makes up Reduce involved around
// 40K distinct symbols...

INLINE constexpr size_t OBHASH_SIZE   = 10007;
INLINE constexpr int MAX_LISPFILES = 30;

// Some Lisp values that I will use frequently...
// I am not quite clear that there would be any nice way to store all these
// list-bases so that I could (a) refer to them by name and (b) scan then
// in a nice loop when garbage collecting. Well I could set up a table that
// contained references to each or a function that could read and write each.
// I guess that that last idea is maybe the best option if I want to avoid
// using the preprocessor in this manner:
//-
//- extern Lispobject nil, undefined, ...;
//- LispObject nil, undefined, ... 
//- INLINE LispObject read_base(size_t n)
//- {   switch (n)
//-     {
//-     case 0:     return nil;
//-     case 1:     return undefined;
//-     ..
//-     }
//- }
//- INLINE void set_base(size_t n, LispObject v)
//- {   switch (n)
//-     {
//-     case 0:     nil = v;
//-     case 1:     undefined = v;
//-     ..
//-     }
//- }

#define nil        listbases[0]
#define undefined  listbases[1]
#define lisptrue   listbases[2]
#define lispsystem listbases[3]
#define echo       listbases[4]
#define symlambda  listbases[5]
#define quote      listbases[6]
#define backquote  listbases[7]
#define comma      listbases[8]
#define comma_at   listbases[9]
#define eofsym     listbases[10]
#define cursym     listbases[11]
#define work1_base      listbases[12]
#define work2_base      listbases[13]
#define restartfn  listbases[14]
#define expr       listbases[15]
#define subr       listbases[16]
#define fexpr      listbases[17]
#define fsubr      listbases[18]
#define macro      listbases[19]
#define input      listbases[20]
#define output     listbases[21]
#define pipe       listbases[22]
#define symraise   listbases[23]
#define symlower   listbases[24]
#define dfprint    listbases[25]
#define bignum     listbases[26]
#define symfluid   listbases[27]
#define symglobal  listbases[28]
const int BASES_SIZE = MAX_LISPFILES+29;

#define filecursym (&listbases[29])

LispObject listbases[BASES_SIZE];
LispObject obhash[OBHASH_SIZE];

// Each thread uses its own threadlocal workspace and shouldn't interfere
// with the others.
static thread_local LispObject work1 = NULLATOM;
static thread_local LispObject work2 = NULLATOM;

// ... and non-LispObject values that need to be saved as part of a
// heap image.

// I will #include the code for big-numbers once I have all my tag bits etc
// specified.

#ifdef BIGNUM
#define LISP 1
#include "arith.hpp"
#endif

// ... and non-LispObject values that need to be saved as part of a
// heap image.

extern LispObject error1(const char *s, LispObject a);
extern LispObject error0(const char *s);

inline bool is_global(LispObject x) {
    return ((qflags(x) & flagGLOBAL) != 0);
}

inline bool is_fluid(LispObject x) {
    return ((qflags(x) & flagFLUID) != 0);
}

#endif // COMMON_HPP