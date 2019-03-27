#ifndef COMMON_HPP
#define COMMON_HPP

#include <cstdint>

#ifdef __WIN32__
#include <windows.h>
#else
#include <sys/mman.h>
#include <sys/resource.h>

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

//#ifdef WIN32
//#define popen _popen
//#endif

#ifdef CRLIBM

// crlibm aims to produce correctly rounded results in all cases.
// The functions from it selected here are the ones that round to
// nearest. Using this should guarantee constent floating point results
// across al platforms.

extern "C"
{
#include "crlibm.h"
}

// sqrt is not provided by crlibm, probably because it believes that
// most other libraries will get that case exactly right anyway.

double SQRT(double a)  { return  sqrt(a); }
double TANH(double x)  { return  tanh(x); }

double SIN(double a)   { return   sin_rn(a); }
double COS(double a)   { return   cos_rn(a); }
double TAN(double a)   { return   tan_rn(a); }
double SINH(double a)  { return  sinh_rn(a); }
double COSH(double a)  { return  cosh_rn(a); }
double ASIN(double a)  { return  asin_rn(a); }
double ACOS(double a)  { return  acos_rn(a); }
double ATAN(double a)  { return  atan_rn(a); }
double EXP(double a)   { return   exp_rn(a); }
double LOG(double a)   { return   log_rn(a); }
double LOG2(double a)  { return  log2_rn(a); }
double LOG10(double a) { return log10_rn(a); }
double POW(double a, double b)   { return   pow_rn(a, b); }

#else // CRLIBM

double SQRT(double a)  { return  sqrt(a); }
double TANH(double x)  { return  tanh(x); }

double SIN(double a)   { return   sin(a); }
double COS(double a)   { return   cos(a); }
double TAN(double a)   { return   tan(a); }
double SINH(double a)  { return  sinh(a); }
double COSH(double a)  { return  cosh(a); }
double ASIN(double a)  { return  asin(a); }
double ACOS(double a)  { return  acos(a); }
double ATAN(double a)  { return  atan(a); }
double EXP(double a)   { return   exp(a); }
double LOG(double a)   { return   log(a); }
double LOG2(double a)  { return  log2(a); }
double LOG10(double a) { return log10(a); }
double POW(double a, double b)   { return   pow(a, b); }

#endif // CRLIBM

// There are a load of elementary functions not provided by standard
// libraries. I deal with that here.

static const double _pi = 3.14159265358979323846;
static const double _log_2 = 0.6931471805599453094;
static const double _half_pi = ((12868.0 - 0.036490896206895257)/8192.0);

double ASINH(double x)
{   bool sign;
    if (x < 0.0) x = -x, sign = true;
    else sign = false;
    if (x < 1.0e-3)
    {   double xx = x*x;
        x = x*(1 - xx*((1.0/6.0) - (3.0/40.0)*xx));
    }
    else if (x < 1.0e9)
    {   x += sqrt(1.0 + x*x);
        x = LOG(x);
    }
    else x = LOG(x) + _log_2;
    if (sign) x = -x;
    return x;
}

static double acosh_coeffs[] =
{   -0.15718655513711019382e-5,          // x^11
    +0.81758779765416234142e-5,          // x^10
    -0.24812280287135584149e-4,          // x^9
    +0.62919005027033514743e-4,          // x^8
    -0.15404104307204835991e-3,          // x^7
    +0.38339903706706128921e-3,          // x^6
    -0.98871347029548821795e-3,          // x^5
    +0.26854094489454297811e-2,          // x^4
    -0.78918167367399344521e-2,          // x^3
    +0.26516504294146930609e-1,          // x^2
    -0.11785113019775570984,             // x
    +1.41421356237309504786              // 1

};

double ACOSH(double x)
{   bool sign;
    if (x < -1.0) x = -x, sign = true;
    else if (1.0 < x) sign = false;
    else return 0.0;
    if (x < 1.5)
    {   int i;
        double r = acosh_coeffs[0];
        x = (x - 0.5) - 0.5;
//
// This is a minimax approximation to acosh(1+x)/sqrt(x) over the
// range x=0 to 0.5
//
        for (i=1; i<=11; i++) r = x*r + acosh_coeffs[i];
        x = sqrt(x)*r;
    }
    else if (x < 1.0e9)
    {   x += sqrt((x - 1.0)*(x + 1.0));
        x = LOG(x);
    }
    else x = LOG(x) + _log_2;
    if (sign) return -x;
    else return x;
}

double ATANH(double z)
{   if (z > -0.01 && z < -0.01)
    {   double zz = z*z;
        return z * (1 + zz*((1.0/3.0) + zz*((1.0/5.0) + zz*(1.0/7.0))));
    }
    z = (1.0 + z) / (1.0 - z);
    if (z < 0.0) z = -z;
    return LOG(z) / 2.0;
}

static const double n180pi = 57.2957795130823208768;   // 180/pi
static const double pi180  =  0.017453292519943295769; // pi/180

double arg_reduce_degrees(double a, int *quadrant)
//
// Reduce argument to the range -45 to 45, and set quadrant to the
// relevant quadant.  Returns arg converted to radians.
//
{   double w = a / 90.0;
    int32_t n = (int)w;
    w = a - 90.0*n;
    while (w < -45.0)
    {   n--;
        w = a - 90.0*n;
    }
    while (w >= 45.0)
    {   n++;
        w = a - 90.0*n;
    }
    *quadrant = (int)(n & 3);
    return pi180*w;
}

double SIND(double a)
{   int quadrant;
    a = arg_reduce_degrees(a, &quadrant);
    switch (quadrant)
    {   default:
        case 0: return SIN(a);
        case 1: return COS(a);
        case 2: return SIN(-a);
        case 3: return -COS(a);
    }
}

double COSD(double a)
{   int quadrant;
    a = arg_reduce_degrees(a, &quadrant);
    switch (quadrant)
    {   default:
        case 0: return COS(a);
        case 1: return SIN(-a);
        case 2: return -COS(a);
        case 3: return SIN(a);
    }
}

double TAND(double a)
{   int quadrant;
    a = arg_reduce_degrees(a, &quadrant);
    switch (quadrant)
    {   default:
        case 0:
        case 2: return TAN(a);
        case 1:
        case 3: return 1.0/TAN(-a);
    }
}

static double COTD(double a)
{   int quadrant;
    a = arg_reduce_degrees(a, &quadrant);
    switch (quadrant)
    {   default:
        case 0:
        case 2: return 1.0/TAN(a);
        case 1:
        case 3: return TAN(-a);
    }
}

double ACOT(double a)
{   if (a >= 0.0)
        if (a > 1.0) return ATAN(1.0/a);
        else return _half_pi - ATAN(a);
    else if (a < -1.0) return _pi - ATAN(-1.0/a);
    else return _half_pi + ATAN(-a);
}

double ACOTD(double a)
{   if (a >= 0.0)
        if (a > 1.0) return n180pi*ATAN(1.0/a);
        else return 90.0 - n180pi*ATAN(a);
    else if (a < -1.0) return 180.0 - n180pi*ATAN(-1.0/a);
    else return 90.0 + n180pi*ATAN(-a);
}

double CSC(double x)     { return 1.0/SIN(x); }
double SEC(double x)     { return 1.0/COS(x); }
double COT(double x)     { return 1.0/TAN(x); }
double CSCH(double x)    { return 1.0/SINH(x); }
double SECH(double x)    { return 1.0/COSH(x); }
double COTH(double x)    { return 1.0/TANH(x); }
double ACSC(double x)    { return ASIN(1.0/x); }
double ASEC(double x)    { return ACOS(1.0/x); }
double ACSCH(double x)   { return ASINH(1.0/x); }
double ASECH(double x)   { return ACOSH(1.0/x); }
double ACOTH(double x)   { return ATANH(1.0/x); }
double ASIND(double x)   { return (180.0/_pi)*ASIN(x); }
double ACOSD(double x)   { return (180.0/_pi)*ACOS(x); }
double ATAND(double x)   { return (180.0/_pi)*ATAN(x); }
double CSCD(double x)    { return 1.0/SIND(x); }
double SECD(double x)    { return 1.0/COSD(x); }
double ACSCD(double x)   { return ASIND(1.0/x); }
double ASECD(double x)   { return ACOSD(1.0/x); }

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

INLINE const LispObject flagTRACED    = 0x0080;
INLINE const LispObject flagSPECFORM  = 0x0100;
INLINE const LispObject flagMACRO     = 0x0200;
INLINE const LispObject flagGLOBAL    = 0x0400;
INLINE const LispObject flagFLUID     = 0x0800;
INLINE const LispObject flagGENSYM    = 0x1000;
// There are LOTS more bits available for flags etc here if needbe!

// Other atoms have a header that gives info about them. Well as a special
// case I will allow that something tagged with tagATOM but with zero as
// its address is a special marker value...

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

static inline constexpr int64_t qfixnum(LispObject x)
{   return ((int64_t)(x & ~(uint64_t)7)) / 8;
}

static inline LispObject packfixnum(int64_t n)
{   return (LispObject)((uint64_t)n << 3) + tagFIXNUM;
}

INLINE const intptr_t MIN_FIXNUM = qfixnum(INTPTR_MIN);
INLINE const intptr_t MAX_FIXNUM = qfixnum(INTPTR_MAX);

static inline double &qfloat(LispObject x)
{   return ((double *)(x-tagFLOAT))[0];
}

static inline bool isBIGNUM(LispObject x)
{   return isATOM(x) && ((qheader(x) & TYPEBITSX) == typeBIGNUM);
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

INLINE constexpr size_t OBHASH_SIZE   = 15013;
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
//#define bignum     listbases[26]
//#define symfluid   listbases[27]
//#define symglobal  listbases[28]
const int BASES_SIZE = MAX_LISPFILES+29;

#define filecursym (&listbases[29])

LispObject listbases[BASES_SIZE];
std::atomic<LispObject> obhash[OBHASH_SIZE];

// Each thread uses its own threadlocal workspace and shouldn't interfere
// with the others.
static thread_local LispObject work1 = NULLATOM;
static thread_local LispObject work2 = NULLATOM;

static inline LispObject allocateatom(size_t n);

// ... and non-LispObject values that need to be saved as part of a
// heap image.

// I will #include the code for big-numbers once I have all my tag bits etc
// specified.

#define LISP 1
#include "arithlib.hpp"

// ... and non-LispObject values that need to be saved as part of a
// heap image.

// Now I have enough to let me define various allocation functions.

void reclaim(int line);
LispObject error2(const char *s, const char *s1, LispObject a);
LispObject error1(const char *s, LispObject a);
LispObject error0(const char *s);
LispObject quiet_error();
void check_space(int nbytes, int line);

inline bool is_global(LispObject x) {
    return ((qflags(x) & flagGLOBAL) != 0);
}

inline bool is_fluid(LispObject x) {
    return ((qflags(x) & flagFLUID) != 0);
}

#endif // COMMON_HPP