// This is a SKETCH for trying to put "proper" arithmetic into
// VSL. It is intended to support fixnums that have 3 bits at the
// bottom as a tag, bignums that are stored as vectors of 32-bit digits
// and double-precision floats.

// At present the support functions are in here just as dummies so I can
// compile things. And the code to do bignum arithmetic has not been
// written at all.

// A complete version of this would need some unary operations (eg add1,
// sub1, minus), type conversion (fix, float), comparisons (greaterp,
// eqn, ...), more arithmetic and logic (lshift, logxor, quotient, gcdn,...)

// In some sense all of the code is rather routine, but there ends up being
// quite a lot of it.

// Note that LispObject is the same width as intptr_t, and so fixnums are
// either 29 or 61 bits wide, and this code ought to be written to support
// both cases. Also note that the Standard for C++ says that performing any
// arithmetic on a signed value such that overflow would arise leads to
// undefined behaviour, and I have observed g++ exploiting that to optimize
// code in ways that lead to less than obvious behaviour in some overflow
// cases, so writing everything so that it is strictly safe in that regard
// may matter even if doing so leads to rather a lot of casts to get some
// steps performed using unsigned arithmetic.


// With modern C++ the next two lines would not be needed, and indeed
// large constants could be written directly. However I still include them
// for safety with older compilers and build systems. And also to remind
// myself to be careful about writing literals that are for values larger
// than will fit in 32-bit words.

#define __STDC_FORMAT_MACROS 1
#define __STDC_CONST_MACROS 1
#define __STDC_LIMIT_MACROS 1

#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>


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
// Codes 0x30, 0x38, 0x40, 0x48, 0x50, 0x58, 0x60, 0x68,
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
{   LispObject r;
//    check_space(8, __LINE__);
//    setheapstartsandfp(fringe1);
//    r = fringe1 + tagFLOAT;
//    qfloat(r) = a;
//    fringe1 += 8;
//    return r;
    return 0;
}

static LispObject error0(const char *mag)
{   return 0;
}

static LispObject error1(const char *mag, LispObject a)
{   return 0;
}

// make_one_digit_bignum() will only be used on 32-bit platforms

static LispObject make_one_digit_bignum(int32_t val)
{   return 0;
}

static LispObject make_two_digit_bignum(int32_t hi, uint32_t lo)
{   return 0;
}

static LispObject make_three_digit_bignum(
    int32_t d2, uint32_t d1, uint32_t d0)
{   return 0;
}

static LispObject make_four_digit_bignum(
    int32_t d3, uint32_t d2, uint32_t d1, uint32_t d0)
{   return 0;
}


template <class T>
static LispObject binary2(LispObject a, LispObject b)
{   switch (b & TAGBITS)
    {
    case tagFIXNUM:
        return T::op_i(a, b);
    case tagFLOAT:
        return T::op_f(a, b);
    case tagATOM:
        switch (qheader(b) & TYPEBITS)
        {
        case typeBIGNUM:
            return T::op_b(a, b);
        default:
            return error1("Non-numeric argument", b);
        }
    default:
        return error1("Non-numeric argument", b);
    }
}

template <class T>
static LispObject integer_binary2(LispObject a, LispObject b)
{   switch (b & TAGBITS)
    {
    case tagFIXNUM:
        return T::op_i(a, b);
    case tagATOM:
        switch (qheader(b) & TYPEBITS)
        {
        case typeBIGNUM:
            return T::op_b(a, b);
        default:
            return error1("Non-integer argument", b);
        }
    default:
        return error1("Non-integer argument", b);
    }
}

template <class T>
static LispObject binary(LispObject a, LispObject b)
{   switch (a & TAGBITS)
    {
    case tagFIXNUM:
        return T::op_i(a, b);
    case tagFLOAT:
        return T::op_f(a, b);
    case tagATOM:
        switch (qheader(a) & TYPEBITS)
        {
        case typeBIGNUM:
            return T::op_b(a, b);
        default:
            return error1("Non-numeric argument", a);
        }
    default:
        return error1("Non-numeric argument", a);
    }
}

template <class T>
static LispObject integer_binary(LispObject a, LispObject b)
{   switch (a & TAGBITS)
    {
    case tagFIXNUM:
        return T::op_i(a, b);
    case tagATOM:
        switch (qheader(a) & TYPEBITS)
        {
        case typeBIGNUM:
            return T::op_b(a, b);
        default:
            return error1("Non-integer argument", a);
        }
    default:
        return error1("Non-integer argument", a);
    }
}



// ====== addition =====

class Adder_i
{
public:
// a and b are both fixnums. I use intptr_t to hold the integer that
// they decode to since that will be "right" whether running on a 32 or
// 64-bit platform.
   static inline LispObject op_i(LispObject a, LispObject b)
   {   intptr_t w = qfixnum(a) + qfixnum(b);
// Because fixnums use the bottom 3 bits as tags their values are 3-bits
// narrower than an intptr_t, and as a result adding their values can not
// overflow.
       if (w >= MIN_FIXNUM && w <= MAX_FIXNUM) return packfixnum(w);
// On a 32-bit machine the result will always fit in a single 32-bit
// digit of a fixnum. On a 64-bit one it will always fit in two such
// digits, and furthermore it will always need both of them. The
// case so that the right shift is done on an explicitly 64-bit value
// is because shifting a 32-bit value right by 32 would not give a defined
// result.
       else if (sizeof(intptr_t)==4)
           return make_one_digit_bignum((uint32_t)w); 
       else make_two_digit_bignum((uint32_t)(((int64_t)w)>>32), (uint32_t)w);
   }
// a is a fixnum and b is a float
   static inline LispObject op_f(LispObject a, LispObject b)
   {   return boxfloat((double)qfixnum(a) + qfloat(b));
   }
// a is a fixnum and b is a bignum
   static LispObject op_b(LispObject a, LispObject b)
   {   return error0("i+b not done yet");
   }
};

class Adder_f
{
public:
   static inline LispObject op_i(LispObject a, LispObject b)
   {   return boxfloat(qfloat(a) + (double)qfixnum(b));
   }
   static inline LispObject op_f(LispObject a, LispObject b)
   {   return boxfloat(qfloat(a) + qfloat(b));
   }
   static LispObject op_b(LispObject a, LispObject b)
   {   return error0("f+b not done yet");
   }
};

class Adder_b
{
public:
   static LispObject op_i(LispObject a, LispObject b)
   {   return error0("b+i not done yet");
   }
   static LispObject op_f(LispObject a, LispObject b)
   {   return error0("b+f not done yet");
   }
   static LispObject op_b(LispObject a, LispObject b)
   {   return error0("b+b not done yet");
   }
};

class Adder
{
public:
   static inline LispObject op_i(LispObject a, LispObject b)
   {   return binary2<Adder_i>(a, b);
   }
   static inline LispObject op_f(LispObject a, LispObject b)
   {   return binary2<Adder_f>(a, b);
   }
   static inline LispObject op_b(LispObject a, LispObject b)
   {   return binary2<Adder_b>(a, b);
   }
};

// ====== subtraction =====

class Subtracter_i
{
public:
   static inline LispObject op_i(LispObject a, LispObject b)
   {   intptr_t w = qfixnum(a) - qfixnum(b);
       if (w >= MIN_FIXNUM && w <= MAX_FIXNUM) return packfixnum(w);
       else if (sizeof(intptr_t)==4)
           return make_one_digit_bignum((uint32_t)w); 
       else make_two_digit_bignum((uint32_t)(((int64_t)w)>>32), (uint32_t)w);
   }
   static inline LispObject op_f(LispObject a, LispObject b)
   {   return boxfloat((double)qfixnum(a) - qfloat(b));
   }
   static LispObject op_b(LispObject a, LispObject b)
   {   return error0("i-b not done yet");
   }
};

class Subtracter_f
{
public:
   static inline LispObject op_i(LispObject a, LispObject b)
   {   return boxfloat(qfloat(a) - (double)qfixnum(b));
   }
   static inline LispObject op_f(LispObject a, LispObject b)
   {   return boxfloat(qfloat(a) - qfloat(b));
   }
   static LispObject op_b(LispObject a, LispObject b)
   {   return error0("f-b not done yet");
   }
};

class Subtracter_b
{
public:
   static LispObject op_i(LispObject a, LispObject b)
   {   return error0("b-i not done yet");
   }
   static LispObject op_f(LispObject a, LispObject b)
   {   return error0("b-f not done yet");
   }
   static LispObject op_b(LispObject a, LispObject b)
   {   return error0("b-b not done yet");
   }
};

class Subtracter
{
public:
   static inline LispObject op_i(LispObject a, LispObject b)
   {   return binary2<Subtracter_i>(a, b);
   }
   static inline LispObject op_f(LispObject a, LispObject b)
   {   return binary2<Subtracter_f>(a, b);
   }
   static inline LispObject op_b(LispObject a, LispObject b)
   {   return binary2<Subtracter_b>(a, b);
   }
};

// ====== multiplication =====

class Multiplier_i
{
public:
   static LispObject op_i(LispObject a, LispObject b)
   {   intptr_t ia = qfixnum(a), ib = qfixnum(b);
       uint32_t ialo = (uint32_t)ia, iblo = (uint32_t)ib;
       int32_t iahi = (int32_t)(((int64_t)ia)>>32);
       int32_t ibhi = (int32_t)(((int64_t)ib)>>32);
// Assuming that the input was as 64-bit (well 60 bit!) numbers I will
// split it into 29+32 parts (hi,lo) and then I can do four multiplications
// each generating a 64-bit result. Well because the inputs were 61 bits
// not 64-bits I have a bit of range in hand...
       uint64_t rlo = ((uint64_t)ialo)*((uint64_t)iblo);
       int64_t rmid = ((uint64_t)ialo)*((int64_t)ibhi) +
                      ((int64_t)iahi)*((uint64_t)iblo) +
                      (rlo >> 32);
       uint32_t r0 = (uint32_t)rlo; // low 32-bits of result
       uint32_t r1 = (uint32_t)rmid;
       int64_t rhi = ((int64_t)iahi)*((int64_t)ibhi) +
                     (rmid >> 32);
       uint32_t r2 = (uint32_t)rhi;
       int32_t r3 = (int32_t)(rhi >> 32);
// Here I need to arrange to create either a fixnum (if that is possible)
// or as short a bignum as I can.
//     if (w >= MIN_FIXNUM && w <= MAX_FIXNUM) return packfixnum(w);
       make_four_digit_bignum(r3, r2, r1, r0);
   }
   static inline LispObject op_f(LispObject a, LispObject b)
   {   return boxfloat((double)qfixnum(a) + qfloat(b));
   }
   static LispObject op_b(LispObject a, LispObject b)
   {   return error0("i+b not done yet");
   }
};

class Multiplier_f
{
public:
   static LispObject op_i(LispObject a, LispObject b)
   {   return boxfloat(qfloat(a) + (double)qfixnum(b));
   }
   static LispObject op_f(LispObject a, LispObject b)
   {   return boxfloat(qfloat(a) + qfloat(b));
   }
   static LispObject op_b(LispObject a, LispObject b)
   {   return error0("f+b not done yet");
   }
};

class Multiplier_b
{
public:
   static LispObject op_i(LispObject a, LispObject b)
   {   return error0("b+i not done yet");
   }
   static LispObject op_f(LispObject a, LispObject b)
   {   return error0("b+f not done yet");
   }
   static LispObject op_b(LispObject a, LispObject b)
   {   return error0("b+b not done yet");
   }
};

class Multiplier
{
public:
   static inline LispObject op_i(LispObject a, LispObject b)
   {   return binary2<Multiplier_i>(a, b);
   }
   static inline LispObject op_f(LispObject a, LispObject b)
   {   return binary2<Multiplier_f>(a, b);
   }
   static inline LispObject op_b(LispObject a, LispObject b)
   {   return binary2<Multiplier_b>(a, b);
   }
};

// ====== and =====

class Ander_i
{
public:
   static inline LispObject op_i(LispObject a, LispObject b)
   {   intptr_t w = qfixnum(a) & qfixnum(b);
       if (w >= MIN_FIXNUM && w <= MAX_FIXNUM) return packfixnum(w);
       else make_two_digit_bignum((uint32_t)(w>>32), (uint32_t)w);
   }
   static LispObject op_b(LispObject a, LispObject b)
   {   return error0("i&b not done yet");
   }
};

class Ander_b
{
public:
   static LispObject op_i(LispObject a, LispObject b)
   {   return error0("b&i not done yet");
   }
   static LispObject op_b(LispObject a, LispObject b)
   {   return error0("b&b not done yet");
   }
};

class Ander
{
public:
   static inline LispObject op_i(LispObject a, LispObject b)
   {   return integer_binary2<Ander_i>(a, b);
   }
   static inline LispObject op_b(LispObject a, LispObject b)
   {   return integer_binary2<Ander_b>(a, b);
   }
};

// ====== or =====

class Orer_i
{
public:
// I am minded to make the (fixnum op fixnum) cases inline.
   static inline LispObject op_i(LispObject a, LispObject b)
   {   intptr_t w = qfixnum(a) | qfixnum(b);
       if (w >= MIN_FIXNUM && w <= MAX_FIXNUM) return packfixnum(w);
       else make_two_digit_bignum((uint32_t)(w>>32), (uint32_t)w);
   }
   static LispObject op_b(LispObject a, LispObject b)
   {   return error0("i|b not done yet");
   }
};

class Orer_b
{
public:
   static LispObject op_i(LispObject a, LispObject b)
   {   return error0("b|i not done yet");
   }
   static LispObject op_b(LispObject a, LispObject b)
   {   return error0("b|b not done yet");
   }
};

class Orer
{
public:
   static inline LispObject op_i(LispObject a, LispObject b)
   {   return integer_binary2<Orer_i>(a, b);
   }
   static inline LispObject op_b(LispObject a, LispObject b)
   {   return integer_binary2<Orer_b>(a, b);
   }
};



// ===== Now the generic dispatch code =====

static LispObject Lplus2(LispObject a, LispObject b)
{   return binary<Adder>(a, b);
}

static LispObject Lplus3(LispObject a, LispObject b, LispObject c)
{   return binary<Adder>(binary<Adder>(a, b), c);
}

static LispObject Ldifference2(LispObject a, LispObject b)
{   return binary<Subtracter>(a, b);
}

static LispObject Ldifference3(LispObject a, LispObject b, LispObject c)
{   return binary<Subtracter>(binary<Subtracter>(a, b), c);
}

static LispObject Ltimes2(LispObject a, LispObject b)
{   return binary<Multiplier>(a, b);
}

static LispObject Ltimes3(LispObject a, LispObject b, LispObject c)
{   return binary<Multiplier>(binary<Multiplier>(a, b), c);
}

static LispObject Llogand2(LispObject a, LispObject b)
{   return integer_binary<Ander>(a, b);
}

static LispObject Llogand3(LispObject a, LispObject b, LispObject c)
{   return integer_binary<Ander>(integer_binary<Ander>(a, b), c);
}

static LispObject Llogor2(LispObject a, LispObject b)
{   return integer_binary<Orer>(a, b);
}

static LispObject Llogor3(LispObject a, LispObject b, LispObject c)
{   return integer_binary<Orer>(integer_binary<Orer>(a, b), c);
}

// end of arith.cpp
