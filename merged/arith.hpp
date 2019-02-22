
// Note that LispObject is the same width as intptr_t, and so fixnums are
// either 29 or 61 bits wide, and this code ought to be written to support
// both cases. Also note that the Standard for C++ says that performing any
// arithmetic on a signed value such that overflow would arise leads to
// undefined behaviour, and I have observed g++ exploiting that to optimize
// code in ways that lead to less than obvious behaviour in some overflow
// cases, so writing everything so that it is strictly safe in that regard
// may matter even if doing so leads to rather a lot of casts to get some
// steps performed using unsigned arithmetic.



// The classes here and the class objects associated with each are purely
// used as markers so that overloading of a function called "op" can
// support all the various combinations of numeric types in Lisp. The
// collection of tags set up here is intended to be enough for CSL even
// though I will be prototyping stuff in VSL.
//
// I am going to hope that a good optimising compiler will observe that
// where I am passing these objects around I am then not making any use of
// them so it can usefully optimise out all representation of them in the
// final exectuable code. That will happen almost naturally where I use
// inline functions!

namespace number_dispatcher
{
static class I {} xI;   // for small integers
static class B {} xB;   // for bignums
//static class R {} xR;   // for rational numbers
//static class S {} xS;   // for short floats
//static class F {} xF;   // for single-precision floats
static class D {} xD;   // for double precision floats
//static class L {} xL;   // for long floats
//static class C {} xC;   // for complex numbers

// Type-dispatch for binary functions happens in two parts - one on the
// first operand and on on the seoond. This overload of the function "binary"
// does the dispatch on argument 2 and ends up calling a function "op" from
// the operation-specific class, with a call of the form
//   op(lhsType, lhsVal, rhsType, rhsVal)
// where the types are indicated by the number_dispatcher classes and the
// values are whatever data the Lisp wants to use to represent them.
// In the class that defined op() the type arguments are used to select a
// suitable overload but do not carry any data, while the actual operands
// are passed in the Val arguments.


template <class R, class T, class U, typename V>
static inline R binary(const char *fname, U lhsType, V lhsVal, LispObject b)
{   using namespace number_dispatcher;
    switch (b & TAGBITS)
    {
    case tagFIXNUM:
        return T::op(lhsType, lhsVal, xI, qfixnum(b));
    case tagFLOAT:
        return T::op(lhsType, lhsVal, xD, qfloat(b));
    case tagATOM:
        switch (qheader(b) & TYPEBITS)
        {
        case typeBIGNUM:
            return T::op(lhsType, lhsVal, xB, arithlib::vector_of_handle(b));
        default:
            return error2("Non-numeric argument", fname, b);
        }
    default:
        return error2("Non-numeric argument", fname, b);
    }
}

// binary is the dispatcher on the left operand of a binary operator.
// The template has two parameters. The first is the result typeof the
// function we want, the second is a class that contains all the overloads
// op op() that implement it.

template <class R, class T>
static inline R binary(const char *fname, LispObject a, LispObject b)
{   using namespace number_dispatcher;
    switch (a & TAGBITS)
    {
    case tagFIXNUM:
        return binary<R,T,I,int64_t>(fname, xI, qfixnum(a), b);
    case tagFLOAT:
        return binary<R,T,D,double>(fname, xD, qfloat(a), b);
    case tagATOM:
        switch (qheader(a) & TYPEBITS)
        {
        case typeBIGNUM:
            return binary<R,T,B,uint64_t *>(fname, xB, arithlib::vector_of_handle(a), b);
        default:
            return error2("Non-numeric argument", fname, a);
        }
    default:
        return error2("Non-numeric argument", fname, a);
    }
}

// Now the same sort of things but for functions that will only accept
// integer arguments, and so which have lighter weight dispatch.

template <class R, class T, class U, typename V>
static inline R ibinary(const char *fname, U lhsType, V lhsVal, LispObject b)
{   using namespace number_dispatcher;
    switch (b & TAGBITS)
    {
    case tagFIXNUM:
        return T::op(lhsType, lhsVal, xI, qfixnum(b));
    case tagATOM:
        switch (qheader(b) & TYPEBITS)
        {
        case typeBIGNUM:
            return T::op(lhsType, lhsVal, xB, arithlib::vector_of_handle(b));
        default:
            return error2("Non-integer argument", fname, b);
        }
    default:
        return error2("Non-integer argument", fname, b);
    }
}

template <class R, class T>
static inline R ibinary(const char *fname, LispObject a, LispObject b)
{   using namespace number_dispatcher;
    switch (a & TAGBITS)
    {
    case tagFIXNUM:
        return ibinary<R,T,I,int64_t>(fname, xI, qfixnum(a), b);
    case tagATOM:
        switch (qheader(a) & TYPEBITS)
        {
        case typeBIGNUM:
            return ibinary<R,T,B,uint64_t *>(fname, xB, arithlib::vector_of_handle(a), b);
        default:
            return error2("Non-integer argument", fname, a);
        }
    default:
        return error2("Non-integer argument", fname, a);
    }
}

// Unary operations


template <class R, class T>
static inline R unary(const char *fname, LispObject a)
{   using namespace number_dispatcher;
    switch (a & TAGBITS)
    {
    case tagFIXNUM:
        return T::op(xI, qfixnum(a));
    case tagFLOAT:
        return T::op(xD, qfloat(a));
    case tagATOM:
        switch (qheader(a) & TYPEBITS)
        {
        case typeBIGNUM:
            return T::op(xB, arithlib::vector_of_handle(a));
        default:
            return error2("Non-numeric argument", fname, a);
        }
    default:
        return error2("Non-numeric argument", fname, a);
    }
}

// Integer unary operations

template <class R, class T>
static inline R iunary(const char *fname, LispObject a)
{   using namespace number_dispatcher;
    switch (a & TAGBITS)
    {
    case tagFIXNUM:
        return T::op(xI, qfixnum(a));
    case tagATOM:
        switch (qheader(a) & TYPEBITS)
        {
        case typeBIGNUM:
            return T::op(xB, arithlib::vector_of_handle(a));
        default:
            return error2("Non-integer argument", fname, a);
        }
    default:
        return error2("Non-integer argument", fname, a);
    }
}

// Things like "leftshift" that take and integer and a fixnum.

template <class R, class T>
static inline R shiftlike(const char *fname, LispObject a, LispObject b)
{   using namespace number_dispatcher;
    if ((b & TAGBITS) != tagFIXNUM)
        return error2("second argument should be a small integer", fname, b);
    intptr_t n = qfixnum(b);
    switch (a & TAGBITS)
    {
    case tagFIXNUM:
        return T::op(xI, qfixnum(a), n);
    case tagATOM:
        switch (qheader(a) & TYPEBITS)
        {
        case typeBIGNUM:
            return T::op(xB, arithlib::vector_of_handle(a), n);
        default:
            return error2("Non-integer argument", fname, a);
        }
    default:
        return error2("Non-integer argument", fname, a);
    }
}

// Things like "expt" that take and integer and a fixnum.

template <class R, class T>
static inline R exptlike(const char *fname, LispObject a, LispObject b)
{   using namespace number_dispatcher;
    if ((b & TAGBITS) == tagFIXNUM)
    {   intptr_t n = qfixnum(b);
        switch (a & TAGBITS)
        {
        case tagFIXNUM:
            return T::op(xI, qfixnum(a), n);
        case tagFLOAT:
            return T::op(xD, qfloat(a), n);
        case tagATOM:
            switch (qheader(a) & TYPEBITS)
            {
            case typeBIGNUM:
                return T::op(xB, arithlib::vector_of_handle(a), n);
            default:
                return error1("Bad argument to expt", a);
            }
        default:
            return error1("Bad argument to expt", a);
        }
    }
    else if ((b & TAGBITS) == tagFLOAT)
    {   double n = qfloat(b);
        switch (a & TAGBITS)
        {
        case tagFIXNUM:
            return T::op(xI, qfixnum(a), n);
        case tagFLOAT:
            return T::op(xD, qfloat(a), n);
        case tagATOM:
            switch (qheader(a) & TYPEBITS)
            {
            case typeBIGNUM:
                return T::op(xB, arithlib::vector_of_handle(a), n);
            default:
                return error1("Bad argument to expt", a);
            }
        default:
            return error1("Bad argument to expt", a);
        }
    }
    else
    {   error1("Bad second argument to expt", b);
        return nil;
    }
}

} // end of number_dispatcher namespace.

// ====== addition =====


class Adder
{
public:
// a and b are both fixnums. I use intptr_t to hold the integer that
// they decode to since that will be "right" whether running on a 32 or
// 64-bit platform. I make SHORT definitions like this one "inline".
    static inline LispObject op(number_dispatcher::I t1, int64_t a,
                                number_dispatcher::I t2, int64_t b)
    {   return arithlib::Plus::op(a, b);
    }
// a is a fixnum and b is a float
    static inline LispObject op(number_dispatcher::I t1, int64_t a,
                                number_dispatcher::D t2, double b)
    {   return boxfloat(arithlib::Double::op(a) + b);
    }
// a is a fixnum and b is a bignum
    static inline LispObject op(number_dispatcher::I t1, int64_t a,
                                number_dispatcher::B t2, uint64_t *b)
    {   return arithlib::Plus::op(a, b);
    }
    static inline LispObject op(number_dispatcher::D t1, double a,
                                number_dispatcher::I t2, int64_t b)
    {   return boxfloat(a + arithlib::Double::op(b));
    }
    static inline LispObject op(number_dispatcher::D t1, double a,
                                number_dispatcher::D t2, double b)
    {   return boxfloat(a + b);
    }
    static inline LispObject op(number_dispatcher::D t1, double a,
                         number_dispatcher::B t2, uint64_t *b)
    {   return boxfloat(a + arithlib::Double::op(b));
    }
    static inline LispObject op(number_dispatcher::B t1, uint64_t *a,
                         number_dispatcher::I t2, int64_t b)
    {   return arithlib::Plus::op(a, b);
    }
    static inline LispObject op(number_dispatcher::B t1, uint64_t *a,
                         number_dispatcher::D t2, double b)
    {   return boxfloat(arithlib::Double::op(a) + b);
    }
// longer operations, such as adding two bignums, are left so that a
// real function call might be used.
    static inline LispObject op(number_dispatcher::B t1, uint64_t *a,
                         number_dispatcher::B t2, uint64_t *b)
    {   return arithlib::Plus::op(a, b);
    }
};

static LispObject Nplus2(LispObject a, LispObject b)
{   return number_dispatcher::binary<LispObject,Adder>("plus", a, b);
}



// ====== subtraction =====

class Subtracter
{
public:
    static inline LispObject op(number_dispatcher::I t1, int64_t a,
                                number_dispatcher::I t2, int64_t b)
    {   return arithlib::Difference::op(a, b);
    }
    static inline LispObject op(number_dispatcher::I t1, int64_t a,
                                number_dispatcher::D t2, double b)
    {   return boxfloat(arithlib::Double::op(a) - b);
    }
    static inline LispObject op(number_dispatcher::I t1, int64_t a,
                                number_dispatcher::B t2, uint64_t *b)
    {   return arithlib::Difference::op(a, b);
    }
    static inline LispObject op(number_dispatcher::D t1, double a,
                                number_dispatcher::I t2, int64_t b)
    {   return boxfloat(a - arithlib::Double::op(b));
    }
    static inline LispObject op(number_dispatcher::D t1, double a,
                                number_dispatcher::D t2, double b)
    {   return boxfloat(a - b);
    }
    static inline LispObject op(number_dispatcher::D t1, double a,
                         number_dispatcher::B t2, uint64_t *b)
    {   return boxfloat(a - arithlib::Double::op(b));
    }
    static inline LispObject op(number_dispatcher::B t1, uint64_t *a,
                         number_dispatcher::I t2, int64_t b)
    {   return arithlib::Difference::op(a, b);
    }
    static inline LispObject op(number_dispatcher::B t1, uint64_t *a,
                         number_dispatcher::D t2, double b)
    {   return boxfloat(arithlib::Double::op(a) - b);
    }
    static inline LispObject op(number_dispatcher::B t1, uint64_t *a,
                         number_dispatcher::B t2, uint64_t *b)
    {   return arithlib::Difference::op(a, b);
    }
};

static LispObject Ndifference2(LispObject a, LispObject b)
{   return number_dispatcher::binary<LispObject,Subtracter>("difference", a, b);
}


// ====== multiplication =====

class Multiplier
{
public:
    static inline LispObject op(number_dispatcher::I t1, int64_t a,
                                number_dispatcher::I t2, int64_t b)
    {   return arithlib::Times::op(a, b);
    }
    static inline LispObject op(number_dispatcher::I t1, int64_t a,
                                number_dispatcher::D t2, double b)
    {   return boxfloat(arithlib::Double::op(a) * b);
    }
    static inline LispObject op(number_dispatcher::I t1, int64_t a,
                                number_dispatcher::B t2, uint64_t *b)
    {   return arithlib::Times::op(a, b);
    }
    static inline LispObject op(number_dispatcher::D t1, double a,
                                number_dispatcher::I t2, int64_t b)
    {   return boxfloat(a * arithlib::Double::op(b));
    }
    static inline LispObject op(number_dispatcher::D t1, double a,
                                number_dispatcher::D t2, double b)
    {   return boxfloat(a * b);
    }
    static inline LispObject op(number_dispatcher::D t1, double a,
                         number_dispatcher::B t2, uint64_t *b)
    {   return boxfloat(a * arithlib::Double::op(b));
    }
    static inline LispObject op(number_dispatcher::B t1, uint64_t *a,
                         number_dispatcher::I t2, int64_t b)
    {   return arithlib::Times::op(a, b);
    }
    static inline LispObject op(number_dispatcher::B t1, uint64_t *a,
                         number_dispatcher::D t2, double b)
    {   return boxfloat(arithlib::Double::op(a) * b);
    }
    static inline LispObject op(number_dispatcher::B t1, uint64_t *a,
                         number_dispatcher::B t2, uint64_t *b)
    {   return arithlib::Times::op(a, b);
    }
};

static LispObject Ntimes2(LispObject a, LispObject b)
{   return number_dispatcher::binary<LispObject,Multiplier>("times", a, b);
}

// ====== quotient =====

class Quotienter
{
public:
    static inline LispObject op(number_dispatcher::I t1, int64_t a,
                                number_dispatcher::I t2, int64_t b)
    {   return arithlib::Quotient::op(a, b);
    }
    static inline LispObject op(number_dispatcher::I t1, int64_t a,
                                number_dispatcher::D t2, double b)
    {   return boxfloat(arithlib::Double::op(a) / b);
    }
    static inline LispObject op(number_dispatcher::I t1, int64_t a,
                                number_dispatcher::B t2, uint64_t *b)
    {   return arithlib::Quotient::op(a, b);
    }
    static inline LispObject op(number_dispatcher::D t1, double a,
                                number_dispatcher::I t2, int64_t b)
    {   return boxfloat(a / arithlib::Double::op(b));
    }
    static inline LispObject op(number_dispatcher::D t1, double a,
                                number_dispatcher::D t2, double b)
    {   return boxfloat(a / b);
    }
    static inline LispObject op(number_dispatcher::D t1, double a,
                         number_dispatcher::B t2, uint64_t *b)
    {   return boxfloat(a / arithlib::Double::op(b));
    }
    static inline LispObject op(number_dispatcher::B t1, uint64_t *a,
                         number_dispatcher::I t2, int64_t b)
    {   return arithlib::Quotient::op(a, b);
    }
    static inline LispObject op(number_dispatcher::B t1, uint64_t *a,
                         number_dispatcher::D t2, double b)
    {   return boxfloat(arithlib::Double::op(a) / b);
    }
    static inline LispObject op(number_dispatcher::B t1, uint64_t *a,
                         number_dispatcher::B t2, uint64_t *b)
    {   return arithlib::Quotient::op(a, b);
    }
};

static LispObject Nquotient2(LispObject a, LispObject b)
{   return number_dispatcher::binary<LispObject,Quotienter>("quotient", a, b);
}

// ====== remainder =====

// Beware or note that Common Lisp allows you to use a remainder function
// on real values as well as integers, even though C++ does not support
// the "%" operator there.

static inline double fpremainder(double a, double b)
{   return a - b*(a/b);
}

class Remainderer
{
public:
    static inline LispObject op(number_dispatcher::I t1, int64_t a,
                                number_dispatcher::I t2, int64_t b)
    {   return arithlib::Remainder::op(a, b);
    }
    static inline LispObject op(number_dispatcher::I t1, int64_t a,
                                number_dispatcher::D t2, double b)
    {   return boxfloat(fpremainder(arithlib::Double::op(a), b));
    }
    static inline LispObject op(number_dispatcher::I t1, int64_t a,
                                number_dispatcher::B t2, uint64_t *b)
    {   return arithlib::Remainder::op(a, b);
    }
    static inline LispObject op(number_dispatcher::D t1, double a,
                                number_dispatcher::I t2, int64_t b)
    {   return boxfloat(fpremainder(a, arithlib::Double::op(b)));
    }
    static inline LispObject op(number_dispatcher::D t1, double a,
                                number_dispatcher::D t2, double b)
    {   return boxfloat(fpremainder(a, b));
    }
    static inline LispObject op(number_dispatcher::D t1, double a,
                         number_dispatcher::B t2, uint64_t *b)
    {   return boxfloat(fpremainder(a, arithlib::Double::op(b)));
    }
    static inline LispObject op(number_dispatcher::B t1, uint64_t *a,
                         number_dispatcher::I t2, int64_t b)
    {   return arithlib::Remainder::op(a, b);
    }
    static inline LispObject op(number_dispatcher::B t1, uint64_t *a,
                         number_dispatcher::D t2, double b)
    {   return boxfloat(fpremainder(arithlib::Double::op(a), b));
    }
    static inline LispObject op(number_dispatcher::B t1, uint64_t *a,
                         number_dispatcher::B t2, uint64_t *b)
    {   return arithlib::Remainder::op(a, b);
    }
};

static LispObject Nremainder2(LispObject a, LispObject b)
{   return number_dispatcher::binary<LispObject,Remainderer>("remainder", a, b);
}

// ====== divide =====

class Divider
{
public:
    static inline LispObject op(number_dispatcher::I t1, int64_t a,
                                number_dispatcher::I t2, int64_t b)
    {   return arithlib::Divide::op(a, b);
    }
    static inline LispObject op(number_dispatcher::I t1, int64_t a,
                                number_dispatcher::D t2, double b)
    {   return cons(boxfloat(arithlib::Double::op(a) / b),
                    boxfloat(fpremainder(arithlib::Double::op(a), b)));
    }
    static inline LispObject op(number_dispatcher::I t1, int64_t a,
                                number_dispatcher::B t2, uint64_t *b)
    {   return arithlib::Divide::op(a, b);
    }
    static inline LispObject op(number_dispatcher::D t1, double a,
                                number_dispatcher::I t2, int64_t b)
    {   return cons(boxfloat(a / arithlib::Double::op(b)),
                    boxfloat(fpremainder(a, arithlib::Double::op(b))));
    }
    static inline LispObject op(number_dispatcher::D t1, double a,
                                number_dispatcher::D t2, double b)
    {   return cons(boxfloat(a / b), boxfloat(fpremainder(a, b)));
    }
    static inline LispObject op(number_dispatcher::D t1, double a,
                         number_dispatcher::B t2, uint64_t *b)
    {   return cons(boxfloat(a / arithlib::Double::op(b)),
                    boxfloat(fpremainder(a, arithlib::Double::op(b))));
    }
    static inline LispObject op(number_dispatcher::B t1, uint64_t *a,
                         number_dispatcher::I t2, int64_t b)
    {   return arithlib::Divide::op(a, b);
    }
    static inline LispObject op(number_dispatcher::B t1, uint64_t *a,
                         number_dispatcher::D t2, double b)
    {   return cons(boxfloat(arithlib::Double::op(a) / b),
                    boxfloat(fpremainder(arithlib::Double::op(a), b)));
    }
    static inline LispObject op(number_dispatcher::B t1, uint64_t *a,
                         number_dispatcher::B t2, uint64_t *b)
    {   return arithlib::Divide::op(a, b);
    }
};

static LispObject Ndivide2(LispObject a, LispObject b)
{   return number_dispatcher::binary<LispObject,Divider>("divide", a, b);
}


// ====== gcdn =====

class Gcdner
{
public:
    static inline LispObject op(number_dispatcher::I t1, int64_t a,
                                number_dispatcher::I t2, int64_t b)
    {   return arithlib::Gcd::op(a, b);
    }
    static inline LispObject op(number_dispatcher::I t1, int64_t a,
                         number_dispatcher::B t2, uint64_t *b)
    {   return arithlib::Gcd::op(a, b);
    }
    static inline LispObject op(number_dispatcher::B t1, uint64_t *a,
                         number_dispatcher::I t2, int64_t b)
    {   return arithlib::Gcd::op(a, b);
    }
    static inline LispObject op(number_dispatcher::B t1, uint64_t *a,
                         number_dispatcher::B t2, uint64_t *b)
    {   return arithlib::Gcd::op(a, b);
    }
};

static LispObject Ngcdn(LispObject a, LispObject b)
{   return number_dispatcher::ibinary<LispObject,Gcdner>("gcdn", a, b);
}


// ====== lcmn =====

class Lcmner
{
public:
    static inline LispObject op(number_dispatcher::I t1, int64_t a,
                                number_dispatcher::I t2, int64_t b)
    {   return arithlib::Lcm::op(a, b);
    }
    static inline LispObject op(number_dispatcher::I t1, int64_t a,
                         number_dispatcher::B t2, uint64_t *b)
    {   return arithlib::Lcm::op(a, b);
    }
    static inline LispObject op(number_dispatcher::B t1, uint64_t *a,
                         number_dispatcher::I t2, int64_t b)
    {   return arithlib::Lcm::op(a, b);
    }
    static inline LispObject op(number_dispatcher::B t1, uint64_t *a,
                         number_dispatcher::B t2, uint64_t *b)
    {   return arithlib::Lcm::op(a, b);
    }
};

static LispObject Nlcmn(LispObject a, LispObject b)
{   return number_dispatcher::ibinary<LispObject,Lcmner>("lcmn", a, b);
}


// ====== and =====

class Ander
{
public:
    static inline LispObject op(number_dispatcher::I t1, int64_t a,
                                number_dispatcher::I t2, int64_t b)
    {   return arithlib::Logand::op(a, b);
    }
    static inline LispObject op(number_dispatcher::I t1, int64_t a,
                         number_dispatcher::B t2, uint64_t *b)
    {   return arithlib::Logand::op(a, b);
    }
    static inline LispObject op(number_dispatcher::B t1, uint64_t *a,
                         number_dispatcher::I t2, int64_t b)
    {   return arithlib::Logand::op(a, b);
    }
    static inline LispObject op(number_dispatcher::B t1, uint64_t *a,
                         number_dispatcher::B t2, uint64_t *b)
    {   return arithlib::Logand::op(a, b);
    }
};

static LispObject Nlogand2(LispObject a, LispObject b)
{   return number_dispatcher::ibinary<LispObject,Ander>("logand", a, b);
}


// ====== or =====

class Orer
{
public:
    static inline LispObject op(number_dispatcher::I t1, int64_t a,
                                number_dispatcher::I t2, int64_t b)
    {   return arithlib::Logor::op(a, b);
    }
    static inline LispObject op(number_dispatcher::I t1, int64_t a,
                         number_dispatcher::B t2, uint64_t *b)
    {   return arithlib::Logor::op(a, b);
    }
    static inline LispObject op(number_dispatcher::B t1, uint64_t *a,
                         number_dispatcher::I t2, int64_t b)
    {   return arithlib::Logor::op(a, b);
    }
    static inline LispObject op(number_dispatcher::B t1, uint64_t *a,
                         number_dispatcher::B t2, uint64_t *b)
    {   return arithlib::Logor::op(a, b);
    }
};

static LispObject Nlogor2(LispObject a, LispObject b)
{   return number_dispatcher::ibinary<LispObject,Orer>("logor", a, b);
}

// ====== xor =====

class Xorer
{
public:
    static inline LispObject op(number_dispatcher::I t1, int64_t a,
                                number_dispatcher::I t2, int64_t b)
    {   return arithlib::Logxor::op(a, b);
    }
    static inline LispObject op(number_dispatcher::I t1, int64_t a,
                         number_dispatcher::B t2, uint64_t *b)
    {   return arithlib::Logxor::op(a, b);
    }
    static inline LispObject op(number_dispatcher::B t1, uint64_t *a,
                         number_dispatcher::I t2, int64_t b)
    {   return arithlib::Logxor::op(a, b);
    }
    static inline LispObject op(number_dispatcher::B t1, uint64_t *a,
                         number_dispatcher::B t2, uint64_t *b)
    {   return arithlib::Logxor::op(a, b);
    }
};

static LispObject Nlogxor2(LispObject a, LispObject b)
{   return number_dispatcher::ibinary<LispObject,Xorer>("logxor", a, b);
}

// ====== greaterp ======

class Greaterper
{
public:
    static inline bool op(number_dispatcher::I t1, int64_t a,
                          number_dispatcher::I t2, int64_t b)
    {   return arithlib::Greaterp::op(a, b);
    }
    static inline bool op(number_dispatcher::I t1, int64_t a,
                          number_dispatcher::D t2, double b)
    {   return arithlib::Greaterp::op(a, b);
    }
    static inline bool op(number_dispatcher::I t1, int64_t a,
                          number_dispatcher::B t2, uint64_t *b)
    {   return arithlib::Greaterp::op(a, b);
    }
    static inline bool op(number_dispatcher::D t1, double a,
                          number_dispatcher::I t2, int64_t b)
    {   return arithlib::Greaterp::op(a, b);
    }
    static inline bool op(number_dispatcher::D t1, double a,
                          number_dispatcher::D t2, double b)
    {   return (a > b);
    }
    static inline bool op(number_dispatcher::D t1, double a,
                          number_dispatcher::B t2, uint64_t *b)
    {   return arithlib::Greaterp::op(a, b);
    }
    static inline bool op(number_dispatcher::B t1, uint64_t *a,
                          number_dispatcher::I t2, int64_t b)
    {   return arithlib::Greaterp::op(a, b);
    }
    static inline bool op(number_dispatcher::B t1, uint64_t *a,
                          number_dispatcher::D t2, double b)
    {   return arithlib::Greaterp::op(a, b);
    }
    static inline bool op(number_dispatcher::B t1, uint64_t *a,
                          number_dispatcher::B t2, uint64_t *b)
    {   return arithlib::Greaterp::op(a, b);
    }
};

static inline bool Bgreaterp2(LispObject a, LispObject b)
{   return number_dispatcher::binary<bool,Greaterper>("greaterp", a, b);
}


// ====== geq ======

class Geqer
{
public:
    static inline bool op(number_dispatcher::I t1, int64_t a,
                                number_dispatcher::I t2, int64_t b)
    {   return arithlib::Geq::op(a, b);
    }
    static inline bool op(number_dispatcher::I t1, int64_t a,
                                number_dispatcher::D t2, double b)
    {   return (arithlib::Double::op(a) >= b);
    }
    static inline bool op(number_dispatcher::I t1, int64_t a,
                                number_dispatcher::B t2, uint64_t *b)
    {   return arithlib::Geq::op(a, b);
    }
    static inline bool op(number_dispatcher::D t1, double a,
                                number_dispatcher::I t2, int64_t b)
    {   return (a >= arithlib::Double::op(b));
    }
    static inline bool op(number_dispatcher::D t1, double a,
                                number_dispatcher::D t2, double b)
    {   return (a >= b);
    }
    static inline bool op(number_dispatcher::D t1, double a,
                         number_dispatcher::B t2, uint64_t *b)
    {   return (a >= arithlib::Double::op(b));
    }
    static inline bool op(number_dispatcher::B t1, uint64_t *a,
                         number_dispatcher::I t2, int64_t b)
    {   return arithlib::Geq::op(a, b);
    }
    static inline bool op(number_dispatcher::B t1, uint64_t *a,
                         number_dispatcher::D t2, double b)
    {   return (arithlib::Double::op(a) >= b);
    }
    static inline bool op(number_dispatcher::B t1, uint64_t *a,
                         number_dispatcher::B t2, uint64_t *b)
    {   return arithlib::Geq::op(a, b);
    }
};

static inline bool Bgeq2(LispObject a, LispObject b)
{   return number_dispatcher::binary<bool,Geqer>("geq", a, b);
}



// ====== lessp ======

class Lessper
{
public:
    static inline bool op(number_dispatcher::I t1, int64_t a,
                                number_dispatcher::I t2, int64_t b)
    {   return arithlib::Lessp::op(a, b);
    }
    static inline bool op(number_dispatcher::I t1, int64_t a,
                                number_dispatcher::D t2, double b)
    {   return (arithlib::Double::op(a) < b);
    }
    static inline bool op(number_dispatcher::I t1, int64_t a,
                                number_dispatcher::B t2, uint64_t *b)
    {   return arithlib::Lessp::op(a, b);
    }
    static inline bool op(number_dispatcher::D t1, double a,
                                number_dispatcher::I t2, int64_t b)
    {   return (a < arithlib::Double::op(b));
    }
    static inline bool op(number_dispatcher::D t1, double a,
                                number_dispatcher::D t2, double b)
    {   return (a < b);
    }
    static inline bool op(number_dispatcher::D t1, double a,
                         number_dispatcher::B t2, uint64_t *b)
    {   return (a < arithlib::Double::op(b));
    }
    static inline bool op(number_dispatcher::B t1, uint64_t *a,
                         number_dispatcher::I t2, int64_t b)
    {   return arithlib::Lessp::op(a, b);
    }
    static inline bool op(number_dispatcher::B t1, uint64_t *a,
                         number_dispatcher::D t2, double b)
    {   return (arithlib::Double::op(a) < b);
    }
    static inline bool op(number_dispatcher::B t1, uint64_t *a,
                         number_dispatcher::B t2, uint64_t *b)
    {   return arithlib::Lessp::op(a, b);
    }
};

static inline bool Blessp2(LispObject a, LispObject b)
{   return number_dispatcher::binary<bool,Lessper>("lessp", a, b);
}

// ====== leq ======

class Leqer
{
public:
    static inline bool op(number_dispatcher::I t1, int64_t a,
                                number_dispatcher::I t2, int64_t b)
    {   return arithlib::Leq::op(a, b);
    }
    static inline bool op(number_dispatcher::I t1, int64_t a,
                                number_dispatcher::D t2, double b)
    {   return (arithlib::Double::op(a) <= b);
    }
    static inline bool op(number_dispatcher::I t1, int64_t a,
                                number_dispatcher::B t2, uint64_t *b)
    {   return arithlib::Leq::op(a, b);
    }
    static inline bool op(number_dispatcher::D t1, double a,
                                number_dispatcher::I t2, int64_t b)
    {   return (a <= arithlib::Double::op(b));
    }
    static inline bool op(number_dispatcher::D t1, double a,
                                number_dispatcher::D t2, double b)
    {   return (a <= b);
    }
    static inline bool op(number_dispatcher::D t1, double a,
                         number_dispatcher::B t2, uint64_t *b)
    {   return (a <= arithlib::Double::op(b));
    }
    static inline bool op(number_dispatcher::B t1, uint64_t *a,
                         number_dispatcher::I t2, int64_t b)
    {   return arithlib::Leq::op(a, b);
    }
    static inline bool op(number_dispatcher::B t1, uint64_t *a,
                         number_dispatcher::D t2, double b)
    {   return (arithlib::Double::op(a) <= b);
    }
    static inline bool op(number_dispatcher::B t1, uint64_t *a,
                         number_dispatcher::B t2, uint64_t *b)
    {   return arithlib::Leq::op(a, b);
    }
};

static inline bool Bleq2(LispObject a, LispObject b)
{   return number_dispatcher::binary<bool,Leqer>("leq", a, b);
}

// ====== add1 ======

class Add1er
{
public:
    static inline LispObject op(number_dispatcher::I t1, int64_t a)
    {   return arithlib::Add1::op(a);
    }
    static inline LispObject op(number_dispatcher::D t1, double a)
    {   return boxfloat(a + 1.0);
    }
    static inline LispObject op(number_dispatcher::B t1, uint64_t *a)
    {   return arithlib::Add1::op(a);
    }
};

static LispObject Nadd1(LispObject a)
{   return number_dispatcher::unary<LispObject,Add1er>("add1", a);
}

// ====== sub1 ======

class Sub1er
{
public:
    static inline LispObject op(number_dispatcher::I t1, int64_t a)
    {   return arithlib::Sub1::op(a);
    }
    static inline LispObject op(number_dispatcher::D t1, double a)
    {   return boxfloat(a - 1.0);
    }
    static inline LispObject op(number_dispatcher::B t1, uint64_t *a)
    {   return arithlib::Sub1::op(a);
    }
};

static LispObject Nsub1(LispObject a)
{   return number_dispatcher::unary<LispObject,Sub1er>("sub1", a);
}

// ====== minus ======

class Minuser
{
public:
    static inline LispObject op(number_dispatcher::I t1, int64_t a)
    {   return arithlib::Minus::op(a);
    }
    static inline LispObject op(number_dispatcher::D t1, double a)
    {   return boxfloat(-a);
    }
    static inline LispObject op(number_dispatcher::B t1, uint64_t *a)
    {   return arithlib::Minus::op(a);
    }
};

static LispObject Nminus(LispObject a)
{   return number_dispatcher::unary<LispObject,Minuser>("minus", a);
}

// ====== minusp ======

class Minusper
{
public:
    static inline bool op(number_dispatcher::I t1, int64_t a)
    {   return arithlib::Minusp::op(a);
    }
    static inline bool op(number_dispatcher::D t1, double a)
    {   return (a < 0);
    }
    static inline bool op(number_dispatcher::B t1, uint64_t *a)
    {   return arithlib::Minusp::op(a);
    }
};

static inline bool Bminusp(LispObject a)
{   return number_dispatcher::unary<bool,Minusper>("minusp", a);
}

// ====== abs ======

class Abser
{
public:
    static inline LispObject op(number_dispatcher::I t1, int64_t a)
    {   return arithlib::Abs::op(a);
    }
    static inline LispObject op(number_dispatcher::D t1, double a)
    {   return boxfloat(a<0.0 ? -a : a);
    }
    static inline LispObject op(number_dispatcher::B t1, uint64_t *a)
    {   return arithlib::Abs::op(a);
    }
};

static LispObject Nabs(LispObject a)
{   return number_dispatcher::unary<LispObject,Abser>("abs", a);
}

// ====== evenp ======

class Evenper
{
public:
    static inline bool op(number_dispatcher::I t1, int64_t a)
    {   return arithlib::Evenp::op(a);
    }
    static inline bool op(number_dispatcher::B t1, uint64_t *a)
    {   return arithlib::Evenp::op(a);
    }
};

static bool Bevenp(LispObject a)
{   return number_dispatcher::iunary<bool,Evenper>("evenp", a);
}

// ====== oddp ======

class Oddper
{
public:
    static inline bool op(number_dispatcher::I t1, int64_t a)
    {   return arithlib::Oddp::op(a);
    }
    static inline bool op(number_dispatcher::B t1, uint64_t *a)
    {   return arithlib::Oddp::op(a);
    }
};

static bool Boddp(LispObject a)
{   return number_dispatcher::iunary<bool,Oddper>("oddp", a);
}

// ====== msd ======

class Msder
{
public:
    static inline int64_t op(number_dispatcher::I t1, int64_t a)
    {   return arithlib::Integer_length::op(a);
    }
    static inline int64_t op(number_dispatcher::B t1, uint64_t *a)
    {   return arithlib::Integer_length::op(a);
    }
};

static LispObject Nmsd(LispObject a)
{   return packfixnum(number_dispatcher::iunary<int64_t,Msder>("msd", a));
}

// ====== lsd ======

class Lsder
{
public:
    static inline int64_t op(number_dispatcher::I t1, int64_t a)
    {   return arithlib::Low_bit::op(a);
    }
    static inline int64_t op(number_dispatcher::B t1, uint64_t *a)
    {   return arithlib::Low_bit::op(a);
    }
};

static LispObject Nlsd(LispObject a)
{   return packfixnum(number_dispatcher::iunary<int64_t,Lsder>("lsd", a));
}

// ====== bitcount ======

class Bitcounter
{
public:
    static inline LispObject op(number_dispatcher::I t1, int64_t a)
    {   return arithlib::Logcount::op(a);
    }
    static inline LispObject op(number_dispatcher::B t1, uint64_t *a)
    {   return arithlib::Logcount::op(a);
    }
};

static LispObject Nbitcount(LispObject a)
{   return packfixnum(number_dispatcher::iunary<int64_t,Bitcounter>("bitcount", a));
}

// ====== lognot ======

class Lognoter
{
public:
    static inline LispObject op(number_dispatcher::I t1, int64_t a)
    {   return arithlib::Lognot::op(a);
    }
    static inline LispObject op(number_dispatcher::B t1, uint64_t *a)
    {   return arithlib::Lognot::op(a);
    }
};

static LispObject Nlognot(LispObject a)
{   return number_dispatcher::iunary<LispObject,Lognoter>("lognot", a);
}

// ====== leftshift ======

class Leftshifter
{
public:
    static inline LispObject op(number_dispatcher::I t1, int64_t a, int64_t n)
    {   return arithlib::Leftshift::op(a, n);
    }
    static inline LispObject op(number_dispatcher::B t1, uint64_t *a, int64_t n)
    {   return arithlib::Leftshift::op(a, n);
    }
};

static LispObject Nleftshift(LispObject a, LispObject b)
{   return number_dispatcher::shiftlike<LispObject,Leftshifter>("leftshift", a, b);
}


// ====== rightshift ======

class Rightshifter
{
public:
    static inline LispObject op(number_dispatcher::I t1, int64_t a, int64_t n)
    {   return arithlib::Rightshift::op(a, n);
    }
    static inline LispObject op(number_dispatcher::B t1, uint64_t *a, int64_t n)
    {   return arithlib::Rightshift::op(a, n);
    }
};

static LispObject Nrightshift(LispObject a, LispObject b)
{   return number_dispatcher::shiftlike<LispObject,Rightshifter>("rightshift", a, b);
}


LispObject Lminus(LispObject lits, LispObject x)
{   return Nminus(x);
}

LispObject Labs_1(LispObject lits, LispObject x)
{   return Nabs(x);
}

// ====== expt ======

class Expter
{
public:
    static inline LispObject op(number_dispatcher::I t1, int64_t a, int64_t n)
    {   return arithlib::Pow::op(a, n);
    }
    static inline LispObject op(number_dispatcher::B t1, uint64_t *a, int64_t n)
    {   return arithlib::Pow::op(a, n);
    }
    static inline LispObject op(number_dispatcher::D t1, double a, int64_t n)
    {   return boxfloat(pow(a, n));
    }
    static inline LispObject op(number_dispatcher::I t1, int64_t a, double n)
    {   return boxfloat(arithlib::Pow::op(a, n));
    }
    static inline LispObject op(number_dispatcher::B t1, uint64_t *a, double n)
    {   return boxfloat(arithlib::Pow::op(a, n));
    }
    static inline LispObject op(number_dispatcher::D t1, double a, double n)
    {   return boxfloat(pow(a, n));
    }
};

static LispObject Nexpt(LispObject a, LispObject b)
{   return number_dispatcher::exptlike<LispObject,Expter>("expt", a, b);
}


// ====== random ======

class Randomer
{
public:
    static inline LispObject op(number_dispatcher::I t1, int64_t a)
    {   return packfixnum(arithlib::uniform_uint64(a));
    }
    static inline LispObject op(number_dispatcher::B t1, uint64_t *a)
    {   size_t lena = arithlib::number_size(a);
        arithlib::push(a);
        uint64_t *r = arithlib::reserve(lena);
        arithlib::pop(a);
        size_t lenr;
        arithlib::uniform_upto(a, lena, r, lenr);
        return arithlib::confirm_size(r, lena, lenr);
    }
    static inline LispObject op(number_dispatcher::D t1, double a)
    {   uint64_t i = arithlib::mersenne_twister() & 0x1ffffffffffffU;
        double d = (double)i;
        double r = d/(double)0x1ffffffffffffU;
        return boxfloat(a*r);
    }
};

static LispObject Nrandom(LispObject a)
{   return number_dispatcher::unary<LispObject,Randomer>("random", a);
}

LispObject Lrandom(LispObject lits, LispObject x)
{   return Nrandom(x);
}

// ====== make-random-state ======

class MakeRandomStater
{
public:
    static inline LispObject op(number_dispatcher::I t1, int64_t a)
    {   if (a == 0)
        {   std::seed_seq random_seed{
                (uint32_t)arithlib::threadid,
                (uint32_t)arithlib::basic_randomness(),
                (uint32_t)arithlib::basic_randomness(),
                (uint32_t)arithlib::basic_randomness(),
                (uint32_t)std::time(NULL),
                (uint32_t)
        std::chrono::high_resolution_clock::now().time_since_epoch().count()};
            arithlib::mersenne_twister.seed(random_seed);
        }
        else arithlib::reseed(a);
        return nil;
    }
    static inline LispObject op(number_dispatcher::B t1, uint64_t *a)
    {
// I ought to be able to use all the digits of the bignum as a seed_sequence
// here, but for now I will just use the low bits.
        arithlib::reseed(a[0]);
        return nil;
    }
};

static LispObject Nmake_random_state(LispObject a)
{   return number_dispatcher::iunary<LispObject,MakeRandomStater>("make-random-state", a);
}

LispObject Lmake_random_state(LispObject lits, LispObject x)
{   return Nmake_random_state(x);
}

LispObject Lminusp(LispObject lits, LispObject x)
{
// Anything non-numeric will not be negative!
    if ((isFIXNUM(x) && qfixnum(x) < 0) ||
        (isFLOAT(x) && qfloat(x) < 0.0) ||
        (isBIGNUM(x) &&
         arithlib::Minusp::op(arithlib::vector_of_handle(x)))) return lisptrue;
    else return nil;
}

LispObject Levenp(LispObject lits, LispObject x)
{   return Bevenp(x) ? lisptrue : nil;
}

LispObject Loddp(LispObject lits, LispObject x)
{   return Boddp(x) ? lisptrue : nil;
}

LispObject Lmsd(LispObject lits, LispObject x)
{   return Nmsd(x);
}

LispObject Llsd(LispObject lits, LispObject x)
{   return Nlsd(x);
}

LispObject Lbitcount(LispObject lits, LispObject x)
{   return Nbitcount(x);
}

LispObject Llognot(LispObject lits, LispObject x)
{   return Nlognot(x);
}

LispObject Ladd1(LispObject lits, LispObject x)
{   return Nadd1(x);
}

LispObject Lsub1(LispObject lits, LispObject x)
{   return Nsub1(x);
}

LispObject Ldifference(LispObject lits, LispObject x, LispObject y)
{   return Ndifference2(x, y);
}

LispObject Lquotient(LispObject lits, LispObject x, LispObject y)
{   if (y == packfixnum(0) ||
        (isFLOAT(y) && qfloat(y) == 0.0)) return error0("division by zero");
    return Nquotient2(x, y);
}

LispObject Lremainder(LispObject lits, LispObject x, LispObject y)
{   if (y == packfixnum(0) ||
        (isFLOAT(y) && qfloat(y) == 0.0)) return error0("division by zero");
    return Nremainder2(x, y);
}

LispObject Ldivide(LispObject lits, LispObject x, LispObject y)
{   if (y == packfixnum(0) ||
        (isFLOAT(y) && qfloat(y) == 0.0)) return error0("division by zero");
    return Ndivide2(x, y);
}

LispObject Lgcdn(LispObject lits, LispObject x, LispObject y)
{   if (x == packfixnum(0)) return Labs_1(lits, y);
    else if (y == packfixnum(0)) return Labs_1(lits, x);
    return Ngcdn(x, y);
}

LispObject Llcmn(LispObject lits, LispObject x, LispObject y)
{   return Nlcmn(x, y);
}

LispObject Lleftshift(LispObject lits, LispObject x, LispObject y)
{   return Nleftshift(x, y);
}

LispObject Lrightshift(LispObject lits, LispObject x, LispObject y)
{   return Nrightshift(x, y);
}

LispObject Lexpt(LispObject lits, LispObject x, LispObject y)
{   return Nexpt(x, y);
}

LispObject Lgreaterp(LispObject lits, LispObject x, LispObject y)
{   return Bgreaterp2(x,y) ? lisptrue : nil;
}

LispObject Lgeq(LispObject lits, LispObject x, LispObject y)
{   return Bgeq2(x,y) ? lisptrue : nil;
}

LispObject Llessp(LispObject lits, LispObject x, LispObject y)
{   return Blessp2(x,y) ? lisptrue : nil;
}

LispObject Lleq(LispObject lits, LispObject x, LispObject y)
{   return Bleq2(x,y) ? lisptrue : nil;
}

LispObject Lmax_1(LispObject lits, LispObject a)
{   return a;
}

LispObject Lmax_2(LispObject lits, LispObject a1, LispObject a2)
{   if (Bgreaterp2(a2, a1)) a1 = a2;
    return a1;
}

LispObject Lmax_3(LispObject lits, LispObject a1, LispObject a2, LispObject a3)
{   if (Bgreaterp2(a2, a1)) a1 = a2;
    if (Bgreaterp2(a3, a1)) a1 = a3;
    return a1;
}

LispObject Lmax_4(LispObject lits, LispObject a1, LispObject a2,
                  LispObject a3, LispObject a4)
{   if (Bgreaterp2(a2, a1)) a1 = a2;
    if (Bgreaterp2(a3, a1)) a1 = a3;
    if (Bgreaterp2(a4, a1)) a1 = a4;
    return a1;
}

LispObject Lmax_5up(LispObject lits, LispObject a1, LispObject a2,
                    LispObject a3, LispObject a4, LispObject a5up)
{   if (Bgreaterp2(a2, a1)) a1 = a2;
    if (Bgreaterp2(a3, a1)) a1 = a3;
    if (Bgreaterp2(a4, a1)) a1 = a4;
    while (isCONS(a5up))
    {   LispObject w = qcar(a5up);
        a5up = qcdr(a5up);
        if (Bgreaterp2(w, a1)) a1 = w;
    }
    return a1;
}

LispObject Lmin_1(LispObject lits, LispObject a)
{   return a;
}

LispObject Lmin_2(LispObject lits, LispObject a1, LispObject a2)
{   if (Blessp2(a2, a1)) a1 = a2;
    return a1;
}

LispObject Lmin_3(LispObject lits, LispObject a1, LispObject a2, LispObject a3)
{   if (Blessp2(a2, a1)) a1 = a2;
    if (Blessp2(a3, a1)) a1 = a3;
    return a1;
}

LispObject Lmin_4(LispObject lits, LispObject a1, LispObject a2,
                  LispObject a3, LispObject a4)
{   if (Blessp2(a2, a1)) a1 = a2;
    if (Blessp2(a3, a1)) a1 = a3;
    if (Blessp2(a4, a1)) a1 = a4;
    return a1;
}

LispObject Lmin_5up(LispObject lits, LispObject a1, LispObject a2,
                    LispObject a3, LispObject a4, LispObject a5up)
{   if (Blessp2(a2, a1)) a1 = a2;
    if (Blessp2(a3, a1)) a1 = a3;
    if (Blessp2(a4, a1)) a1 = a4;
    while (isCONS(a5up))
    {   LispObject w = qcar(a5up);
        a5up = qcdr(a5up);
        if (Blessp2(w, a1)) a1 = w;
    }
    return a1;
}

LispObject Lplus_0(LispObject data)
{
    return packfixnum(0);
}

LispObject Lplus_1(LispObject data, LispObject a1)
{
    return a1;
}

LispObject Lplus_2(LispObject data, LispObject a1, LispObject a2)
{
    return Nplus2(a1, a2);
}

LispObject Lplus_3(LispObject data, LispObject a1,
                   LispObject a2, LispObject a3)
{
    LispObject r = Nplus2(a1, a2);
    if (unwindflag != unwindNONE) return nil;
    return Nplus2(r, a3);
}

LispObject Lplus_4(LispObject data, LispObject a1, LispObject a2,
                   LispObject a3, LispObject a4)
{
    LispObject r = Nplus2(a1, a2);
    if (unwindflag != unwindNONE) return nil;
    r = Nplus2(r, a3);
    if (unwindflag != unwindNONE) return nil;
    return Nplus2(r, a4);
}

LispObject Lplus_5up(LispObject data, LispObject a1, LispObject a2,
                      LispObject a3, LispObject a4, LispObject a5up)
{
    LispObject r = Nplus2(a1, a2);
    if (unwindflag != unwindNONE) return nil;
    r = Nplus2(r, a3);
    if (unwindflag != unwindNONE) return nil;
    r = Nplus2(r, a4);
    if (unwindflag != unwindNONE) return nil;
    while (isCONS(a5up))
    {   r = Nplus2(r, qcar(a5up));
        if (unwindflag != unwindNONE) return nil;
        a5up = qcdr(a5up);
    }
    return r;
}

LispObject Ltimes_0(LispObject data)
{
    return packfixnum(1);
}

LispObject Ltimes_1(LispObject data, LispObject a1)
{
    return a1;
}

LispObject Ltimes_2(LispObject data, LispObject a1, LispObject a2)
{
    return Ntimes2(a1, a2);
}

LispObject Ltimes_3(LispObject data, LispObject a1,
                   LispObject a2, LispObject a3)
{
    LispObject r = Ntimes2(a1, a2);
    if (unwindflag != unwindNONE) return nil;
    return Ntimes2(r, a3);
}

LispObject Ltimes_4(LispObject data, LispObject a1, LispObject a2,
                   LispObject a3, LispObject a4)
{
    LispObject r = Ntimes2(a1, a2);
    if (unwindflag != unwindNONE) return nil;
    r = Ntimes2(r, a3);
    if (unwindflag != unwindNONE) return nil;
    return Ntimes2(r, a4);
}

LispObject Ltimes_5up(LispObject data, LispObject a1, LispObject a2,
                      LispObject a3, LispObject a4, LispObject a5up)
{
    LispObject r = Ntimes2(a1, a2);
    if (unwindflag != unwindNONE) return nil;
    r = Ntimes2(r, a3);
    if (unwindflag != unwindNONE) return nil;
    r = Ntimes2(r, a4);
    if (unwindflag != unwindNONE) return nil;
    while (isCONS(a5up))
    {   r = Ntimes2(r, qcar(a5up));
        if (unwindflag != unwindNONE) return nil;
        a5up = qcdr(a5up);
    }
    return r;
}

LispObject Llogand_0(LispObject data)
{
    return packfixnum(-1);
}

LispObject Llogand_1(LispObject data, LispObject a1)
{
    return a1;
}

LispObject Llogand_2(LispObject data, LispObject a1, LispObject a2)
{
    return Nlogand2(a1, a2);
}

LispObject Llogand_3(LispObject data, LispObject a1,
                     LispObject a2, LispObject a3)
{
    LispObject r = Nlogand2(a1, a2);
    if (unwindflag != unwindNONE) return nil;
    return Nlogand2(r, a3);
}

LispObject Llogand_4(LispObject data, LispObject a1, LispObject a2,
                   LispObject a3, LispObject a4)
{
    LispObject r = Nlogand2(a1, a2);
    if (unwindflag != unwindNONE) return nil;
    r = Nlogand2(r, a3);
    if (unwindflag != unwindNONE) return nil;
    return Nlogand2(r, a4);
}

LispObject Llogand_5up(LispObject data, LispObject a1, LispObject a2,
                      LispObject a3, LispObject a4, LispObject a5up)
{
    LispObject r = Nlogand2(a1, a2);
    if (unwindflag != unwindNONE) return nil;
    r = Nlogand2(r, a3);
    if (unwindflag != unwindNONE) return nil;
    r = Nlogand2(r, a4);
    if (unwindflag != unwindNONE) return nil;
    while (isCONS(a5up))
    {   r = Nlogand2(r, qcar(a5up));
        if (unwindflag != unwindNONE) return nil;
        a5up = qcdr(a5up);
    }
    return r;
}

LispObject Llogor_0(LispObject data)
{
    return packfixnum(0);
}

LispObject Llogor_1(LispObject data, LispObject a1)
{
    return a1;
}

LispObject Llogor_2(LispObject data, LispObject a1, LispObject a2)
{
    return Nlogor2(a1, a2);
}

LispObject Llogor_3(LispObject data, LispObject a1,
                   LispObject a2, LispObject a3)
{
    LispObject r = Nlogor2(a1, a2);
    if (unwindflag != unwindNONE) return nil;
    return Nlogor2(r, a3);
}

LispObject Llogor_4(LispObject data, LispObject a1, LispObject a2,
                   LispObject a3, LispObject a4)
{
    LispObject r = Nlogor2(a1, a2);
    if (unwindflag != unwindNONE) return nil;
    r = Nlogor2(r, a3);
    if (unwindflag != unwindNONE) return nil;
    return Nlogor2(r, a4);
}

LispObject Llogor_5up(LispObject data, LispObject a1, LispObject a2,
                      LispObject a3, LispObject a4, LispObject a5up)
{
    LispObject r = Nlogor2(a1, a2);
    if (unwindflag != unwindNONE) return nil;
    r = Nlogor2(r, a3);
    if (unwindflag != unwindNONE) return nil;
    r = Nlogor2(r, a4);
    if (unwindflag != unwindNONE) return nil;
    while (isCONS(a5up))
    {   r = Nlogor2(r, qcar(a5up));
        if (unwindflag != unwindNONE) return nil;
        a5up = qcdr(a5up);
    }
    return r;
}

LispObject Llogxor_0(LispObject data)
{
    return packfixnum(0);
}

LispObject Llogxor_1(LispObject data, LispObject a1)
{
    return a1;
}

LispObject Llogxor_2(LispObject data, LispObject a1, LispObject a2)
{
    return Nlogxor2(a1, a2);
}

LispObject Llogxor_3(LispObject data, LispObject a1,
                   LispObject a2, LispObject a3)
{
    LispObject r = Nlogxor2(a1, a2);
    if (unwindflag != unwindNONE) return nil;
    return Nlogxor2(r, a3);
}

LispObject Llogxor_4(LispObject data, LispObject a1, LispObject a2,
                   LispObject a3, LispObject a4)
{
    LispObject r = Nlogxor2(a1, a2);
    if (unwindflag != unwindNONE) return nil;
    r = Nlogxor2(r, a3);
    if (unwindflag != unwindNONE) return nil;
    return Nlogxor2(r, a4);
}

LispObject Llogxor_5up(LispObject data, LispObject a1, LispObject a2,
                      LispObject a3, LispObject a4, LispObject a5up)
{
    LispObject r = Nlogxor2(a1, a2);
    if (unwindflag != unwindNONE) return nil;
    r = Nlogxor2(r, a3);
    if (unwindflag != unwindNONE) return nil;
    r = Nlogxor2(r, a4);
    if (unwindflag != unwindNONE) return nil;
    while (isCONS(a5up))
    {   r = Nlogxor2(r, qcar(a5up));
        if (unwindflag != unwindNONE) return nil;
        a5up = qcdr(a5up);
    }
    return r;
}

