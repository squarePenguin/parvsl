
LispObject Lcar(LispObject lits, LispObject x)
{   if (isCONS(x)) return qcar(x);
    else return error1("car of an atom", x);
}

LispObject Lcdr(LispObject lits, LispObject x)
{   if (isCONS(x)) return qcdr(x);
    else return error1("cdr of an atom", x);
}

LispObject Lcaar(LispObject lits, LispObject x)
{   if (isCONS(x)) x = qcar(x);
    else return error1("car of an atom", x);
    if (isCONS(x)) return qcar(x);
    else return error1("car of an atom", x);
}

LispObject Lcdar(LispObject lits, LispObject x)
{   if (isCONS(x)) x = qcar(x);
    else return error1("car of an atom", x);
    if (isCONS(x)) return qcdr(x);
    else return error1("cdr of an atom", x);
}

LispObject Lcadr(LispObject lits, LispObject x)
{   if (isCONS(x)) x = qcdr(x);
    else return error1("cdr of an atom", x);
    if (isCONS(x)) return qcar(x);
    else return error1("car of an atom", x);
}

LispObject Lcddr(LispObject lits, LispObject x)
{   if (isCONS(x)) x = qcdr(x);
    else return error1("cdr of an atom", x);
    if (isCONS(x)) return qcdr(x);
    else return error1("cdr of an atom", x);
}

LispObject Lcaaar(LispObject lits, LispObject x)
{   if (isCONS(x)) x = qcar(x);
    else return error1("car of an atom", x);
    if (isCONS(x)) x = qcar(x);
    else return error1("car of an atom", x);
    if (isCONS(x)) return qcar(x);
    else return error1("car of an atom", x);
}

LispObject Lcdaar(LispObject lits, LispObject x)
{   if (isCONS(x)) x = qcar(x);
    else return error1("car of an atom", x);
    if (isCONS(x)) x = qcar(x);
    else return error1("car of an atom", x);
    if (isCONS(x)) return qcdr(x);
    else return error1("cdr of an atom", x);
}

LispObject Lcadar(LispObject lits, LispObject x)
{   if (isCONS(x)) x = qcar(x);
    else return error1("car of an atom", x);
    if (isCONS(x)) x = qcdr(x);
    else return error1("cdr of an atom", x);
    if (isCONS(x)) return qcar(x);
    else return error1("car of an atom", x);
}

LispObject Lcddar(LispObject lits, LispObject x)
{   if (isCONS(x)) x = qcar(x);
    else return error1("car of an atom", x);
    if (isCONS(x)) x = qcdr(x);
    else return error1("cdr of an atom", x);
    if (isCONS(x)) return qcdr(x);
    else return error1("cdr of an atom", x);
}

LispObject Lcaadr(LispObject lits, LispObject x)
{   if (isCONS(x)) x = qcdr(x);
    else return error1("cdr of an atom", x);
    if (isCONS(x)) x = qcar(x);
    else return error1("car of an atom", x);
    if (isCONS(x)) return qcar(x);
    else return error1("car of an atom", x);
}

LispObject Lcdadr(LispObject lits, LispObject x)
{   if (isCONS(x)) x = qcdr(x);
    else return error1("cdr of an atom", x);
    if (isCONS(x)) x = qcar(x);
    else return error1("car of an atom", x);
    if (isCONS(x)) return qcdr(x);
    else return error1("cdr of an atom", x);
}

LispObject Lcaddr(LispObject lits, LispObject x)
{   if (isCONS(x)) x = qcdr(x);
    else return error1("cdr of an atom", x);
    if (isCONS(x)) x = qcdr(x);
    else return error1("cdr of an atom", x);
    if (isCONS(x)) return qcar(x);
    else return error1("car of an atom", x);
}

LispObject Lcdddr(LispObject lits, LispObject x)
{   if (isCONS(x)) x = qcdr(x);
    else return error1("cdr of an atom", x);
    if (isCONS(x)) x = qcdr(x);
    else return error1("cdr of an atom", x);
    if (isCONS(x)) return qcdr(x);
    else return error1("cdr of an atom", x);
}

LispObject Lcaaaar(LispObject lits, LispObject x)
{   if (isCONS(x)) x = qcar(x);
    else return error1("car of an atom", x);
    if (isCONS(x)) x = qcar(x);
    else return error1("car of an atom", x);
    if (isCONS(x)) x = qcar(x);
    else return error1("car of an atom", x);
    if (isCONS(x)) return qcar(x);
    else return error1("car of an atom", x);
}

LispObject Lcdaaar(LispObject lits, LispObject x)
{   if (isCONS(x)) x = qcar(x);
    else return error1("car of an atom", x);
    if (isCONS(x)) x = qcar(x);
    else return error1("car of an atom", x);
    if (isCONS(x)) x = qcar(x);
    else return error1("car of an atom", x);
    if (isCONS(x)) return qcdr(x);
    else return error1("cdr of an atom", x);
}

LispObject Lcadaar(LispObject lits, LispObject x)
{   if (isCONS(x)) x = qcar(x);
    else return error1("car of an atom", x);
    if (isCONS(x)) x = qcar(x);
    else return error1("car of an atom", x);
    if (isCONS(x)) x = qcdr(x);
    else return error1("cdr of an atom", x);
    if (isCONS(x)) return qcar(x);
    else return error1("car of an atom", x);
}

LispObject Lcddaar(LispObject lits, LispObject x)
{   if (isCONS(x)) x = qcar(x);
    else return error1("car of an atom", x);
    if (isCONS(x)) x = qcar(x);
    else return error1("car of an atom", x);
    if (isCONS(x)) x = qcdr(x);
    else return error1("cdr of an atom", x);
    if (isCONS(x)) return qcdr(x);
    else return error1("cdr of an atom", x);
}

LispObject Lcaadar(LispObject lits, LispObject x)
{   if (isCONS(x)) x = qcar(x);
    else return error1("car of an atom", x);
    if (isCONS(x)) x = qcdr(x);
    else return error1("cdr of an atom", x);
    if (isCONS(x)) x = qcar(x);
    else return error1("car of an atom", x);
    if (isCONS(x)) return qcar(x);
    else return error1("car of an atom", x);
}

LispObject Lcdadar(LispObject lits, LispObject x)
{   if (isCONS(x)) x = qcar(x);
    else return error1("car of an atom", x);
    if (isCONS(x)) x = qcdr(x);
    else return error1("cdr of an atom", x);
    if (isCONS(x)) x = qcar(x);
    else return error1("car of an atom", x);
    if (isCONS(x)) return qcdr(x);
    else return error1("cdr of an atom", x);
}

LispObject Lcaddar(LispObject lits, LispObject x)
{   if (isCONS(x)) x = qcar(x);
    else return error1("car of an atom", x);
    if (isCONS(x)) x = qcdr(x);
    else return error1("cdr of an atom", x);
    if (isCONS(x)) x = qcdr(x);
    else return error1("cdr of an atom", x);
    if (isCONS(x)) return qcar(x);
    else return error1("car of an atom", x);
}

LispObject Lcdddar(LispObject lits, LispObject x)
{   if (isCONS(x)) x = qcar(x);
    else return error1("car of an atom", x);
    if (isCONS(x)) x = qcdr(x);
    else return error1("cdr of an atom", x);
    if (isCONS(x)) x = qcdr(x);
    else return error1("cdr of an atom", x);
    if (isCONS(x)) return qcdr(x);
    else return error1("cdr of an atom", x);
}

LispObject Lcaaadr(LispObject lits, LispObject x)
{   if (isCONS(x)) x = qcdr(x);
    else return error1("cdr of an atom", x);
    if (isCONS(x)) x = qcar(x);
    else return error1("car of an atom", x);
    if (isCONS(x)) x = qcar(x);
    else return error1("car of an atom", x);
    if (isCONS(x)) return qcar(x);
    else return error1("car of an atom", x);
}

LispObject Lcdaadr(LispObject lits, LispObject x)
{   if (isCONS(x)) x = qcdr(x);
    else return error1("cdr of an atom", x);
    if (isCONS(x)) x = qcar(x);
    else return error1("car of an atom", x);
    if (isCONS(x)) x = qcar(x);
    else return error1("car of an atom", x);
    if (isCONS(x)) return qcdr(x);
    else return error1("cdr of an atom", x);
}

LispObject Lcadadr(LispObject lits, LispObject x)
{   if (isCONS(x)) x = qcdr(x);
    else return error1("cdr of an atom", x);
    if (isCONS(x)) x = qcar(x);
    else return error1("car of an atom", x);
    if (isCONS(x)) x = qcdr(x);
    else return error1("cdr of an atom", x);
    if (isCONS(x)) return qcar(x);
    else return error1("car of an atom", x);
}

LispObject Lcddadr(LispObject lits, LispObject x)
{   if (isCONS(x)) x = qcdr(x);
    else return error1("cdr of an atom", x);
    if (isCONS(x)) x = qcar(x);
    else return error1("car of an atom", x);
    if (isCONS(x)) x = qcdr(x);
    else return error1("cdr of an atom", x);
    if (isCONS(x)) return qcdr(x);
    else return error1("cdr of an atom", x);
}

LispObject Lcaaddr(LispObject lits, LispObject x)
{   if (isCONS(x)) x = qcdr(x);
    else return error1("cdr of an atom", x);
    if (isCONS(x)) x = qcdr(x);
    else return error1("cdr of an atom", x);
    if (isCONS(x)) x = qcar(x);
    else return error1("car of an atom", x);
    if (isCONS(x)) return qcar(x);
    else return error1("car of an atom", x);
}

LispObject Lcdaddr(LispObject lits, LispObject x)
{   if (isCONS(x)) x = qcdr(x);
    else return error1("cdr of an atom", x);
    if (isCONS(x)) x = qcdr(x);
    else return error1("cdr of an atom", x);
    if (isCONS(x)) x = qcar(x);
    else return error1("car of an atom", x);
    if (isCONS(x)) return qcdr(x);
    else return error1("cdr of an atom", x);
}

LispObject Lcadddr(LispObject lits, LispObject x)
{   if (isCONS(x)) x = qcdr(x);
    else return error1("cdr of an atom", x);
    if (isCONS(x)) x = qcdr(x);
    else return error1("cdr of an atom", x);
    if (isCONS(x)) x = qcdr(x);
    else return error1("cdr of an atom", x);
    if (isCONS(x)) return qcar(x);
    else return error1("car of an atom", x);
}

LispObject Lcddddr(LispObject lits, LispObject x)
{   if (isCONS(x)) x = qcdr(x);
    else return error1("cdr of an atom", x);
    if (isCONS(x)) x = qcdr(x);
    else return error1("cdr of an atom", x);
    if (isCONS(x)) x = qcdr(x);
    else return error1("cdr of an atom", x);
    if (isCONS(x)) return qcdr(x);
    else return error1("cdr of an atom", x);
}

LispObject Lrplaca(LispObject lits, LispObject x, LispObject y)
{
    if (isCONS(x))
    {   qcar(x) = y;
        return x;
    }
    else return error1("rplaca on an atom", x);
}

LispObject Lrplacd(LispObject lits, LispObject x, LispObject y)
{
    if (isCONS(x))
    {   qcdr(x) = y;
        return x;
    }
    else return error1("rplaca on an atom", x);
}

LispObject Lreclaim_1(LispObject lits, LispObject x)
{
    reclaim(__LINE__);
    return nil;
}

LispObject Lreclaim_0(LispObject lits)
{
    reclaim(__LINE__);
    return nil;
}

LispObject Lcons(LispObject lits, LispObject x, LispObject y)
{
    return cons(x, y);
}

LispObject Latom(LispObject lits, LispObject x)
{
    return (isCONS(x) ? nil : lisptrue);
}

LispObject Lpairp(LispObject lits, LispObject x)
{
    return (isCONS(x) ? lisptrue : nil);
}

LispObject Lsymbolp(LispObject lits, LispObject x)
{
    return (isSYMBOL(x) ? lisptrue : nil);
}

LispObject Lstringp(LispObject lits, LispObject x)
{
    return (isSTRING(x) ? lisptrue : nil);
}

LispObject Lvectorp(LispObject lits, LispObject x)
{
    return (isVEC(x) ? lisptrue : nil);
}

LispObject Lprog1_2(LispObject lits, LispObject x, LispObject y)
{   return x;
}

LispObject Lprog2_2(LispObject lits, LispObject x, LispObject y)
{   return y;
}

LispObject Lprog1_3(LispObject lits, LispObject x, LispObject y, LispObject z)
{   return x;
}

LispObject Lprog2_3(LispObject lits, LispObject x, LispObject y, LispObject z)
{   return y;
}

LispObject Lprog1_4(LispObject lits, LispObject x, LispObject y,
                    LispObject z, LispObject a)
{   return x;
}

LispObject Lprog2_4(LispObject lits, LispObject x, LispObject y,
                    LispObject z, LispObject a)
{   return y;
}

LispObject Lprog1_5up(LispObject lits, LispObject x, LispObject y,
                      LispObject z, LispObject a, LispObject b)
{   return x;
}

LispObject Lprog2_5up(LispObject lits, LispObject x, LispObject y,
                      LispObject z, LispObject a, LispObject b)
{   return y;
}

LispObject Lreverse(LispObject lits, LispObject a)
{   LispObject r = nil;
    while (isCONS(a))
    {   r = cons(qcar(a), r);
        a = qcdr(a);
    }
    return r;
}

LispObject Lreversip(LispObject lits, LispObject a)
{   LispObject r = nil;
    while (isCONS(a))
    {   LispObject w = a;
        a = qcdr(a);
        qcdr(w) = r;
        r = w;
    }
    return r;
}

LispObject Lreversip_2(LispObject lits, LispObject a, LispObject r)
{   while (isCONS(a))
    {   LispObject w = a;
        a = qcdr(a);
        qcdr(w) = r;
        r = w;
    }
    return r;
}

LispObject Lappend_0(LispObject lits)
{   return nil;
}

LispObject Lappend_1(LispObject lits, LispObject a1)
{   return a1;
}

LispObject append(LispObject a, LispObject b)
{   LispObject r = nil;
    while (isCONS(a))
    {   r = cons(qcar(a), r);
        a = qcdr(a);
    }
    while (isCONS(r))
    {   LispObject w = r;
        r = qcdr(r);
        qcdr(w) = b;
        b = w;
    }
    return b;
}

LispObject Lappend_2(LispObject lits, LispObject a1, LispObject a2)
{   return append(a1, a2);
}

LispObject Lappend_3(LispObject lits, LispObject a1, LispObject a2,
                     LispObject a3)
{   return append(a1, append(a2, a3));
}

LispObject Lappend_4(LispObject lits, LispObject a1, LispObject a2,
                     LispObject a3, LispObject a4)
{   return append(a1, append(a2, append(a3, a4)));
}

LispObject Lappend_5up(LispObject lits, LispObject a1, LispObject a2,
                       LispObject a3, LispObject a4, LispObject a5up)
{   LispObject r=nil;
    if (isCONS(a5up))
    {   while (isCONS(a5up))
        {   r = cons(qcar(a5up), r);
            a5up = qcdr(a5up);
        }
        a5up = qcdr(r);
        r = qcar(r);
        while (isCONS(a5up))
        {   r = append(qcar(a5up), r);
            a5up = qcdr(a5up);
        }
    }
    return append(a1, append(a2, append(a3, append(a4, r))));
}

LispObject Lnumberp(LispObject lits, LispObject x)
{
    return (isFIXNUM(x) || isBIGNUM(x) || isFLOAT(x) ? lisptrue : nil);
}

LispObject Lfixp(LispObject lits, LispObject x)
{
    return (isFIXNUM(x) || isBIGNUM(x) ? lisptrue : nil);
}

LispObject Lfloatp(LispObject lits, LispObject x)
{
    return (isFLOAT(x) ? lisptrue : nil);
}

LispObject Lfrexp(LispObject lits, LispObject a)
{   double d = 0.0;
    int x = 0;
    if (isFLOAT(a)) d = std::frexp(qfloat(a), &x);
    return cons(packfixnum(x), boxfloat(d));
}

LispObject Lfix(LispObject lits, LispObject x)
{
    return (isFIXNUM(x) || isBIGNUM(x) ? x :
            isFLOAT(x) ? arithlib::double_to_bignum(qfloat(x)) :
            error1("arg for fix", x));
}

LispObject Lfloor(LispObject lits, LispObject x)
{
    return (isFIXNUM(x) || isBIGNUM(x) ? x :
            isFLOAT(x) ? arithlib::double_to_floor(qfloat(x)) :
            error1("arg for floor", x));
}

LispObject Lceiling(LispObject lits, LispObject x)
{
    return (isFIXNUM(x) || isBIGNUM(x) ? x :
            isFLOAT(x) ? arithlib::double_to_ceiling(qfloat(x)) :
            error1("arg for ceiling", x));
}

LispObject Lfloat(LispObject lits, LispObject x)
{
    return (isFLOAT(x) ? x :
            isFIXNUM(x) ? boxfloat(arithlib::Double::op(qfixnum(x))) :
            isBIGNUM(x) ? boxfloat(arithlib::Double::op(arithlib::vector_of_handle(x))) :
            error1("arg for float", x));
}

INLINE double floatval(LispObject x)
{   return isFLOAT(x) ? qfloat(x) :
           isFIXNUM(x) ? arithlib::Double::op(qfixnum(x)) :
           isBIGNUM(x) ? arithlib::Double::op(arithlib::vector_of_handle(x)) :
           0.0;
}

LispObject Lfp_subnorm(LispObject lits, LispObject arg)
{   if (!isFLOAT(arg)) return nil;
    double d = qfloat(arg);
    return (std::isfinite(d) && !std::isnormal(d)) ? lisptrue : nil;
}

LispObject Lfp_infinite(LispObject lits, LispObject arg)
{   if (!isFLOAT(arg)) return nil;
    double d = qfloat(arg);
    return (std::isinf(d)) ? lisptrue : nil;
}

LispObject Lfp_nan(LispObject lits, LispObject arg)
{   if (!isFLOAT(arg)) return nil;
    double d = qfloat(arg);
    return (std::isnan(d)) ? lisptrue : nil;
}

LispObject Lfp_finite(LispObject lits, LispObject arg)
{   if (!isFLOAT(arg)) return nil;
    double d = qfloat(arg);
    return (std::isfinite(d)) ? lisptrue : nil;
}

LispObject Lfp_signbit(LispObject lits, LispObject arg)
{   if (!isFLOAT(arg)) return nil;
    double d = qfloat(arg);
    return std::signbit(d) ? lisptrue : nil;
}

LispObject Lsqrt(LispObject lits, LispObject x)
{   return boxfloat(SQRT(floatval(x)));
}

LispObject Lexp(LispObject lits, LispObject x)
{   return boxfloat(EXP(floatval(x)));
}

LispObject Llog(LispObject lits, LispObject x)
{   double d = floatval(x);
    if (d < 0.0) return error1("argument out of range", x);
    return boxfloat(LOG(d));
}

LispObject Llog2(LispObject lits, LispObject x)
{   double d = floatval(x);
    if (d < 0.0) return error1("argument out of range", x);
    return boxfloat(LOG2(d));
}

LispObject Llog10(LispObject lits, LispObject x)
{   double d = floatval(x);
    if (d < 0.0) return error1("argument out of range", x);
    return boxfloat(LOG10(d));
}

LispObject Lsin(LispObject lits, LispObject x)
{   return boxfloat(SIN(floatval(x)));
}

LispObject Lcos(LispObject lits, LispObject x)
{   return boxfloat(COS(floatval(x)));
}

LispObject Ltan(LispObject lits, LispObject x)
{   return boxfloat(TAN(floatval(x)));
}

LispObject Lsec(LispObject lits, LispObject x)
{   return boxfloat(SEC(floatval(x)));
}

LispObject Lcsc(LispObject lits, LispObject x)
{   return boxfloat(CSC(floatval(x)));
}

LispObject Lcot(LispObject lits, LispObject x)
{   return boxfloat(COT(floatval(x)));
}

LispObject Lsind(LispObject lits, LispObject x)
{   return boxfloat(SIND(floatval(x)));
}

LispObject Lcosd(LispObject lits, LispObject x)
{   return boxfloat(COSD(floatval(x)));
}

LispObject Ltand(LispObject lits, LispObject x)
{   return boxfloat(TAND(floatval(x)));
}

LispObject Lsecd(LispObject lits, LispObject x)
{   return boxfloat(SECD(floatval(x)));
}

LispObject Lcscd(LispObject lits, LispObject x)
{   return boxfloat(CSCD(floatval(x)));
}

LispObject Lcotd(LispObject lits, LispObject x)
{   return boxfloat(COTD(floatval(x)));
}

LispObject Lsinh(LispObject lits, LispObject x)
{   return boxfloat(SINH(floatval(x)));
}

LispObject Lcosh(LispObject lits, LispObject x)
{   return boxfloat(COSH(floatval(x)));
}

LispObject Ltanh(LispObject lits, LispObject x)
{   return boxfloat(TANH(floatval(x)));
}

LispObject Lsech(LispObject lits, LispObject x)
{   return boxfloat(SECH(floatval(x)));
}

LispObject Lcsch(LispObject lits, LispObject x)
{   return boxfloat(CSCH(floatval(x)));
}

LispObject Lcoth(LispObject lits, LispObject x)
{   return boxfloat(COTH(floatval(x)));
}

LispObject Lasin(LispObject lits, LispObject x)
{   double d = floatval(x);
    if (d < -1.0 || d > 1.0) return error1("argument out of range", x);
    return boxfloat(ASIN(d));
}

LispObject Lacos(LispObject lits, LispObject x)
{   double d = floatval(x);
    if (d < -1.0 || d > 1.0) return error1("argument out of range", x);
    return boxfloat(ACOS(d));
}

LispObject Latan(LispObject lits, LispObject x)
{   double d = floatval(x);
    return boxfloat(ATAN(d));
}

LispObject Lasec(LispObject lits, LispObject x)
{   double d = floatval(x);
    if (d > -1.0 && d < 1.0) return error1("argument out of range", x);
    return boxfloat(ASEC(d));
}

LispObject Lacsc(LispObject lits, LispObject x)
{   double d = floatval(x);
    if (d > -1.0 && d < 1.0) return error1("argument out of range", x);
    return boxfloat(ACSC(d));
}

LispObject Lacot(LispObject lits, LispObject x)
{   double d = floatval(x);
    return boxfloat(ACOT(d));
}

LispObject Lasind(LispObject lits, LispObject x)
{   double d = floatval(x);
    if (d < -1.0 || d > 1.0) return error1("argument out of range", x);
    return boxfloat(ASIND(d));
}

LispObject Lacosd(LispObject lits, LispObject x)
{   double d = floatval(x);
    if (d < -1.0 || d > 1.0) return error1("argument out of range", x);
    return boxfloat(ACOSD(d));
}

LispObject Latand(LispObject lits, LispObject x)
{   double d = floatval(x);
    return boxfloat(ATAND(d));
}

LispObject Lasecd(LispObject lits, LispObject x)
{   double d = floatval(x);
    if (d > -1.0 && d < 1.0) return error1("argument out of range", x);
    return boxfloat(ASECD(d));
}

LispObject Lacscd(LispObject lits, LispObject x)
{   double d = floatval(x);
    if (d > -1.0 && d < 1.0) return error1("argument out of range", x);
    return boxfloat(ACSCD(d));
}

LispObject Lacotd(LispObject lits, LispObject x)
{   double d = floatval(x);
    return boxfloat(ACOTD(d));
}

LispObject Lasinh(LispObject lits, LispObject x)
{   double d = floatval(x);
    return boxfloat(ASINH(d));
}

LispObject Lacosh(LispObject lits, LispObject x)
{   double d = floatval(x);
    if (d < 1.0) return error1("argument out of range", x);
    return boxfloat(ACOSH(d));
}

LispObject Latanh(LispObject lits, LispObject x)
{   double d = floatval(x);
    if (d < -1.0 || d > 1.0) return error1("argument out of range", x);
    return boxfloat(ATANH(d));
}

LispObject Lasech(LispObject lits, LispObject x)
{   double d = floatval(x);
    if (d < 0.0 || d > 1.0) return error1("argument out of range", x);
    return boxfloat(ASECH(d));
}

LispObject Lacsch(LispObject lits, LispObject x)
{   double d = floatval(x);
    return boxfloat(ACSCH(d));
}

LispObject Lacoth(LispObject lits, LispObject x)
{   double d = floatval(x);
    if (d > -1.0 && d < 1.0) return error1("argument out of range", x);
    return boxfloat(ACOTH(d));
}

LispObject Latan_2(LispObject lits, LispObject x, LispObject y)
{   double fx = floatval(x), fy=floatval(y), r;
#ifdef CRLIBM
// This implementation will breach the "correctly rounded" ideal at the very
// least because the division of y by x can round the input to the ATAN
// function.
    r = ATAN(fy/fx);
    if (fx <= 0.0)
    {   if (fy >= 0.0) r += _pi;
        else r -= _pi;
    }
#else
    r = atan2(fx, fy);
#endif
    return boxfloat(r);
}

LispObject Lnull(LispObject lits, LispObject x)
{
    return (x == nil ? lisptrue : nil);
}

LispObject Llength(LispObject lits, LispObject a)
{   size_t n = 0;
    while (isCONS(a))
    {   n++;
        a = qcdr(a);
    }
    return packfixnum(n);
}

LispObject Leq(LispObject lits, LispObject x, LispObject y)
{
    return (x == y ? lisptrue : nil);
}

LispObject Leqcar(LispObject lits, LispObject x, LispObject y)
{
    return ((isCONS(x) && (qcar(x) == y)) ? lisptrue : nil);
}

LispObject Lequal(LispObject lits, LispObject x, LispObject y)
{
    while (x != y && isCONS(x) && isCONS(y))
    {   if (Lequal(lits, qcar(x), qcar(y)) == nil) return nil;
        x = qcdr(x); y = qcdr(y);
    }
    if (x == y) return lisptrue;
    if ((x & TAGBITS) != (y & TAGBITS)) return nil;
    if (isSYMBOL(x) || isFIXNUM(x)) return nil;
    if (isFLOAT(x)) return (qfloat(x) == qfloat(y) ? lisptrue : nil);
    if (qheader(x) != qheader(y)) return nil;
    switch (qheader(x) & TYPEBITS)
    {   case typeVEC: case typeEQHASH: case typeEQHASHX:
        {   uintptr_t i;
            for (i=0; i<veclength(qheader(x))/sizeof(LispObject); i++)
                if (Lequal(lits, elt(x, i), elt(y, i)) == nil) return nil;
            return lisptrue;
        }
        default: // Treat all other cases as containing binary information.
        {   uintptr_t i;
            const char *xx = qstring(x), *yy = qstring(y);
            for (i=0; i<veclength(qheader(x)); i++)
                if (xx[i] != yy[i]) return nil;
            return lisptrue;
        }
    }
}

LispObject Lneq(LispObject lits, LispObject x, LispObject y)
{
    return (Lequal(lits, x, y) == nil ? lisptrue : nil);
}

LispObject Lmemq(LispObject lits, LispObject a, LispObject l)
{   while (isCONS(l))
    {   if (a == qcar(l)) return l;
        l = qcdr(l);
    }
    return nil;
}

LispObject Lmember(LispObject lits, LispObject a, LispObject l)
{   while (isCONS(l))
    {   if (Lequal(nil, a, qcar(l)) != nil) return l;
        l = qcdr(l);
    }
    return nil;
}

LispObject Lset(LispObject lits, LispObject x, LispObject y)
{
    if (!isSYMBOL(x)) return error1("bad arg for set", x);
    return (qvalue(x) = y);
}

LispObject Lboundp(LispObject lits, LispObject x)
{
    return (isSYMBOL(x) && qvalue(x)!=undefined) ? lisptrue : nil;
}

LispObject Lgensym_0(LispObject lits)
{   LispObject r = allocatesymbol(nil);
    qflags(r) |= flagGENSYM;
    return r;
}

// I want to have gensyms where I can control their name at least a bit,
// but do not have that implemented yet...

LispObject Lgensym_1(LispObject lits, LispObject a1)
{   LispObject r = allocatesymbol(nil);
    qflags(r) |= flagGENSYM;
    return r;
}

LispObject Lcharcode (LispObject lits, LispObject x)
{   if (isFIXNUM(x)) return x;
    if (isSYMBOL(x)) x = qpname(x);
    if (!isSTRING(x)) return error1("bad arg for char-code", x);
    return packfixnum(*qstring(x));
}

LispObject Lcodechar(LispObject lits, LispObject x)
{   char ch[4];
    if (!isFIXNUM(x)) return error1("bad arg for code-char", x);
    ch[0] = (char)qfixnum(x); ch[1] = 0;
    return lookup(ch, 1, 1);
}

LispObject Ltime(LispObject lits)
{   clock_t c = clock();
    return packfixnum((intptr_t)((1000*(int64_t )c)/CLOCKS_PER_SEC));
}

LispObject Ldate(LispObject lits)
{   time_t t = time(NULL);
    char today[32];
    char today1[32];
    strcpy(today, ctime(&t));  // e.g. "Sun Sep 16 01:03:52 1973\n"
//                                      012345678901234567890123
    today[24] = 0;             // loses final '\n'
    today1[0] = today[8]==' ' ? '0' : today[8];
    today1[1] = today[9];
    today1[2] = '-';
    today1[3] = today[4];
    today1[4] = today[5];
    today1[5] = today[6];
    today1[6] = '-';
    today1[7] = today[22];
    today1[8] = today[23];
    today1[9] = 0;             // Now as in 03-Apr-09
    return makestring(today1, 9);
}

LispObject Ldate_and_time_0(LispObject lits)
{   time_t t = time(NULL);
    char today[32];
    strcpy(today, ctime(&t));  // e.g. "Sun Sep 16 01:03:52 1973\n"
    today[24] = 0;             // loses final '\n'
    return makestring(today, 24);
}

LispObject Ldate_and_time_1(LispObject lits, LispObject a1)
{   time_t t = time(NULL);
    char today[32], today1[32];
    strcpy(today, ctime(&t));  // e.g. "Sun Sep 16 01:03:52 1973\n"
                               //       012345678901234567890123
    today[24] = 0;             // loses final '\n'
    today1[0] = today[8]==' ' ? '0' : today[8];
    today1[1] = today[9];
    today1[2] = '-';
    today1[3] = today[4];
    today1[4] = today[5];
    today1[5] = today[6];
    today1[6] = '-';
    today1[7] = today[22];
    today1[8] = today[23];
    today1[9] = 0;             // Now as in 03-Apr-09
    return makestring(today1, 9);
}

LispObject Llist2string(LispObject lits, LispObject a)
{   size_t len = 0;
    for (LispObject w=a; isCONS(w); w=qcdr(w)) len++;
    LispObject r = allocateatom(len);
    char *p = qstring(r);
    while (isCONS(a))
    {   int c;
        LispObject w = qcar(a);
        if (isFIXNUM(w)) c = qfixnum(w);
        else if (isSYMBOL(w)) c = *qstring(qpname(w));
        else if (isSTRING(w)) c = *qstring(w);
        else c = '?';
        *p++ = c;
        a = qcdr(a);
    }
    return r;
}

LispObject Loblist(LispObject lits)
{   size_t i;
    work1 = nil;
    for (i=0; i<OBHASH_SIZE; i++)
        for (work2=obhash[i]; isCONS(work2); work2 = qcdr(work2))
        {   if (qcar(work2) != undefined)
                work1 = cons(qcar(work2), work1);
        }
    return work1;
}

LispObject Leval(LispObject lits, LispObject x)
{
    return eval(x);
}

LispObject Lapply(LispObject lits, LispObject x, LispObject y)
{   LispObject a1, a2, a3, a4;
    if (isCONS(x) && qcar(x) == symlambda)
    {   if (!isCONS(y)) return interpreted0(qcdr(x));
        a1 = qcar(y);
        y = qcdr(y);
        if (!isCONS(y)) return interpreted1(qcdr(x), a1);
        a2 = qcar(y);
        y = qcdr(y);
        if (!isCONS(y)) return interpreted2(qcdr(x), a1, a2);
        a3 = qcar(y);
        y = qcdr(y);
        if (!isCONS(y)) return interpreted3(qcdr(x), a1, a2, a3);
        a4 = qcar(y);
        y = qcdr(y);
        if (!isCONS(y)) return interpreted4(qcdr(x), a1, a2, a3, a4);
        return interpreted5up(qcdr(x), a1, a2, a3, a4, shallow_copy(y));
    }
    else if (!isSYMBOL(x)) return error1("bad arg to apply", x);
    if (!isCONS(y)) return (*qdefn0(x))(qlits(x));
    a1 = qcar(y);
    y = qcdr(y);
    if (!isCONS(y)) return (*qdefn1(x))(qlits(x), a1);
    a2 = qcar(y);
    y = qcdr(y);
    if (!isCONS(y)) return (*qdefn2(x))(qlits(x), a1, a2);
    a3 = qcar(y);
    y = qcdr(y);
    if (!isCONS(y)) return (*qdefn3(x))(qlits(x), a1, a2, a3);
    a4 = qcar(y);
    y = qcdr(y);
    if (!isCONS(y)) return (*qdefn4(x))(qlits(x), a1, a2, a3, a4);
// Functions with 5 or more arguments have to unpack their own arguments
// for themselves.
    return (*qdefn5up(x))(qlits(x), a1, a2, a3, a4, y);
}

LispObject Lplist(LispObject lits, LispObject x)
{
    if (!isSYMBOL(x)) return nil;
    else return qplist(x);
}

LispObject Lput(LispObject lits, LispObject x, LispObject y, LispObject z)
{   LispObject w;
    if (!isSYMBOL(x)) return error1("bad arg put", x);
    w = qplist(x);
    while (isCONS(w))
    {   LispObject a = qcar(w);
        w = qcdr(w);
        if (isCONS(a) && qcar(a) == y)
        {   qcdr(a) = z;
            return z;
        }
    }
    w = acons(y, z, qplist(x));
    qplist(x) = w;
    return z;
}

LispObject Lget(LispObject lits, LispObject x, LispObject y)
{
    if (!isSYMBOL(x)) return nil;
    x = qplist(x);
    while (isCONS(x))
    {   LispObject a = qcar(x);
        x = qcdr(x);
        if (isCONS(a) && qcar(a) == y) return qcdr(a);
    }
    return nil;
}

LispObject Lremprop(LispObject lits, LispObject x, LispObject y)
{   LispObject p, r, *prev;
    if (!isSYMBOL(x)) return nil;
    p = *(prev = &qplist(x));
    while (p != nil)
    {   if (isCONS(r = qcar(p)) && qcar(qcar(p)) == y)
        {   *prev = qcdr(p);
            return r;
        }
        p = *(prev = &qcdr(p));
    }
    return nil;
}

LispObject Lfluid(LispObject lits, LispObject x)
{   while (isCONS(x))
    {   LispObject v = qcar(x);
        x = qcdr(x);
        if (!isSYMBOL(v)) continue;
        qflags(v) &= ~flagGLOBAL;
        qflags(v) |= flagFLUID;
        if (qvalue(v) == undefined) qvalue(v) = nil;
    }
    return nil;
}

LispObject Lglobal(LispObject lits, LispObject x)
{   while (isCONS(x))
    {   LispObject v = qcar(x);
        x = qcdr(x);
        if (!isSYMBOL(v)) continue;
        qflags(v) &= ~flagFLUID;
        qflags(v) |= flagGLOBAL;
        if (qvalue(v) == undefined) qvalue(v) = nil;
    }
    return nil;
}

LispObject Lunfluid(LispObject lits, LispObject x)
{   while (isCONS(x))
    {   LispObject v = qcar(x);
        x = qcdr(x);
        if (!isSYMBOL(v)) continue;
        qflags(v) &= ~flagFLUID;
    }
    return nil;
}

LispObject Lunglobal(LispObject lits, LispObject x)
{   while (isCONS(x))
    {   LispObject v = qcar(x);
        x = qcdr(x);
        if (!isSYMBOL(v)) continue;
        qflags(v) &= ~flagGLOBAL;
    }
    return nil;
}

LispObject Lfluidp(LispObject lits, LispObject x)
{   if (isSYMBOL(x) && (qflags(x)&flagFLUID)!=0) return lisptrue;
    else return nil;
}

LispObject Lglobalp(LispObject lits, LispObject x)
{   if (isSYMBOL(x) && (qflags(x)&flagGLOBAL)!=0) return lisptrue;
    else return nil;
}

LispObject Lgensymp(LispObject lits, LispObject x)
{   if (isSYMBOL(x) && (qflags(x)&flagGENSYM)!=0) return lisptrue;
    else return nil;
}

LispObject Lmkvect(LispObject lits, LispObject x)
{   int n;
    if (!isFIXNUM(x)) return error1("bad size in mkvect", x);
    n = (int)qfixnum(x);
// I put an (arbitrary) limit on the size of the largest vector.
    if (n < 0 || n > 100000) return error1("bad size in mkvect", x);
    return makevector(n);
}

LispObject Lupbv(LispObject lits, LispObject x)
{
    if (!isVEC(x)) return error1("bad arg to upbv", x);
    return arithlib::int_to_handle(veclength(qheader(x))/sizeof(LispObject)-1);
}

LispObject Lputv(LispObject lits, LispObject x, LispObject y, LispObject z)
{   int n;
    if (!isVEC(x) || !isFIXNUM(y))
        return error1("bad arg to putv", cons(x, y));
    n = (int)qfixnum(y);
    if (n < 0 || (uintptr_t)n >= veclength(qheader(x))/sizeof(LispObject))
        return error1("subscript out of range in putv", y);
    elt(x, n) = z;
    return z;
}

LispObject Lgetv(LispObject lits, LispObject x, LispObject y)
{   int n;
// As a matter of convenience and generosity I will allow "getv" to
// access items from hash tables as well as ordinary vectors.
    if ((!isVEC(x) && !isEQHASH(x) && !isEQHASHX(x)) || !isFIXNUM(y))
        return error1("bad arg to getv", cons(x, y));
    n = (int)qfixnum(y);
    if (n < 0 || (uintptr_t)n >= veclength(qheader(x))/sizeof(LispObject))
        return error1("subscript out of range in getv", y);
    return elt(x, n);
}

LispObject Lmkhash_3(LispObject lits, LispObject x, LispObject y, LispObject z)
{   int n;
    LispObject r;
    if (!isFIXNUM(x)) return error1("bad size in mkhash", x);
    n = (int)qfixnum(x);
// I force hash tables to be of limited size.
    if (n <= 10) n = 11;
    else if (n > 1000) n = 997;
    n |= 1;  // Force table-size to be an odd number
    r = makevector(n-1);
    qheader(r) ^= (typeVEC ^ typeEQHASH);
    return r;
}

LispObject Lmkhash_2(LispObject lits, LispObject x, LispObject y)
{
    return Lmkhash_3(lits, x, y, nil);
}

LispObject Lmkhash_1(LispObject lits, LispObject x)
{
    return Lmkhash_3(lits, x, nil, nil);
}

void rehash(LispObject x)
{
    int n = veclength(qheader(x));
    int i;
// At the moment that this is invoked it is at least certain that
// garbage collection is not in progress. Hence the second half-space
// is all memory available for use! So on a temporary basis I will put
// a copy of the hash table there.
// Well actually some pinned items may be present in the second half-
// space, but I still expect to be able to find plenty of free space
// somewhere there... If somehow because of fragmentation caused by
// pinned items it was not possible to find the space I need here
// then that is probably fatal, but that is always going to be the
// case if ensureheap2space is unable to find space.
    LispObject x1;
    block2 = 0;
    fringe2 = ((block_header *)blocks_by_age[0])->h2base;
    limit2 = ((block_header *)blocks_by_age[0])->h2top;
    ensureheap2space(sizeof(LispObject)+n);
    x1 = fringe2 + tagATOM;
// Note that this is short term use of memory in heap2, and I do not
// set an object-start mark bit for the "vector" I use here.
    memcpy((void *)(x1 - tagATOM), (void *)(x - tagATOM),
            n + sizeof(LispObject));
    n = n/sizeof(LispObject); // Now a count of slots in the table.
// I will now re-hash from the copy that I made back into the hash table, but
// now using the new hash values that reflect and changes that have
// arisen.
    for (i=0; i<n; i++) elt(x, i) = nil;
    for (i=0; i<n; i++)
    {   LispObject b = elt(x1, i);
        while (b != nil)
        {   LispObject ca = qcar(b), cd = qcdr(b);
            int h = (int)(((uintptr_t)qcar(ca))%((uintptr_t)n)); // New bucket.
            qcdr(b) = elt(x, h);
            elt(x, h) = b;    // Re-inserted in table.
            b = cd;
        }
    }
    if (isEQHASHX(x)) qheader(x) ^= (typeEQHASH ^ typeEQHASHX);
}

LispObject Lputhash(LispObject lits, LispObject x, LispObject y, LispObject z)
{   int n, h;
    LispObject c;
    if (isEQHASHX(y)) rehash(y);
    if (!isEQHASH(y)) return error1("not a hash table in puthash", cons(x, y));
    n = veclength(qheader(y))/sizeof(LispObject);
// I use unsigned types so I get a positive remainder.
    h = (int)(((uintptr_t)x) % ((uintptr_t)n));
    c = elt(y, h);
    while (isCONS(c))
    {   if (qcar(qcar(c)) == x)
        {   qcdr(qcar(c)) = z;
            return z;
        }
        c = qcdr(c);
    }
    c = acons(x, z, elt(y, h));
    elt(y, h) = c;
    return z;
}

LispObject Lremhash(LispObject lits, LispObject x, LispObject y)
{   int n, h;
    LispObject c, *cp;
    if (isEQHASHX(y)) rehash(y);
    if (!isEQHASH(y)) return error1("not a hash table in remhash", cons(x, y));
    n = veclength(qheader(y))/sizeof(LispObject);
    h = (int)(((uintptr_t)x) % ((uintptr_t)n));
    c = *(cp = &elt(y, h));
    while (isCONS(c))
    {   if (qcar(qcar(c)) == x)
        {   *cp = qcdr(c);
            return qcdr(qcar(c));
        }
        c = *(cp = &qcdr(c));
    }
    return nil;
}

LispObject Lgethash(LispObject lits, LispObject x, LispObject y)
{   int n, h;
    LispObject c;
    if (isEQHASHX(y)) rehash(y);
    if (!isEQHASH(y)) return error1("not a hash table in gethash", cons(x, y));
    n = veclength(qheader(y))/sizeof(LispObject);
    h = (int)(((uintptr_t)x) % ((uintptr_t)n));
    c = elt(y, h);
    while (isCONS(c))
    {   if (qcar(qcar(c)) == x) return qcdr(qcar(c));
        c = qcdr(c);
    }
    return nil;
}

LispObject Lgetd(LispObject lits, LispObject x)
{   LispObject r;
    if (!isSYMBOL(x)) return nil;
    r = qlits(x);
    if ((qflags(x) & flagSPECFORM) != 0)
    {   if (qdefn1(x) == (LispFn1 *)interpretspecform)
// I copy the bound variable list that is returned so that nobody can
// use rplaca/rplacd to corrupt it.
            return list2star(fexpr,
                symlambda,
//@@@                cons(shallow_copy(qcar(r)), qcdr(r)));
                r);
        else return cons(fsubr, x);
    }
// I know I only look in the qdefn1 cell here, but undefined and interpreted
// things always have the values I test for here in them, so that is
// in fact OK.
    else if (qdefn0(x) == undefined0 &&
             qdefn1(x) == undefined1 &&
             qdefn2(x) == undefined2 &&
             qdefn3(x) == undefined3 &&
             qdefn4(x) == undefined4 &&
             qdefn5up(x) == undefined5up) return nil;
    else if ((qflags(x) & flagMACRO) != 0)
        return list2star(macro, symlambda, r);
    else if (qdefn0(x) == interpreted0 &&
             (qdefn1(x) == interpreted1 ||
              qdefn1(x) == interpretspecform) &&
             qdefn2(x) == interpreted2 &&
             qdefn3(x) == interpreted3 &&
             qdefn4(x) == interpreted4 &&
             qdefn5up(x) == interpreted5up)
        return list2star(expr, symlambda, r);
//***            cons(shallow_copy(qcar(r)), qcdr(r)));
    else return cons(subr, x);
}

LispObject Lreturn_0(LispObject lits)
{
    work1 = nil;
    unwindflag = unwindRETURN;
    return nil;
}

LispObject Lreturn_1(LispObject lits, LispObject x)
{
    work1 = x;
    if (unwindflag == unwindNONE) unwindflag = unwindRETURN;
    return nil;
}

LispObject Lzerop(LispObject lits, LispObject x)
{
// Note that a bignum can never be zero! Because that is not "big".
// This code is generous and anything non-numeric is not zero.
    if (x == packfixnum(0) ||
        (isFLOAT(x) && qfloat(x) == 0.0)) return lisptrue;
    else return nil;
}

LispObject Lonep(LispObject lits, LispObject x)
{
    if (x == packfixnum(1) ||
        (isFLOAT(x) && qfloat(x) == 1.0)) return lisptrue;
    else return nil;
}
