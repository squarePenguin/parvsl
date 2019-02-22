LispObject Lcar(LispObject lits, LispObject x)
{
    if (isCONS(x)) return qcar(x);
    else return error1("car of an atom", x);
}

LispObject Lcdr(LispObject lits, LispObject x)
{
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
    return (isCONS(x) && qcar(x) != bignum ? nil : lisptrue);
}

LispObject Lbignump(LispObject lits, LispObject x)
{
    return (isCONS(x) && qcar(x) == bignum ? lisptrue : nil);
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

LispObject Lprog1(LispObject lits, LispObject x, LispObject y)
{   return x;
}

LispObject Lprog2(LispObject lits, LispObject x, LispObject y)
{   return y;
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

LispObject Lfix(LispObject lits, LispObject x)
{
    return (isFIXNUM(x) || isBIGNUM(x) ? x :
            isFLOAT(x) ? boxint64((int64_t)qfloat(x)) :
            error1("arg for fix", x));
}

LispObject Lfloor(LispObject lits, LispObject x)
{
    return (isFIXNUM(x) || isBIGNUM(x) ? x :
            isFLOAT(x) ? boxint64((int64_t)floor(qfloat(x))) :
            error1("arg for floor", x));
}

LispObject Lceiling(LispObject lits, LispObject x)
{
    return (isFIXNUM(x) || isBIGNUM(x) ? x :
            isFLOAT(x) ? boxint64((int64_t)ceil(qfloat(x))) :
            error1("arg for ceiling", x));
}

LispObject Lfloat(LispObject lits, LispObject x)
{
    return (isFLOAT(x) ? x :
            isFIXNUM(x) ? boxfloat((double)qfixnum(x)) :
            isBIGNUM(x) ? boxfloat((double)qint64(x)) :
            error1("arg for float", x));
}

INLINE unsigned int floatval(LispObject x)
{   return isFLOAT(x) ? qfloat(x) :
           isFIXNUM(x) ? (double)qfixnum(x) :
           isBIGNUM(x) ? (double)qint64(x) :
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

LispObject Lcos(LispObject lits, LispObject x)
{
    return boxfloat(cos(floatval(x)));
}

LispObject Lsin(LispObject lits, LispObject x)
{
    return boxfloat(sin(floatval(x)));
}

LispObject Lsqrt(LispObject lits, LispObject x)
{
    return boxfloat(sqrt(floatval(x)));
}

LispObject Llog(LispObject lits, LispObject x)
{
    return boxfloat(log(floatval(x)));
}

LispObject Lexp(LispObject lits, LispObject x)
{
    return boxfloat(exp(floatval(x)));
}

LispObject Latan(LispObject lits, LispObject x)
{
    return boxfloat(atan(floatval(x)));
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
    return (par::symval(x) = y);
}

LispObject Lboundp(LispObject lits, LispObject x)
{
    return (isSYMBOL(x) && par::symval(x)!=undefined) ? lisptrue : nil;
}

LispObject Lgensym_0(LispObject lits)
{   return allocatesymbol(nil);
}

// I want to have gensyms where I can control their name at least a bit,
// but do not have that implemented yet...

LispObject Lgensym_1(LispObject lits, LispObject a1)
{   return allocatesymbol(nil);
}

LispObject Lcharcode (LispObject lits, LispObject x)
{
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
    return makestring(today1, 10);
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

LispObject Lmkvect(LispObject lits, LispObject x)
{   int n;
    if (!isFIXNUM(x)) return error1("bad size in mkvect", x);
    n = (int)qfixnum(x);
// I put an (arbitrary) limit on the size of the largest vector.
    if (n < 0 || n > 100000) return error1("bad size in mkvect", x);
    return makevector(n);
}

extern LispObject boxint64(int64_t a);

LispObject Lupbv(LispObject lits, LispObject x)
{
    if (!isVEC(x)) return error1("bad arg to upbv", x);
    return boxint64(veclength(qheader(x))/sizeof(LispObject)-1);
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

