INLINE unsigned int unwindNONE      = 0;
INLINE unsigned int unwindERROR     = 1;
INLINE unsigned int unwindBACKTRACE = 2;
INLINE unsigned int unwindGO        = 4;
INLINE unsigned int unwindRETURN    = 8;
INLINE unsigned int unwindPRESERVE  = 16;
INLINE unsigned int unwindRESTART   = 32;

unsigned int unwindflag = unwindNONE;

int backtraceflag = 0;
INLINE int backtraceHEADER = 1;
INLINE int backtraceTRACE  = 2;

LispObject error0(const char *msg)
{   if ((backtraceflag & backtraceHEADER) != 0)
    {   linepos = printf("\n+++ Error: %s\n", msg);
#ifdef DEBUG
        if (logfile != NULL) fprintf(logfile, "\n+++ Error: %s\n", msg);
#endif // DEBUG
    }
    unwindflag = (backtraceflag & backtraceTRACE) != 0 ? unwindBACKTRACE :
                 unwindERROR;
    return nil;
}

LispObject quiet_error()
{   unwindflag = unwindERROR;
    return nil;
}

LispObject error1(const char *msg, LispObject data)
{   if ((backtraceflag & backtraceHEADER) != 0)
    {   linepos = printf("\n+++ Error: %s: ", msg);
#ifdef DEBUG
        if (logfile != NULL) fprintf(logfile, "\n+++ Error: %s: ", msg);
#endif // DEBUG
        errprint(data);
    }
    unwindflag = (backtraceflag & backtraceTRACE) != 0 ? unwindBACKTRACE :
                 unwindERROR;
    return nil;
}

LispObject error2(const char *msg, const char *s1, LispObject data)
{   if ((backtraceflag & backtraceHEADER) != 0)
    {   linepos = printf("\n+++ Error: %s (%s): ", msg, s1);
#ifdef DEBUG
        if (logfile != NULL) fprintf(logfile, "\n+++ Error: %s (%s): ", msg, s1);
#endif // DEBUG
        errprint(data);
    }
    unwindflag = (backtraceflag & backtraceTRACE) != 0 ? unwindBACKTRACE :
                 unwindERROR;
    return nil;
}

LispObject error1s(const char *msg, const char *data)
{   if ((backtraceflag & backtraceHEADER) != 0)
#ifdef DEBUG
    {   printf("\n+++ Error: %s %s\n", msg, data);
        if (logfile != NULL) fprintf(logfile, "\n+++ Error: %s %s\n", msg, data);
    }
#else // DEBUG
        printf("\n+++ Error: %s %s\n", msg, data);
#endif // DEBUG
    unwindflag = (backtraceflag & backtraceTRACE) != 0 ? unwindBACKTRACE :
                 unwindERROR;
    return nil;
}

LispObject call1(const char *name, LispObject a1)
{
    LispObject fn = lookup(name, strlen(name), 2);
    if (fn == undefined || qdefn1(fn) == undefined1 ||
        qdefn1(fn) == wrongnumber1) return NULLATOM;
// Attempting to trace the function used here will be ineffective.
    return (*(LispFn1 *)qdefn1(fn))(qlits(fn), a1);
}

LispObject eval(LispObject x);

// In the interpreted() family of functions I can assume that the expression
// passed will be of the form
//    ((v1 v2 ...) ...)
// with a properly nil-terminated list of symbols

LispObject interpreted0(LispObject b)
{
    LispObject bvl, r;
    bvl = qcar(b);
    b = qcdr(b);       // Body of the function.
    if (bvl != nil)    // Could legally be (&rest v)
    {
        return error1("Not enough arguments provided", bvl);
    }
    r = nil;
    while (isCONS(b))
    {   r = eval(qcar(b));
        if (unwindflag != unwindNONE) break;
        b = qcdr(b);
    }
    return r;
}

LispObject interpreted1(LispObject b, LispObject a1)
{
    LispObject bvl, r, save1;
    bvl = qcar(b);
    b = qcdr(b);       // Body of the function.
    if (bvl == nil) return error1("Too many arguments provided", bvl);
    r = qcar(bvl);
    bvl = qcdr(bvl);
    if (bvl != nil)  // Could legally be (v1 &rest v2)
    {
        return error1("Not enough arguments provided", bvl);
    }
    bvl = r;
    save1 = qvalue(bvl);
    qvalue(bvl) = a1;
    r = nil;
    while (isCONS(b))
    {   r = eval(qcar(b));
        if (unwindflag != unwindNONE) break;
        b = qcdr(b);
    }
    qvalue(bvl) = save1;
    return r;
}

LispObject interpreted2(LispObject b, LispObject a1, LispObject a2)
{
    LispObject bvl, v2, w;
    bvl = qcar(b);
    b = qcdr(b);       // Body of the function.
    if (bvl == nil ||
        (v2 = qcdr(bvl)) == nil)
        return error1("Too many arguments provided", bvl);
    if (qcdr(v2) != nil)
    {   // beware &rest
        return error1("Not enough arguments provided", bvl);
    }
    bvl = qcar(bvl);
    v2 = qcar(v2);
    swap(a1, qvalue(bvl));
    swap(a2, qvalue(v2));
    w = nil;
    while (isCONS(b))
    {   w = eval(qcar(b));
        if (unwindflag != unwindNONE) break;
        b = qcdr(b);
    }
    qvalue(v2) = a2;
    qvalue(bvl) = a1;
    return w;
}

LispObject interpreted3(LispObject b, LispObject a1,
                        LispObject a2, LispObject a3)
{
    LispObject bvl, v2, v3, w;
    bvl = qcar(b);
    b = qcdr(b);       // Body of the function.
    if (bvl == nil ||
        (v2 = qcdr(bvl)) == nil ||
        (v3 = qcdr(v2)) == nil)
        return error1("Too many arguments provided", bvl);
    if (qcdr(v3) != nil)
    {   // beware &rest
        return error1("Not enough arguments provided", bvl);
    }
    bvl = qcar(bvl);
    v2 = qcar(v2);
    v3 = qcar(v3);
    swap(a1, qvalue(bvl));
    swap(a2, qvalue(v2));
    swap(a3, qvalue(v3));
    w = nil;
    while (isCONS(b))
    {   w = eval(qcar(b));
        if (unwindflag != unwindNONE) break;
        b = qcdr(b);
    }
    qvalue(v3) = a3;
    qvalue(v2) = a2;
    qvalue(bvl) = a1;
    return w;
}

LispObject interpreted4(LispObject b, LispObject a1, LispObject a2,
                        LispObject a3, LispObject a4)
{
    LispObject bvl, v2, v3, v4, w;
    bvl = qcar(b);
    b = qcdr(b);       // Body of the function.
    if (bvl == nil ||
        (v2 = qcdr(bvl)) == nil ||
        (v3 = qcdr(v2)) == nil ||
        (v4 = qcdr(v3)) == nil)
        return error1("Too many arguments provided", bvl);
    if (qcdr(v4) != nil)
    {   // beware &rest
        return error1("Not enough arguments provided", bvl);
    }
    bvl = qcar(bvl);
    v2 = qcar(v2);
    v3 = qcar(v3);
    v4 = qcar(v4);
    swap(a1, qvalue(bvl));
    swap(a2, qvalue(v2));
    swap(a3, qvalue(v3));
    swap(a4, qvalue(v4));
    w = nil;
    while (isCONS(b))
    {   w = eval(qcar(b));
        if (unwindflag != unwindNONE) break;
        b = qcdr(b);
    }
    qvalue(v4) = a4;
    qvalue(v3) = a3;
    qvalue(v2) = a2;
    qvalue(bvl) = a1;
    return w;
}

LispObject nreverse(LispObject a)
{   LispObject b = nil, w;
    while (isCONS(a))
    {   w = qcdr(a);
        qcdr(a) = b;
        b = a;
        a = w;
    }
    return b;
}

LispObject interpreted5up(LispObject b, LispObject a1, LispObject a2,
                          LispObject a3, LispObject a4, LispObject a5up)
{
    LispObject bvl, v2, v3, v4, v5up, w, v, a;
    bvl = qcar(b);
    b = qcdr(b);       // Body of the function.
    if (bvl == nil ||
        (v2 = qcdr(bvl)) == nil ||
        (v3 = qcdr(v2)) == nil ||
        (v4 = qcdr(v3)) == nil ||
        (v5up = qcdr(v4)) == nil)
        return error1("Too many arguments provided", bvl);
    bvl = qcar(bvl);
    v2 = qcar(v2);
    v3 = qcar(v3);
    v4 = qcar(v4);
    {   int n = 0; // Ignoring &rest here!
        for (w=v5up; w!=nil; w=qcdr(w)) n++;    // count needed
        for (w=a5up; isCONS(w); w=qcdr(w)) n--; // count supplied
        if (n != 0) return error1(
           (n>0 ? "Not enough arguments provided" :
                  "Too many arguments provided"), bvl);
    }
    swap(a1, qvalue(bvl));
    swap(a2, qvalue(v2));
    swap(a3, qvalue(v3));
    swap(a4, qvalue(v4));
    v = v5up;
    a = nil;
    while (v != nil)
    {   swap(qvalue(qcar(v)), qcar(a5up)); // bind another argument
        v = qcdr(v);
        w = qcdr(a5up);
        qcdr(a5up) = a;
        a = a5up;
        a5up = w;   // Collect saved values in reversed order.
    }
    w = nil;
    while (isCONS(b))
    {   w = eval(qcar(b));
        if (unwindflag != unwindNONE) break;
        b = qcdr(b);
    }
    v = v5up = nreverse(v5up);
    while (v != nil)
    {   qvalue(qcar(v)) = qcar(a);
        v = qcdr(v);
        a = qcdr(a);
    }
    nreverse(v5up);
    qvalue(v4) = a4;
    qvalue(v3) = a3;
    qvalue(v2) = a2;
    qvalue(bvl) = a1;
    return w;
}

LispObject evlis(LispObject x)
{
    LispObject p, q;
    if (!isCONS(x)) return nil;
    p = eval(qcar(x));
    if (unwindflag != unwindNONE) return nil;
    p = q = cons(p, nil);
    if (unwindflag != unwindNONE) return nil;
    x = qcdr(x);
    while (isCONS(x))
    {   LispObject w = eval(qcar(x));
        if (unwindflag != unwindNONE) return nil;
        w = cons(w, nil);
        if (unwindflag != unwindNONE) return nil;
        qcdr(q) = w;
        q = w;
        x = qcdr(x);
    }
    return p;
}

LispObject eval(LispObject x)
{
#ifdef TRACEALL
    if (isCONS(x))
    {  printf("eval: "); print(qcar(x)); // Eek! for desparate debugging
    }
#endif
// The intent of fname is that when running VSL under a debugger it can
// be inspected to reveal the name of a function that is being called.
    const char *fname = "unknown";
    if (isCONS(x) && isSYMBOL(qcar(x))) fname = qstring(qpname(qcar(x)));
    while (isCONS(x) && isSYMBOL(qcar(x)) && (qflags(qcar(x)) & flagMACRO))
    {   LispObject fn = qcar(x);
        int traced = qflags(fn) & flagTRACED;
        if (traced != 0)
        {   linepos += printf("Macroexpand: ");
            if (unwindflag != unwindNONE) return nil;
            errprint(x);
        }
        x = (*(LispFn1 *)qdefn1(fn))(qlits(fn), x);
        if (unwindflag == unwindBACKTRACE)
        {   linepos += printf("Call to ");
            errprin(fn);
            printf(" failed\n");
            linepos = 0;
            return nil;
        }
        if (traced != 0)
        {   linepos += printf("= ");
            errprint(x);
            if (unwindflag != unwindNONE) return nil;
        }
    }
    if (isSYMBOL(x))
    {   LispObject v = qvalue(x);
        if (v == undefined)
            return error1("undefined variable", x);
        else return v;
    }
    else if (!isCONS(x)) return x;
// Now I have something of the form
//     (f arg1 ... argn)
// to process.
    {   LispObject f = qcar(x);
        if (isSYMBOL(f))
        {   LispObject flags = qflags(f), aa;
            int n = 0;
            if (flags & flagSPECFORM)
            {   SpecialForm *fn = (SpecialForm *)qdefn1(f);
                return (*fn)(qlits(f), qcdr(x));
            }
            aa = qcdr(x);
            while (isCONS(aa))
            {   n++;             // Count number of args supplied.
                aa = qcdr(aa);
            }
            aa = qcdr(x);
// Here I will evaluate all the arguments for the function.
            switch (n)
            {
            case 0:
                if (flags & flagTRACED)
                {   if (linepos!=0) wrch('\n');
                    linepos += printf("Calling: ");
                    printf("%s\n", fname); linepos = 0;
//                  errprint(f);
                    if (unwindflag != unwindNONE) return nil;
                }
                x = (*qdefn0(f))(qlits(f));
                if (unwindflag == unwindBACKTRACE)
                {   if (linepos!=0) wrch('\n');
                    linepos += printf("Call to ");
                    errprin(f);
                    printf(" failed\n");
                    linepos = 0;
                    return nil;
                }
                if (flags & flagTRACED)
                {   if (linepos!=0) wrch('\n');
                    errprin(f);
                    linepos += printf(" = ");
                    if (unwindflag != unwindNONE) return nil;
                    errprint(x);
                    if (unwindflag != unwindNONE) return nil;
                }
                return x;
            case 1:
                x = eval(qcar(aa));
                if (unwindflag != unwindNONE) return nil;
                if (flags & flagTRACED)
                {   if (linepos!=0) wrch('\n');
                    linepos += printf("Calling: ");
                    errprint(f);
                    if (unwindflag != unwindNONE) return nil;
                    linepos += printf("Arg1: ");
                    errprint(x);
                    if (unwindflag != unwindNONE) return nil;
                }
                x = (*qdefn1(f))(qlits(f), x);
                if (unwindflag == unwindBACKTRACE)
                {   if (linepos!=0) wrch('\n');
                    linepos += printf("Call to ");
                    errprin(f);
                    printf(" failed\n");
                    linepos = 0;
                    return nil;
                }
                if (flags & flagTRACED)
                {   if (linepos!=0) wrch('\n');
                    errprin(f);
                    linepos += printf(" = ");
                    if (unwindflag != unwindNONE) return nil;
                    errprint(x);
                    if (unwindflag != unwindNONE) return nil;
                }
                return x;
            case 2:
                x = eval(qcar(aa));
                if (unwindflag != unwindNONE) return nil;
                aa = eval(qcar(qcdr(aa)));
                if (unwindflag != unwindNONE) return nil;
                if (flags & flagTRACED)
                {   if (linepos!=0) wrch('\n');
                    linepos += printf("Calling: ");
                    errprint(f);
                    if (unwindflag != unwindNONE) return nil;
                    linepos += printf("Arg1: ");
                    errprint(x);
                    if (unwindflag != unwindNONE) return nil;
                    linepos += printf("Arg2: ");
                    errprint(aa);
                    if (unwindflag != unwindNONE) return nil;
                }
                x = (*qdefn2(f))(qlits(f), x, aa);
                if (unwindflag == unwindBACKTRACE)
                {   if (linepos!=0) wrch('\n');
                    linepos += printf("Call to ");
                    errprin(f);
                    printf(" failed\n");
                    linepos = 0;
                    return nil;
                }
                if (flags & flagTRACED)
                {   if (linepos!=0) wrch('\n');
                    errprin(f);
                    linepos += printf(" = ");
                    if (unwindflag != unwindNONE) return nil;
                    errprint(x);
                    if (unwindflag != unwindNONE) return nil;
                }
                return x;
            case 3:
                x = eval(qcar(aa));
                if (unwindflag != unwindNONE) return nil;
                aa = qcdr(aa);
                {   LispObject a2 = eval(qcar(aa));
                    if (unwindflag != unwindNONE) return nil;
                    aa = eval(qcar(qcdr(aa)));
                    if (unwindflag != unwindNONE) return nil;
                    if (flags & flagTRACED)
                    {   if (linepos!=0) wrch('\n');
                        linepos += printf("Calling: ");
                        errprint(f);
                        if (unwindflag != unwindNONE) return nil;
                        linepos += printf("Arg1: ");
                        errprint(x);
                        if (unwindflag != unwindNONE) return nil;
                        linepos += printf("Arg2: ");
                        errprint(a2);
                        if (unwindflag != unwindNONE) return nil;
                        linepos += printf("Arg3: ");
                        errprint(aa);
                        if (unwindflag != unwindNONE) return nil;
                    }
                    x = (*qdefn3(f))(qlits(f), x, a2, aa);
                    if (unwindflag == unwindBACKTRACE)
                    {   if (linepos!=0) wrch('\n');
                        linepos += printf("Call to ");
                        errprin(f);
                        printf(" failed\n");
                        linepos = 0;
                        return nil;
                    }
                    if (flags & flagTRACED)
                    {   if (linepos!=0) wrch('\n');
                        errprin(f);
                        linepos += printf(" = ");
                        if (unwindflag != unwindNONE) return nil;
                        errprint(x);
                        if (unwindflag != unwindNONE) return nil;
                    }
                    return x;
                }
            case 4:
                x = eval(qcar(aa));
                if (unwindflag != unwindNONE) return nil;
                aa = qcdr(aa);
                {   LispObject a2 = eval(qcar(aa)), a3;
                    if (unwindflag != unwindNONE) return nil;
                    aa = qcdr(aa);
                    a3 = eval(qcar(aa));
                    if (unwindflag != unwindNONE) return nil;
                    aa = eval(qcar(qcdr(aa)));
                    if (unwindflag != unwindNONE) return nil;
                    if (flags & flagTRACED)
                    {   if (linepos!=0) wrch('\n');
                        linepos += printf("Calling: ");
                        errprint(f);
                        if (unwindflag != unwindNONE) return nil;
                        linepos += printf("Arg1: ");
                        errprint(x);
                        if (unwindflag != unwindNONE) return nil;
                        linepos += printf("Arg2: ");
                        errprint(a2);
                        if (unwindflag != unwindNONE) return nil;
                        linepos += printf("Arg3: ");
                        errprint(a3);
                        if (unwindflag != unwindNONE) return nil;
                        linepos += printf("Arg4: ");
                        errprint(aa);
                        if (unwindflag != unwindNONE) return nil;
                    }
                    x = (*qdefn4(f))(qlits(f), x, a2, a3, aa);
                    if (unwindflag == unwindBACKTRACE)
                    {   if (linepos!=0) wrch('\n');
                        linepos += printf("Call to ");
                        errprin(f);
                        printf(" failed\n");
                        linepos = 0;
                        return nil;
                    }
                    if (flags & flagTRACED)
                    {   if (linepos!=0) wrch('\n');
                        errprin(f);
                        linepos += printf(" = ");
                        if (unwindflag != unwindNONE) return nil;
                        errprint(x);
                        if (unwindflag != unwindNONE) return nil;
                    }
                    return x;
                }
            default:
                x = eval(qcar(aa));
                if (unwindflag != unwindNONE) return nil;
                aa = qcdr(aa);
                {   LispObject a2 = eval(qcar(aa)), a3, a4;
                    if (unwindflag != unwindNONE) return nil;
                    aa = qcdr(aa);
                    a3 = eval(qcar(aa));
                    if (unwindflag != unwindNONE) return nil;
                    aa = qcdr(aa);
                    a4 = eval(qcar(aa));
                    if (unwindflag != unwindNONE) return nil;
                    aa = evlis(qcdr(aa));
                    if (unwindflag != unwindNONE) return nil;
                    if (flags & flagTRACED)
                    {   if (linepos!=0) wrch('\n');
                        linepos += printf("Calling: ");
                        errprint(f);
                        if (unwindflag != unwindNONE) return nil;
                        linepos += printf("Arg1: ");
                        errprint(x);
                        if (unwindflag != unwindNONE) return nil;
                        linepos += printf("Arg2: ");
                        errprint(a2);
                        if (unwindflag != unwindNONE) return nil;
                        linepos += printf("Arg3: ");
                        errprint(a3);
                        if (unwindflag != unwindNONE) return nil;
                        linepos += printf("Arg4: ");
                        errprint(a4);
                        if (unwindflag != unwindNONE) return nil;
                        linepos += printf("Arg5...: ");
                        errprint(aa);
                        if (unwindflag != unwindNONE) return nil;
                    }
                    x = (*qdefn5up(f))(qlits(f), x, a2, a3, a4, aa);
                    if (unwindflag == unwindBACKTRACE)
                    {   if (linepos!=0) wrch('\n');
                        linepos += printf("Call to ");
                        errprin(f);
                        printf(" failed\n");
                        linepos = 0;
                        return nil;
                    }
                    if (flags & flagTRACED)
                    {   if (linepos!=0) wrch('\n');
                        errprin(f);
                        linepos += printf(" = ");
                        if (unwindflag != unwindNONE) return nil;
                        errprint(x);
                        if (unwindflag != unwindNONE) return nil;
                    }
                    return x;
                }
            }
        }
        else if (isCONS(f) && qcar(f) == symlambda)
        {   LispObject aa = qcdr(x);
            int n = 0;
            while (isCONS(aa))
            {   n++;             // Count number of args supplied.
                aa = qcdr(aa);
            }
            aa = qcdr(x);
            switch (n)
            {
            case 0:
// A raw lambda embedded in a form that has to be interpreted does not
// provide anywhere to request that it be traced, so there is no need to
// think about tracing here. Otherwise this is much the same as the code
// used when a named function is called.
                return interpreted0(qcdr(f));
            case 1:
                x = eval(qcar(aa));
                if (unwindflag != unwindNONE) return nil;
                return interpreted1(qcdr(f), x);
            case 2:
                x = eval(qcar(aa));
                if (unwindflag != unwindNONE) return nil;
                aa = eval(qcar(qcdr(aa)));
                if (unwindflag != unwindNONE) return nil;
                return interpreted2(qcdr(f), x, aa);
            case 3:
                x = eval(qcar(aa));
                if (unwindflag != unwindNONE) return nil;
                aa = qcdr(aa);
                {   LispObject a2 = eval(qcar(aa));
                    if (unwindflag != unwindNONE) return nil;
                    aa = eval(qcar(qcdr(aa)));
                    if (unwindflag != unwindNONE) return nil;
                    return interpreted3(qcdr(f), x, a2, aa);
                }
            case 4:
                x = eval(qcar(aa));
                if (unwindflag != unwindNONE) return nil;
                aa = qcdr(aa);
                {   LispObject a2 = eval(qcar(aa)), a3;
                    if (unwindflag != unwindNONE) return nil;
                    aa = qcdr(aa);
                    a3 = eval(qcar(aa));
                    if (unwindflag != unwindNONE) return nil;
                    aa = eval(qcar(qcdr(aa)));
                    if (unwindflag != unwindNONE) return nil;
                    return interpreted4(qcdr(f), x, a2, a3, aa);
                }
            default:
                x = eval(qcar(aa));
                if (unwindflag != unwindNONE) return nil;
                aa = qcdr(aa);
                {   LispObject a2 = eval(qcar(aa)), a3, a4;
                    if (unwindflag != unwindNONE) return nil;
                    aa = qcdr(aa);
                    a3 = eval(qcar(aa));
                    if (unwindflag != unwindNONE) return nil;
                    aa = qcdr(aa);
                    a4 = eval(qcar(aa));
                    if (unwindflag != unwindNONE) return nil;
                    aa = evlis(qcdr(aa));
                    if (unwindflag != unwindNONE) return nil;
                    return interpreted5up(qcdr(f), x, a2, a3, a4, aa);
                }
            }
        }
        else return error1("invalid function", f);
    }
}

LispObject Lprogn(LispObject lits, LispObject x);


LispObject interpretspecform(LispObject lits, LispObject x)
{   // lits should be ((var) body...)
    LispObject v, v_value;
    if (!isCONS(lits)) return nil;
    v = qcar(lits);
    lits = qcdr(lits);
    if (!isCONS(v) || !isSYMBOL(v = qcar(v))) return nil;
    v_value = qvalue(v);
    qvalue(v) = x;
    lits = Lprogn(nil, lits);
    qvalue(v) = v_value;
    return lits;
}

// Special forms are things that do not have their arguments pre-evaluated.

LispObject Lquote(LispObject lits, LispObject x)
{   if (isCONS(x)) return qcar(x);
    else return nil;
}

LispObject Lcond(LispObject lits, LispObject x)
{
//   Arg is in form
//      ((predicate1 val1a val1b ...)
//       (predicate2 val2a val2b ...)
//       ...)
    while (isCONS(x))
    {   LispObject carx = qcar(x);
        if (isCONS(carx))
        {   LispObject p = eval(qcar(carx));
            if (unwindflag != unwindNONE) return nil;
            else if (p != nil) return Lprogn(nil, qcdr(carx));
        }
        x = qcdr(x);
    }
    return nil;
}

