INLINE constexpr unsigned int unwindNONE      = 0;
INLINE constexpr unsigned int unwindERROR     = 1;
INLINE constexpr unsigned int unwindBACKTRACE = 2;
INLINE constexpr unsigned int unwindGO        = 4;
INLINE constexpr unsigned int unwindRETURN    = 8;
INLINE constexpr unsigned int unwindPRESERVE  = 16;
INLINE constexpr unsigned int unwindRESTART   = 32;

thread_local unsigned int unwindflag = unwindNONE;

thread_local int backtraceflag = -1;
INLINE constexpr int backtraceHEADER = 1;
INLINE constexpr int backtraceTRACE  = 2;

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

LispObject call2(const char *name, LispObject a1, LispObject a2)
{
    LispObject fn = lookup(name, strlen(name), 2);
    if (fn == undefined || qdefn2(fn) == undefined2 ||
        qdefn2(fn) == wrongnumber2) return NULLATOM;
    return (*(LispFn2 *)qdefn2(fn))(qlits(fn), a1, a2);
}

LispObject eval(LispObject x);

// In the interpreted() family of functions I can assume that the expression
// passed will be of the form
//    ((v1 v2 ...) ...)
// with a properly nil-terminated list of symbols

LispObject interpreted0(LispObject b)
{
    guard_gc();

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
    guard_gc();

    // LispObject bvl, r, save1;
    LispObject bvl = qcar(b);
    b = qcdr(b);       // Body of the function.
    if (bvl == nil) return error1("Too many arguments provided", bvl);
    LispObject r = qcar(bvl);
    bvl = qcdr(bvl);
    if (bvl != nil)  // Could legally be (v1 &rest v2)
    {
        return error1("Not enough arguments provided", bvl);
    }
    bvl = r;
    par::Shallow_bind bind_bvl(bvl, a1);
    r = nil;
    while (isCONS(b))
    {   r = eval(qcar(b));
        if (unwindflag != unwindNONE) break;
        b = qcdr(b);
    }
    return r;
}

LispObject interpreted2(LispObject b, LispObject a1, LispObject a2)
{
    guard_gc();
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

    par::Shallow_bind bind_bvl(bvl, a1);
    par::Shallow_bind bind_v2(v2, a2);
    w = nil;
    while (isCONS(b))
    {   w = eval(qcar(b));
        if (unwindflag != unwindNONE) break;
        b = qcdr(b);
    }
    return w;
}

LispObject interpreted3(LispObject b, LispObject a1,
                        LispObject a2, LispObject a3)
{
    guard_gc();
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
    par::Shallow_bind bind_bvl(bvl, a1);
    par::Shallow_bind bind_v2(v2, a2);
    par::Shallow_bind bind_v3(v3, a3);
    w = nil;
    while (isCONS(b))
    {   w = eval(qcar(b));
        if (unwindflag != unwindNONE) break;
        b = qcdr(b);
    }
    return w;
}

LispObject interpreted4(LispObject b, LispObject a1, LispObject a2,
                        LispObject a3, LispObject a4)
{
    guard_gc();
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
    par::Shallow_bind bind_bvl(bvl, a1);
    par::Shallow_bind bind_v2(v2, a2);
    par::Shallow_bind bind_v3(v3, a3);
    par::Shallow_bind bind_v4(v4, a4);
    w = nil;
    while (isCONS(b))
    {   w = eval(qcar(b));
        if (unwindflag != unwindNONE) break;
        b = qcdr(b);
    }
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
    guard_gc();
    LispObject bvl=nil, v2=nil, v3=nil, v4=nil, v5up=nil, w, v;
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
    par::Shallow_bind bind_bvl(bvl, a1);
    par::Shallow_bind bind_v2(v2, a2);
    par::Shallow_bind bind_v3(v3, a3);
    par::Shallow_bind bind_v4(v4, a4);

    v = v5up;
    w = a5up;

    // I'm using a list here to make sure there's no resizing, and thus destruction
    std::list<par::Shallow_bind> binds_a5up;
    while (v != nil)
    {   
        binds_a5up.emplace_back(qcar(v), qcar(w)); // bind another argument
        v = qcdr(v);
        w = qcdr(w);
    }

    while (isCONS(b))
    {   w = eval(qcar(b));
        if (unwindflag != unwindNONE) break;
        b = qcdr(b);
    }

    while (not binds_a5up.empty()) {
        // make sure the elements destruct in reverse order
        binds_a5up.pop_back();
    }

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

// This function exists just so I can set a breakpoint on it!
void breakpoint()
{
}

LispObject eval(LispObject x)
{
#ifdef TRACEALL
    if (isCONS(x))
    {  LispObject ff = qcar(x);
       printf("eval: "); print(ff); // Eek! @@@@
       if (isSYMBOL(ff) &&
           (strncmp(qstring(qpname(ff)), "leq", 3)==0 ||
            strncmp(qstring(qpname(ff)), "widestring", 10)==0 ||
            strncmp(qstring(qpname(ff)), "~sizecheck", 10)==0 ||
            strncmp(qstring(qpname(ff)), "prin2*", 6)==0))
       {  printf("!!: "); print(x);
          for (LispObject a=qcdr(x); !isSYMBOL(a); a=qcdr(a))
          {  LispObject aa = eval(qcar(a));
             printf("arg: "); print(aa);
          }
          breakpoint();
       }
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
    {   LispObject v = par::symval(x);
        if (v == undefined) {
            return error1("undefined variable", x);
        }
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
                {   linepos += printf("Calling: ");
                    printf("%s\n", fname); linepos = 0;
//                  errprint(f);
                    if (unwindflag != unwindNONE) return nil;
                }
                x = (*qdefn0(f))(qlits(f));
                if (unwindflag == unwindBACKTRACE)
                {   linepos += printf("Call to ");
                    errprin(f);
                    printf(" failed\n");
                    linepos = 0;
                    return nil;
                }
                if (flags & flagTRACED)
                {   errprin(f);
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
                {   linepos += printf("Calling: ");
                    errprint(f);
                    if (unwindflag != unwindNONE) return nil;
                    linepos += printf("Arg1: ");
                    errprint(x);
                    if (unwindflag != unwindNONE) return nil;
                }
                x = (*qdefn1(f))(qlits(f), x);
                if (unwindflag == unwindBACKTRACE)
                {   linepos += printf("Call to ");
                    errprin(f);
                    printf(" failed\n");
                    linepos = 0;
                    return nil;
                }
                if (flags & flagTRACED)
                {   errprin(f);
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
                {   linepos += printf("Calling: ");
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
                {   linepos += printf("Call to ");
                    errprin(f);
                    printf(" failed\n");
                    linepos = 0;
                    return nil;
                }
                if (flags & flagTRACED)
                {   errprin(f);
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
                    {   linepos += printf("Calling: ");
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
                    {   linepos += printf("Call to ");
                        errprin(f);
                        printf(" failed\n");
                        linepos = 0;
                        return nil;
                    }
                    if (flags & flagTRACED)
                    {   errprin(f);
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
                    {   linepos += printf("Calling: ");
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
                    {   linepos += printf("Call to ");
                        errprin(f);
                        printf(" failed\n");
                        linepos = 0;
                        return nil;
                    }
                    if (flags & flagTRACED)
                    {   errprin(f);
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
                    {   linepos += printf("Calling: ");
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
                    {   linepos += printf("Call to ");
                        errprin(f);
                        printf(" failed\n");
                        linepos = 0;
                        return nil;
                    }
                    if (flags & flagTRACED)
                    {   errprin(f);
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


// @@@ interpret used to live here...

LispObject interpretspecform(LispObject lits, LispObject x)
{   // lits should be ((var) body...)
    LispObject v;
    if (!isCONS(lits)) return nil;
    v = qcar(lits);
    lits = qcdr(lits);
    if (!isCONS(v) || !isSYMBOL(v = qcar(v))) return nil;
    par::Shallow_bind(v, x);
    lits = Lprogn(nil, lits);
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

