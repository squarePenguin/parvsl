
LispObject Lstop_0(LispObject lits)
{
    exit(EXIT_SUCCESS);
    return nil;
}

LispObject Lstop_1(LispObject lits, LispObject x)
{
    exit(isFIXNUM(x) ? (int)qfixnum(x) : EXIT_SUCCESS);
    return nil;
}

int coldstart = 0;

// (restart!-lisp)       Cold restart (as for command-line "-z" option)...
// OR (restart!-lisp nil)Runs standard Read-Eval-Print loop.
// (restart!-lisp t)     Reload current heap image then uses its restart fn.
// (restart!-lisp f)     Reload heap image then invoke (f). (f!=nil, f!=t)
// (restart!-lisp (m f)) Reload heap, load module m, then call f.
// (restart!-lisp f a)   Reload heap, call (f a). a=nil is NOT special, so
//                       this case depends on the number of args passed rather
//                       than just using default values.
// (restart!-list (m f) a) Reload heap, load module m, call (f a).

LispObject Lrestart_lisp_0(LispObject data)
{
    work1 = cons(nil, nil);
    if (unwindflag == unwindNONE) unwindflag = unwindRESTART;
    return nil;
}

LispObject Lrestart_lisp_1(LispObject data, LispObject a1)
{
    work1 = cons(a1, nil);
    if (unwindflag == unwindNONE) unwindflag = unwindRESTART;
    return nil;
}

LispObject Lrestart_lisp_2(LispObject data, LispObject a1, LispObject a2)
{
    work1 = list2star(a1, a2, nil);
    if (unwindflag == unwindNONE) unwindflag = unwindRESTART;
    return nil;
}

// (preserve)           Dump image, leave restart fn unchanged, exit.
// (preserve f)         Dump image with new restart fn if f!=nil, exit.
// (preserve f b)       As above, but also change banner to b if b!=nil.
// (preserve f b nil)   As above.
// (preserve f b t)     Dump image as before, then do restart that loads
//                      the newly created image and uses its restart fn.
// (preserve f b g)     Dump image, readload it but override restart fn
//                      to be g just this time.
// (preserve f b (m g)) Dump image, reload, load-module m, call function g.
// (preserve f b g a)   Reserved to pass a as argument to the restart function.
//                      not implemented yet.

LispObject Lpreserve_0(LispObject data)
{
    restartfn = nil;
    work1 = cons(nil, nil);
    if (unwindflag == unwindNONE) unwindflag = unwindPRESERVE;
    return nil;
}

LispObject Lpreserve_1(LispObject data, LispObject a1)
{
    restartfn = a1;
    work1 = cons(nil, nil);
    if (unwindflag == unwindNONE) unwindflag = unwindPRESERVE;
    return nil;
}

LispObject Lpreserve_2(LispObject data, LispObject a1, LispObject a2)
{
    restartfn = a1;
    work1 = cons(nil, nil);
    if (unwindflag == unwindNONE) unwindflag = unwindPRESERVE;
    return nil;
}

LispObject Lpreserve_3(LispObject data, LispObject a1,
                       LispObject a2, LispObject a3)
{
    restartfn = a1;
    work1 = cons(a3, nil);
    if (unwindflag == unwindNONE)
    {   unwindflag = unwindPRESERVE;
        if (a3 != nil) unwindflag |= unwindRESTART;
    }
    return nil;
}

LispObject Lpreserve_4(LispObject data, LispObject a1,
                       LispObject a2, LispObject a3, LispObject a4)
{
    restartfn = a1;
    work1 = cons(a3, nil);
    if (unwindflag == unwindNONE)
    {   unwindflag = unwindPRESERVE;
        if (a3 != nil) unwindflag |= unwindRESTART;
    }
    return nil;
}

LispObject Lprin(LispObject lits, LispObject x)
{
    return prin(x);
}

LispObject Lprint(LispObject lits, LispObject x)
{
    return print(x);
}

LispObject Lprinc(LispObject lits, LispObject x)
{
    return princ(x);
}

LispObject Lprintc(LispObject lits, LispObject x)
{
    return printc(x);
}

LispObject Lterpri(LispObject lits)
{
    wrch('\n');
    return nil;
}

LispObject Lposn(LispObject lits)
{
    return packfixnum(linepos);
}

LispObject Lnreverse(LispObject lits, LispObject x)
{
    return nreverse(x);
}

LispObject Lexplode(LispObject lits, LispObject x)
{   int f = lispout;
    lispout = -1;
    work1 = nil;
    prin(x);
    lispout = f;
    return nreverse(work1);
}

LispObject Lexplodec(LispObject lits, LispObject x)
{   int f = lispout;
    lispout = -1;
    work1 = nil;
    princ(x);
    lispout = f;
    return nreverse(work1);
}

LispObject Lexploden(LispObject lits, LispObject x)
{   int f = lispout;
    lispout = -3;
    work1 = nil;
    prin(x);
    lispout = f;
    return nreverse(work1);
}

LispObject Lexplodecn(LispObject lits, LispObject x)
{   int f = lispout;
    lispout = -3;
    work1 = nil;
    princ(x);
    lispout = f;
    return nreverse(work1);
}

LispObject Lreadch(LispObject lits)
{   char ch[4];
    if (curchar == EOF) return eofsym;
    ch[0] = par::symval(symlower) != nil ? tolower(curchar) :
            par::symval(symraise) != nil ? toupper(curchar) : curchar;
    ch[1] = 0;
    curchar = rdch();
    return lookup(ch, 1, 1);
}

LispObject Lreadline(LispObject lits)
{   char ch[200];
    uintptr_t n = 0;
    if (curchar == '\n') curchar = rdch();
    while (curchar != '\n' && curchar != EOF)
    {   if (n < sizeof(ch)-1) ch[n++] = curchar;
        curchar = rdch();
    }
    if (n == 0 && curchar == EOF) return eofsym;
    ch[n] = 0;
    return lookup(ch, n, 1);
}

LispObject Lread(LispObject lits)
{
    return readS();
}

LispObject Lcompress(LispObject lits, LispObject x)
{   int f = lispin;
    LispObject r, save_cursym;
    int savetype = symtype, savech = curchar;
    lispin = -1;
    symtype = '?';
    curchar = '\n';
    save_cursym = cursym;
    work1 = x;
    r = readS();
    lispin = f;
    cursym = save_cursym;
    symtype = savetype;
    curchar = savech;
    return r;
}

LispObject Lrds(LispObject lits, LispObject x)
{   int old = lispin;
    if (x == nil) x = packfixnum(3);
    if (isFIXNUM(x))
    {   int n = (int)qfixnum(x);
        if (0 <= n && n < MAX_LISPFILES && lispfiles[n] != NULL &&
            (file_direction & (1<<n)) == 0)
        {   filecurchar[old] = curchar;
            filesymtype[old] = symtype;
            lispin = n;
            curchar = filecurchar[n];
            symtype = filesymtype[n];
            if (curchar == EOF) curchar = '\n';
            if (symtype == EOF) symtype = '?';
            return packfixnum(old);
        }
    }
    return error1("rds failed", x);
}

LispObject Lwrs(LispObject lits, LispObject x)
{   int old = lispout;
    if (x == nil) x = packfixnum(1);
    if (isFIXNUM(x))
    {   int n = (int)qfixnum(x);
        if (0 <= n && n < MAX_LISPFILES && lispfiles[n] != NULL &&
            (file_direction & (1<<n)) != 0)
        {   lispout = n;
            return packfixnum(old);
        }
    }
    return error1("wrs failed", x);
}

INLINE constexpr unsigned int LONGEST_LEGAL_FILENAME = 1000;
char filename[LONGEST_LEGAL_FILENAME];
static char imagename[LONGEST_LEGAL_FILENAME];
INLINE const char *programDir = ".";

LispObject Lget_lisp_directory(LispObject lits)
{   return makestring(programDir, strlen(programDir));
}

LispObject Lopen(LispObject lits, LispObject x, LispObject y)
{   FILE *f;
    int n, how = 0;
    char *p;
    if (isSYMBOL(x)) x = qpname(x);
    if (!isSTRING(x) ||
        !((y == input && (how=1)!=0) ||
          (y == output && (how=2)!=0) ||
          (y == pipe && (how=3)!=0)))
        return error1("bad arg for open", cons(x, y));
// If the filename that is passed is something like "$word/rest" then I look
// for a Lisp variable "@word" and look at its value. If that value is a
// string I use it for to replace the "$word" part, leaving "/rest" unchanged. 
    if (*qstring(x)=='$' && (p=strchr(qstring(x), '/'))!=NULL)
    {   _s_=snprintf(filename, sizeof(filename), "@%.*s", (int)(p-qstring(x))-1, 1+qstring(x));
        lits = par::symval(lookup(filename, strlen(filename), 0));
        if (isSTRING(lits)) _s_=snprintf(filename, sizeof(filename), "%.*s%.*s",
           (int)veclength(qheader(lits)), qstring(lits),
           (int)(veclength(qheader(x)) - (p-qstring(x))), p);
        else _s_=snprintf(filename, sizeof(filename), "%.*s", (int)veclength(qheader(x)), qstring(x));
    }
    else _s_=snprintf(filename, sizeof(filename), "%.*s", (int)veclength(qheader(x)), qstring(x));
#ifdef __WIN32__
//  while (strchr(filename, '/') != NULL) *strchr(filename, '/') = '\\';
#endif // __WIN32__
    if (how == 3) f = popen(filename, "w");
    else f = fopen(filename, (how == 1 ? "r" : "w"));
    if (f == NULL) return error1("file could not be opened", x);
    for (n=4; n<MAX_LISPFILES && lispfiles[n]!=NULL; n++);
    if (n<MAX_LISPFILES)
    {   lispfiles[n] = f;
        if (y != input) file_direction |= (1 << n);
        filecurchar[n] = '\n';
        filesymtype[n] = '?';
        return packfixnum(n);
    }
    return error1("too many open files", x);
}

LispObject Lfilep(LispObject lits, LispObject x)
{   FILE *f;
    char *p;
    if (isSYMBOL(x)) x = qpname(x);
    if (!isSTRING(x))
        return error1("bad arg for filep", x);
    if (*qstring(x)=='$' && (p=strchr(qstring(x), '/'))!=NULL)
    {   _s_=snprintf(filename, sizeof(filename), "@%.*s", (int)(p-qstring(x))-1, 1+qstring(x));
        lits = par::symval(lookup(filename, strlen(filename), 0));
        if (isSTRING(lits)) _s_=snprintf(filename, sizeof(filename), "%.*s%.*s",
           (int)veclength(qheader(lits)), qstring(lits),
           (int)(veclength(qheader(x)) - (p-qstring(x))), p);
        else _s_=snprintf(filename, sizeof(filename), "%.*s", (int)veclength(qheader(x)), qstring(x));
    }
    else _s_=snprintf(filename, sizeof(filename), "%.*s", (int)veclength(qheader(x)), qstring(x));
#ifdef __WIN32__
//  while (strchr(filename, '/') != NULL) *strchr(filename, '/') = '\\';
#endif // __WIN32__
    f = fopen(filename, "r");
    if (f == NULL) return nil;
    fclose(f);
    return lisptrue;
}

LispObject Lopen_module(LispObject lits, LispObject x, LispObject y)
{   FILE *f;
    int n, how = 0;
    if (isSYMBOL(x)) x = qpname(x);
    if (!isSTRING(x) ||
        !((y == input && (how=1)!=0) ||
          (y == output && (how=2)!=0)))
        return error1("bad arg for open-module", cons(x, y));
    _s_=snprintf(filename, sizeof(filename), "%s.modules/%.*s.fasl", imagename,
                      (int)veclength(qheader(x)), qstring(x));
#ifdef __WIN32__
//  while (strchr(filename, '/') != NULL) *strchr(filename, '/') = '\\';
#endif // __WIN32__
    f = fopen(filename, (how == 1 ? "r" : "w"));
    if (f == NULL)
    {   printf("\n@@@Filename is <%s>, how=%d\n", filename, how);
        return error1("file could not be opened", x);
    }
    for (n=4; n<MAX_LISPFILES && lispfiles[n]!=NULL; n++);
    if (n<MAX_LISPFILES)
    {   lispfiles[n] = f;
        if (y != input) file_direction |= (1 << n);
        return packfixnum(n);
    }
    return error1("too many open files", x);
}

LispObject Lclose(LispObject lits, LispObject x)
{
    if (isFIXNUM(x))
    {   int n = (int)qfixnum(x);
        if (n > 3 && n < MAX_LISPFILES)
        {   if (lispin == n) Lrds(nil, packfixnum(3));
            if (lispout == n) Lwrs(nil, packfixnum(1));
            if (lispfiles[n] != NULL) fclose(lispfiles[n]);
            lispfiles[n] = NULL;
            file_direction &= ~(1<<n);
        }
    }
    return nil;
}

// flag on next line for desparate debugging
static bool showallreads = false;

void readevalprint(int loadp)
{   while (symtype != EOF)
    {   LispObject r;
        // I make sure here that echo is locally bound here.
        // Otherwise threads would content over the global value.
        {
            par::Shallow_bind(echo, par::symval(echo));
            unwindflag = unwindNONE;
            if (loadp) par::symval(echo) = nil;
            if (showallreads) par::symval(echo) = lisptrue;
            backtraceflag = backtraceHEADER | backtraceTRACE;
            r = readS();
        }

        if (showallreads)
        {   printf("item read was: ");
            print(r);
        }
        fflush(stdout);
        if (unwindflag != unwindNONE) /* Do nothing */ ;
        else if (loadp || par::symval(dfprint) == nil ||
            (isCONS(r) && (qcar(r) == lookup("rdf", 3, 2) ||
                           qcar(r) == lookup("faslend", 7, 2))))
        {
            r = eval(r);
            if (showallreads || (unwindflag == unwindNONE && !loadp))
            {
                linepos += printf("Value: ");
#ifdef DEBUG
                if (logfile != NULL) fprintf(logfile, "Value: ");
#endif // DEBUG
                print(r);
                fflush(stdout);
            }
        }
        else
        {   r = cons(r, nil);
            if (unwindflag == unwindNONE) Lapply(nil, par::symval(dfprint), r);
        }
        if ((unwindflag & (unwindPRESERVE | unwindRESTART)) != 0) return;
    }
}

LispObject Lrdf(LispObject lits, LispObject x)
{   int f, f1;
    f1 = Lopen(nil, x, input);
    if (unwindflag != unwindNONE) return nil;
    f = Lrds(nil, f1);
    readevalprint(0);
    Lrds(nil, f);
    Lclose(nil, f1);
    printf("+++ End of rdf\n");
    return nil;
}

LispObject Lload_module(LispObject lits, LispObject x)
{   int f, f1;
    f1 = Lopen_module(nil, x, input);
    if (unwindflag != unwindNONE)
    {   printf("+++ Module could not be opened\n");
        return nil;
    }
    f = Lrds(nil, f1);
    readevalprint(1);
    if (unwindflag != unwindNONE) printf("+++ Error loading module\n");
    Lrds(nil, f);
    Lclose(nil, f1);
    return nil;
}

LispObject Ltrace(LispObject lits, LispObject x)
{
    while (isCONS(x))
    {   if (isSYMBOL(qcar(x))) qflags(qcar(x)) |= flagTRACED;
        x = qcdr(x);
    }
    return nil;
}

LispObject Luntrace(LispObject lits, LispObject x)
{
    while (isCONS(x))
    {   if (isSYMBOL(qcar(x))) qflags(qcar(x)) &= ~flagTRACED;
        x = qcdr(x);
    }
    return nil;
}

LispObject Lerror_0(LispObject lits)
{
    return error1("error function called", nil);
}

LispObject Lerror_1(LispObject lits, LispObject x)
{
    return error1("error function called", x);
}

LispObject Lerror_2(LispObject lits, LispObject x, LispObject y)
{
    return error1("error function called", list2star(x,y,nil));
}

// This flag lets me make every error noisy. For desparate debugging
static bool debugFlag = true;

LispObject Lerrorset_3(LispObject lits, LispObject a1,
                       LispObject a2, LispObject a3)
{   int save = backtraceflag;
    backtraceflag = 0;
    if (a2 != nil || debugFlag) backtraceflag |= backtraceHEADER;
    if (a3 != nil || debugFlag) backtraceflag |= backtraceTRACE;
    if (debugFlag) backtraceflag = backtraceHEADER | backtraceTRACE;
    a1 = eval(a1);
    if (unwindflag == unwindERROR ||
        unwindflag == unwindBACKTRACE)
    {   unwindflag = unwindNONE;
        a1 = nil;
    }
    else a1 = cons(a1, nil);
    backtraceflag = save;
    return a1;
}


LispObject Lerrorset_2(LispObject lits, LispObject a1, LispObject a2)
{   return Lerrorset_3(lits, a1, a2, nil);
}

LispObject Lerrorset_1(LispObject lits, LispObject a1)
{   return Lerrorset_3(lits, a1, nil, nil);
}

