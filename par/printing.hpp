INLINE constexpr int printPLAIN = 1;
INLINE constexpr int printESCAPES = 2;

// I suspect that linelength and linepos need to be maintained
// independently for each output stream. At present that is not
// done.
thread_local int linelength = 80, linepos = 0, printflags = printESCAPES;

#ifdef DEBUG
FILE *lispfiles[MAX_LISPFILES], *logfile = NULL;
#else // DEBUG
FILE *lispfiles[MAX_LISPFILES];
#endif // DEBUG
int32_t file_direction = 0, interactive = 0;
thread_local int lispin = 0, lispout = 1;
int filecurchar[MAX_LISPFILES], filesymtype[MAX_LISPFILES];

void wrch(int c)
{
    if (lispout == -1)
    {   char w[4];
// This bit is for the benefit of explode and explodec.
        LispObject r;
        w[0] = c; w[1] = 0;
        r = lookup(w, 1, 1);
        work1 = cons(r, work1);
    }
    else if (lispout == -3)
    {
// This bit is for the benefit of exploden and explodecn.
        LispObject r;
        r = packfixnum(c & 0xff);
        work1 = cons(r, work1);
    }
    else if (lispout == -2) boffo[boffop++] = c;
    else
    {   putc(c, lispfiles[lispout]);
#ifdef DEBUG
        if (logfile != NULL)
        {   putc(c, logfile);
            if (c == '\n')
            {   fprintf(logfile, "%d]", lispout);
            }
        }
#endif // DEBUG
        if (c == '\n')
        {   linepos = 0;
            fflush(lispfiles[lispout]);
        }
        else linepos++;
    }
}

static bool stdin_tty = false;
static EditLine *el_struct;
static History *el_history;
static HistEvent el_history_event;

INLINE constexpr int INPUT_LINE_SIZE = 256;
thread_local static char input_line[INPUT_LINE_SIZE];
thread_local static size_t input_ptr = 0, input_max = 0;
char the_prompt[80] = "> ";

// gcc moans if the value of snprintf is unused and there is any chance that
// truncation arose. To get rid of the warning message I dump the value of
// snprintf somewhere were in fact I will not look at it!.

static volatile int _s_;

LispObject Lsetpchar(LispObject lits, LispObject a)
{   LispObject r = makestring(the_prompt, strlen(the_prompt));
    if (isSYMBOL(a)) a = qpname(a);
    if (!isSTRING(a)) return error1("bad arg to setpchar", a);
    uintptr_t len = veclength(qheader(a));
    if (len > sizeof(the_prompt)-1) len = sizeof(the_prompt)-1;
    _s_=snprintf(the_prompt, sizeof(the_prompt), "%.*s", (int)len, qstring(a));
    return r;
}

int rdch()
{   LispObject w;
    if (lispin == -1)
    {   if (!isCONS(work1)) return EOF;
        w = qcar(work1);
        work1 = qcdr(work1);
        if (isFIXNUM(w)) return (qfixnum(w) & 0xff);
        if (isSYMBOL(w)) w = qpname(w);
        if (!isSTRING(w)) return EOF;
        return *qstring(w);
    }
    else
    {   int c;
        if (lispfiles[lispin] == stdin && stdin_tty)
        {   if (input_ptr >= input_max)
            {   int n = -1;
                const char *s;
                
                {
                    par::Gc_guard guard;
                    s = el_gets(el_struct, &n);
                }

                // Need to manually enter line to history.
                history(el_history, &el_history_event, H_ENTER, s);
                if (s == NULL) return EOF;
                if (n > INPUT_LINE_SIZE-1) n = INPUT_LINE_SIZE-1;
                strncpy(input_line, s, n);
                input_line[INPUT_LINE_SIZE-1] = 0;
                input_ptr = 0;
                input_max = n;
            }
            c = input_line[input_ptr++];
        }
        else c = getc(lispfiles[lispin]);
        if (c != EOF && par::symval(echo) != nil) wrch(c);
        return c;
    }
}

int gensymcounter = 1;

void checkspace(int n)
{   if (linepos + n >= linelength && lispout != -1 && lispout != -3) wrch('\n');
}

thread_local char printbuffer[32];

extern LispObject call1(const char *name, LispObject a1);
extern LispObject call2(const char *name, LispObject a1, LispObject a2);

void internalprint(LispObject x)
{   int sep = '(', esc;
    uintptr_t i, len;
    char *s;
    LispObject pn;
    switch (x & TAGBITS)
    {   case tagCONS:
            if (x == 0)    // can only occur in case of bugs here.
            {   wrch('#');
                return;
            }
            while (isCONS(x))
            {   i = printflags;
// With the software bignum scheme this is messy! a data structure of the
// for (~bignum d1 d2 ...) must be interpreted as a number not a list. But
// then one gets the case (a b !~bignum x y) and that maybe needs to render
// as (a b . NUMBER).
                if (qcar(x) == bignum &&
                    (pn = call1("~big2str", qcdr(x))) != NULLATOM &&
                    pn != nil)
                {   printflags = printPLAIN;
                    if (sep == ' ')
                    {   checkspace(3);
                        wrch(' '); wrch('.'); wrch(' ');
                    }
                    internalprint(pn);
                    if (sep == ' ')
                    {   checkspace(1);
                        wrch(')');
                    }
                    printflags = i;
                    return;
                }
                printflags = i;
                checkspace(1);
                if (linepos != 0 || sep != ' ' || lispout < 0) wrch(sep);
                sep = ' ';
                internalprint(qcar(x));
                x = qcdr(x);
            }
            if (x != nil)
            {   checkspace(3);
                wrch(' '); wrch('.'); wrch(' ');
                internalprint(x);
            }
            checkspace(1);
            wrch(')');
            return;
        case tagSYMBOL:
            pn = qpname(x);
            if (pn == nil)
            {   int len = snprintf(printbuffer, sizeof(printbuffer), "g%.3d", gensymcounter++);
                pn = makestring(printbuffer, len);
                qpname(x) = pn;
            }
            len = veclength(qheader(pn));
            s = qstring(pn);
            if ((printflags & printESCAPES) == 0)
            {   uintptr_t i;
                checkspace(len);
                for (i=0; i<len; i++) wrch(s[i]);
            }
            else if (len != 0)
            {   esc = 0;
                if (!islower((int)s[0])) esc++;
                for (i=1; i<len; i++)
                {   if (!islower((int)s[i]) &&
                        !isdigit((int)s[i]) &&
                        s[i]!='_') esc++;
                }
                checkspace(len + esc);
                if (!islower((int)s[0])) wrch('!');
                wrch(s[0]);
                for (i=1; i<len; i++)
                {   if (!islower((int)s[i]) &&
                        !isdigit((int)s[i]) &&
                        s[i]!='_')
                        wrch('!');
                    wrch(s[i]);
                }
            }
            return;
        case tagATOM:
            if (x == NULLATOM)
            {   checkspace(5);
                wrch('#'); wrch('n'); wrch('u'); wrch('l'); wrch('l');
                return;
            }
            else switch (qheader(x) & TYPEBITS)
                {   case typeSTRING:
                        len = veclength(qheader(x));
                        s = qstring(x);
#define RAWSTRING       s
                        if ((printflags & printESCAPES) == 0)
                        {   uintptr_t i;
                            checkspace(len);
                            for (i=0; i<len; i++) wrch(RAWSTRING[i]);
                        }
                        else
                        {   esc = 2;
                            for (i=0; i<len; i++)
                                if (RAWSTRING[i] == '"') esc++;
                            checkspace(len+esc);
                            wrch('"');
                            for (i=0; i<len; i++)
                            {   if (RAWSTRING[i] == '"') wrch('"');
                                wrch(RAWSTRING[i]);
                            }
                            wrch('"');
                        }
#undef RAWSTRING
                        return;
                    case typeBIGNUM:
// At present the case typeBIGNUM is merely a fixed-precision 64-bit case,
// which is not very adventurous!
                        _s_=snprintf(printbuffer, sizeof(printbuffer), "%" PRId64, qint64(x));
                        checkspace(len = strlen(printbuffer));
                        for (i=0; i<len; i++) wrch(printbuffer[i]);
                        return;
                    case typeVEC:
                    case typeEQHASH:
                    case typeEQHASHX:
                        if ((qheader(x) & TYPEBITS) == typeEQHASH)
                        {   checkspace(3);
                            wrch('#');
                            wrch('H');
                        }
                        else if ((qheader(x) & TYPEBITS) == typeEQHASHX)
                        {   checkspace(3);
                            wrch('#');
                            wrch('h');
                        }
                        sep = '[';
                        len = veclength(qheader(x))/sizeof(LispObject);
                        if (len==0)
                        {   checkspace(1);
                            wrch('[');
                        }
                        else for (i=0; i<len; i++)
                        {   checkspace(1);
                            wrch(sep);
                            sep = ' ';
                            internalprint(elt(x, i));
                        }
                        checkspace(1);
                        wrch(']');
                        return;
                    default:
                        //case typeSYM:
                        // also the spare codes!
                        assert(0);
                }
        case tagFLOAT:
            {   double d =  *((double *)(x - tagFLOAT));
                if (isnan(d)) strcpy(printbuffer, "NaN");
                else if (isfinite(d)) _s_=snprintf(printbuffer, sizeof(printbuffer), "%.14g", d);
                else strcpy(printbuffer, "inf");
            }
            s = printbuffer;
// The C printing of floating point values is not to my taste, so I (slightly)
// asjust the output here...
            if (*s == '+' || *s == '-') s++;
            while (isdigit((int)*s)) s++;
            if (*s == 0 || *s == 'e')  // No decimal point present!
            {   len = strlen(s);
                while (len != 0)       // Move existing text up 2 places
                {   s[len+2] = s[len];
                    len--;
                }
                s[2] = s[0];
                s[0] = '.'; s[1] = '0'; // insert ".0"
            }
            checkspace(len = strlen(printbuffer));
            for (i=0; i<len; i++) wrch(printbuffer[i]);
            return;
        case tagFIXNUM:
            _s_=snprintf(printbuffer, sizeof(printbuffer), "%" PRId64, (int64_t)qfixnum(x));
            checkspace(len = strlen(printbuffer));
            for (i=0; i<len; i++) wrch(printbuffer[i]);
            return;
        default:
//case tagFORWARD:
//case tagHDR:
//          _s_=snprintf(printbuffer, sizeof(printbuffer), "??%#" PRIxPTR "??\n", x);
//          checkspace(len = strlen(printbuffer));
//          for (i=0; i<len; i++) wrch(printbuffer[i]);
            assert(0);
    }
}

LispObject prin(LispObject a)
{   printflags = printESCAPES;
    internalprint(a);
    return a;
}

LispObject princ(LispObject a)
{   printflags = printPLAIN;
    internalprint(a);
    return a;
}

LispObject print(LispObject a)
{   printflags = printESCAPES;
    internalprint(a);
    wrch('\n');
    return a;
}

void errprint(LispObject a)
{   int saveout = lispout, saveflags = printflags;
    lispout = 1; printflags = printESCAPES;
    internalprint(a);
    wrch('\n');
    lispout = saveout; printflags = saveflags;
}

void errprin(LispObject a)
{   int saveout = lispout, saveflags = printflags;
    lispout = 1; printflags = printESCAPES;
    internalprint(a);
    lispout = saveout; printflags = saveflags;
}

LispObject printc(LispObject a)
{   printflags = printPLAIN;
    internalprint(a);
    wrch('\n');
    return a;
}

