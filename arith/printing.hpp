INLINE const int printPLAIN = 1;
INLINE const int printESCAPES = 2;
INLINE const int printHEX = 4;

// I suspect that linelength and linepos need to be maintained
// independently for each output stream. At present that is not
// done. And also blank_pending.
int linelength = 80, linepos = 0, printflags = printESCAPES;
bool blank_pending = false;

LispObject Llinelength(LispObject lits, LispObject a1)
{   int oo = linelength;
    if (isFIXNUM(a1))
    {   int nn = qfixnum(a1);
        if (nn > 0 && nn < 1000000) linelength = nn;
    }
    return packfixnum(oo);
}
    

#ifdef DEBUG
FILE *lispfiles[MAX_LISPFILES], *logfile = NULL;
#else // DEBUG
FILE *lispfiles[MAX_LISPFILES];
#endif // DEBUG
int32_t file_direction = 0, interactive = 0;
int lispin = 0, lispout = 1;
int filecurchar[MAX_LISPFILES], filesymtype[MAX_LISPFILES];

void wrch1(int c)
{   //????if (c == '\r') return;
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
        else if (c == '\t') linepos = (linepos + 8) & ~7;
        else linepos++;
    }
}


void wrch(int ch)
{   if (blank_pending)
    {   if (lispout < 0 || linepos != 0) wrch1(' ');
        blank_pending = false;
    }
    wrch1(ch);
}

static bool stdin_tty = false;
static EditLine *el_struct;
static History *el_history;
static HistEvent el_history_event;

INLINE const int INPUT_LINE_SIZE = 256;
static char input_line[INPUT_LINE_SIZE];
static size_t input_ptr = 0, input_max = 0;
char the_prompt[80] = "> ";


LispObject Lsetpchar(LispObject lits, LispObject a)
{   LispObject r = makestring(the_prompt, strlen(the_prompt));
    if (isSYMBOL(a)) a = qpname(a);
    if (!isSTRING(a)) return error1("bad arg to setpchar", a);
    uintptr_t len = veclength(qheader(a));
    if (len > sizeof(the_prompt)-1) len = sizeof(the_prompt)-1;
    int n = snprintf(the_prompt, sizeof(the_prompt), "%.*s",
                     (int)len, qstring(a));
    if (n<0 || (unsigned int)n>=sizeof(the_prompt)) strcpy(the_prompt, "> ");
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
                const char *s = el_gets(el_struct, &n);
                if (s == NULL) return EOF;
                // Need to manually enter line to history.
                history(el_history, &el_history_event, H_ENTER, s);
                if (n > INPUT_LINE_SIZE-1) n = INPUT_LINE_SIZE-1;
                strncpy(input_line, s, n);
                input_line[INPUT_LINE_SIZE-1] = 0;
                input_ptr = 0;
                input_max = n;
            }
            c = input_line[input_ptr++];
        }
        else c = getc(lispfiles[lispin]);
        if (c != EOF && qvalue(echo) != nil) wrch(c);
        return c;
    }
}

int gensymcounter = 1;

void checkspace(int n)
{   if (linepos + n + (blank_pending ? 1 : 0) > linelength &&
        lispout != -1 &&
        lispout != -2 &&
        lispout != -3) wrch('\n');
}

char printbuffer[32];

//
// I want the floating point print style that I use to match the
// one used by PSL rather carefully. So here is some code so that
// everything I do about it is in one place.
//

intptr_t print_precision = 6;

LispObject Lprint_precision(LispObject lits, LispObject a)
{   intptr_t old = print_precision;
    if (a == nil) return packfixnum(old);
    if (!isFIXNUM(a)) error1("print-precision", a);
    print_precision = qfixnum(a);
    if (print_precision > 36) print_precision = 36;
    else if (print_precision < 1) print_precision = 15;
    return packfixnum(old);
}


//
// Two crummy little functions to delete and insert chars from strings.
//

static void char_del(char *s)
{   while (*s != 0)
    {   *s = *(s+1);
        s++;
    }
}

static void char_ins(char *s, int c)
{   char *p = s;
    while (*p != 0) p++;
    while (p != s)
    {   *(p+1) = *p;
        p--;
    }
    *(s+1) = *s;
    *s = c;
//  printf("After char_ins \"%s\"\n", s);
}

static void fp_sprint(char *buff, double x, int prec, int xmark)
{
// Note that I am assuming IEEE arithmetic here so the tricks that I use
// to detect -0.0, NaN and infinities ought to be OK. Just remember that
// -0.0 is equal to 0.0 and not less than it, so the simple test
// "x < 0.0" will not pick up the case of -0.0.
    if (x == 0.0)
    {   if (xmark != 'e')
        {   if (1.0/x < 0.0) sprintf(buff, "-0.0%c+00", xmark);
            else sprintf(buff, "0.0%c+00", xmark);
        }
        else if (1.0/x < 0.0) strcpy(buff, "-0.0");
        else strcpy(buff, "0.0");
        return;
    }
    if (x != x)
    {   strcpy(buff, "NaN"); // The length of the NaN will not be visible
        return;
    }
    if (x == 2.0*x)
    {   if (x < 0.0) strcpy(buff, "minusinf"); // Length of infinity not shown.
        else strcpy(buff, "inf");
        return;
    }
// Limit the precision used for printing based on the type of float involved.
    switch (xmark)
    {   case 's': case 'S':
            if (prec > 7) prec = 7;
            break;
        case 'f': case 'F':
            if (prec > 8) prec = 8;
            break;
        default:
            if (prec > 17) prec = 17;
    }
    if (x < 0.0)
    {   *buff++ = '-';
        x = -x;
    }
// Now I just have strictly positive values to worry about
    sprintf(buff, "%.*g", prec, x);
// I will allow for pathologically bad versions of sprintf...
    if (*buff == '+') char_del(buff);      // Explicit "+" not wanted
    if (*buff == '.') char_ins(buff, '0'); // turn .nn to 0.nn
    else if (*buff == 'e')                 // turn Ennn to 0.0Ennn
    {   char_ins(buff, '0');
        char_ins(buff, '.');
        char_ins(buff, '0');
    }
// I now have at lesst one digit before any "." or "E"
    while (*buff != 0 && *buff != '.' && *buff != 'e') buff++;
    if (*buff == 'e') *buff = xmark;    // force style of exponent mark
    if (*buff == 0 || *buff == xmark)   // ddd to ddd.0
    {   char_ins(buff, '0');            // and dddEnnn to ddd.0Ennn
        char_ins(buff, '.');
    }
// I now have a "." in there
    while (*buff != 0 && *buff != 'e' && *buff != xmark) buff++;
    if (*(buff-1) == '.') char_ins(buff++, '0');// ddd. to ddd.0
    while (*(buff-1) == '0' &&                  // ddd.nnn0 to ddd.nnn
           *(buff-2) != '.') char_del(--buff);
    if (*buff == 0)
    {   if (xmark != 'e')
        {   *buff++ = xmark;
            *buff++ = '+';
            *buff++ = '0';
            *buff++ = '0';
            *buff = 0;
        }
        return; // no E present. Add exponent mark if not default type
    }
    if (xmark != 'e') *buff = xmark; 
    buff++;
// At this stage I am looking at the exponent part
    if (*buff == 0) strcpy(buff, "+00");
    else if (isdigit((unsigned char)*buff)) char_ins(buff, '+');
// Exponent should now start with explicit + or - sign
    buff++;
// Force exponent to have at least 2 digits
    if (*(buff+1) == 0) char_ins(buff, '0');
// Three-digit exponent with leading zero gets trimmed here
    else if (*buff == '0' && *(buff+2) != 0) char_del(buff);
}

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
            {
// Dealing with line-end is a bit delicate, so I will write about it here.
// The same issues arose earlier regarding "(a . b)" style output.
// When I have got "... abd def" and am about to want to display " ghi" I
// want to output a blank unless I am at the end of the line. But there is
// no merit in displaying the blank unless something will fit on the line
// after it! A consequence of that is that I can not tell if I need to
// print a blank unril I have assessed the width of whatever will follow it!
// So when a blank is or may be due I set a flag to indicate that and wrch and
// checkspace will need to interact with it.
                if (sep == ' ') blank_pending = true;
                else
                {   checkspace(1);
                    wrch(sep);
                }
                sep = ' ';
                internalprint(qcar(x));
                x = qcdr(x);
            }
            if (x != nil)
            {   blank_pending = true;
                checkspace(1);
                wrch('.');
                blank_pending = true;
                internalprint(x);
            }
            checkspace(1);
            wrch(')');
            return;
        case tagSYMBOL:
            pn = qpname(x);
            if (pn == nil)
            {   int len = snprintf(printbuffer, sizeof(printbuffer),
                                   "g%.3d", gensymcounter++);
                if (len<0 || (unsigned int)len>=sizeof(printbuffer))
                    pn = makestring("?gensym?", 8);
                else pn = makestring(printbuffer, len);
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
                        {   LispObject ss =
                                (printflags&printHEX) != 0 ?
                                     arithlib::bignum_to_string_hex(x) :
                                     arithlib::bignum_to_string(x);
                            len = veclength(qheader(ss));
                            s = qstring(ss);
                            checkspace(len);
                            for (i=0; i<len; i++) wrch(s[i]);
                            arithlib::abandon_string(ss);
                        }
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
                        {   if (sep==' ') blank_pending = true;
                            else
                            {   checkspace(1);
                                wrch(sep);
                            }
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
                fp_sprint(printbuffer, d, print_precision, 'e');
            }
            len = strlen(printbuffer);
            for (i=0; i<len; i++) wrch(printbuffer[i]);
            return;
        case tagFIXNUM:
            if ((printflags & printHEX) != 0)
                snprintf(printbuffer, sizeof(printbuffer),
                         "%" PRIx64, (int64_t)qfixnum(x));
            else snprintf(printbuffer, sizeof(printbuffer),
                         "%" PRId64, (int64_t)qfixnum(x));
            checkspace(len = strlen(printbuffer));
            for (i=0; i<len; i++) wrch(printbuffer[i]);
            return;
        default:
//case tagFORWARD:
//case tagHDR:
//          snprintf(printbuffer, sizeof(printbuffer), "??%#" PRIxPTR "??\n", x);
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

LispObject printc(LispObject a)
{   printflags = printPLAIN;
    internalprint(a);
    wrch('\n');
    return a;
}

LispObject prinhex(LispObject a)
{   printflags = printESCAPES | printHEX;
    internalprint(a);
    return a;
}

LispObject princhex(LispObject a)
{   printflags = printPLAIN | printHEX;
    internalprint(a);
    return a;
}

LispObject printhex(LispObject a)
{   printflags = printESCAPES | printHEX;
    internalprint(a);
    wrch('\n');
    return a;
}

LispObject printchex(LispObject a)
{   printflags = printPLAIN | printHEX;
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

