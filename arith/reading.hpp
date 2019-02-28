
int curchar = '\n', symtype = 0;

int hexval(int n)
{   if (isdigit(n)) return n - '0';
    else if ('a' <= n && n <= 'f') return n - 'a' + 10;
    else if ('A' <= n && n <= 'F') return n - 'A' + 10;
    else return 0;
}
static LispObject Nminus(LispObject a);
static LispObject Nplus2(LispObject a, LispObject b);
static LispObject Ntimes2(LispObject a, LispObject b);

LispObject token()
{   symtype = 'a';           // Default result is an atom.
    while (1)
    {   while (curchar == ' ' ||
               curchar == '\t' ||
               curchar == '\n') curchar = rdch(); // Skip whitespace
// Discard comments from "%" to end of line.
        if (curchar == '%')
        {   while (curchar != '\n' &&
                   curchar != EOF) curchar = rdch();
            continue;
        }
        break;
    }
    if (curchar == EOF)
    {   symtype = curchar;
        return NULLATOM;     // End of file marker.
    }
    if (curchar == '(' || curchar == '.' ||
        curchar == ')' || curchar == '\'' ||
        curchar == '`' || curchar == ',')
    {   symtype = curchar;   // Lisp special characters.
        curchar = rdch();
        if (symtype == ',' && curchar == '@')
        {   symtype = '@';
            curchar = rdch();
        }
        return NULLATOM;
    }
    boffop = 0;
    if (isalpha(curchar) || curchar == '!') // Start a symbol.
    {   while (isalpha(curchar) ||
               isdigit(curchar) ||
               curchar == '_' ||
               curchar == '!')
        {   if (curchar == '!') curchar = rdch();
            else if (curchar != EOF && qvalue(symlower) != nil) curchar = tolower(curchar);
            else if (curchar != EOF && qvalue(symraise) != nil) curchar = toupper(curchar);
            if (curchar != EOF)
            {   if (boffop < BOFFO_SIZE) boffo[boffop++] = curchar;
                curchar = rdch();
            }
        }
        boffo[boffop] = 0;
        return lookup(boffo, boffop, 1);
    }
    if (curchar == '"')                     // Start a string
    {   curchar = rdch();
        while (1)
        {   while (curchar != '"' && curchar != EOF)
            {   if (boffop < BOFFO_SIZE) boffo[boffop++] = curchar;
                curchar = rdch();
            }
// Note that a double-quote can be repeated within a string to denote
// a string with that character within it. As in
//   "abc""def"   is a string with contents   abc"def.
            if (curchar != EOF) curchar = rdch();
            if (curchar != '"') break;
            if (boffop < BOFFO_SIZE) boffo[boffop++] = curchar;
            curchar = rdch();
        }
        return makestring(boffo, boffop);
    }
    if (curchar == '+' || curchar == '-')
    {   boffo[boffop++] = curchar;
        curchar = rdch();
// + and - are treated specially, since if followed by a digit they
// introduce a (signed) number, but otherwise they are treated as punctuation.
        if (!isdigit(curchar))
        {   boffo[boffop] = 0;
            return lookup(boffo, boffop, 1);
        }
    }
// Note that in some cases after a + or - I drop through to here.
    if (curchar == '0' && boffop == 0)  // "0" without a sign in front
    {   boffo[boffop++] = curchar;
        curchar = rdch();
        if (curchar == 'x' || curchar == 'X') // Ahah - hexadecimal input
        {   LispObject r;
            boffop = 0;
            curchar = rdch();
            while (isxdigit(curchar))
            {   if (boffop < BOFFO_SIZE) boffo[boffop++] = curchar;
                curchar = rdch();
            }
            r = packfixnum(0);
            boffop = 0;
            while (boffo[boffop] != 0)
            {   r = Nplus2(Ntimes2(packfixnum(16), r),
                           packfixnum(hexval(boffo[boffop++])));
            }
            return r;
        }
    }
    if (isdigit(curchar) || (boffop == 1 && boffo[0] == '0'))
    {   while (isdigit(curchar))
        {   if (boffop < BOFFO_SIZE) boffo[boffop++] = curchar;
            curchar = rdch();
        }
// At this point I have a (possibly signed) integer. If it is immediately
// followed by a "." then a floating point value is indicated.
        if (curchar == '.')
        {   symtype = 'f';
            if (boffop < BOFFO_SIZE) boffo[boffop++] = curchar;
            curchar = rdch();
            while (isdigit(curchar))
            {   if (boffop < BOFFO_SIZE) boffo[boffop++] = curchar;
                curchar = rdch();
            }
// To make things tidy If I have a "." not followed by any digits I will
// insert a "0".
            if (!isdigit((int)boffo[boffop-1])) boffo[boffop++] = '0';
        }
// Whether or not there was a ".", an "e" or "E" introduces an exponent and
// hence indicates a floating point value.
        if (curchar == 'e' || curchar == 'E')
        {   symtype = 'f';
            if (boffop < BOFFO_SIZE) boffo[boffop++] = curchar;
            curchar = rdch();
            if (curchar == '+' || curchar == '-')
            {   if (boffop < BOFFO_SIZE) boffo[boffop++] = curchar;
                curchar = rdch();
            }
            while (isdigit(curchar))
            {   if (boffop < BOFFO_SIZE) boffo[boffop++] = curchar;
                curchar = rdch();
            }
// If there had been an "e" I force at least one digit in following it.
            if (!isdigit((int)boffo[boffop-1])) boffo[boffop++] = '0';
        }
        boffo[boffop] = 0;
        if (symtype == 'a')
        {   int neg = 0;
            LispObject r = packfixnum(0);
            boffop = 0;
            if (boffo[boffop] == '+') boffop++;
            else if (boffo[boffop] == '-') neg=1, boffop++;
            while (boffo[boffop] != 0)
            {   r = Nplus2(Ntimes2(packfixnum(10), r),
                           packfixnum(boffo[boffop++] - '0'));
            }
            if (neg) r = Nminus(r);
            return r;
        }
        else
        {   double d;
            sscanf(boffo, "%lg", &d);
            return boxfloat(d);
        }
    }
    boffo[boffop++] = curchar;
    curchar = rdch();
    boffo[boffop] = 0;
    symtype = 'a';
    return lookup(boffo, boffop, 1);
}

// Syntax for Lisp input
//
//   S ::= name
//     |   integer
//     |   float
//     |   string
//     |   ' S   | ` S  | , S  | ,@ S
//     |   ( T
//     ;
//
//   T ::= )
//     |   . S )
//     |   S T
//     ;

extern LispObject readT();

LispObject readS()
{   LispObject q, w;
    while (1)
    {   switch (symtype)
        {   case '?':
                cursym = token();
                continue;
            case '(':
                cursym = token();
                return readT();
            case '.':
            case ')':     // Ignore spurious "." and ")" input.
                cursym = token();
                continue;
            case '\'':
                w = quote;
                break;
            case '`':
                w = backquote;
                break;
            case ',':
                w = comma;
                break;
            case '@':
                w = comma_at;
                break;
            case EOF:
                return eofsym;
            default:
                symtype = '?';
                return cursym;
        }
        cursym = token();
        q = readS();
        return list2star(w, q, nil);
    }
}

LispObject readT()
{   LispObject q, r;
    if (symtype == '?') cursym = token();
    switch (symtype)
    {   case EOF:
            return eofsym;
        case '.':
            cursym = token();
            q = readS();
            if (symtype == '?') cursym = token();
            if (symtype == ')') symtype = '?'; // Ignore if not ")".
            return q;
        case ')':
            symtype = '?';
            return nil;
        // case '(':  case '\'':
        // case '`':  case ',':
        // case '@':
        default:
            q = readS();
            r = readT();
            return cons(q, r);
    }
}


LispObject lookup(const char *s, size_t len, int flag)
{   LispObject w, pn;
    size_t i, hash = 1;
    for (i=0; i<len; i++) hash = 13*hash + s[i];
    hash = hash % OBHASH_SIZE;
    w = obhash[hash];
    while (w != tagFIXNUM)
    {   LispObject a = qcar(w);        // Will be a symbol.
        LispObject n = qpname(a);      // Will be a string.
        size_t l = veclength(qheader(n)); // Length of the name.
        if (l == len &&
            strncmp(s, qstring(n), len) == 0)
            return a;                  // Existing symbol found.
        w = qcdr(w);
    }
// here the symbol as required was not already present.
    if ((flag & 1) == 0) return undefined;
    pn = makestring(s, len);
    w = allocatesymbol(pn);
    obhash[hash] = cons(w, obhash[hash]);
    return w;
}

