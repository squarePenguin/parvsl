#ifndef COMMON_HPP
#define COMMON_HPP

#include <cstdint>

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

// Accessor macros the extract fields from LispObjects ...

#define qcar(x) ((heapaddr(x))[0])
#define qcdr(x) ((heapaddr(x))[1])

// For all other types I must remove the tagging information before I
// can use the item as a pointer.

// An especially important case is that of Symbols. These are the fields that
// they provide.

typedef LispObject SpecialForm(LispObject lits, LispObject a1);
// I will have separate entried for functions with from 0-4 args. For 5
// or more arguments the first 4 will be passed individually with the rest
// in a list.
typedef LispObject LispFn0(LispObject lits);
typedef LispObject LispFn1(LispObject lits, LispObject a1);
typedef LispObject LispFn2(LispObject lits, LispObject a1, LispObject a2);
typedef LispObject LispFn3(LispObject lits, LispObject a1,
                           LispObject a2, LispObject a3);
typedef LispObject LispFn4(LispObject lits, LispObject a1, LispObject a2,
                           LispObject a3, LispObject a4);
typedef LispObject LispFn5up(LispObject lits, LispObject a1, LispObject a2,
                             LispObject a3, LispObject a4, LispObject a5up);

#define qflags(x) ((heapaddr((x)-tagSYMBOL))[0])
#define qvalue(x) ((heapaddr((x)-tagSYMBOL))[1])
#define qplist(x) ((heapaddr((x)-tagSYMBOL))[2])
#define qpname(x) ((heapaddr((x)-tagSYMBOL))[3])
#define qlits(x)  ((heapaddr((x)-tagSYMBOL))[4])
#define qspare(x) ((heapaddr((x)-tagSYMBOL))[5])
#define qdefn0(x) (((LispFn0 **)    (heapaddr((x)-tagSYMBOL)))[6])
#define qdefn1(x) (((LispFn1 **)    (heapaddr((x)-tagSYMBOL)))[7])
#define qdefn2(x) (((LispFn2 **)    (heapaddr((x)-tagSYMBOL)))[8])
#define qdefn3(x) (((LispFn3 **)    (heapaddr((x)-tagSYMBOL)))[9])
#define qdefn4(x) (((LispFn4 **)    (heapaddr((x)-tagSYMBOL)))[10])
#define qdefn5up(x) (((LispFn5up **)(heapaddr((x)-tagSYMBOL)))[11])
#define SYMSIZE 12

// Bits within the flags field of a symbol. Uses explained later on.

#define flagTRACED    0x080
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

// This sets the size of the hash table used to store all the symbols
// that Lisp knows about. I note that if I built a serious application
// such as the Reduce algebra system (reduce-algebra.sourceforge.net) I would
// end up with around 7000 symbols in a basic installation! So the size
// table I use here intended to give decent performance out to that scale.
// This is (of course) utterly over the top for the purpose of toy and
// demonstration applications! I make the table size a prime in the hope that
// that will help keep hashed distribution even across it.
// Hmm - a full copy of everything that makes up Reduce involved around
// 40K distinct symbols...

#define OBHASH_SIZE 10007

// Some Lisp values that I will use frequently...

#define nil        bases[0]
#define undefined  bases[1]
#define lisptrue   bases[2]
#define lispsystem bases[3]
#define echo       bases[4]
#define lambda     bases[5]
#define quote      bases[6]
#define backquote  bases[7]
#define comma      bases[8]
#define comma_at   bases[9]
#define eofsym     bases[10]
#define cursym     bases[11]
#define work1      bases[12]
#define work2      bases[13]
#define restartfn  bases[14]
#define expr       bases[15]
#define subr       bases[16]
#define fexpr      bases[17]
#define fsubr      bases[18]
#define macro      bases[19]
#define input      bases[20]
#define output     bases[21]
#define pipe       bases[22]
#define raise      bases[23]
#define lower      bases[24]
#define dfprint    bases[25]
#define bignum     bases[26]
#define BASES_SIZE       27

LispObject bases[BASES_SIZE];
LispObject obhash[OBHASH_SIZE];

// ... and non-LispObject values that need to be saved as part of a
// heap image.

#endif // COMMON_HPP