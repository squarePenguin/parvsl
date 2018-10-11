// Things to work on...


//    heap allocated in segments hence dynamically expandable.
//       actually make it have more than 1 segment to start so I can test.
//       fix GC and memory allocation to cope, including case where
//         reloaded object spans page boundary.
//    testing testing testing.
//    heap images reloadable across word order and length.
//       implement cross-width loading and test cross byte-order case.
//    full set of data types as in CSL.
//       will interact with heap reloading code & GC.
//    gc to use freelist in heap2 to avoid waste on fragmentation due
//       to pinned items.
//    native-coded bignum arithmetic.
//    fixnums to length that varies with machine word-length.
//    scan for free space bitmap to go word at a time not bit at a time.
//    &rest in lambda-lists.
//    tracing fully implemented. Other backtrace and debugging issues.
//    lots more CSL compatibility.
//=========================================================================

// Visible Lisp                                  A C Norman, August 2012-18
//
// This is a small Lisp system. It is especially
// intended for use of the Raspberry Pi board, but should build
// on almost any computer with a modern C compiler.

/**************************************************************************
 * Copyright (C) 2018.                                   A C Norman       *
 *                                                                        *
 * Redistribution and use in source and binary forms, with or without     *
 * modification, are permitted provided that the following conditions are *
 * met:                                                                   *
 *                                                                        *
 *     * Redistributions of source code must retain the relevant          *
 *       copyright notice, this list of conditions and the following      *
 *       disclaimer.                                                      *
 *     * Redistributions in binary form must reproduce the above          *
 *       copyright notice, this list of conditions and the following      *
 *       disclaimer in the documentation and/or other materials provided  *
 *       with the distribution.                                           *
 *                                                                        *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS    *
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT      *
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS      *
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE         *
 * COPYRIGHT OWNERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,   *
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,   *
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS  *
 * OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND *
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR  *
 * TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF     *
 * THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH   *
 * DAMAGE.                                                                *
 *************************************************************************/



// The following are not needed if you are using a sufficiently up-to-date
// C++ compiler, but there were times annd there are historical compiler
// that need them.

#ifndef __STDC_CONSTANT_MACROS
#define __STDC_CONSTANT_MACROS 1
#endif

#ifndef __STDC_LIMIT_MACROS
#define __STDC_LIMIT_MACROS 1
#endif

#ifndef __STDC_FORMAT_MACROS
#define __STDC_FORMAT_MACROS 1
#endif


#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <wctype.h>
#include <time.h>
#include <errno.h>
#include <math.h>
#include <cmath>
#include <stdint.h>
#include <inttypes.h>
#include <stdarg.h>
#include <assert.h>
#include <zlib.h>
// I want libedit for local editing and history.
#include <histedit.h>
#ifdef __WIN32__
#include <windows.h>
#else
#include <sys/mman.h>

// There is a portability issue about MAP_ANONYMOUS: I hope that
// the adjustments made here will leave everything workable
// everywhere.
#ifdef MAP_ANONYMOUS
#define MMAP_FLAGS (MAP_PRIVATE|MAP_ANONYMOUS)
#define MMAP_FD    -1
#else 
#ifdef MAP_ANON
#define MMAP_FLAGS (MAP_PRIVATE|MAP_ANON)
#define MMAP_FD    -1
#else
int devzero_fd = 0;
#define MMAP_FLAGS MAP_PRIVATE
#define MMAP_FD                                                   \
    (devzero_fd == 0 ? (devzero_fd = open("/dev/zero", O_RDWR)) : \
                       devzero_fd)
#endif // MMAP_ANON
#endif // MMAP_ANONYMOUS
#endif // __WIN32__

#ifdef WIN32
#define popen _popen
#endif


// This version is an extension of the minimal vsl system. It uses
// a conservative garbage collector and will support a fuller and
// higher performance Lisp.


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

// The Lisp heap will have fixed size. Here I make it 8192 Mbytes.

#ifndef MEM
#define MEM 8192
#endif // MEM

#define HALFBITMAPSIZE ((uintptr_t)MEM*1024*(1024/128))
// Each byte in the bitmap will allow marking for 8 entities, and each
// entity is 8 bytes wide (both on 32 and 64-bit systems), hence each
// bitmap uses 1/64th of the memory used by the region it maps.

LispObject *C_stackbase;

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


void my_exit(int n)
{
    printf("\n+++++ Exit called %d\n", n);
    fflush(stdout);
    fflush(stderr);
    *((int *)(-100)) = 0;
    abort();
}

// Many of these are "really" pointers... but I am using uintptr_t as
// a way of talking about the low level abstraction of "bits and bytes".

uintptr_t heap1_pinchain = packfixnum(0), heap2_pinchain = packfixnum(0);
uintptr_t block1, fringe1, limit1, block2, block2s, fringe2, limit2;
uintptr_t heap2_freechain;

// I will maintain some information about how much space might be
// consumed by items that are pinned by the conservative garbage collector.

uintptr_t npins = 0, heap1_pads = 0, heap2_pads = 0;

// I am liable to want to support memory that has been allocated in segments
// (ie not all at the start of the run). I will arrange that each allocated
// segment contains a "heap1" part, a "heap2" part and associated
// bitmaps, all of which can be accessed given its start address.

// Finding a heap segment that is referred to is done using a directly
// coded search tree.

#define search2(x, h0, h1) (x < h1 ? h0 : h1)

#define search4(x, h0, h1, h2, h3)                              \
    (x < h2 ? search2(x, h0, h1) : search2(x, h2, h3))

#define search8(x, h0, h1, h2, h3, h4, h5, h6, h7)              \
    (x < h4 ? search4(x, h0, h1, h2, h3) :                      \
              search4(x, h4, h5, h6, h7))

#define search16(x, h0, h1, h2, h3, h4, h5, h6, h7,             \
                    h8, h9, h10, h11, h12, h13, h14, h15)       \
    (x < h8 ? search8(x, h0, h1, h2, h3, h4, h5, h6, h7) :      \
              search8(x, h8, h9, h10, h11, h12, h13, h14, h15))

// The variants that come next do the same sort of search but return
// results that can be specified separately from the key values.

#define search2a(x, h0, h1, v0, v1) (x < h1 ? v0 : v1)

#define search4a(x, h0, h1, h2, h3, v0, v1, v2, v3)              \
    (x < h2 ? search2a(x, h0, h1, v0, v1) : search2a(x, h2, h3, v2, v3))

#define search8a(x, h0, h1, h2, h3, h4, h5, h6, h7,              \
                    v0, v1, v2, v3, v4, v5, v6, v7)              \
    (x < h4 ? search4a(x, h0, h1, h2, h3, v0, v1, v2, v3) :      \
              search4a(x, h4, h5, h6, h7, v4, v5, v6, v7))

#define search16a(x, h0, h1, h2, h3, h4, h5, h6, h7,             \
                     h8, h9, h10, h11, h12, h13, h14, h15,       \
                     v0, v1, v2, v3, v4, v5, v6, v7,             \
                     v8, v9, v10, v11, v12, v13, v14, v15)       \
    (x < h8 ? search8a(x, h0, h1, h2, h3, h4, h5, h6, h7,        \
                          v0, v1, v2, v3, v4, v5, v6, v7) :      \
              search8a(x, h8, h9, h10, h11, h12, h13, h14, h15,  \
                          v8, v9, v10, v11, v12, v13, v14, v15))

// In general I will allocate some initial heap segment and each time
// I allocate another I will double the total memory that is in use. So
// on a 32-bit system I might start with a 4 Mbyte block (which is
// rather small by today's standards) and the next blocks allocated
// will be sized as 4, 8, 16, 32, 64, 128, 256, 512, 1024 and 2048 Mbyte.
// On a 32-bit system I will not actually be able to get that far, but the
// point is that there are just 10 blocks being used. When at some stage
// I am unable to allocate the full size as indicated above I will allocate
// some approximation to "as much as I can" so the final block allocated
// will typically be somewhat small.
// On a 64-bit machine I intend to make the initial size 128M, so if I allow
// for up to 16 blocks the memory would end up as
// 128M, 128M, 256M, 512M, 1G, 2G, 4G, 8G, 16G, 32G, 64G, 128G,
// 256G, 512G, 1T with the result that when all 16 blocks were in use I would
// have 2 Tb active. At present that seems sufficiently far above the
// amount of memory that I normally have access to that I am happy with it.
// If at some stage in the future this became a serious problem then the
// search across heap segments could be extended to support 32 rather than
// 16 and that would provide a major amount of extra range.


// This array must contain references to the start of each memory block
// that has been allocated (cast to unsigned integers of suitable width),
// sorted so that the lowest value comes first and with any spare space
// in the array filled with an address higher than that of the final
// block. (uintptr_t)(-1) should suffice! It is seached using a binary
// search so exactly four comparisons are used to identify a block. Further
// tests are then required to verify that the address is within the relevant
// section of that block.

uintptr_t nblocks = 0;
uintptr_t blocks[16], blocks_by_age[16], blocks_offset[16];

typedef struct block_header
{
    uintptr_t halfbitmapsize;
    uintptr_t hbase;        // pointers, but stored as uintptr_t so that..
    uintptr_t htop;         // arithmetic and comparisons are easier.
    uintptr_t h1base;
    uintptr_t h1top;
    uintptr_t h2base;
    uintptr_t h2top;
    uint32_t  *hstarts;     // bitmaps
    uint32_t  *h1starts;
    uint32_t  *h2starts;
    uint32_t  *hfp;
    uint32_t  *h1fp;
    uint32_t  *h2fp;
    uint32_t  *pinned;      // only covers heap1
} block_header;

void hexdump()
{
    uintptr_t i;
    uintptr_t j, k;
    return;
//@@
    for (i=0; i<nblocks; i++)
    {   block_header *b = (block_header *)blocks_by_age[i];
        printf("Block %" PRIdPTR "\n", i);
        printf("h1base = %#" PRIxPTR "   h2base = %#" PRIxPTR "\n",
            b->h1base, b->h2base);
        if (fringe1 >= b->h1base && fringe1 <= b->h1top) k = fringe1;
        else k = b->h1top;
        for (j=b->h1base; j<k; j+=sizeof(LispObject))
            printf("%#" PRIxPTR ": %#" PRIxPTR "\n", j, qcar(j));
    }
    printf("========\n");
}

static inline block_header *find_block(uintptr_t x)
{
    uintptr_t block = search16(x,
        blocks[0],  blocks[1],  blocks[2],  blocks[3],
        blocks[4],  blocks[5],  blocks[6],  blocks[7],
        blocks[8],  blocks[9],  blocks[10], blocks[11],
        blocks[12], blocks[13], blocks[14], blocks[15]);
    return (block_header *)block;
}

static inline int inheap(uintptr_t x)
{
    block_header *block = find_block(x);
    if ((uintptr_t)block == (uintptr_t)(-1)) return 0;
    return (block->hbase <= x && x < block->htop);
}

static inline int inheap1(uintptr_t x)
{
    block_header *block = find_block(x);
    if ((uintptr_t)block == (uintptr_t)(-1)) return 0;
    return (block->h1base <= x && x < block->h1top);
}

static inline int inheap2(uintptr_t x)
{
    block_header *block = find_block(x);
    if ((uintptr_t)block == (uintptr_t)(-1)) return 0;
    return (block->h2base <= x && x < block->h2top);
}

// Any address in either heap1 or heap2 has information about it in the
// starts and fp bitmaps. The meaning is as follows:
//
//  starts  fp
//    1     0    start of a cons cell, symbol or vector.
//    1     1    the single 64-bit item for a floating point value.
//    0     1    [while in GC] used to be a float, but now has been
//               copied to the second heap, so a cell containing a
//               floating point forwarding address.
//    0     0    unused space, or one of the non-inital calls of a
//               cons, symbol or vector.

static inline void setheapstarts(uintptr_t x)
{   block_header *block = find_block(x);
    assert((uintptr_t)block != (uintptr_t)(-1));
    assert(block->hbase <= x && x < block->htop);
    (block->hstarts)[(x - block->hbase)>>8] |=
        (1U << (((x-block->hbase)>>3) & 0x1f));
}

static inline void setheapfp(uintptr_t x)
{   block_header *block = find_block(x);
    assert((uintptr_t)block != (uintptr_t)(-1));
    assert(block->hbase <= x && x < block->htop);
    (block->hfp)[(x - block->hbase)>>8] |=
        (1U << (((x-block->hbase)>>3) & 0x1f));
}

static inline void setheapstartsandfp(uintptr_t x)
{   block_header *block = find_block(x);
    assert((uintptr_t)block != (uintptr_t)(-1));
    assert(block->hbase <= x && x < block->htop);
    (block->hstarts)[(x - block->hbase)>>8] |=
        (1U << (((x-block->hbase)>>3) & 0x1f));
    (block->hfp)[(x - block->hbase)>>8] |=
        (1U << (((x-block->hbase)>>3) & 0x1f));
}

static inline void setpinned(uintptr_t x)
{
    block_header *block = find_block(x);
    assert((uintptr_t)block != (uintptr_t)(-1));
    assert(block->h1base <= x && x < block->h1top);
    (block->pinned)[(x - block->h1base)>>8] |=
        (1U << (((x-block->h1base)>>3) & 0x1f));
}

static inline void resetheapstarts(uintptr_t x)
{
    block_header *block = find_block(x);
    assert((uintptr_t)block != (uintptr_t)(-1));
    assert(block->hbase <= x && x < block->htop);
    (block->hstarts)[(x - block->hbase)>>8] &=
        ~(1U << (((x-block->hbase)>>3) & 0x1f));
}

static inline void resetpinned(uintptr_t x)
{
    block_header *block = find_block(x);
    assert((uintptr_t)block != (uintptr_t)(-1));
    assert(block->h1base <= x && x < block->h1top);
    (block->pinned)[(x - block->h1base)>>8] &=
        ~(1U << (((x-block->h1base)>>3) & 0x1f));
}

static inline int32_t getheapstarts(uintptr_t x)
{
    block_header *block = find_block(x);
    assert((uintptr_t)block != (uintptr_t)(-1));
    assert(block->hbase <= x && x < block->htop);
    return (block->hstarts)[(x - block->hbase)>>8] &
        (1U << (((x-block->hbase)>>3) & 0x1f));
}

static inline int32_t getheapfp(uintptr_t x)
{
    block_header *block = find_block(x);
    assert((uintptr_t)block != (uintptr_t)(-1));
    assert(block->hbase <= x && x < block->htop);
    return (block->hfp)[(x - block->hbase)>>8] &
        (1U << (((x-block->hbase)>>3) & 0x1f));
}

static inline int32_t getpinned(uintptr_t x)
{
    block_header *block = find_block(x);
    assert((uintptr_t)block != (uintptr_t)(-1));
    assert(block->h1base <= x && x < block->h1top);
    return (block->pinned)[(x - block->h1base)>>8] &
        (1U << (((x-block->h1base)>>3) & 0x1f));
}

static inline void clearheapstarts(block_header *block)
{
    assert((uintptr_t)block != (uintptr_t)(-1));
    memset(block->hstarts, 0, (size_t)(2*block->halfbitmapsize));
}

static inline void clearheapfp(block_header *block)
{
    assert((uintptr_t)block != (uintptr_t)(-1));
    memset(block->hfp, 0, (size_t)(2*block->halfbitmapsize));
}

static inline void clearpinned(block_header *block)
{
    assert((uintptr_t)block != (uintptr_t)(-1));
    memset(block->pinned, 0, (size_t)block->halfbitmapsize);
}


// Here there are some memory blocks allocated, each with their
// halfbitmapsize field filled in. Fill in the rest of the entries
// in them. 

#define ALIGN8(a) (((a) + 7) & ~(LispObject)7)

void allocateheap()
{
    uintptr_t i;
    for (i=0; i<nblocks; i++)
    {   uintptr_t bi = blocks[i];
        block_header *b = (block_header *)bi;
printf("allocateheap: b[%" PRIdPTR "] = %#" PRIxPTR "\n", i, bi);
        b->hbase = b->h1base = ALIGN8(bi + sizeof(block_header));
        b->h1top = b->h2base = b->h1base + 64*b->halfbitmapsize;
        b->h2top = b->htop = b->h2base + 64*b->halfbitmapsize;
        b->h1starts = b->hstarts = (uint32_t *)b->htop;
        b->h2starts = (uint32_t *)((uintptr_t)b->hstarts + b->halfbitmapsize);
        b->h1fp = b->hfp = (uint32_t *)((uintptr_t)b->hstarts + 2*b->halfbitmapsize);
        b->h2fp = (uint32_t *)((uintptr_t)b->hfp + b->halfbitmapsize);
        b->pinned = (uint32_t *)((uintptr_t)b->hfp + 2*b->halfbitmapsize);
printf("allocateheap: b=h1[%" PRIdPTR "] = %#" PRIxPTR "\n", i, b->h1base);
printf("allocateheap: b=h2[%" PRIdPTR "] = %#" PRIxPTR "\n", i, b->h2base);
        clearheapstarts(b);
        clearheapfp(b);
    }
    block1 = block2 = 0;
    fringe1 = ((block_header *)blocks_by_age[0])->h1base;
    fringe2 = ((block_header *)blocks_by_age[0])->h2base;
    limit1  = ((block_header *)blocks_by_age[0])->h1top;
    limit2  = ((block_header *)blocks_by_age[0])->h2top;
    heap1_pinchain = heap2_pinchain = packfixnum(0);
}

// Now I have enough to let me define various allocation functions.

extern void reclaim(int line);
extern LispObject error1(const char *s, LispObject a);
extern void check_space(int nbytes, int line);

LispObject undefined0(LispObject env)
{
    return error1("Undefined function", env);
}

LispObject undefined1(LispObject env, LispObject a1)
{
    return error1("Undefined function", env);
}

LispObject undefined2(LispObject env, LispObject a1, LispObject a2)
{
    return error1("Undefined function", env);
}

LispObject undefined3(LispObject env, LispObject a1,
                      LispObject a2, LispObject a3)
{
    return error1("Undefined function", env);
}

LispObject undefined4(LispObject env, LispObject a1, LispObject a2,
                      LispObject a3, LispObject a4)
{
    return error1("Undefined function", env);
}

LispObject undefined5up(LispObject env, LispObject a, LispObject a2,
                        LispObject a3, LispObject a4, LispObject a5up)
{
    return error1("Undefined function", env);
}

LispObject wrongnumber0(LispObject env)
{
    return error1("Call with wrong number of arguments", env);
}

LispObject wrongnumber1(LispObject env, LispObject a1)
{
    return error1("Call with wrong number of arguments", env);
}

LispObject wrongnumber2(LispObject env, LispObject a1, LispObject a2)
{
    return error1("Call with wrong number of arguments", env);
}

LispObject wrongnumber3(LispObject env, LispObject a1,
                      LispObject a2, LispObject a3)
{
    return error1("Call with wrong number of arguments", env);
}

LispObject wrongnumber4(LispObject env, LispObject a1, LispObject a2,
                      LispObject a3, LispObject a4)
{
    return error1("Call with wrong number of arguments", env);
}

LispObject wrongnumber5up(LispObject env, LispObject a, LispObject a2,
                        LispObject a3, LispObject a4, LispObject a5up)
{
    return error1("Call with wrong number of arguments", env);
}

static inline LispObject cons(LispObject a, LispObject b)
{
    check_space(2*sizeof(LispObject), __LINE__);
    setheapstarts(fringe1);
    qcar(fringe1) = a;
    qcdr(fringe1) = b;
    a = fringe1;
    fringe1 += 2*sizeof(LispObject);
    return a;
}

static inline LispObject list2star(LispObject a, LispObject b, LispObject c)
{   // (cons a (cons b c))
    check_space(4*sizeof(LispObject), __LINE__);
    setheapstarts(fringe1);
    qcar(fringe1) = a;
    qcdr(fringe1) = fringe1 + 2*sizeof(LispObject);
    a = fringe1;
    fringe1 += 2*sizeof(LispObject);
    setheapstarts(fringe1);
    qcar(fringe1) = b;
    qcdr(fringe1) = c;
    fringe1 += 2*sizeof(LispObject);
    return a;
}

static inline LispObject acons(LispObject a, LispObject b, LispObject c)
{   // (cons (cons a b) c)
    check_space(4*sizeof(LispObject), __LINE__);
    setheapstarts(fringe1);
    qcar(fringe1) = fringe1 + 2*sizeof(LispObject);
    qcdr(fringe1) = c;
    c = fringe1;
    fringe1 += 2*sizeof(LispObject);
    setheapstarts(fringe1);
    qcar(fringe1) = a;
    qcdr(fringe1) = b;
    fringe1 += 2*sizeof(LispObject);
    return c;
}

static inline LispObject boxfloat(double a)
{   LispObject r;
    check_space(8, __LINE__);
    setheapstartsandfp(fringe1);
    r = fringe1 + tagFLOAT;
    qfloat(r) = a;
    fringe1 += 8;
    return r;
}

// The code here does not fill in ANY of the fields within the symbol. That
// needs to be done promptly.

static inline LispObject allocatesymbol(LispObject pname)
{   LispObject r;
    check_space(SYMSIZE*sizeof(LispObject), __LINE__);
    setheapstarts(fringe1);
    r = fringe1 + tagSYMBOL;
    qflags(r) = tagHDR + typeSYM;
    qvalue(r) = undefined;
    qplist(r) = nil;
    qpname(r) = pname;
    qspare(r) = nil;
    qdefn0(r) = undefined0;
    qdefn1(r) = undefined1;
    qdefn2(r) = undefined2;
    qdefn3(r) = undefined3;
    qdefn4(r) = undefined4;
    qdefn5up(r) = undefined5up;
    qlits(r)  = r;
    fringe1 += SYMSIZE*sizeof(LispObject);
    return r;
}

// This one allocates an atom that is n bytes long (plus its header
// word) and again does not fill in ANY of the fields.

static inline LispObject allocateatom(int n)
{   LispObject r;
// The actual amount of space allocated must include a word for the
// header and must then be rounded up to be a multiple of 8.
    int nn = ALIGN8(sizeof(LispObject) + n);
    check_space(nn, __LINE__);
    setheapstarts(fringe1);
    r = fringe1 + tagATOM;
// I mark the new vector as being a string so that it is GC safe
    qheader(r) = tagHDR + typeSTRING + packlength(n);
    fringe1 += nn;
    return r;
}

static inline LispObject makestring(const char *s, int len)
{
    LispObject r = allocateatom(len);
//  qheader(r) = tagHDR + typeSTRING + packlength(len); // already done!
    memcpy(qstring(r), s, len);
    return r;
}

#define elt(v, n) \
    (((LispObject *)((v)-tagATOM+sizeof(LispObject)))[n])

static inline LispObject makevector(int maxindex)
{   int i, len = (maxindex+1)*sizeof(LispObject);
    LispObject r = allocateatom(len);
    qheader(r) = tagHDR + typeVEC + packlength(len);
    for (i=0; i<=maxindex; i++) elt(r, i) = nil;
    return r;
}

static inline LispObject boxint64(int64_t a)
{
    LispObject r = allocateatom(8);
    qheader(r) = tagHDR + typeBIGNUM + packlength(8);
    qint64(r) = a;
    return r;
}

static LispObject Lallocate_string(LispObject data, LispObject a1)
{   if (!isFIXNUM(a1)) return error1("bad arg for allocate-string", a1);
    intptr_t n = qfixnum(a1);
    if (n < 0) return error1("bad arg for allocate-string", a1);
    LispObject r = allocateatom(n);
//  qheader(r) = tagHDR + typeSTRING + packlength(len); // already done!
    memset(qstring(r), ' ', n);
    return r;
}

// The odd functions here help Reduce support UTF8/Unicode input a bit.

static LispObject Lstring_store1(LispObject data, LispObject s,
        LispObject n, LispObject c)
{   if (!isSTRING(s)) return error1("not a string for string-store", s);
    if (!isFIXNUM(n) || (intptr_t)n < 0)
        return error1("bad index for string-store", n);
    if (isSYMBOL(c)) c = qpname(c);
    int ch1 = 0;
    if (isSTRING(c)) ch1 = qstring(c)[0];
    else if (isFIXNUM(c)) ch1 = (int)qfixnum(c) & 0xff;
    else return error1("bad char in string-store", c);
    qstring(s)[qfixnum(n)] = ch1;
    return s;
}

static LispObject Lstring_store2(LispObject data, LispObject s,
        LispObject n, LispObject c1, LispObject c2)
{   if (!isSTRING(s)) return error1("not a string for string-store2", s);
    if (!isFIXNUM(n) || (intptr_t)n < 0)
        return error1("bad index for string-store", n);
    if (isSYMBOL(c1)) c1 = qpname(c1);
    int ch1 = 0;
    if (isSTRING(c1)) ch1 = qstring(c1)[0];
    else if (isFIXNUM(c1)) ch1 = (int)qfixnum(c1) & 0xff;
    else return error1("bad char in string-store", c1);
    if (isSYMBOL(c2)) c2 = qpname(c2);
    int ch2 = 0;
    if (isSTRING(c2)) ch2 = qstring(c2)[0];
    else if (isFIXNUM(c2)) ch2 = (int)qfixnum(c2) & 0xff;
    else return error1("bad char in string-store", c2);
    qstring(s)[qfixnum(n)] = ch1;
    qstring(s)[qfixnum(n)+1] = ch2;
    return s;
}

static LispObject Lstring_store3(LispObject data, LispObject s,
        LispObject n, LispObject c1, LispObject c2, LispObject lc3)
{   if (!isCONS(lc3) || isCONS(qcdr(lc3)))
        return error1("wrong no args for string-store3", lc3);
    LispObject c3 = qcar(lc3);
    if (!isSTRING(s)) return error1("not a string for string-store3", s);
    if (!isFIXNUM(n) || (intptr_t)n < 0)
        return error1("bad index for string-store", n);
    if (isSYMBOL(c1)) c1 = qpname(c1);
    int ch1 = 0;
    if (isSTRING(c1)) ch1 = qstring(c1)[0];
    else if (isFIXNUM(c1)) ch1 = (int)qfixnum(c1) & 0xff;
    else return error1("bad char in string-store", c1);
    if (isSYMBOL(c2)) c2 = qpname(c2);
    int ch2 = 0;
    if (isSTRING(c2)) ch2 = qstring(c2)[0];
    else if (isFIXNUM(c2)) ch2 = (int)qfixnum(c2) & 0xff;
    else return error1("bad char in string-store", c2);
    if (isSYMBOL(c3)) c3 = qpname(c3);
    int ch3 = 0;
    if (isSTRING(c3)) ch3 = qstring(c3)[0];
    else if (isFIXNUM(c3)) ch3 = (int)qfixnum(c3) & 0xff;
    else return error1("bad char in string-store", c3);
    qstring(s)[qfixnum(n)] = ch1;
    qstring(s)[qfixnum(n)+1] = ch2;
    qstring(s)[qfixnum(n)+2] = ch3;
    return s;
}

static LispObject Lstring_store4(LispObject data, LispObject s,
        LispObject n, LispObject c1, LispObject c2, LispObject c3c4)
{   if (!isCONS(c3c4) || !isCONS(qcdr(c3c4)) || isCONS(qcdr(qcdr(c3c4))))
        return error1("wrong no args for string-store4", c3c4);
    LispObject c3 = qcar(c3c4);
    LispObject c4 = qcar(qcdr(c3c4));
    if (!isSTRING(s)) return error1("not a string for string-store3", s);
    if (!isFIXNUM(n) || (intptr_t)n < 0)
        return error1("bad index for string-store", n);
    if (isSYMBOL(c1)) c1 = qpname(c1);
    int ch1 = 0;
    if (isSTRING(c1)) ch1 = qstring(c1)[0];
    else if (isFIXNUM(c1)) ch1 = (int)qfixnum(c1) & 0xff;
    else return error1("bad char in string-store", c1);
    if (isSYMBOL(c2)) c2 = qpname(c2);
    int ch2 = 0;
    if (isSTRING(c2)) ch2 = qstring(c2)[0];
    else if (isFIXNUM(c2)) ch2 = (int)qfixnum(c2) & 0xff;
    else return error1("bad char in string-store", c2);
    if (isSYMBOL(c3)) c3 = qpname(c3);
    int ch3 = 0;
    if (isSTRING(c3)) ch3 = qstring(c3)[0];
    else if (isFIXNUM(c3)) ch3 = (int)qfixnum(c3) & 0xff;
    else return error1("bad char in string-store", c3);
    if (isSYMBOL(c4)) c4 = qpname(c4);
    int ch4 = 0;
    if (isSTRING(c4)) ch4 = qstring(c4)[0];
    else if (isFIXNUM(c4)) ch4 = (int)qfixnum(c4) & 0xff;
    else return error1("bad char in string-store", c4);
    qstring(s)[qfixnum(n)] = ch1;
    qstring(s)[qfixnum(n)+1] = ch2;
    qstring(s)[qfixnum(n)+2] = ch3;
    qstring(s)[qfixnum(n)+3] = ch4;
    return s;
}

extern LispObject lookup(const char *s, int n, int flags);

// The code here is ugly and could be tidied up!

static LispObject Lchar_upcase(LispObject data, LispObject arg)
{   LispObject a = arg;
    if (isSYMBOL(a)) a = qpname(a);
    int ch = 0;
    if (isSTRING(a))
    {   ch = qstring(a)[0] & 0xff;
        size_t len = strlen(qstring(a));
        if (len==1 && (ch&0x80) == 0) ch = toupper(ch);
        else if (len==2 && (ch & 0xe0) == 0xc0) // 2-byte UTF8
        {   ch = ((ch & 0x1f)<<6) | (qstring(a)[1] & 0x3f);
            ch = towupper(ch);
        }
        else if (len==4 && (ch & 0xf0) == 0xe0) // 3 byte UTF8
        {   ch = ((ch & 0x0f)<<12) |
                 ((qstring(a)[1] & 0x3f) << 6) |
                 (qstring(a)[2] & 0x3f);
            ch = towupper(ch);

        }
        else return arg;
    }
    else if (isFIXNUM(a))
    {   ch = (int)qfixnum(a);
        if (ch < 0 || ch > 0xffff) return arg;
        ch = towupper(ch);
    }
    else return error1("bad char in char-upcase", a);
    char b[4];
    if (ch <= 0x7f)
    {   b[0] = ch;
        b[1] = 0;
        return lookup(b, 1, 1);
    }
    else if (ch <= 0x7ff)
    {   b[0] = 0xc0 + (ch>>6);
        b[1] = 0x80 + (ch & 0x3f);
        b[2] = 0;
        return lookup(b, 2, 1);
    }
    else if (ch <= 0xffff)
    {   b[0] = 0xe0 + (ch>>12);
        b[1] = 0x80 + ((ch>>6) & 0x3f);
        b[2] = 0x80 + (ch & 0x3f);
        b[3] = 0;
        return lookup(b, 3, 1);
    }
    else return arg;
}

static LispObject Lchar_downcase(LispObject data, LispObject arg)
{   LispObject a = arg;
    if (isSYMBOL(a)) a = qpname(a);
    int ch = 0;
    if (isSTRING(a))
    {   ch = qstring(a)[0] & 0xff;
        size_t len = strlen(qstring(a));
        if (len==1 && (ch&0x80) == 0) ch = tolower(ch);
        else if (len==2 && (ch & 0xe0) == 0xc0) // 2-byte UTF8
        {   ch = ((ch & 0x1f)<<6) | (qstring(a)[1] & 0x3f);
            ch = towlower(ch);
        }
        else if (len==4 && (ch & 0xf0) == 0xe0) // 3 byte UTF8
        {   ch = ((ch & 0x0f)<<12) |
                 ((qstring(a)[1] & 0x3f) << 6) |
                 (qstring(a)[2] & 0x3f);
            ch = towlower(ch);

        }
        else return arg;
    }
    else if (isFIXNUM(a))
    {   ch = (int)qfixnum(a);
        if (ch < 0 || ch > 0xffff) return arg;
        ch = towlower(ch);
    }
    else return error1("bad char in char-downcase", a);
    char b[4];
    if (ch <= 0x7f)
    {   b[0] = ch;
        b[1] = 0;
        return lookup(b, 1, 1);
    }
    else if (ch <= 0x7ff)
    {   b[0] = 0xc0 + (ch>>6);
        b[1] = 0x80 + (ch & 0x3f);
        b[2] = 0;
        return lookup(b, 2, 1);
    }
    else if (ch <= 0xffff)
    {   b[0] = 0xe0 + (ch>>12);
        b[1] = 0x80 + ((ch>>6) & 0x3f);
        b[2] = 0x80 + (ch & 0x3f);
        b[3] = 0;
        return lookup(b, 3, 1);
    }
    else return arg;
}

// I will try to have a general macro that will help me with bringing
// everything to consistent numeric types - ie I can start off with a
// mix of fixnums, bignums and floats. The strategy here is that if either
// args is a float then the other is forced to that, and then for all sorts
// of pure integer work everything will be done as int64_t

#define NUMOP(name, a, b)                                                \
    if (isFLOAT(a))                                                      \
    {   if (isFLOAT(b)) return FF(qfloat(a), qfloat(b));                 \
        else if (isFIXNUM(b)) return FF(qfloat(a), (double)qfixnum(b));  \
        else if (isBIGNUM(b)) return FF(qfloat(a), (double)qint64(b));   \
        else return error1("Bad argument for " name, b);                 \
    }                                                                    \
    else if (isBIGNUM(a))                                                \
    {   if (isFLOAT(b)) return FF((double)qint64(a), qfloat(b));         \
        else if (isFIXNUM(b)) return BB(qint64(a), (int64_t)qfixnum(b)); \
        else if (isBIGNUM(b)) return BB(qint64(a), qint64(b));           \
        else return error1("Bad argument for " name, b);                 \
    }                                                                    \
    else if (isFIXNUM(a))                                                \
    {   if (isFLOAT(b)) return FF((double)qfixnum(a), qfloat(b));        \
        else if (isFIXNUM(b)) return BB((int64_t)qfixnum(a),             \
                                        (int64_t)qfixnum(b));            \
        else if (isBIGNUM(b)) return BB((int64_t)qfixnum(a), qint64(b)); \
        else return error1("Bad argument for " name, b);                 \
    }                                                                    \
    else return error1("Bad argument for " name, a)

#define UNARYOP(name, a)                                                 \
    if (isFIXNUM(a)) return BB((int64_t)qfixnum(a));                     \
    else if (isFLOAT(a)) return FF(qfloat(a));                           \
    else if (isBIGNUM(a)) return BB(qint64(a));                          \
    else return error1("Bad argument for " name, a)

// Similar, but only supporting integer (not floating point) values

#define INTOP(name, a, b)                                                \
    if (isBIGNUM(a))                                                     \
    {   if (isFIXNUM(b)) return BB(qint64(a), (int64_t)qfixnum(b));      \
        else if (isBIGNUM(b)) return BB(qint64(a), qint64(b));           \
        else return error1("Bad argument for " name, b);                 \
    }                                                                    \
    else if (isFIXNUM(a))                                                \
    {   if (isFIXNUM(b)) return BB((int64_t)qfixnum(a),                  \
                                   (int64_t)qfixnum(b));                 \
        else if (isBIGNUM(b)) return BB((int64_t)qfixnum(a), qint64(b)); \
        else return error1("Bad argument for " name, b);                 \
    }                                                                    \
    else return error1("Bad argument for " name, a)

#define UNARYINTOP(name, a)                                              \
    if (isFIXNUM(a)) return BB((int64_t)qfixnum(a));                     \
    else if (isBIGNUM(a)) return BB(qint64(a));                          \
    else return error1("Bad argument for " name, a)

// This takes an arbitrary 64-bit integer and returns either a fixnum
// or a bignum as necessary.

LispObject makeinteger(int64_t a)
{   if (a >= MIN_FIXNUM && a <= MAX_FIXNUM) return packfixnum(a);
    else return boxint64(a);
}

#undef FF
#undef BB
#define FF(a) boxfloat(-(a))
#define BB(a) makeinteger(-(a))

LispObject Nminus(LispObject a)
{   UNARYOP("minus", a);
}

#undef FF
#undef BB
#define FF(a, b) boxfloat((a) + (b))
#define BB(a, b) makeinteger((a) + (b))

LispObject Nplus2(LispObject a, LispObject b)
{   NUMOP("plus", a, b);
}

#undef FF
#undef BB
#define FF(a, b) boxfloat((a) * (b))
#define BB(a, b) makeinteger((a) * (b))

LispObject Ntimes2(LispObject a, LispObject b)
{   NUMOP("times", a, b);
}

#undef BB
#define BB(a, b) makeinteger((a) & (b))

LispObject Nlogand2(LispObject a, LispObject b)
{   INTOP("logand", a, b);
}

#undef BB
#define BB(a, b) makeinteger((a) | (b))

LispObject Nlogor2(LispObject a, LispObject b)
{   INTOP("logor", a, b);
}

#undef BB
#define BB(a, b) makeinteger((a) ^ (b))

LispObject Nlogxor2(LispObject a, LispObject b)
{   INTOP("logxor", a, b);
}

#undef FF
#undef BB

#define BOFFO_SIZE 4096
char boffo[BOFFO_SIZE+4];
int boffop;

#define swap(a,b) w = (a); (a) = (b); (b) = w;
#define swapmap(a,b) wmap = (a); (a) = (b); (b) = wmap;

static inline LispObject copy(LispObject x);
static inline LispObject copycontent(LispObject s);

int gccount = 1;

// The version of this implemented at present only supports a single block
// of memory. I really need to look things up in the nice table of memory
// blocks that I have using search16a...

static uintptr_t mem_base, mem_end;
// static bool check_an_address(uintptr_t p)
// {   return (p >= mem_base) && (p < mem_end);
// }

void *allocate_memory(uintptr_t n)
{
    void *p;
#ifdef __WIN32__
// This allocation grabs some memory that can be executed from, and so will
// allow me to put native code in my heap.
    p = VirtualAlloc(NULL,
        n,
        MEM_COMMIT|MEM_RESERVE,
        PAGE_EXECUTE_READWRITE);
// After any write of executable code but before I invoke it I must go
// FlushInstructionCache((HANDLE)my_process, (void *)base, (size_t)length);
// if I am running Windows on ppc, mips, Alpha or Itanium. On x86 and
// x86_64 I do not need to do anything special. Will I *EVER* come across
// a Windows machine running one of those less-common architectures?
#else // __WIN32__
// This should allocate executable memory on any Posix system. If
// MMAP_ANONYMOUS or MMAP_ANON is avialable it will use it - otherwise
// it will map the pseudo-file /dev/zero.
    p = mmap(NULL,
        n,
        PROT_READ|PROT_WRITE|PROT_EXEC,
        MMAP_FLAGS,
        MMAP_FD,
        0);
// mmap is specified to return MAP_FAILED if it is unable to grab memory as
// requested. I will translate that into returning NULL for failure since
// that is more what I am used to with malloc.
    if (p == MAP_FAILED) p = NULL;
#endif // __WIN32__
    mem_base = (uintptr_t)p;
    mem_end = mem_base + n;
printf("n = %" PRIxPTR " base = %" PRIxPTR " end = %" PRIxPTR "\n", n, mem_base, mem_end);
    return p;
}


extern void ensureheap2space(uintptr_t len);

void inner_reclaim(LispObject *C_stack)
{
// The strategy here is due to C J Cheyney ("A Nonrecursive List Compacting
// Algorithm". Communications of the ACM 13 (11): 677-678, 1970),
// adapted to be "conservative". C_stack to C_stackbase will be viewed as
// a set of ambiguous roots.
    uintptr_t s, w;
    uintptr_t o;
    uintptr_t i;
    uint32_t *wmap;
    LispObject pp;
    npins = heap2_pads = 0;
    printf("+++ GC number %d", gccount++);
    for (i=0; i<nblocks; i++)
    {   block_header *b = (block_header *)blocks[i];
        clearpinned(b);
    }
// Here at the start of garbage collection heap1 contains a list (now
// called heap2_pinchain) of all the items in heap2 that were pinned.
// These should be the only things present in heap2, and the list is used
// so that their content can be updated.
    heap1_pinchain = packfixnum(0);
// During garbage collection data will be copied into heap2 (skipping
// over those pinned items), so the three values block2, fringe2 and limit2
// indicate where the next item should be allocated there, and
// heap2_freechain can assist in avoiding undue waste around the pinned
// items and at the end of each big memory block.
    block2 = 0;
    fringe2 = ((block_header *)blocks_by_age[0])->h2base;
    limit2 = ((block_header *)blocks_by_age[0])->h2top;
    heap2_freechain = packfixnum(0);
// I will not want to treat anything in the private data structure
// heap2_pinchains as a candidate for pinning, since it will want to
// be discarded at the end of this garbage collection. I can arrange that
// by temporarily removing the "object starts" flags on each cell.
    for (pp=heap2_pinchain; isCONS(pp); pp=qcdr(pp)) resetheapstarts(pp);
// Now scan the stack (ie the ambiguous bases) looking for a reference
// to the start of an object in heap1 that has not alread been pinned.
// When I find one I pin it and add it to heap2_pinchain.
    for (s=(uintptr_t)C_stack;
         s<(uintptr_t)C_stackbase;
         s+=sizeof(LispObject))
    {   LispObject a = qind(s);
// If a value points within heap1 then any item if points to or within is
// to be treated as "pinned". I will allow pinning even if the ambiguous
// value has dubious tag bits or appears to point within an object rather
// than properly at its head. Taking this stance should provide more
// resilience aginst agressive optimistion by a compiler, and also could
// allow me to put either native or byte-coded compiled material in the heap
// secure in the knowledge that program counters and return addresses
// that reference into it will be coped with gracefully.
        if (inheap1(a))
        {   a &= ~(LispObject)TAGBITS;
// The next lne is goingto assume that the very first location in any heap
// section has its "starts" bit set, and so the loop can never zoom down and
// drop beyond the bottom of a segment.
            while (!getheapstarts(a)) a -= 8;
            if (!getpinned(a))
            {   LispObject h;
// ensureheapspace is here to arrange to skip past any of the pinned items
// that are in heap2 already.
                ensureheap2space(2*sizeof(LispObject));
                setheapstarts(fringe2);
// Arrange that the ambiguous pointer gets proper tag-bits attached
// so that it can not cause confusion later on. I could have had a model
// where a value was only treated as a reference if its tag bits were at
// least consistent with the data in memory pointed at. But by ignoring tag
// bits in the (ambigious) reference (but re-creating some here) I am more
// secure against compiler optimisations that save intermediate values that
// have tags removed.
                h = qcar(a);
                if (getheapfp(a)) a += tagFLOAT;
                else if (isHDR(h))
                {   if ((h & TYPEBITS) == typeSYM) a += tagSYMBOL;
                    else a += tagATOM;
                }
                qcar(fringe2) = a;
                qcdr(fringe2) = heap1_pinchain;
                heap1_pinchain = fringe2;
                fringe2 += 2*sizeof(LispObject);
                setpinned(a);
                npins++;
            }
        }
    }
//-- // Now as a matter of being tidy I will re-instate the "object start"
//-- // information for cells in heap2_pinchain. Actually since I will
//-- // not attempting any linear scan of heap1 I do not think I need to
//-- // do this. However it is fairly cheap and it gives me the advantage
//-- // that I could do better diagnostic consistency checking on heap1
//-- // if debugging called for it.
//--     for (pp=heap2_pinchain; isCONS(pp); pp=qcdr(pp)) setheapstarts(pp);
//    
// Now all pinned items are recorded in heap1_pinchain or heap2_pinchain.
// Ones in heap1 can be identified because they have a pinned bit. Those
// in heap2 can be identified just on the basis of having a start bit.
//
// As things are to be copied into heap2 it is necessary to skip
// over any item that is already pinned there. That could leave wasted
// space. To reduce that I will fill any gap with a free-chain of cons-cell
// sized units. Then when I need to allocate anything that size I get
// a chance to use up the space that risked ending up wasted. This may help
// to reduce any big waste blocks that could arise when a vector needed
// to be copied.
//
// Next I copy all objects directly accessible from proper list bases.
    for (o=0; o<BASES_SIZE; o++) bases[o] = copy(bases[o]);
    for (o=0; o<OBHASH_SIZE; o++)
        obhash[o] = copy(obhash[o]);
// Items that are pinned and are in heap1 may have the pinning pointer as
// the only reference to them. Ones in heap2 may lie beyond where fringe2
// will reach to. So I need to take special action to do copycontent on
// each pinned item.
// It is important here that it should be safe to do copycontent on
// an object more than once. That is because later on I will do a linear
// scan in heap2 performing copycontent, and that can traverse past some
// items that where pinned there. Note that the values I had put in the
// pinchains do refer to the starts of blocks and they do have valid
// tagging.
    for (pp=heap1_pinchain; isCONS(pp); pp=qcdr(pp))
        copycontent(qcar(pp) & ~(LispObject)TAGBITS);
    for (pp=heap2_pinchain; isCONS(pp); pp=qcdr(pp))
        copycontent(qcar(pp) & ~(LispObject)TAGBITS);
//
// Now perform the second part of Cheyney's algorithm, scanning the
// data that has been put in the new heap.
// For the scan of heap2 I use the two values (block2s, s) to indicate
// where I am. copycontent needs to be ready to update block2s when the
// scan goes from one block of memory to the next.
    block2s = 0;
    s = ((block_header *)blocks_by_age[0])->h2base;
    while (s != fringe2) s = copycontent(s);
// All non-pinned data has now been moved to heap2, so (block2,fringe2,limit2)
// mark the end-point of allocation there. The only material left in heap1
// is stuff that was pinned. So swap a lot of values so that heap1 and
// heap2 exchange roles.
    for (i=0; i<nblocks; i++)
    {   block_header *b = (block_header *)blocks[i];
        swap(b->h1base, b->h2base);
        swap(b->h1top, b->h2top);
        swapmap(b->h1starts, b->h2starts);
        swapmap(b->h1fp, b->h2fp);
    }
// The pinchain now refers to heap2.
    heap2_pinchain = heap1_pinchain;
// Future allocation in block1 resumes from where it ended within what
// has just been block2.
    block1 = block2;
    fringe1 = fringe2;
    limit1 = limit2;
    assert(fringe1 >= ((block_header *)blocks_by_age[block1])->h1base);
    assert(fringe1 < ((block_header *)blocks_by_age[block1])->h1base +
                      64*((block_header *)blocks_by_age[block1])->halfbitmapsize);
// Now I need to tidy up the new heap2. At present it has starts and fp
// information relating to all the material that had been in it before
// that material was copied elsewhere. The only things that should end
// up alive in it are those that were pinned. So I cope the pinned map
// into the map recording object starts, and "and" this into the floating
// point map.
    for (i=0; i<nblocks; i++)
    {   block_header *b = (block_header *)blocks[i];
        memcpy(b->h2starts, b->pinned, HALFBITMAPSIZE);
        for (o=0; o<HALFBITMAPSIZE/4; o++)
            b->h2fp[o] &= b->h2starts[o];
    }
// Garbage collection is now complete. I can see what proportion of
// my memory is in use. If I am getting full I will try expanding the
// heap. If that fails and I am sufficiently full I will report
// failure.
    {   uintptr_t used = 0, avail = 0;
        block_header *b;
        for (i=0; i<nblocks; i++)
        {   b = (block_header *)blocks_by_age[i];
            avail += 64*b->halfbitmapsize;
            if (i < block1) used += 64*b->halfbitmapsize;
            else if (i == block1) used += fringe1 - b->h1base;
        }
// If active data is filling at least half of my heap I will try to
// expand the heap, in a way that will typically double its size.
// The choice of a target of keeping the half-space between (1/4) and
// (1/2) full is a policy that could be tuned if need be,
        if (used > avail/2 && nblocks < 16)
        {   uint64_t w, w1;
            b = (block_header *)blocks_by_age[nblocks-1];
            w = b->halfbitmapsize;
            w1 = 2*w;
// I will first try to allocate a new block that is twice the size
// of the last one I allocated. If that is not possible I will
// try smaller sizes, scaling down to (2/3) of that until either I
// manage to grab some space or I fail to allocate at least (1/5)
// of the previous block size. The sequence of allocation sizes
// used is one that could be tuned since it is desirable top end up
// with enough memory to keep garbage collection overhead low but to
// be respectably economical in memory use. It is possible that
// measuring the proportion of CPU time that has been spent in the
// garbage collector so far could inform the judgement made...
            for (;;)            
            {   b = (block_header *)allocate_memory(sizeof(block_header) +
                                                    (2*64 + 5)*w1 + 16);
                if (b != NULL) break; // allocated another block
                w1 = ((w1/3)*2) & ~(uintptr_t)0xff;
                if (w1 <= w/5) break;
            }
// If I can not expand memory and over 90% is now in use I will declare that
// I have run out. That will count as a fatal situation. Once again the
// cut-off at 90% here is a pretty arbitrary choice, but it is certain that
// as memory use goes up so does the overhead that garbage collection
// imposes. I rather expect that at 90% occupancy of each half space that
// performance will have become rather low.
            if (b == NULL)
            {   if (used/9 > avail/10)
                {   printf("\nRun out of memory.\n");
                    my_exit(EXIT_FAILURE);
                }
            }
            else
            {   uintptr_t bi = (uintptr_t)b;
                b->halfbitmapsize = w1;
                b->hbase = b->h1base = ALIGN8(bi + sizeof(block_header));
                b->h1top = b->h2base = b->h1base + 64*b->halfbitmapsize;
                b->h2top = b->htop = b->h2base + 64*b->halfbitmapsize;
                b->h1starts = b->hstarts = (uint32_t *)b->htop;
                b->h2starts = (uint32_t *)((uintptr_t)b->hstarts + b->halfbitmapsize);
                b->h1fp = b->hfp = (uint32_t *)((uintptr_t)b->hstarts + 2*b->halfbitmapsize);
                b->h2fp = (uint32_t *)((uintptr_t)b->hfp + b->halfbitmapsize);
                b->pinned = (uint32_t *)((uintptr_t)b->hfp + 2*b->halfbitmapsize);
                blocks_by_age[nblocks] = bi;
                i = nblocks-1;
                while (i>0 && blocks[i]>bi)
                {   blocks[i+1] = blocks[i];
                    i--;
                }
                blocks[i] = bi;
                nblocks++;
            }
        } 
    }
    printf(" - collection complete\n");
    printf("Pins = %" PRIuPTR
           " heap1_pads = %" PRIuPTR
           " heap2_pads = %" PRIuPTR "\n",
           npins, heap1_pads, heap2_pads);
    heap1_pads = 0;
    fflush(stdout);
}

volatile int volatile_variable = 12345;

void check_space(int len, int line)
{
// The value passed will always be a multiple of 8. Ensure that that
// many bytes are available at fringe1, calling the garbage collector if
// necessary. This has to allow for the obstruction that pinned items
// lead to. There is some code rather similar to this that is used within
// the garbage collector while copying into heap2. The treatment of large
// memory blocks here will be a little tedious and coule be improved by
// checking the bitmap word at a time not bit at a time.
    intptr_t i;
    for (;;) // loop for when pinned items intrude.
    {   if (fringe1+len >= limit1)
        {   reclaim(line);
            continue;
        }
        for (i=0; i<len; i+=8)
            if (getheapstarts(fringe1+i)) break;
        if (i >= len) return; // success
// a block that looks like a string will serve as a padder...
        if (i > 0)
        {   qcar(fringe1) =
                tagHDR + typeSTRING + packlength(i-sizeof(LispObject));
            setheapstarts(fringe1);
            fringe1 += i;
            heap1_pads += i;
        }
        while (getheapstarts(fringe1))
        {   LispObject h;
// I now need to skip over the pinned item. If it is floating point,
// a cons cell or a symbol I have to detect that, otherwise its
// header gives its length explicitly.
            if (getheapfp(fringe1)) fringe1 += 8;
            else if (!isHDR(h = qcar(fringe1)))
                fringe1 += 2*sizeof(LispObject);
            else if ((h & TYPEBITS) == typeSYM)
                 fringe1 += SYMSIZE*sizeof(LispObject);
            else fringe1 += ALIGN8(sizeof(LispObject) + veclength(h));
        }
    }
}

void middle_reclaim()
{
// This function is here to have a stack frame (containing w) that will
// lie between that of reclaim and inner_reclaim. The stupid-looking test
// on volatile_variable is intended to persuade clever compilers that they
// should not compile this procedure in-line or consolidate its stack
// frame with either its caller or callee.
    int w;
    if (volatile_variable != volatile_variable)
    {   inner_reclaim(NULL);  // never executed!
        middle_reclaim();     // never executed!
    }
    inner_reclaim((LispObject *)((intptr_t)&w & -sizeof(LispObject)));
}

void reclaim(int line)
{
// The purpose of this function is to force any even partially
// reasonable C compiler into putting all registers that contain
// values from the caller onto the stack. It assumes that there
// could not be more than 12 "callee saves" registers, that the
// "register" qualifier in the declaraion here will cause a1-a12 to
// take precedence when allocating same, and that the volatile
// qualifier on the variable that is repeatedly referenced is there
// to try to tell the compiler that it may not make any assumptions
// (eg that a1-a12 might all have the same value), and the use
// of it again after the call to inner_reclaim() should force
// each of those values to be saved across that call. The work done
// is of course a waste (but assigning back to a volatile variable
// may force it to be done!) but is modest in the large scheme of
// things. On most machines I can think of there are a lot fewer
// than 12 callee-save registers and so this is overkill!
    register int a1 = volatile_variable,  a2 = volatile_variable,
                 a3 = volatile_variable,  a4 = volatile_variable,
                 a5 = volatile_variable,  a6 = volatile_variable,
                 a7 = volatile_variable,  a8 = volatile_variable,
                 a9 = volatile_variable,  a10 = volatile_variable,
                 a11 = volatile_variable, a12 = volatile_variable;
    middle_reclaim();
    volatile_variable +=
        volatile_variable*a1  + volatile_variable*a2 +
        volatile_variable*a3  + volatile_variable*a4 +
        volatile_variable*a5  + volatile_variable*a6 +
        volatile_variable*a7  + volatile_variable*a8 +
        volatile_variable*a9  + volatile_variable*a10 +
        volatile_variable*a11 + volatile_variable*a12;
}

// @@@ At present this does not understand about a segmented memory
// model...

void ensureheap2space(uintptr_t len)
{
// fringe2 points somewhere. Ensure that there are (at least) len
// bytes starting from there until the next used item in the heap
// (which will be something that had been pinned last time)
// If there is not a big enough gap here
// then fill any little gap with a padder item and skip past the
// pinned item that is in the way. Note with some alarm that if
// this skipping leads to a lot of wasted space then in the worst
// case heap2 could overflow. But ignore that issue for now and
// assume that there is always sufficient space in heap2, and so
// checking for its end is not necessary. Hmm the view I should take is
// is that failing to allocate here (because of fragmentation) is
// "just" a variant on "run out of memory".
    uintptr_t i;
// The outer loop is for potential repeated attempts. The inner loop
// checks each 8-byte location one at a time. If len was huge this
// could be an unpleasant expense that could be reduced by checking the
// bitmap word at a time rather than bit a time, but for the moment I
// value simplicity over performance.
    for (;;)
    {   for (i=0; i<len; i+=8)
            if (getheapstarts(fringe2+i)) break;
        if (i >= len) return; // success
// a block that looks like a string will serve as a padder...
// @@@ but if I have at least 2*sizeof(LispObject) free I should
// set up cells in heap2_freechain.
        if (i != 0)
        {   qcar(fringe2) =
                tagHDR + typeSTRING + packlength(i-sizeof(LispObject));
            setheapstarts(fringe2);
            fringe2 += i;
            heap2_pads += i;
        }
        while (getheapstarts(fringe2))
        {   LispObject h;
// I now need to skip over the pinned item. If it is floating point,
// a cons cell or a symbol I have to detect that, otherwise its
// header gives its length explicitly.
            if (getheapfp(fringe2)) fringe2 += 8;
            else if (!isHDR(h = qcar(fringe2)))
                fringe2 += 2*sizeof(LispObject);
            else if ((h & TYPEBITS) == typeSYM)
                 fringe2 += SYMSIZE*sizeof(LispObject);
            else fringe2 += ALIGN8(sizeof(LispObject) + veclength(h));
        }
    }
}

static inline LispObject copy(LispObject x)
{   LispObject h;
    int o;
// If the item has been pinned then it is returned unaltered.
// Note that FIXNUMs could give false positives in these tests,
// but since they would end up returned unaltered anyway that is
// not a problem!
    if ((inheap1(x) && getpinned(x)) ||
        inheap2(x)) return x;
    switch (x & TAGBITS)
    {   case tagCONS:
            assert(x != 0);
            assert(!isFORWARD(qcdr(x)));
            h = qcar(x);
            if (isFORWARD(h)) return (h - tagFORWARD);
//@@@
//          if (heap2_freechain != packfixnum(0))
//              @@@@
            ensureheap2space(2*sizeof(LispObject));
            setheapstarts(fringe2);
            qcar(fringe2) = h;
            qcdr(fringe2) = qcdr(x);
            h = fringe2;
            qcar(x) = tagFORWARD + h;
            fringe2 += 2*sizeof(LispObject);
            return h;
        case tagSYMBOL:
            h = qflags(x);
            if (isFORWARD(h)) return (h - tagFORWARD + tagSYMBOL);
            assert(isHDR(h));
            ensureheap2space(SYMSIZE*sizeof(LispObject));
            setheapstarts(fringe2);
            h = fringe2 + tagSYMBOL;
            qflags(h) = qflags(x);
            assert(!isFORWARD(qvalue(x)));
            qvalue(h) = qvalue(x);
            assert(!isFORWARD(qplist(x)));
            qplist(h) = qplist(x);
            assert(!isFORWARD(qpname(x))) ;
            qpname(h) = qpname(x);
            assert(!isFORWARD(qlits(x)));
            qlits(h)  = qlits(x);
            assert(!isFORWARD(qspare(x)));
            qspare(h) = qspare(x);
            qdefn0(h) = qdefn0(x);
            qdefn1(h) = qdefn1(x);
            qdefn2(h) = qdefn2(x);
            qdefn3(h) = qdefn3(x);
            qdefn4(h) = qdefn4(x);
            qdefn5up(h) = qdefn5up(x);
            fringe2 += SYMSIZE*sizeof(LispObject);
            qflags(x) = h - tagSYMBOL + tagFORWARD;
            return h;
        case tagATOM:
            if (x == NULLATOM) return x; // special case!
            h = qheader(x);
            if (isFORWARD(h)) return (h - tagFORWARD + tagATOM);
            assert(isHDR(h));
            o = ALIGN8(sizeof(LispObject) + veclength(h));
            ensureheap2space(o);
            setheapstarts(fringe2);
            o -= sizeof(LispObject); // space used by header word
            switch (h & TYPEBITS)
            {   case typeEQHASH:
// When a hash table is copied its header is changes to EQHASHX, which
// indicates that it will need rehashing before further use.
                    h ^= (typeEQHASH ^ typeEQHASHX);
                case typeEQHASHX:
                case typeSTRING:
                case typeVEC:
                case typeBIGNUM:
                    qcar(fringe2) = h; // copy header word across
                    h = fringe2 + tagATOM;
                    qheader(x) = fringe2 + tagFORWARD;
                    fringe2 += sizeof(LispObject);
                    x = x - tagATOM + sizeof(LispObject);
                    while (o > 0)
                    {   qcar(fringe2) = qcar(x);
                        fringe2 += sizeof(LispObject);
                        x += sizeof(LispObject);
                        o -= sizeof(LispObject);
                    }
                    return h;
                default:
                    //case typeSYM:
                    // also the spare codes!
                    assert(0);
            }
        case tagFLOAT:
// every float is 8 bytes wide, regardless of what sort of machine I am on.
            if (!getheapstarts(x)) // seen already.
                return qcar(x - tagFLOAT);
            else
            {   resetheapstarts(x);
                ensureheap2space(8);
                setheapstartsandfp(fringe2);
                h = fringe2 + tagFLOAT;
                fringe2 += 8;
                qfloat(h) = qfloat(x);             // copy the float.
                qcar(x-tagFLOAT) = h;              // forwarding address.
                return h;
            }
        case tagFIXNUM:
            return x;
        default:
//case tagFORWARD:
//case tagHDR:
            assert(0);
            return 0;  // avoid GCC moans.
    }
}

// s points at an object in the heap (old or new). Copy the
// components of the object and return the address one beyond it.

static inline LispObject copycontent(LispObject s)
{   LispObject h = s, w;
    assert(inheap(h) && getheapstarts(h));
    if (getheapfp(h)) return h + 8;
    h = qcar(s);
// I call copycontent with pointers into heap2 and there should never
// be forwarding pointers there. The only case where I call copycontent
// on something in heap1 is when it is pinned, and in that case it should
// not be forwarded either.
    assert(!isFORWARD(h));
    if (!isHDR(h)) // The item to be processed is a simple cons cell
    {   qcar(s) = copy(h);
        qcdr(s) = copy(qcdr(s));
        return s+2*sizeof(LispObject);
    }
// The item is one that uses a header
    switch (h & TYPEBITS)
    {   case typeSYM:
            w = s + tagSYMBOL;
            // qflags(w) does not need adjusting
            qvalue(w) = copy(qvalue(w));
            qplist(w) = copy(qplist(w));
            qpname(w) = copy(qpname(w));
            qlits(w)  = copy(qlits(w));
            qspare(w) = copy(qspare(w));
            // qdefn(w) does not need adjusting
            return s + SYMSIZE*sizeof(LispObject);
        case typeSTRING:
        case typeBIGNUM:
// These only contain binary information, so none of their content needs
// any more processing.
            return s + ALIGN8(sizeof(LispObject) + veclength(h));
        case typeVEC:
        case typeEQHASH:
        case typeEQHASHX:
// These are to be processed the same way. They contain a bunch of
// reference items.
            s+=sizeof(LispObject); // Past the header
            w = veclength(h);
            while (w > 0)
            {   qcar(s) = copy(qcar(s));
                s += sizeof(LispObject);
                w -= sizeof(LispObject);
            }
            return ALIGN8(s);
        default:
            // all the "spare" codes!
            assert(0);
            return 0;
    }
}

#define printPLAIN   1
#define printESCAPES 2

// I suspect that linelength and linepos need to be maintained
// independently for each output stream. At present that is not
// done.
int linelength = 80, linepos = 0, printflags = printESCAPES;

#define MAX_LISPFILES 30
#ifdef DEBUG
FILE *lispfiles[MAX_LISPFILES], *logfile = NULL;
#else // DEBUG
FILE *lispfiles[MAX_LISPFILES];
#endif // DEBUG
int32_t file_direction = 0, interactive = 0;
int lispin = 0, lispout = 1;

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

#define INPUT_LINE_SIZE 256
static char input_line[INPUT_LINE_SIZE];
static int input_ptr = 0, input_max = 0;

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
        if (c != EOF && qvalue(echo) != nil) wrch(c);
        return c;
    }
}

int gensymcounter = 1;

void checkspace(int n)
{   if (linepos + n >= linelength && lispout != -1 && lispout != -3) wrch('\n');
}

char printbuffer[32];

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
                if (qcar(x) == bignum &&
                    (pn = call1("~big2str", qcdr(x))) != NULLATOM &&
                    pn != nil)
                {   printflags = printPLAIN;
                    internalprint(pn);
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
            {   int len = sprintf(printbuffer, "g%.3d", gensymcounter++);
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
                        sprintf(printbuffer, "%" PRId64, qint64(x));
                        checkspace(len = strlen(printbuffer));
                        for (i=0; i<len; i++) wrch(printbuffer[i]);
                        return;
                    case typeVEC:
                    case typeEQHASH:
                    case typeEQHASHX:
                        sep = '[';
                        for (i=0; i<veclength(qheader(x))/sizeof(LispObject); i++)
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
                else if (isfinite(d)) sprintf(printbuffer, "%.14g", d);
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
            sprintf(printbuffer, "%" PRId64, (int64_t)qfixnum(x));
            checkspace(len = strlen(printbuffer));
            for (i=0; i<len; i++) wrch(printbuffer[i]);
            return;
        default:
//case tagFORWARD:
//case tagHDR:
//          sprintf(printbuffer, "??%#" PRIxPTR "??\n", x);
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

int curchar = '\n', symtype = 0;

int hexval(int n)
{   if (isdigit(n)) return n - '0';
    else if ('a' <= n && n <= 'f') return n - 'a' + 10;
    else if ('A' <= n && n <= 'F') return n - 'A' + 10;
    else return 0;
}

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
            else if (curchar != EOF && qvalue(lower) != nil) curchar = tolower(curchar);
            else if (curchar != EOF && qvalue(raise) != nil) curchar = toupper(curchar);
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
            {   r = call2("plus2", call2("times2", packfixnum(16), r),
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
            {   r = call2("plus2", call2("times2", packfixnum(10), r),
                           packfixnum(boffo[boffop++] - '0'));
            }
            if (neg) r = call1("minus", r);
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


LispObject lookup(const char *s, int len, int flag)
{   LispObject w, pn;
    int i, hash = 1;
    for (i=0; i<len; i++) hash = 13*hash + s[i];
    hash = (hash & 0x7fffffff) % OBHASH_SIZE;
    w = obhash[hash];
    while (w != tagFIXNUM)
    {   LispObject a = qcar(w);        // Will be a symbol.
        LispObject n = qpname(a);      // Will be a string.
        int l = veclength(qheader(n)); // Length of the name.
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

#define unwindNONE      0
#define unwindERROR     1
#define unwindBACKTRACE 2
#define unwindGO        4
#define unwindRETURN    8
#define unwindPRESERVE  16
#define unwindRESTART   32

int unwindflag = unwindNONE;

int backtraceflag = -1;
#define backtraceHEADER 1
#define backtraceTRACE  2

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
//- if (isCONS(x))
//- {  printf("eval: "); print(qcar(x)); // Eek! @@@@
//- }
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
    {
// This is GRIM debugging!
if ((uintptr_t)x < (uintptr_t)mem_base || (uintptr_t)x >= (uintptr_t)mem_end)
{  printf("x = %" PRIxPTR "\n", (uintptr_t)x);
   return error1("Invalid address for variable", nil);
}
        LispObject v = qvalue(x);
        if (v == undefined)
        {   backtraceflag |= backtraceHEADER | backtraceTRACE; // @@@
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
// @@@ need to respond to trace requests here... on both entry to
// @@@ and return from functions.
            switch (n)
            {
            case 0:
                if (flags & flagTRACED)
                {   linepos += printf("Calling: ");
                    errprint(f);
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
                x = (*qdefn2(f))(qlits(f), x, aa);
                if (unwindflag == unwindBACKTRACE)
                {   linepos += printf("Call to ");
                    errprint(f);
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
        else if (isCONS(f) && qcar(f) == lambda)
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

LispObject Land(LispObject lits, LispObject x)
{   LispObject r = lisptrue;
    while (isCONS(x))
    {   r = eval(qcar(x));
        if (r == nil || unwindflag != unwindNONE) return nil;
        x = qcdr(x);
    }
    return r;
}

LispObject Lor(LispObject lits, LispObject x)
{   while (isCONS(x))
    {   LispObject r;
        r = eval(qcar(x));
        if (r != nil || unwindflag != unwindNONE) return r;
        x = qcdr(x);
    }
    return nil;
}

// A list of lambda-variables should be a properly nil-terminated list
// of symbols, not including keywords or anyting declared global.

int allsymbols(LispObject bvl)
{
    while (isCONS(bvl))
    {   if (!isSYMBOL(qcar(bvl)) ||
            (qflags(qcar(bvl)) & flagGLOBAL) != 0) return 0;
        bvl = qcdr(bvl);
    }
    return (bvl == nil);
}

LispObject shallow_copy(LispObject x)
{
    LispObject p, q, w;
    if (!isCONS(x)) return x;
    p = q = cons(qcar(x), w = qcdr(x));
    if (unwindflag != unwindNONE) return nil;
    while (isCONS(w))
    {   x = w;
        w = cons(qcar(w), qcdr(w));
        if (unwindflag != unwindNONE) return nil;
        qcdr(q) = w;
        q = w;
        w = qcdr(x);
    }
    return p;
}

LispObject definer(LispObject x, int flags)
{
// x should be of the form
//     (name (arg list ...) body)
    LispObject name, def;
    if (!isCONS(x) ||
        !isSYMBOL(name = qcar(x)) ||
        !isCONS(def = qcdr(x)) ||
        !allsymbols(qcar(def)))
        return error1("malformed use of de, df or dm", x);
    if ((qflags(name) & flagSPECFORM) != 0)
        return error1("attempt to redefine special form", name);
    qflags(name) |= flags;
    if (flags == 0)
    {   qdefn0(name) = interpreted0;
        qdefn1(name) = interpreted1;
        qdefn2(name) = interpreted2;
        qdefn3(name) = interpreted3;
        qdefn4(name) = interpreted4;
        qdefn5up(name) = interpreted5up;
    }
    else
    {   qdefn0(name) = undefined0;
        qdefn1(name) = flags == flagSPECFORM ? interpretspecform :
                       interpreted1;
        qdefn2(name) = undefined2;
        qdefn3(name) = undefined3;
        qdefn4(name) = undefined4;
        qdefn5up(name) = undefined5up;
    }
// I make a copy of the bound variable list here so that the copy
// that will be used is not shared with any data that the user has access
// to. A consequence is that I can be certain that it is a list of
// symbols.
    def = cons(shallow_copy(qcar(def)), qcdr(def));
    qlits(name) = def;
// Now I will try to call macroexpand_list to expand all macros.
    x = call1("macroexpand_list", qcdr(def));
    if (x == NULLATOM || unwindflag != unwindNONE) return name;
    qlits(name) = cons(qcar(def), x);
    return name;
}

LispObject Lde(LispObject lits, LispObject x)
{   return definer(x, 0);
}

LispObject Ldf(LispObject lits, LispObject x)
{   return definer(x, flagSPECFORM);
}

LispObject Ldm(LispObject lits, LispObject x)
{   return definer(x, flagMACRO);
}

LispObject Lputd(LispObject lits, LispObject name, LispObject type, LispObject def)
{   if (!isSYMBOL(name)) return error1("bad name in putd", name);
    if (type == expr)
    {   if (!isCONS(def) || qcar(def) != lambda)
            return error1("bad definition in putd", def);
        return definer(cons(name, qcdr(def)), 0);
    }
    else if (type == fexpr)
    {   if (!isCONS(def) || qcar(def) != lambda)
            return error1("bad definition in putd", def);
        return definer(cons(name, qcdr(def)), flagSPECFORM);
    }
    else if (type == macro)
    {   if (!isCONS(def) || qcar(def) != lambda)
            return error1("bad definition in putd", def);
        return definer(cons(name, qcdr(def)), flagMACRO);
    }
    else if (type == subr)
    {   if (!isSYMBOL(def)) return error1("bad input with putd/subr", def);
        qlits(name) = qlits(def);
        qdefn0(name) = qdefn0(def);
        qdefn1(name) = qdefn1(def);
        qdefn2(name) = qdefn2(def);
        qdefn3(name) = qdefn3(def);
        qdefn4(name) = qdefn4(def);
        qdefn5up(name) = qdefn5up(def);
        return name;
    }
    else return error1("Bad type in putd", type);
}

LispObject Lsetq(LispObject lits, LispObject x)
{ // (setq var1 val1 var2 val2 ...)
    LispObject w = nil;
    while (isCONS(x) && isCONS(qcdr(x)))
    {   if (!isSYMBOL(w=qcar(x)) ||
            w == nil || w == lisptrue)
            return error1("bad variable in setq", x);
        w = eval(qcar(qcdr(x)));
        if (unwindflag != unwindNONE) return nil;
        qvalue(qcar(x)) = w;
        x = qcdr(qcdr(x));
    }
    return w;
}

LispObject Lprogn(LispObject lits, LispObject x)
{   LispObject r = nil;
    while (isCONS(x))
    {   r = eval(qcar(x));
        x = qcdr(x);
        if (unwindflag != unwindNONE) return nil;
    }
    return r;
}

LispObject Lprog(LispObject lits, LispObject x)
{   LispObject w, vars, saved = nil, save_x;
    if (!isCONS(x)) return nil;
    vars = qcar(x);
    x = qcdr(x);
// Now bind all the local variables, giving them the value nil.
    for (w=vars; isCONS(w); w=qcdr(w))
    {   LispObject v = qcar(w);
        if (!isSYMBOL(v))
            return error1("Not a symbol in variable list for prog", v);
        saved = cons(qvalue(v), saved);
        if (unwindflag != unwindNONE) return nil;
    }
    for (w=vars; isCONS(w); w=qcdr(w)) qvalue(qcar(w)) = nil;
    save_x = x;  // So that "go" can scan the whole block to find a label.
    work1 = nil;
    while (isCONS(x))
    {   if (isCONS(qcar(x))) eval(qcar(x));
        x = qcdr(x);
        if (unwindflag == unwindRETURN)
        {   unwindflag = unwindNONE;
            break;
        }
        else if (unwindflag == unwindGO)
        {   unwindflag = unwindNONE;
            x = save_x;
            while (isCONS(x) && qcar(x) != work1) x = qcdr(x);
            continue;
        }
        if (unwindflag != unwindNONE) break;
    }
// Now I must unbind all the variables.
    w = nreverse(vars);
    vars = nil;
    while (isCONS(w))
    {   LispObject x = w;
        w = qcdr(w);
        qcdr(x) = vars;
        vars = x;
        x = qcar(vars);
        qvalue(x) = qcar(saved);
        saved = qcdr(saved);
    }
    return work1;
}

LispObject Lgo(LispObject lits, LispObject x)
{   if (!isCONS(x) || !isSYMBOL(work1 = qcar(x)))
        return error1("bad go", x);
    work1 = qcar(x);
    if (unwindflag == unwindNONE) unwindflag = unwindGO;
    return nil;
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

LispObject Llist_0(LispObject lits)
{
    return nil;
}

LispObject Llist_1(LispObject lits, LispObject a1)
{
    return cons(a1, nil);
}

LispObject Llist_2(LispObject lits, LispObject a1, LispObject a2)
{
    return list2star(a1, a2, nil);
}

LispObject Llist_3(LispObject lits, LispObject a1,
                   LispObject a2, LispObject a3)
{
    return list2star(a1, a2, cons(a3, nil));
}

LispObject Llist_4(LispObject lits, LispObject a1, LispObject a2,
                   LispObject a3, LispObject a4)
{
    return list2star(a1, a2, list2star(a3, a4, nil));
}

LispObject Llist_5up(LispObject lits, LispObject a1, LispObject a2,
                   LispObject a3, LispObject a4, LispObject a5up)
{
    return list2star(a1, a2, list2star(a3, a4, a5up));
}

LispObject Lliststar_1(LispObject lits, LispObject a1)
{
    return a1;
}

LispObject Lliststar_2(LispObject lits, LispObject a1, LispObject a2)
{
    return cons(a1, a2);
}

LispObject Lliststar_3(LispObject lits, LispObject a1,
                       LispObject a2, LispObject a3)
{
    return list2star(a1, a2, a3);
}

LispObject Lliststar_4(LispObject lits, LispObject a1, LispObject a2,
                       LispObject a3, LispObject a4)
{
    return list2star(a1, a2, cons(a3, a4));
}

LispObject Lliststar_5up(LispObject lits, LispObject a1, LispObject a2,
                         LispObject a3, LispObject a4, LispObject a5up)
{
    LispObject p = a5up, q = qcdr(a5up), r;
    if (!isCONS(q))
    {   r = a5up;
        a5up = qcar(a5up);
    }
    else
    {   r = qcdr(q);
        while (isCONS(r))
        {   p = q;
            q = r;
            r = qcdr(q);
        }
        r = q;
        qcdr(p) = qcar(q);
    }
    qcar(r) = a4;    // re-use the cons cell.
    qcdr(r) = a5up;
    return list2star(a1, a2, cons(a3, r));
}

LispObject Lvector_0(LispObject lits)
{   return makevector(-1);
}

LispObject Lvector_1(LispObject lits, LispObject a1)
{   LispObject r = makevector(0);
    elt(r, 0) = a1;
    return r;
}

LispObject Lvector_2(LispObject lits, LispObject a1, LispObject a2)
{   LispObject r = makevector(1);
    elt(r, 0) = a1;
    elt(r, 1) = a2;
    return r;
}

LispObject Lvector_3(LispObject lits, LispObject a1,
                   LispObject a2, LispObject a3)
{   LispObject r = makevector(2);
    elt(r, 0) = a1;
    elt(r, 1) = a2;
    elt(r, 2) = a3;
    return r;
}

LispObject Lvector_4(LispObject lits, LispObject a1, LispObject a2,
                   LispObject a3, LispObject a4)
{   LispObject r = makevector(3);
    elt(r, 0) = a1;
    elt(r, 1) = a2;
    elt(r, 2) = a3;
    elt(r, 3) = a4;
    return r;
}

LispObject Lvector_5up(LispObject lits, LispObject a1, LispObject a2,
                   LispObject a3, LispObject a4, LispObject a5up)
{   int len = 3;
    for (LispObject w=a5up; isCONS(w); w=qcdr(w)) len++;
    LispObject r = makevector(len);
    elt(r, 0) = a1;
    elt(r, 1) = a2;
    elt(r, 2) = a3;
    elt(r, 3) = a4;
    len = 4;
    while (isCONS(a5up))
    {   elt(r, len) = qcar(a5up);
        a5up = qcdr(a5up);
        len++;
    }
    return r;
}

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

#define floatval(x)                   \
   isFLOAT(x) ? qfloat(x) :           \
   isFIXNUM(x) ? (double)qfixnum(x) : \
   isBIGNUM(x) ? (double)qint64(x) :  \
   0.0

LispObject Lfp_subnorm(LispObject lits, LispObject arg)
{   if (isFLOAT(arg)) return nil;
    double d = qfloat(arg);
    return (std::isfinite(d) && !std::isnormal(d)) ? lisptrue : nil;
}

LispObject Lfp_infinite(LispObject lits, LispObject arg)
{   if (isFLOAT(arg)) return nil;
    double d = qfloat(arg);
    return (std::isinf(d)) ? lisptrue : nil;
}

LispObject Lfp_nan(LispObject lits, LispObject arg)
{   if (isFLOAT(arg)) return nil;
    double d = qfloat(arg);
    return (std::isnan(d)) ? lisptrue : nil;
}

LispObject Lfp_finite(LispObject lits, LispObject arg)
{   if (isFLOAT(arg)) return nil;
    double d = qfloat(arg);
    return (std::isfinite(d)) ? lisptrue : nil;
}

LispObject Lfp_signbit(LispObject lits, LispObject arg)
{   if (isFLOAT(arg)) return nil;
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
    return (qvalue(x) = y);
}

LispObject Lboundp(LispObject lits, LispObject x)
{
    return (isSYMBOL(x) && qvalue(x)!=undefined) ? lisptrue : nil;
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

LispObject Loblist(LispObject lits)
{   int i;
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
    if (isCONS(x) && qcar(x) == lambda)
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

LispObject Lupbv(LispObject lits, LispObject x)
{
    if (!isVEC(x)) return error1("bad arg to upbv", x);
    return makeinteger(veclength(qheader(x))/sizeof(LispObject)-1);
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
                lambda,
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
    else if (qdefn0(x) == interpreted0 &&
             (qdefn1(x) == interpreted1 ||
              qdefn1(x) == interpretspecform) &&
             qdefn2(x) == interpreted2 &&
             qdefn3(x) == interpreted3 &&
             qdefn4(x) == interpreted4 &&
             qdefn5up(x) == interpreted5up)
        return list2star((qflags(x) & flagMACRO) ? macro : expr,
            lambda,
            r);
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

// Now some numeric functions

#undef FF
#undef BB
#define FF(a, b) ((a) > (b) ? lisptrue : nil)
#define BB(a, b) ((a) > (b) ? lisptrue : nil)

LispObject Lgreaterp(LispObject lits, LispObject x, LispObject y)
{
    NUMOP("greaterp", x, y);
}

#undef FF
#undef BB
#define FF(a, b) ((a) >= (b) ? lisptrue : nil)
#define BB(a, b) ((a) >= (b) ? lisptrue : nil)

LispObject Lgeq(LispObject lits, LispObject x, LispObject y)
{
    NUMOP("geq", x, y);
}

#undef FF
#undef BB
#define FF(a, b) ((a) < (b) ? lisptrue : nil)
#define BB(a, b) ((a) < (b) ? lisptrue : nil)

LispObject Llessp(LispObject lits, LispObject x, LispObject y)
{
    NUMOP("lessp", x, y);
}

#undef FF
#undef BB
#define FF(a, b) ((a) <= (b) ? lisptrue : nil)
#define BB(a, b) ((a) <= (b) ? lisptrue : nil)

LispObject Lleq(LispObject lits, LispObject x, LispObject y)
{
    NUMOP("leq", x, y);
}

LispObject Lminus(LispObject lits, LispObject x)
{
    return Nminus(x);
}

LispObject Lminusp(LispObject lits, LispObject x)
{
// Anything non-numeric will not be negative!
    if ((isFIXNUM(x) && x < 0) ||
        (isFLOAT(x) && qfloat(x) < 0.0) ||
        (isATOM(x) &&
         (qheader(x) & TYPEBITS) == typeBIGNUM &&
         qint64(x) < 0)) return lisptrue;
    else return nil;
}

#undef BB
#define BB(a) makeinteger(~(a))

LispObject Llognot(LispObject lits, LispObject x)
{
    UNARYINTOP("lognot", x);
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

#undef FF
#undef BB
#define FF(a) boxfloat((a) + 1.0)
#define BB(a) makeinteger((a) + 1)

LispObject Ladd1(LispObject lits, LispObject x)
{
    UNARYOP("add1", x);
}

#undef FF
#undef BB
#define FF(a) boxfloat((a) - 1.0)
#define BB(a) makeinteger((a) - 1)

LispObject Lsub1(LispObject lits, LispObject x)
{
    UNARYOP("sub1", x);
}

#undef FF
#undef BB
#define FF(a, b) boxfloat((a) - (b))
#define BB(a, b) makeinteger((a) - (b))

LispObject Ldifference(LispObject lits, LispObject x, LispObject y)
{
    NUMOP("difference", x, y);
}

#undef FF
#undef BB
#define FF(a, b) ((b) == 0.0 ? error1("division by 0.0", nil) : \
                  boxfloat((a) / (b)))
#define BB(a, b) ((b) == 0 ? error1("division by 0", nil) : \
                  makeinteger((a) / (b)))

LispObject Lquotient(LispObject lits, LispObject x, LispObject y)
{
    NUMOP("quotient", x, y);
}

#undef BB
#define BB(a, b) ((b) == 0 ? error1("remainder by 0", nil) : \
                  makeinteger((a) % (b)))

LispObject Lremainder(LispObject lits, LispObject x, LispObject y)
{
    INTOP("remainder", x, y);
}

#undef BB
#define BB(a, b) ((b) == 0 ? error1("division by 0", nil) : \
                  cons(makeinteger((a) / (b)), makeinteger((a) % (b))))

LispObject Ldivide(LispObject lits, LispObject x, LispObject y)
{
    INTOP("divide", x, y);
}

#undef BB
#define BB(a) makeinteger((a) << sh)

LispObject Lleftshift(LispObject lits, LispObject x, LispObject y)
{   int sh;
    if (!isFIXNUM(y)) return error1("Bad argument for leftshift", y);
    sh = (int)qfixnum(y);
    UNARYINTOP("leftshift", x);
}

#undef BB
#define BB(a) makeinteger((a) >> sh)

LispObject Lrightshift(LispObject lits, LispObject x, LispObject y)
{   int sh;
    if (!isFIXNUM(y)) return error1("Bad argument for rightshift", y);
    sh = (int)qfixnum(y);
    UNARYINTOP("rightshift", x);
}

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
    ch[0] = qvalue(lower) != nil ? tolower(curchar) :
            qvalue(raise) != nil ? toupper(curchar) : curchar;
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
        {   lispin = n;
            symtype = '?';
            if (curchar == EOF) curchar = '\n';
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

#define LONGEST_FILENAME 1000
char filename[LONGEST_FILENAME];
static char imagename[LONGEST_FILENAME];

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
    if (*qstring(x)=='$' && (p=strchr(qstring(x), '/'))!=NULL)
    {   sprintf(filename, "@%.*s", (int)(p-qstring(x))-1, 1+qstring(x));
        lits = qvalue(lookup(filename, strlen(filename), 0));
        if (isSTRING(lits)) sprintf(filename, "%.*s%.*s",
           (int)veclength(qheader(lits)), qstring(lits),
           (int)(veclength(qheader(x)) - (p-qstring(x))), p);
        else sprintf(filename, "%.*s", (int)veclength(qheader(x)), qstring(x));
    }
    else sprintf(filename, "%.*s", (int)veclength(qheader(x)), qstring(x));
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
        return packfixnum(n);
    }
    return error1("too many open files", x);;
}

LispObject Lopen_module(LispObject lits, LispObject x, LispObject y)
{   FILE *f;
    int n, how = 0;
    if (isSYMBOL(x)) x = qpname(x);
    if (!isSTRING(x) ||
        !((y == input && (how=1)!=0) ||
          (y == output && (how=2)!=0)))
        return error1("bad arg for open-module", cons(x, y));
    sprintf(filename, "%s.modules/%.*s.fasl", imagename,
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
        {   if (lispin == n) lispin = 3;
            if (lispout == n) lispout = 1;
            if (lispfiles[n] != NULL) fclose(lispfiles[n]);
            lispfiles[n] = NULL;
            file_direction &= ~(1<<n);
        }
    }
    return nil;
}

void readevalprint(int loadp)
{   while (symtype != EOF)
    {   LispObject r;
        LispObject save_echo = qvalue(echo);
        unwindflag = unwindNONE;
        if (loadp) qvalue(echo) = nil;
        backtraceflag = backtraceHEADER | backtraceTRACE;
        r = readS();
        qvalue(echo) = save_echo;
        fflush(stdout);
        if (unwindflag != unwindNONE) /* Do nothing */ ;
        else if (loadp || qvalue(dfprint) == nil ||
            (isCONS(r) && (qcar(r) == lookup("rdf", 3, 2) ||
                           qcar(r) == lookup("faslend", 7, 2))))
        {   r = eval(r);
            if (unwindflag == unwindNONE && !loadp)
            {   linepos += printf("Value: ");
#ifdef DEBUG
                if (logfile != NULL) fprintf(logfile, "Value: ");
#endif // DEBUG
                print(r);
                fflush(stdout);
            }
        }
        else
        {   r = cons(r, nil);
            if (unwindflag == unwindNONE) Lapply(nil, qvalue(dfprint), r);
        }
        if ((unwindflag & (unwindPRESERVE | unwindRESTART)) != 0) return;
    }
}

LispObject Lrdf(LispObject lits, LispObject x)
{   int f, f1, savech = curchar, savetype = symtype;
    f1 = Lopen(nil, x, input);
    if (unwindflag != unwindNONE) return nil;
    f = Lrds(nil, f1);
    readevalprint(0);
    Lrds(nil, f);
    Lclose(nil, f1);
    curchar = savech;
    symtype = savetype;
    printf("+++ End of rdf\n");
    return nil;
}

LispObject Lload_module(LispObject lits, LispObject x)
{   int f, f1, savech = curchar, savetype = symtype;
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
    curchar = savech;
    symtype = savetype;
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

LispObject Lerrorset_3(LispObject lits, LispObject a1,
                       LispObject a2, LispObject a3)
{   int save = backtraceflag;
    backtraceflag = 0;
    if (a2 != nil) backtraceflag |= backtraceHEADER;
    if (a3 != nil) backtraceflag |= backtraceTRACE;
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

#define SETUPSPEC                                               \
    SETUP_TABLE_SELECT("quote",             Lquote),            \
    SETUP_TABLE_SELECT("cond",              Lcond),             \
    SETUP_TABLE_SELECT("and",               Land),              \
    SETUP_TABLE_SELECT("or",                Lor),               \
    SETUP_TABLE_SELECT("setq",              Lsetq),             \
    SETUP_TABLE_SELECT("progn",             Lprogn),            \
    SETUP_TABLE_SELECT("go",                Lgo),

#define SETUPSPECa                                              \
    SETUP_TABLE_SELECT("de",                Lde),               \
    SETUP_TABLE_SELECT("df",                Ldf),               \
    SETUP_TABLE_SELECT("dm",                Ldm),               \
    SETUP_TABLE_SELECT("prog",              Lprog),

#define SETUP0                                                  \
    SETUP_TABLE_SELECT("date",              Ldate),             \
    SETUP_TABLE_SELECT("list",              Llist_0),           \
    SETUP_TABLE_SELECT("iplus",             Lplus_0),           \
    SETUP_TABLE_SELECT("itimes",            Ltimes_0),          \
    SETUP_TABLE_SELECT("ilogand",           Llogand_0),         \
    SETUP_TABLE_SELECT("ilogor",            Llogor_0),          \
    SETUP_TABLE_SELECT("ilogxor",           Llogxor_0),         \
    SETUP_TABLE_SELECT("checkpoint",        Lpreserve_0),       \
    SETUP_TABLE_SELECT("error",             Lerror_0),          \
    SETUP_TABLE_SELECT("gensym",            Lgensym_0),         \
    SETUP_TABLE_SELECT("oblist",            Loblist),           \
    SETUP_TABLE_SELECT("posn",              Lposn),             \
    SETUP_TABLE_SELECT("preserve",          Lpreserve_0),       \
    SETUP_TABLE_SELECT("read",              Lread),             \
    SETUP_TABLE_SELECT("readch",            Lreadch),           \
    SETUP_TABLE_SELECT("readline",          Lreadline),         \
    SETUP_TABLE_SELECT("reclaim",           Lreclaim_0),        \
    SETUP_TABLE_SELECT("restart-csl",       Lrestart_lisp_0),   \
    SETUP_TABLE_SELECT("restart-lisp",      Lrestart_lisp_0),   \
    SETUP_TABLE_SELECT("return",            Lreturn_0),         \
    SETUP_TABLE_SELECT("stop",              Lstop_0),           \
    SETUP_TABLE_SELECT("terpri",            Lterpri),           \
    SETUP_TABLE_SELECT("time",              Ltime),             \
    SETUP_TABLE_SELECT("vector",            Lvector_0),

#define SETUP0a

#define SETUP1                                                  \
    SETUP_TABLE_SELECT("list",              Llist_1),           \
    SETUP_TABLE_SELECT("list*",             Lliststar_1),       \
    SETUP_TABLE_SELECT("iplus",             Lplus_1),           \
    SETUP_TABLE_SELECT("itimes",            Ltimes_1),          \
    SETUP_TABLE_SELECT("ilogand",           Llogand_1),         \
    SETUP_TABLE_SELECT("ilogor",            Llogor_1),          \
    SETUP_TABLE_SELECT("ilogxor",           Llogxor_1),         \
    SETUP_TABLE_SELECT("allocate-string",   Lallocate_string),  \
    SETUP_TABLE_SELECT("atan",              Latan),             \
    SETUP_TABLE_SELECT("atom",              Latom),             \
    SETUP_TABLE_SELECT("bignump",           Lbignump),          \
    SETUP_TABLE_SELECT("boundp",            Lboundp),           \
    SETUP_TABLE_SELECT("car",               Lcar),              \
    SETUP_TABLE_SELECT("cdr",               Lcdr),              \
    SETUP_TABLE_SELECT("char-code",         Lcharcode),         \
    SETUP_TABLE_SELECT("char-downcase",     Lchar_downcase),    \
    SETUP_TABLE_SELECT("char-upcase",       Lchar_upcase),      \
    SETUP_TABLE_SELECT("checkpoint",        Lpreserve_1),       \
    SETUP_TABLE_SELECT("close",             Lclose),            \
    SETUP_TABLE_SELECT("code-char",         Lcodechar),         \
    SETUP_TABLE_SELECT("compress",          Lcompress),         \
    SETUP_TABLE_SELECT("cos",               Lcos),              \
    SETUP_TABLE_SELECT("error",             Lerror_1),          \
    SETUP_TABLE_SELECT("errorset",          Lerrorset_1),       \
    SETUP_TABLE_SELECT("eval",              Leval),             \
    SETUP_TABLE_SELECT("exp",               Lexp),              \
    SETUP_TABLE_SELECT("explode",           Lexplode),          \
    SETUP_TABLE_SELECT("explodec",          Lexplodec),         \
    SETUP_TABLE_SELECT("exploden",          Lexploden),         \
    SETUP_TABLE_SELECT("explodecn",         Lexplodecn),        \
    SETUP_TABLE_SELECT("float-denormalized-p", Lfp_subnorm),    \
    SETUP_TABLE_SELECT("float-infinity-p",  Lfp_infinite),      \
    SETUP_TABLE_SELECT("fp-infinite",       Lfp_infinite),      \
    SETUP_TABLE_SELECT("fp-nan",            Lfp_nan),           \
    SETUP_TABLE_SELECT("fp-finite",         Lfp_finite),        \
    SETUP_TABLE_SELECT("fp-subnorm",        Lfp_subnorm),       \
    SETUP_TABLE_SELECT("fp-signbit",        Lfp_signbit),       \
    SETUP_TABLE_SELECT("iadd1",             Ladd1),             \
    SETUP_TABLE_SELECT("iceiling",          Lceiling),          \
    SETUP_TABLE_SELECT("ifix",              Lfix),              \
    SETUP_TABLE_SELECT("ifixp",             Lfixp),             \
    SETUP_TABLE_SELECT("ifloat",            Lfloat),            \
    SETUP_TABLE_SELECT("ilognot",           Llognot),           \
    SETUP_TABLE_SELECT("iminus",            Lminus),            \
    SETUP_TABLE_SELECT("iminusp",           Lminusp),           \
    SETUP_TABLE_SELECT("inumberp",          Lnumberp),          \
    SETUP_TABLE_SELECT("isub1",             Lsub1),             \
    SETUP_TABLE_SELECT("floatp",            Lfloatp),           \
    SETUP_TABLE_SELECT("ifloor",            Lfloor),            \
    SETUP_TABLE_SELECT("gensym",            Lgensym_1),         \
    SETUP_TABLE_SELECT("getd",              Lgetd),             \
    SETUP_TABLE_SELECT("load-module",       Lload_module),      \
    SETUP_TABLE_SELECT("log",               Llog),              \
    SETUP_TABLE_SELECT("mkhash",            Lmkhash_1),         \
    SETUP_TABLE_SELECT("mkvect",            Lmkvect),           \
    SETUP_TABLE_SELECT("null",              Lnull),             \
    SETUP_TABLE_SELECT("onep",              Lonep),             \
    SETUP_TABLE_SELECT("plist",             Lplist),            \
    SETUP_TABLE_SELECT("preserve",          Lpreserve_1),       \
    SETUP_TABLE_SELECT("prin",              Lprin),             \
    SETUP_TABLE_SELECT("princ",             Lprinc),            \
    SETUP_TABLE_SELECT("prin1",             Lprin),             \
    SETUP_TABLE_SELECT("prin2",             Lprinc),            \
    SETUP_TABLE_SELECT("print",             Lprint),            \
    SETUP_TABLE_SELECT("printc",            Lprintc),           \
    SETUP_TABLE_SELECT("rdf",               Lrdf),              \
    SETUP_TABLE_SELECT("rds",               Lrds),              \
    SETUP_TABLE_SELECT("reclaim",           Lreclaim_1),        \
    SETUP_TABLE_SELECT("restart-csl",       Lrestart_lisp_1),   \
    SETUP_TABLE_SELECT("restart-lisp",      Lrestart_lisp_1),   \
    SETUP_TABLE_SELECT("return",            Lreturn_1),         \
    SETUP_TABLE_SELECT("sin",               Lsin),              \
    SETUP_TABLE_SELECT("sqrt",              Lsqrt),             \
    SETUP_TABLE_SELECT("stop",              Lstop_1),           \
    SETUP_TABLE_SELECT("stringp",           Lstringp),          \
    SETUP_TABLE_SELECT("symbolp",           Lsymbolp),          \
    SETUP_TABLE_SELECT("trace",             Ltrace),            \
    SETUP_TABLE_SELECT("untrace",           Luntrace),          \
    SETUP_TABLE_SELECT("upbv",              Lupbv),             \
    SETUP_TABLE_SELECT("vectorp",           Lvectorp),          \
    SETUP_TABLE_SELECT("wrs",               Lwrs),              \
    SETUP_TABLE_SELECT("vector",            Lvector_1),         \
    SETUP_TABLE_SELECT("zerop",             Lzerop),

#define SETUP1a

#define SETUP2                                                  \
    SETUP_TABLE_SELECT("list",              Llist_2),           \
    SETUP_TABLE_SELECT("list*",             Lliststar_2),       \
    SETUP_TABLE_SELECT("iplus",             Lplus_2),           \
    SETUP_TABLE_SELECT("itimes",            Ltimes_2),          \
    SETUP_TABLE_SELECT("ilogand",           Llogand_2),         \
    SETUP_TABLE_SELECT("ilogor",            Llogor_2),          \
    SETUP_TABLE_SELECT("ilogxor",           Llogxor_2),         \
    SETUP_TABLE_SELECT("apply",             Lapply),            \
    SETUP_TABLE_SELECT("checkpoint",        Lpreserve_2),       \
    SETUP_TABLE_SELECT("cons",              Lcons),             \
    SETUP_TABLE_SELECT("eq",                Leq),               \
    SETUP_TABLE_SELECT("equal",             Lequal),            \
    SETUP_TABLE_SELECT("error",             Lerror_2),          \
    SETUP_TABLE_SELECT("errorset",          Lerrorset_2),       \
    SETUP_TABLE_SELECT("idifference",       Ldifference),       \
    SETUP_TABLE_SELECT("idivide",           Ldivide),           \
    SETUP_TABLE_SELECT("iequal",            Lequal),            \
    SETUP_TABLE_SELECT("igeq",              Lgeq),              \
    SETUP_TABLE_SELECT("igreaterp",         Lgreaterp),         \
    SETUP_TABLE_SELECT("ileftshift",        Lleftshift),        \
    SETUP_TABLE_SELECT("ileq",              Lleq),              \
    SETUP_TABLE_SELECT("ilessp",            Llessp),            \
    SETUP_TABLE_SELECT("iquotient",         Lquotient),         \
    SETUP_TABLE_SELECT("iremainder",        Lremainder),        \
    SETUP_TABLE_SELECT("irightshift",       Lrightshift),       \
    SETUP_TABLE_SELECT("get",               Lget),              \
    SETUP_TABLE_SELECT("gethash",           Lgethash),          \
    SETUP_TABLE_SELECT("getv",              Lgetv),             \
    SETUP_TABLE_SELECT("member",            Lmember),           \
    SETUP_TABLE_SELECT("memq",              Lmemq),             \
    SETUP_TABLE_SELECT("mkhash",            Lmkhash_2),         \
    SETUP_TABLE_SELECT("open",              Lopen),             \
    SETUP_TABLE_SELECT("open-module",       Lopen_module),      \
    SETUP_TABLE_SELECT("preserve",          Lpreserve_2),       \
    SETUP_TABLE_SELECT("prog1",             Lprog1),            \
    SETUP_TABLE_SELECT("prog2",             Lprog2),            \
    SETUP_TABLE_SELECT("remhash",           Lremhash),          \
    SETUP_TABLE_SELECT("remprop",           Lremprop),          \
    SETUP_TABLE_SELECT("restart-csl",       Lrestart_lisp_2),   \
    SETUP_TABLE_SELECT("restart-lisp",      Lrestart_lisp_2),   \
    SETUP_TABLE_SELECT("rplaca",            Lrplaca),           \
    SETUP_TABLE_SELECT("rplacd",            Lrplacd),           \
    SETUP_TABLE_SELECT("set",               Lset),              \
    SETUP_TABLE_SELECT("vector",            Lvector_2),

#define SETUP2a

#define SETUP3                                                  \
    SETUP_TABLE_SELECT("list",              Llist_3),           \
    SETUP_TABLE_SELECT("list*",             Lliststar_3),       \
    SETUP_TABLE_SELECT("iplus",             Lplus_3),           \
    SETUP_TABLE_SELECT("itimes",            Ltimes_3),          \
    SETUP_TABLE_SELECT("ilogand",           Llogand_3),         \
    SETUP_TABLE_SELECT("ilogor",            Llogor_3),          \
    SETUP_TABLE_SELECT("ilogxor",           Llogxor_3),         \
    SETUP_TABLE_SELECT("checkpoint",        Lpreserve_3),       \
    SETUP_TABLE_SELECT("errorset",          Lerrorset_3),       \
    SETUP_TABLE_SELECT("mkhash",            Lmkhash_3),         \
    SETUP_TABLE_SELECT("preserve",          Lpreserve_3),       \
    SETUP_TABLE_SELECT("put",               Lput),              \
    SETUP_TABLE_SELECT("putd",              Lputd),             \
    SETUP_TABLE_SELECT("puthash",           Lputhash),          \
    SETUP_TABLE_SELECT("putv",              Lputv),             \
    SETUP_TABLE_SELECT("string-store",      Lstring_store1),    \
    SETUP_TABLE_SELECT("string-store1",     Lstring_store1),    \
    SETUP_TABLE_SELECT("vector",            Lvector_3),

#define SETUP3a

#define SETUP4                                                  \
    SETUP_TABLE_SELECT("list",              Llist_4),           \
    SETUP_TABLE_SELECT("list*",             Lliststar_4),       \
    SETUP_TABLE_SELECT("iplus",             Lplus_4),           \
    SETUP_TABLE_SELECT("itimes",            Ltimes_4),          \
    SETUP_TABLE_SELECT("ilogand",           Llogand_4),         \
    SETUP_TABLE_SELECT("ilogor",            Llogor_4),          \
    SETUP_TABLE_SELECT("ilogxor",           Llogxor_4),         \
    SETUP_TABLE_SELECT("checkpoint",        Lpreserve_4),       \
    SETUP_TABLE_SELECT("preserve",          Lpreserve_4),       \
    SETUP_TABLE_SELECT("string-store2",     Lstring_store2),    \
    SETUP_TABLE_SELECT("vector",            Lvector_4),

#define SETUP4a

#define SETUP5UP                                                \
    SETUP_TABLE_SELECT("list",              Llist_5up),         \
    SETUP_TABLE_SELECT("list*",             Lliststar_5up),     \
    SETUP_TABLE_SELECT("iplus",             Lplus_5up),         \
    SETUP_TABLE_SELECT("itimes",            Ltimes_5up),        \
    SETUP_TABLE_SELECT("ilogand",           Llogand_5up),       \
    SETUP_TABLE_SELECT("ilogor",            Llogor_5up),        \
    SETUP_TABLE_SELECT("ilogxor",           Llogxor_5up),       \
    SETUP_TABLE_SELECT("string-store3",     Lstring_store3),    \
    SETUP_TABLE_SELECT("string-store4",     Lstring_store4),    \
    SETUP_TABLE_SELECT("vector",            Lvector_5up),

#define SETUP5UPa

// The following are things that can be in function cells but that are
// not there as straightforward definitions of particular functions.
// They are listed here to cope with the needs of dumping and restoring
// heap images.

#define SETUP_INTERNAL                                          \
    SETUP_TABLE_SELECT("0undefined0",       undefined0),        \
    SETUP_TABLE_SELECT("1undefined1",       undefined1),        \
    SETUP_TABLE_SELECT("2undefined2",       undefined2),        \
    SETUP_TABLE_SELECT("3undefined3",       undefined3),        \
    SETUP_TABLE_SELECT("4undefined4",       undefined4),        \
    SETUP_TABLE_SELECT("5undefined5",       undefined5up),      \
    SETUP_TABLE_SELECT("0wrongnumber0",     wrongnumber0),      \
    SETUP_TABLE_SELECT("1wrongnumber1",     wrongnumber1),      \
    SETUP_TABLE_SELECT("2wrongnumber2",     wrongnumber2),      \
    SETUP_TABLE_SELECT("3wrongnumber3",     wrongnumber3),      \
    SETUP_TABLE_SELECT("4wrongnumber4",     wrongnumber4),      \
    SETUP_TABLE_SELECT("5wrongnumber5",     wrongnumber5up),    \
    SETUP_TABLE_SELECT("0interpreted0",     interpreted0),      \
    SETUP_TABLE_SELECT("1interpreted1",     interpreted1),      \
    SETUP_TABLE_SELECT("2interpreted2",     interpreted2),      \
    SETUP_TABLE_SELECT("3interpreted3",     interpreted3),      \
    SETUP_TABLE_SELECT("4interpreted4",     interpreted4),      \
    SETUP_TABLE_SELECT("5interpreted5",     interpreted5up)

// In order that it is possible to save and restore images and end up with
// function entrypoints correctly fixed up I need to be certain that the
// version of vsl that saved an image has at least the same set of functions
// provided as the version reloading. 


// With subversion there is an unambiguos concept of "revision number" and
// I can insert that here. If one used git the collection of checkins
// is not linear and it is much harder to have an obvious way of setting
// automatic labels on things!

const char *setup_revision = "$Revision: 0000 $";

#define MAX_NAMESIZE  24

const char setup_names[][MAX_NAMESIZE] =
{
#define SETUP_TABLE_SELECT(a, b) { "s" a }
    SETUPSPEC
    SETUPSPECa
#undef SETUP_TABLE_SELECT
#define SETUP_TABLE_SELECT(a, b) { "0" a }
    SETUP0
    SETUP0a
#undef SETUP_TABLE_SELECT
#define SETUP_TABLE_SELECT(a, b) { "1" a }
    SETUP1
    SETUP1a
#undef SETUP_TABLE_SELECT
#define SETUP_TABLE_SELECT(a, b) { "2" a }
    SETUP2
    SETUP2a
#undef SETUP_TABLE_SELECT
#define SETUP_TABLE_SELECT(a, b) { "3" a }
    SETUP3
    SETUP3a
#undef SETUP_TABLE_SELECT
#define SETUP_TABLE_SELECT(a, b) { "4" a }
    SETUP4
    SETUP4a
#undef SETUP_TABLE_SELECT
#define SETUP_TABLE_SELECT(a, b) { "5" a }
    SETUP5UP
    SETUP5UPa
    { "x" },                      // Marks end of real functions
#undef SETUP_TABLE_SELECT
#define SETUP_TABLE_SELECT(a, b) { a }
    SETUP_INTERNAL
};

#undef SETUP_TABLE_SELECT
#define SETUP_TABLE_SELECT(a, b) (void *)b

void *setup_defs[] =
{
    SETUPSPEC
    SETUPSPECa
    SETUP0
    SETUP0a
    SETUP1
    SETUP1a
    SETUP2
    SETUP2a
    SETUP3
    SETUP3a
    SETUP4
    SETUP4a
    SETUP5UP
    SETUP5UPa
    NULL,
    SETUP_INTERNAL
};

#define SETUPSIZE  ((int)(sizeof(setup_defs)/sizeof(void *)))

// The number of entries I have in my setup hash table will be about
// twice the number of entries to put in it, and is not a multiple of
// 2, 3, 5, 7 or 11. Well until I have over 1000 built-in functions its
// size is just established by the rounding-up procedure here, however
// it should scale reasonably happily to almost any size of system.

#define SETUPHASHSIZE (((2*3*5*7*11)*((int)SETUPSIZE/1000+1))+1)
#define HASHPTR(x)    ((int)(((uintptr_t)(x)*314159u)%SETUPHASHSIZE))

void setwrongnumbers(LispObject w)
{   if (qdefn0(w) == undefined0) qdefn0(w) = wrongnumber0;
    if (qdefn1(w) == undefined1) qdefn1(w) = wrongnumber1;
    if (qdefn2(w) == undefined2) qdefn2(w) = wrongnumber2;
    if (qdefn3(w) == undefined3) qdefn3(w) = wrongnumber3;
    if (qdefn4(w) == undefined4) qdefn4(w) = wrongnumber4;
    if (qdefn5up(w) == undefined5up) qdefn5up(w) = wrongnumber5up;
}

void setup()
{
// Ensure that initial symbols and functions are in place. Parts of this
// code are rather rambling and repetitive but this is at least a simple
// way to do things. I am going to assume that nothing can fail within this
// setup code, so I can omit all checks for error conditions.
    int i;
    undefined = lookup("~indefinite-value~", 18, 3);
    qflags(undefined) |= flagGLOBAL;
    qvalue(undefined) = undefined;
    nil = lookup("nil", 3, 3);
    qflags(nil) |= flagGLOBAL;
    qvalue(nil) = nil;
    lisptrue = lookup("t", 1, 3);
    qflags(lisptrue) |= flagGLOBAL;
    qvalue(lisptrue) = lisptrue;
    qvalue(echo = lookup("*echo", 5, 3)) = interactive ? nil : lisptrue;
    qflags(echo) |= flagFLUID;
    {   LispObject nn;
        qvalue(nn = lookup("*nocompile", 10, 3)) = lisptrue;
        qflags(nn) |= flagFLUID;
    }
    qvalue(lispsystem = lookup("lispsystem*", 11, 1)) =
        list2star(lookup("vsl", 3, 1), lookup("csl", 3, 1),
                  list2star(lookup("embedded", 8, 1),
                      cons(lookup("image", 5, 3),
                           makestring(imagename, strlen(imagename))), nil));
    qflags(lispsystem) |= flagGLOBAL;
    quote = lookup("quote", 5, 3);
    backquote = lookup("`", 1, 3);
    comma = lookup(",", 1, 3);
    comma_at = lookup(",@", 2, 3);
    eofsym = lookup("$eof$", 5, 3);
    qflags(eofsym) |= flagGLOBAL;
    qvalue(eofsym) = eofsym;
    lambda = lookup("lambda", 6, 3);
    expr = lookup("expr", 4, 3);
    subr = lookup("subr", 4, 3);
    fexpr = lookup("fexpr", 5, 3);
    fsubr = lookup("fsubr", 5, 3);
    macro = lookup("macro", 5, 3);
    input = lookup("input", 5, 3);
    output = lookup("output", 6, 3);
    pipe = lookup("pipe", 4, 3);
    qvalue(dfprint = lookup("dfprint*", 6, 3)) = nil;
    qflags(dfprint) |= flagFLUID;
    bignum = lookup("~bignum", 7, 3);
    qvalue(raise = lookup("*raise", 6, 3)) = nil;
    qvalue(lower = lookup("*lower", 6, 3)) = lisptrue;
    qflags(raise) |= flagFLUID;
    qflags(lower) |= flagFLUID;
    cursym = nil;
    work1 = work2 = nil;
    for (i=0; setup_names[i][0]!='x'; i++)
    {   LispObject w = lookup(1+setup_names[i], strlen(1+setup_names[i]), 3);
        if (qdefn0(w) == undefined0) qdefn0(w) = wrongnumber0;
        if (qdefn1(w) == undefined1) qdefn1(w) = wrongnumber1;
        if (qdefn2(w) == undefined2) qdefn2(w) = wrongnumber2;
        if (qdefn3(w) == undefined3) qdefn3(w) = wrongnumber3;
        if (qdefn4(w) == undefined4) qdefn4(w) = wrongnumber4;
        if (qdefn5up(w) == undefined5up) qdefn5up(w) = wrongnumber5up;
        switch (setup_names[i][0])
        {
        case '0':
            qdefn0(w) = (LispFn0 *)setup_defs[i];
            break;
        case 's':
            qflags(w) |= flagSPECFORM;
            // drop through.
        case '1':
            qdefn1(w) = (LispFn1 *)setup_defs[i];
            break;
        case '2':
            qdefn2(w) = (LispFn2 *)setup_defs[i];
            break;
        case '3':
            qdefn3(w) = (LispFn3 *)setup_defs[i];
            break;
        case '4':
            qdefn4(w) = (LispFn4 *)setup_defs[i];
            break;
        case '5':
            qdefn5up(w) = (LispFn5up *)setup_defs[i];
            break;
        }
    }
}

void cold_start()
{
// version of setup to call when there is no initial heap image at all.
    int i;
// I make the object-hash-table lists end in a fixnum rather than nil
// because I want to create the hash table before even the symbol nil
// exists.
    for (i=0; i<OBHASH_SIZE; i++) obhash[i] = tagFIXNUM;
    for (i=0; i<BASES_SIZE; i++) bases[i] = NULLATOM;
    setup();
// The following fields could not be set up quite early enough in the
// cold start case, so I repair them now.
    restartfn = qplist(undefined) = qlits(undefined) =
        qplist(nil) = qlits(nil) = nil;
}

// I will comment here on some of the special features of VSL+ heap images.
// The following objective apply:
// (1) An image created on any version of vsl+ should be re-loadable
//     on any other (at least that is not TOO far away in terms of revision
//     levels). Specifically there will not be a fixed heap size implicit
//     in an image file, and word-length, byte order and machine architecture
//     should be allowed for.
// (2) Compiled code should be preserved within a heap image, but if the
//     image is reloaded on a machine with a different architecture then
//     a form of just-in-time compilation will be activated to create
//     code for the new machine from a compact bytecode representation
//     stored in the image. If the architecture has not been provided with
//     a native JIT compiler then the bytecodes will be interpreted
//     directly.
// (3) Image files will be compressed (using zlib) and so will tend to be
//     compact.

int32_t read32(gzFile f)
{
    unsigned char c[4];
    if (gzread(f, c, 4) != 4) return 0;
    return (c[0] + (c[1]<<8) + (c[2]<<16) + (c[3]<<24));
}

#define FILEID (('v' << 0) | ('s' << 8) | ('l' << 16) | ('+' << 24))
#define ENDID  (('\n' << 0) | ('e' << 8) | ('n' << 16) | ('d' << 24))

// Crude allocation for temporary space within the heap2 area.

void *h2alloc(int n)
{
    void *r = (void *)fringe2;
    if (fringe2+n > limit2) return NULL;
    fringe2 += n;
    return r;
}

uint32_t revision = 0, version=0;

// This is the normal reload case where the word-width of the image
// matches that of the system it is being used on. However it is
// still possible that byte orders differ.

void **setuphash1k;
const char **setuphash1v;
const char **setuphash2k;
void **setuphash2v;

// If the item that is sought is not found then this hash lookup code
// will return NULL.

void *setuphashlookup(void *k)
{
    int i = HASHPTR(k);
    const uint64_t *p;
//@@    printf("Lookup function %#" PRIxPTR " with hash value %d\n", (uintptr_t)k, i);
//@@    {   int j;
//@@        printf("setup hash table contents\n");
//@@        for (j=0; j<SETUPHASHSIZE; j++)
//@@        {   void *k = setuphash1k[j];
//@@            const char *v = setuphash1v[j];
//@@            if (k != NULL) printf("%#" PRIxPTR " : %s\n", (uintptr_t)k, v);
//@@        }
//@@    }
    while (setuphash1k[i] != k &&
           setuphash1k[i] != NULL) i = (i + 1) % SETUPHASHSIZE;
//@@    printf("Entrypoint %#" PRIxPTR " looks up as entry %d (%#" PRIxPTR ")\n",
//@@        (uintptr_t)k, i, (uintptr_t)setuphash1k[i]);
    if (setuphash1k[i] == NULL) return NULL;
    p = (uint64_t *)setuphash1v[i];
//@@    printf("name is <%.16s>\n", (const char *)p);
    i = (int)((p[0] + p[1]) % SETUPHASHSIZE);
//@@    printf("namehash = %d, s2k=%#" PRIxPTR " s2v=%#" PRIxPTR "\n", i, (uintptr_t)setuphash2k, (uintptr_t)setuphash2v);
//@@    printf("setuphash2k[%d] = %#" PRIxPTR "\n", i, (uintptr_t)setuphash2k[i]);
//@@    printf("setuphash2k[%d] = <%.16s>\n", i, setuphash2k[i]);
    while (setuphash2k[i] != NULL &&
           memcmp(setuphash2k[i], p, MAX_NAMESIZE) != 0)
        i = (i + 1) % SETUPHASHSIZE;
//@@    printf("converted entrypoint is %#" PRIxPTR "\n", (uintptr_t)setuphash2v[i]);
    return setuphash2v[i];
}

uint32_t image_nblocks;
uintptr_t image_blocks_by_age[16], image_blocks[16],
          image_bitmapsizes[16], image_offsets[16], image_h1base[16];
uintptr_t image_fringe1;

LispObject relocate(LispObject x)
{
    int i;
//@@     printf("\nrelocate x=%#" PRIxPTR "  %" PRIdPTR "\n", x, x);
    switch (x & TAGBITS)
    {   case tagATOM:
           if (x == NULLATOM) return x;
        case tagCONS:
        case tagSYMBOL:
        case tagFLOAT:
            break; // These things actually need relocating.
        default:
//case tagFIXNUM:
//case tagFORWARD:
//case tagHDR:
            return x;
    }
// Now x should be a reference into the image heap. I first want to
// convert it into a simple offset from the start of the heap as if
// the heap had all been put in a single contiguous chunk. If the
// reference is not within the heap1 part of the image heap then I
// have a corrupted image...
    i = search16a((uintptr_t)x,
        image_blocks[0], image_blocks[1], image_blocks[2], image_blocks[3],
        image_blocks[4], image_blocks[5], image_blocks[6], image_blocks[7],
        image_blocks[8], image_blocks[9], image_blocks[10],image_blocks[11],
        image_blocks[12],image_blocks[13],image_blocks[14],image_blocks[15],
        0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15);
//@@     printf("i %x  %d\n", i, i);
//@@     printf("image_offsets (x) = %#" PRIxPTR " image_blocks = %#" PRIxPTR "\n",
//@@         image_offsets[i], image_blocks[i]);
    x = image_offsets[i] + x - image_h1base[i];
//@@     printf("x relative to image heap start = %#" PRIxPTR "\n", x);
    i = search16a((uintptr_t)x,
        blocks_offset[0], blocks_offset[1], blocks_offset[2], blocks_offset[3],
        blocks_offset[4], blocks_offset[5], blocks_offset[6], blocks_offset[7],
        blocks_offset[8], blocks_offset[9], blocks_offset[10],blocks_offset[11],
        blocks_offset[12],blocks_offset[13],blocks_offset[14],blocks_offset[15],
        0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15);
//@@     printf("x will be in block %d (%#" PRIxPTR ")\n", i, blocks[i]);
    x += ((block_header *)blocks[i])->h1base;
//@@     printf("So relocated item is %" PRIdPTR " = %#" PRIxPTR "\n", x, x);
    return x;
}

void *relocate_fn(void *x)
{
    void *r = setuphashlookup(x);
    uintptr_t rr;
    if (r != NULL) return r;
    printf("Function entrypoint did not relocate\n");
    rr = (LispObject)x;
// I must remove the tag bits before calling relocate because otherwise
// they might make the object appear to be a FIXNUM or other immediate
// data that would not need anything done to it.
    rr = relocate(rr & ~(LispObject)TAGBITS) | (rr & TAGBITS);
// If, as here, the code pointer seemed to be in the heap I should probably
// verify that not only is it within heap1 but that it points at an
// object there that has a relevant header.
    return (void *)rr;
}

// The following returns 0 on success and a line number on failure.

int warm_start_1(gzFile f, int *errcode)
{
    uintptr_t i, b1;
    uint32_t setupsize;
    char (*imagesetup_names)[MAX_NAMESIZE];
    void **imagesetup_defs;
    uintptr_t fr1, lim1, total_size, remaining_size;
// I have the table of names of entrypoints that are defined by
// the kernel. Note that the table as present in the saved image may not
// be the same size as the one in the code I am executing, so I can not
// allocate fixed space for it. However while I reload an image file I will
// only be filling in heap1 with data, so the region that will count as
// heap2 is free. And since nothing at all is in that at present I can
// use it without formality. I am going to assume for now that I have
// more than enough space in the first block for all that I need.
    fringe2 = ((block_header *)blocks_by_age[0])->h2base;
    limit2 = ((block_header *)blocks_by_age[0])->h2top;
    setupsize = read32(f);
// I will reject the image if the size of the setup table has changed by
// at least a factor of 2. That level of change would indicate such
// large adjustments that forcing all images to be re-built surely makes
// sense.
    if (setupsize <= SETUPSIZE/2 || setupsize >= 2*SETUPSIZE) return __LINE__;
    imagesetup_names = (char (*)[MAX_NAMESIZE])h2alloc(setupsize*MAX_NAMESIZE);    
    if (imagesetup_names == NULL) return __LINE__;
// Note that gzread and gzwrite return an int not an unsigned value, so
// when I want to check if they processed the expected number of bytes I
// cast the byte-count to an int even if it was a size_t to start with.
    if (gzread(f, imagesetup_names, (unsigned int)(setupsize*MAX_NAMESIZE)) !=
        (int)(setupsize*MAX_NAMESIZE)) return __LINE__;
    imagesetup_defs = (void **)h2alloc(setupsize*sizeof(void *));
    if (imagesetup_defs == NULL) return __LINE__;
    if (gzread(f, imagesetup_defs, (unsigned int)(setupsize*sizeof(void *))) !=
        (int)(setupsize*sizeof(void *))) return __LINE__;
//@@ // I will display the first 25 names just to convince myself that I have
//@@ // read them in OK.
//@@     for (i=0; i<setupsize && i<25; i++)
//@@     {   char *name = imagesetup_names[i];
//@@         printf("%d) %.*s %#" PRIxPTR "\n", i, MAX_NAMESIZE, name, (uintptr_t)imagesetup_defs[i]);
//@@     } 
    setuphash1k = (void **)h2alloc(SETUPHASHSIZE*sizeof(void *));
    setuphash1v = (const char **)h2alloc(SETUPHASHSIZE*sizeof(char *));
    setuphash2k = (const char **)h2alloc(SETUPHASHSIZE*sizeof(char *));
    setuphash2v = (void **)h2alloc(SETUPHASHSIZE*sizeof(void *));
        if (setuphash1k == NULL ||
        setuphash1v == NULL ||
        setuphash2k == NULL ||
        setuphash2v == NULL) return __LINE__;
    memset(setuphash1k, 0, SETUPHASHSIZE*sizeof(void *));
    memset(setuphash1v, 0, SETUPHASHSIZE*sizeof(char *));
    memset(setuphash2k, 0, SETUPHASHSIZE*sizeof(char *));
    memset(setuphash2v, 0, SETUPHASHSIZE*sizeof(void *));
// To map function entrypoints in the image file into ones in that are
// usable in the running system I first look up an associated name and then
// use that to find the new entrypoint. I have allocated and cleared the
// hash tables used for this - now populate them. The process should only
// ever insert items that are not already in the hash tables, and this
// simplifies the code somewhat.
    for (i=0; i<setupsize; i++)
    {   int h = HASHPTR(imagesetup_defs[i]);
//@@         printf("insert %.16s in imagehash\n", imagesetup_names[i]);
//@@         printf("def at %#" PRIxPTR " hashes to %d\n", (uintptr_t)imagesetup_defs[i], h);
        while (setuphash1k[h] != NULL) h = (h + 1) % SETUPHASHSIZE;
        setuphash1k[h] = imagesetup_defs[i];
        setuphash1v[h] = imagesetup_names[i];
//@@         printf("%d) %#" PRIxPTR " : %s\n", h, (uintptr_t)setuphash1k[h], setuphash1v[h]);
    }
    for (i=0; i<SETUPSIZE; i++)
    {   const uint64_t *s = (const uint64_t *)setup_names[i];
// The next line computes a value based on the first 16 bytes of the
// name of an entrypoint. Since it is just used as as hash value the
// fact that the exact value loaded will depend on whether a big or
// little-endian machine is in use does not matter, and the fact that
// I insert each name just once means that it is Ok to avoid any
// real comparison of the strings concerned.
        int h = (int)((s[0] + s[1]) % SETUPHASHSIZE);
        while (setuphash2k[h] != NULL) h = (h + 1) % SETUPHASHSIZE;
        setuphash2k[h] = setup_names[i];
        setuphash2v[h] = setup_defs[i];
    }
    printf("function pointer relocation tables now in place\n");
// The hash tables that cope with entrypoints are now in place - next
// the reliable heap bases can be reloaded. I am going to assume that
// the sizes of these match between image file and running system.
// I should probably encode the sizes as part of the image format so as
// to police that...
    if (gzread(f, bases, (unsigned int)sizeof(bases)) !=
        (int)sizeof(bases)) return __LINE__;
// A small reminder here: when I move to more complete Common Lisp
// support the single (and fixed size) hash table for use as an object
// list will need to be replaced with something much more elaborate.
// That change will render image files incompatible.
    if (gzread(f, obhash, (unsigned int)sizeof(obhash)) !=
        (int)sizeof(obhash)) return __LINE__;
    printf("bases and obhash loaded\n");
    if ((image_nblocks = read32(f)) == 0 ||
         image_nblocks > 16) return __LINE__;
    printf("image_nblocks = %d\n", (int)image_nblocks);
    if (gzread(f, image_blocks_by_age,
                  (unsigned int)sizeof(blocks_by_age)) !=
        (int)sizeof(blocks_by_age)) return __LINE__;
    for (i=0; i<image_nblocks; i++)
    {   uintptr_t nn;
        if (gzread(f, &nn, (unsigned int)sizeof(nn)) != (int)sizeof(nn)) return __LINE__;
        image_bitmapsizes[i] = nn;
    }
    for (i=0; i<image_nblocks; i++)
    {   uintptr_t nn;
        if (gzread(f, &nn, (unsigned int)sizeof(nn)) != (int)sizeof(nn)) return __LINE__;
        image_h1base[i] = nn;
    }
// image_fringe1 is the offset from h1base in the block that fringe1 is
// within.
    if (gzread(f, &image_fringe1,
                  (unsigned int)sizeof(image_fringe1)) !=
        (int)sizeof(image_fringe1)) return __LINE__;
    for (;i<16; i++)
    {   image_h1base[i] = (uintptr_t)(-1);
        image_bitmapsizes[i] = 0;
    }
// I will now set up a map of offsets of the sections within the CURRENT
// heap...
    total_size = 0;
    for (i=0; i<nblocks; i++)
    {   blocks_offset[i] = total_size;
        total_size += 64*((block_header *)blocks_by_age[i])->halfbitmapsize;
    }
    for (;i<16; i++) blocks_offset[i] = total_size;
// Now back to dealing with the image heap.
    total_size = 0;
    for (i=0; i<image_nblocks; i++)
    {
//@@        printf("block %#18" PRIxPTR " bitmapsize %#" PRIxPTR "\n",
//@@               image_blocks_by_age[i], image_bitmapsizes[i]);
        image_offsets[i] = total_size*64;
        total_size += image_bitmapsizes[i];
        printf("total size now %#" PRIxPTR "\n", total_size);
    }
    for (;i<16; i++) image_offsets[i] = total_size*64;
//@@    for (i=0; i<16; i++)
//@@        printf("image_offset[%d] = %#" PRIxPTR "\n", i, image_offsets[i]);
// The total size recorded here is the size of bitmap data present in
// the image. The amount of heap will be 64 times as great.
// There is an issue whereby the final block dumped only had information
// written as far as data was valid.
    total_size *= 64;
    printf("size of whole image half-heap = %#" PRIxPTR "\n", total_size);
    printf("final image block %#" PRIxPTR ", fringe1 = %#" PRIxPTR ", length=%#" PRIxPTR "\n",
        image_blocks_by_age[image_nblocks-1],
        image_fringe1,
        64*image_bitmapsizes[image_nblocks-1]);
    assert(image_fringe1 >= 0);
    assert(image_fringe1 < 64*image_bitmapsizes[image_nblocks-1]);
    total_size -=
        (64*image_bitmapsizes[image_nblocks-1] - image_fringe1);
    printf("Bytes of heap to reload = %" PRIdPTR "\n", total_size);
// Next I need to reload the body of the image. The version in the
// image file is a single chunk of bytes...
    printf("Reloading the image will need around %#" PRIxPTR " bytes\n",
           total_size);
// I suppose that if there was not enough memory already allocated I could
// try extending the heap somewhere around here. For now and for simplicity
// I will not do that.
//
// image_blocks needs to have the same blocks listed as image_blocks_by_age
// but sorted by address. Since there are at most 16 items I will use a simple
// insertion sort.
    for (i=0; i<image_nblocks; i++)
    {   uintptr_t j;
        uintptr_t w;
        w = image_blocks_by_age[i];
        j = i;
        while (j>1 && w < image_blocks[j-1])
        {   image_blocks[j] = image_blocks[j-1];
            j = j-1;
        }
        image_blocks[j] = w;
    }
// Fill out the end of image_blocks with an address higher than ay that
// I will ever use.
    for (;i<16; i++) image_blocks[i] = (uintptr_t)(-1);
//@@    for (i=0; i<16; i++)
//@@        printf("image_blocks[%d] = %" PRIdPTR " = %#" PRIxPTR "\n",
//@@               i, image_blocks[i], image_blocks[i]);
// Now I need to load the heap data from my image file into the memory
// I have in the current running system.
    block1 = 0;
    fringe1 = ((block_header *)blocks_by_age[0])->h1base;
    limit1 = ((block_header *)blocks_by_age[0])->h1top;
    remaining_size = total_size;
    while (remaining_size != 0)
    {   uintptr_t bs = remaining_size;
        if (bs > 0x60000000) bs = 0x40000000;
        if (bs > limit1 - fringe1) bs = limit1 - fringe1;
        if (gzread(f, (void *)fringe1, (unsigned int)bs) != (int)bs) return __LINE__;
        fringe1 += bs;
        remaining_size -= bs;
        if (fringe1 == limit1)
        {   block1++;
            if (block1 == nblocks)
            {   printf("Not enough memory allocated to reload this image\n");
// Allocate some more?
                return __LINE__;
            }
            fringe1 = ((block_header *)blocks_by_age[block1])->h1base;
            limit1 = ((block_header *)blocks_by_age[block1])->h1top;
        }
    }
    printf("heap1 data re-loaded\n");
// Note that the heap1 data may still be in a mangled byte-order, but I can
// not correct that without parsing it, and I can not do that until I have
// the associated bitmaps available.
// Now rather similar jobs to cope with the starts and fp bitmaps.
    b1 = 0;
    fr1 = (uintptr_t)((block_header *)blocks_by_age[0])->h1starts;
    lim1 = ((block_header *)blocks_by_age[0])->halfbitmapsize;
    memset((void *)fr1, 0, 2*lim1);
    lim1 += fr1;
    remaining_size = (total_size + 63)/64;
    remaining_size = (remaining_size + 3) & ~(uintptr_t)3;
    while (remaining_size != 0)
    {   uintptr_t bs = remaining_size;
        if (bs > 0x60000000) bs = 0x40000000;
        if (bs > lim1 - fr1) bs = lim1 - fr1;
        if (gzread(f, (void *)fr1, (unsigned int)bs) != (int)bs) return __LINE__;
        fr1 += bs;
        remaining_size -= bs;
        if (fr1 == lim1)
        {   b1++;
            if (b1 == nblocks) return __LINE__; // inconsistency here.
            fr1 = (uintptr_t)((block_header *)blocks_by_age[block1])->h1starts;
            lim1 = fr1 + ((block_header *)blocks_by_age[block1])->halfbitmapsize;
            memset((void *)fr1, 0, lim1-fr1);
        }
    }
    b1 = 0;
    fr1 = (uintptr_t)((block_header *)blocks_by_age[0])->h1fp;
    lim1 = ((block_header *)blocks_by_age[0])->halfbitmapsize;
    memset((void *)fr1, 0, 2*lim1);
    lim1 += fr1;
    remaining_size = (total_size + 63)/64;
    remaining_size = (remaining_size + 3) & ~(uintptr_t)3;
    while (remaining_size != 0)
    {   uintptr_t bs = remaining_size;
        if (bs > 0x60000000) bs = 0x40000000;
        if (bs > lim1 - fr1) bs = lim1 - fr1;
        if (gzread(f, (void *)fr1, (unsigned int)bs) != (int)bs) return __LINE__;
        fr1 += bs;
        remaining_size -= bs;
        if (fr1 == lim1)
        {   b1++;
            if (b1 == nblocks) return __LINE__; // inconsistency here.
            fr1 = (uintptr_t)((block_header *)blocks_by_age[block1])->h1fp;
            lim1 = fr1 + ((block_header *)blocks_by_age[block1])->halfbitmapsize;
            memset((void *)fr1, 0, lim1-fr1);
        }
    }
// I will zero out the bitmaps in any blocks of memory that the image
// did not get as far as using. 
    for (i=b1+1; i<nblocks; i++)
    {   uint32_t *s = (uint32_t *)((block_header *)blocks_by_age[i])->h1starts;
        uint32_t *f = (uint32_t *)((block_header *)blocks_by_age[i])->h1fp;
        uintptr_t l = ((block_header *)blocks_by_age[i])->halfbitmapsize;
        memset(s, 0, 2*l);
        memset(f, 0, 2*l);
    }
    printf("heap1 data re-loaded\n");
// The FILEID at the end of an image file gives me a chance to confirm that
// I have kept in step while decoding it.
    if (read32(f) != ENDID) return __LINE__;
    if (read32(f) != FILEID) return __LINE__;
//@@    hexdump();
// Now the data is all in place, but heap1 may have its bytes in a bad order
// and it certainly contains address references that relate to the computer
// that created the image, not the one that is now running. So I need to
// scan and fix things up. First deal with the list bases...
    for (i=0; i<BASES_SIZE; i++)
        bases[i] = relocate(bases[i]);
    for (i=0; i<OBHASH_SIZE; i++)
        obhash[i] = relocate(obhash[i]);
// Now do a scan of the heap... There is a further horrid issue here. I
// reloaded the heap into what might have been several blocks, and
// it could be that some object (especially a vector) ended up straddling
// the end of one block and the start of the next. If the sequence of block
// sizes on the original and new machine are identical or if the whole
// reloaded image fits within a single block that will not happen, but
// I should not rely on those circumstances. When I come to reload a
// 32-bit image on a 64-bit computer or vice versa the chances of effective
// block sizes being different will increase. So in the scan here I
// just detect that case, and if it arises I will force a garbage
// collection just after the reload. The process of copying everything
// there will provide a chance to repair the mess.
    b1 = 0;
    fr1 = ((block_header *)blocks_by_age[b1])->h1base;
    lim1 = ((block_header *)blocks_by_age[b1])->h1top;
    while (fr1 != fringe1)
    {   LispObject h, w;
        if (fr1 == lim1 )
        {   b1++;
            assert(b1 != nblocks);
            fr1 = ((block_header *)blocks_by_age[b1])->h1base;
            lim1 = ((block_header *)blocks_by_age[b1])->h1top;
        }
        if (getheapfp(fr1))
        {   // @@@@ re-byte-sex the float at fr1 here...
            fr1 += 8;
            continue;
        }
        if (!isHDR(h = qcar(fr1))) // A simple cons cell
        {   qcar(fr1) = relocate(h);
            fr1 += sizeof(LispObject);
            if (fr1 == lim1 )
            {   b1++;
                assert(b1 != nblocks);
                fr1 = ((block_header *)blocks_by_age[b1])->h1base;
                lim1 = ((block_header *)blocks_by_age[b1])->h1top;
                abort();
            }
            qcar(fr1) = relocate(qcar(fr1));
            fr1 += sizeof(LispObject);
        }
        else              // The item is one that uses a header
            switch (h & TYPEBITS)
            {   case typeSYM:
                    w = fr1 + tagSYMBOL;
// qflags(w) does not need adjusting.
//
// If a symbol was represented as a C struct here I could use offsetof
// to control which fields needed copying while I cope with the mess of
// having memory in (possibly) several chunks. However it is not so the
// exact layout is relied upon here. The first chunk of code here relocates
// that part of the symbol that lies within the current segment of heap.
                    if (fr1+sizeof(LispObject) < lim1)
                        qvalue(w) = relocate(qvalue(w));
                    if (fr1+2*sizeof(LispObject) < lim1)
                        qplist(w) = relocate(qplist(w));
                    if (fr1+3*sizeof(LispObject) < lim1)
                        qpname(w) = relocate(qpname(w));
                    if (fr1+4*sizeof(LispObject) < lim1)
                        qlits(w)  = relocate(qlits(w));
                    if (fr1+5*sizeof(LispObject) < lim1)
                        qspare(w) = relocate(qspare(w));
                    if (fr1+6*sizeof(LispObject) < lim1)
                        qdefn0(w) = (LispFn0 *)relocate_fn((void *)qdefn0(w));
                    if (fr1+7*sizeof(LispObject) < lim1)
                        qdefn1(w) = (LispFn1 *)relocate_fn((void *)qdefn1(w));
                    if (fr1+8*sizeof(LispObject) < lim1)
                        qdefn2(w) = (LispFn2 *)relocate_fn((void *)qdefn2(w));
                    if (fr1+9*sizeof(LispObject) < lim1)
                        qdefn3(w) = (LispFn3 *)relocate_fn((void *)qdefn3(w));
                    if (fr1+10*sizeof(LispObject) < lim1)
                        qdefn4(w) = (LispFn4 *)relocate_fn((void *)qdefn4(w));
                    if (fr1+11*sizeof(LispObject) < lim1)
                        qdefn5up(w) = (LispFn5up *)relocate_fn((void *)qdefn5up(w));
                    fr1 += SYMSIZE*sizeof(LispObject);
// Now if the symbol was split across two heap segments I need to relocate
// the parts of it at the start of the next heap block. What a mess!
                    if (fr1 > lim1 )
                    {   uintptr_t leftover = fr1 - lim1, newblock;
                        b1++;
                        assert(b1 != nblocks);
                        fr1 = ((block_header *)blocks_by_age[b1])->h1base;
                        lim1 = ((block_header *)blocks_by_age[b1])->h1top;
                        newblock = fr1;
                        fr1 += (leftover - SYMSIZE*sizeof(LispObject));
                        w = fr1 + tagSYMBOL;
                        if (fr1+sizeof(LispObject) >= newblock)
                            qvalue(w) = relocate(qvalue(w));
                        if (fr1+2*sizeof(LispObject) >= newblock)
                            qplist(w) = relocate(qplist(w));
                        if (fr1+3*sizeof(LispObject) >= newblock)
                            qpname(w) = relocate(qpname(w));
                        if (fr1+4*sizeof(LispObject) >= newblock)
                            qlits(w)  = relocate(qlits(w));
                        if (fr1+5*sizeof(LispObject) >= newblock)
                            qspare(w) = relocate(qspare(w));
                        if (fr1+6*sizeof(LispObject) >= newblock)
                            qdefn0(w) = (LispFn0 *)relocate_fn((void *)qdefn0(w));
                        if (fr1+7*sizeof(LispObject) >= newblock)
                            qdefn1(w) = (LispFn1 *)relocate_fn((void *)qdefn1(w));
                        if (fr1+8*sizeof(LispObject) >= newblock)
                            qdefn2(w) = (LispFn2 *)relocate_fn((void *)qdefn2(w));
                        if (fr1+9*sizeof(LispObject) >= newblock)
                            qdefn3(w) = (LispFn3 *)relocate_fn((void *)qdefn3(w));
                        if (fr1+10*sizeof(LispObject) >= newblock)
                            qdefn4(w) = (LispFn4 *)relocate_fn((void *)qdefn4(w));
                        if (fr1+11*sizeof(LispObject) >= newblock)
                            qdefn5up(w) = (LispFn5up *)relocate_fn((void *)qdefn5up(w));
                        fr1 += SYMSIZE*sizeof(LispObject);
                        abort();
                    }
                    continue;
                case typeSTRING:
// Pure byte-structured binary data, so nothing much to do here. But see
// the typeVEC code and think about VERY long strings that could overlap
// several memory blocks...
                    {   fr1 += ALIGN8(sizeof(LispObject) + veclength(h));
                        if (fr1 > lim1)
                        {   uintptr_t leftover = fr1 - lim1;
                            b1++;
                            assert(b1 != nblocks);
                            fr1 = ((block_header *)blocks_by_age[b1])->h1base + leftover;
                            lim1 = ((block_header *)blocks_by_age[b1])->h1top;
                        }
                        continue;
                    }
                case typeBIGNUM:
// No relocation, but I need to fix up byte orders... This will all
// change soon as I move to supporting C-coded bignums using 32-bit
// digits, so I will not put a lot of work into it just now.
                    fr1 += ALIGN8(sizeof(LispObject) + veclength(h));
//@@ This does NOT cope with overlapping data in this case.
                    if (fr1 > lim1)
                    {   uintptr_t leftover = fr1 - lim1;
                        assert(0); // Buggy at present.
                        b1++;
                        assert(b1 != nblocks);
                        fr1 = ((block_header *)blocks_by_age[b1])->h1base + leftover;
                        lim1 = ((block_header *)blocks_by_age[b1])->h1top;
                    }
                    continue;
                case typeVEC: case typeEQHASH: case typeEQHASHX:
                    fr1 += sizeof(LispObject);
                    w = veclength(h);
                    while (w > 0)
                    {   if (fr1 == lim1)
                        {    b1++;
                             assert(b1 != nblocks);
                             fr1 = ((block_header *)blocks_by_age[b1])->h1base;
                             lim1 = ((block_header *)blocks_by_age[b1])->h1top;
                        }
                        qcar(fr1) = relocate(qcar(fr1));
                        fr1 += sizeof(LispObject);
                        w -= sizeof(LispObject);
                    }
                    fr1 = ALIGN8(fr1);
                    continue;
                default:
                    // The spare codes!
                    assert(0);
            }
    }
    printf("relocating code might be complete!!!\n");
// This setting may change from run to run so a setting saved in the
// image file should be clobbered here!
     qvalue(echo) = interactive ? nil : lisptrue;
     return 0;
}

int warm_start(gzFile f, int *errcode)
{
    int32_t w;
    char banner[64];
    if (read32(f) != FILEID) return 1;
    if (gzread(f, &w, 4) != 4) return 1;
    if ((revision = read32(f)) == 0) return 1;
    version = (revision >>= 4) & 0x3fff;
    revision >>= 14;
    printf("Subversion revision %d VERSION %d.%.3d\n",
        revision, version/1000, version%1000);
    if (gzread(f, banner, 64) != 64) return 1;
    if (banner[0] != 0)
    {   printf("%.64s\n", banner);
        fflush(stdout);
    }
// The date and time of day that the image file was created, in textual
// form as in "Sat Apr 15 12:03:52 1972" (whatever the ctime() function
// delivers).
    if (gzread(f, banner, 24) != 24) return 1;
    printf("Image created: %.24s\n", banner);
    if (gzread(f, banner, 16) != 16) return 1;
    return warm_start_1(f, errcode);
}

// This writes out a 32-bit integer in a defined byte-order.
int write32(gzFile f, uint32_t n)
{
    char b[4];
    b[0] = n & 0xff;
    b[1] = (n >> 8) & 0xff;
    b[2] = (n >> 16) & 0xff;
    b[3] = (n >> 24) & 0xff;
    return gzwrite(f, b, 4);
}


// The version number has an integer part and a fraction part, and I
// want the fractional part to be in the range 100 to 999 please.
#define IVERSION 1
#define FVERSION 501
#define stringify(x) #x
char startup_banner[64] =
   "VSL+ version " stringify(IVERSION) "." stringify(VERSION);

// Return 0 on success, 1 on most failures and 2 if the gzclose failed.
// In the case that things got as far as gzclose then errcode is set
// to show what happened. Otherwise all that is reported is that something
// had gone wrong since gzerror can be used to discover what the problem
// had been.

static inline int write_image_1(gzFile f, int *errcode)
{
    size_t i;
// First a file-format identifier "vsl+"
    if (write32(f, FILEID) != 4) return 1;
// Next a 32-bit word that used to signal the byte-ordering and word-length
// of the machine that created the image.
    {   int32_t n = 0x76543210;
        if (gzwrite(f, &n, 4) != 4) return 1;
    }
// Another 32-bit word that has a revision number derived from subversion
// and information about the floating point format in use.
    {   int revision;
        char dollar[4];
// If I had a literal "$" at the start of the string I use to decode
// stuff here then overall it would be in exactly the format that subversion
// rewrites, and the "%d" in the middle would get replaced with a revision
// number next time I checked it in! So I match the "$" manually.
        if (sscanf(setup_revision, "%cRevision: %d $", dollar, &revision)!=2 ||
            dollar[0] != '$')
            revision = 0;
        int version = (1000*IVERSION) + FVERSION;
        if (write32(f, (revision<<18) | (version<<4)) != 4)
            return 1;
    }
// A banner of up to 64 characters that can be displayed early as the
// image starts to be loaded.
    if (gzwrite(f, startup_banner, 64) != 64) return 1;
// The date and time of day that the image file was created, in textual
// form as in "Sat Apr 15 12:03:52 1972" (whatever the ctime() function
// delivers).
    {   time_t t0 = time(0);
        const char *tt = ctime(&t0);
        if (tt == NULL) tt = "Mon Jan  1 00:00:00 1900"; // Date unknown
        if (gzwrite(f, tt, 24) != 24) return 1;
    }
// 16 bytes whose purpose at present escapes me.
    if (gzwrite(f, "0123456789abcdef", 16) != 16) return 1; // junk at present
// Next I want to dump the table of entrypoints to functions that are
// built into the kernel. First I write an integer that indicates how
// many there are, then the table of their names and then the associated
// entry addresses. Note that the amount of data written for the table
// of entrypoints will be different as between 32 and 64-bit images.
    if (write32(f, SETUPSIZE) != 4) return 1; // items in setup table
    if (gzwrite(f, setup_names, (unsigned int)sizeof(setup_names)) !=
        (int)sizeof(setup_names)) return 1;
    if (gzwrite(f, setup_defs, (unsigned int)sizeof(setup_defs)) !=
        (int)sizeof(setup_defs)) return 1;
// There are a number of list bases that need to be saved. If the
// number or layout of these ever changes then it will be important to
// change VERSION, and a discrepancy in that must cause images to
// be rejected as un-re-loadable.
    if (gzwrite(f, bases, (unsigned int)sizeof(bases)) !=
        (int)sizeof(bases)) return 1;
    if (gzwrite(f, obhash, (unsigned int)sizeof(obhash)) !=
        (int)sizeof(obhash)) return 1;
// Finally I need to dump imformation relating to the heap. I need
// to record information about the way it was allocated in segments.
// Since there are at most 16 segments I will always write out the
// segment map information for all 16 potential segments.
    if (write32(f, block1+1) != 4) return 1;
    if (gzwrite(f, blocks_by_age, (unsigned int)sizeof(blocks_by_age)) !=
        (int)sizeof(blocks_by_age)) return 1;
    for (i=0; i<=block1; i++)
    {   block_header *b = (block_header *)blocks_by_age[i];
        uintptr_t nn = b->halfbitmapsize;
        printf("block size[%d] = %" PRIdPTR "\n", (int)i, nn);
        if (gzwrite(f, &nn, (unsigned int)sizeof(nn)) != (int)sizeof(nn)) return 1;
    }
    for (i=0; i<=block1; i++)
    {   block_header *b = (block_header *)blocks_by_age[i];
        uintptr_t nn = b->h1base;
        printf("h1base[%d] = %#" PRIxPTR "\n", (int)i, nn);
        if (gzwrite(f, &nn, (unsigned int)sizeof(nn)) != (int)sizeof(nn)) return 1;
    }
    {   LispObject fringe1_offset = fringe1 -
           ((block_header *)blocks_by_age[block1])->h1base;
        printf("Write fringe1_offset = %#" PRIxPTR "\n", fringe1_offset);
        if (gzwrite(f, &fringe1_offset, sizeof(fringe1_offset)) !=
            (int)sizeof(fringe1_offset)) return 1;
    }
    for (i=0; i<=block1; i++)
    {   block_header *b = (block_header *)blocks_by_age[i];
        uintptr_t bs = 64*b->halfbitmapsize;
        char *p = (char *)b->h1base;
// I will write out the whole of most blocks, but for the final one
// (and fringe1 should point within it) I will only dump the part that
// is active.
        if (i==block1) bs = fringe1-(b->h1base);
        printf("Write out %" PRIdPTR " bytes of heap image\n", bs);
// Despite it feeling a bit ridiculous, I will allow for the possibility
// that a block that I am dumping is so large that mere 32-bit "unsigned int"
// values (as used by gzwrite for length information) will prove inadequate.
// In extreme cases I will write things out in multiple chunks of
// about a gigabyte each, with the final write using up to 1.5 Gbytes.
        while (bs >= 0x60000000)
        {   if (gzwrite(f, p, 0x40000000) != 0x40000000) return 1;
            p += 0x40000000;
            bs -= 0x40000000;
        }
        if (gzwrite(f, p, (unsigned int)bs) != (int)bs) return 1;
    }
    printf("Main heap bit written\n");
// Next bitmap information about the places where objects start in the heap.
// A bitmap is (1/64)th of the size of a main memory block, so by the
// time a bitmap is bigger than the Gigabyte that I use for writing
// chunks I will have 64G for one half of my main memory block, in
// other words I will be using over 128G in all. What is more that has to
// be a situation where I have at least 64G of ACTIVE data at the end of
// a garbage collection and that I wish to preserve.
    for (i=0; i<=block1; i++)
    {   block_header *b = (block_header *)blocks_by_age[i];
        uintptr_t bs = b->halfbitmapsize;
        char *p = (char *)b->h1starts;
// I will write out bytes from the bitmap sufficient to cover the
// active region of the final active segment of memory, and will always
// write a number of bytes that is a multiple of 4.
        if (i==block1)
        {   bs = (fringe1-(b->h1base) + 63)/64;
            bs = (bs + 3) & ~(uintptr_t)3;
        }
        while (bs >= 0x60000000)
        {   if (gzwrite(f, p, 0x40000000) != 0x40000000) return 1;
            p += 0x40000000;
            bs -= 0x40000000;
        }
        if (gzwrite(f, p, (unsigned int)bs) != (int)bs) return 1;
    }
    printf("starts bitmap done\n");
// Bitmap information about where there are floating point numbers stored.
    for (i=0; i<=block1; i++)
    {   block_header *b = (block_header *)blocks_by_age[i];
        uintptr_t bs = b->halfbitmapsize;
        char *p = (char *)b->h1fp;
        if (i==block1)
        {   bs = (fringe1-(b->h1base) + 63)/64;
            bs = (bs + 3) & ~(uintptr_t)3;
        }
        while (bs >= 0x60000000)
        {   if (gzwrite(f, p, 0x40000000) != 0x40000000) return 1;
            p += 0x40000000;
            bs -= 0x40000000;
        }
        if (gzwrite(f, p, (unsigned int)bs) != (int)bs) return 1;
    }
    printf("fp bitmap done\n");
// That is the lot. I will write a second copy of the marker word
// since that will help verify that everything is present.
    if (write32(f, ENDID) != 4) return 1;
    if (write32(f, FILEID) != 4) return 1;
    if ((*errcode = gzclose(f)) == Z_OK) return 0; // Success!
    return 2;
}

void write_image(gzFile f)
{
    int errcode;
    inner_reclaim(C_stackbase); // To compact memory.
    inner_reclaim(C_stackbase); // in the conservative case GC twice.
    hexdump();
    switch (write_image_1(f, &errcode))
    {
    default:
    // case 0:
        printf("image written OK\n");
        return;
    case 1:
        gzerror(f, &errcode);
        gzclose(f);
        // drop through;
    case 2:
        if (errcode == Z_ERRNO)
            printf("+++ Error writing image file (code=%d)\n", errno);
        else printf("+++ Error compressing image file (code=%d)\n", errcode);
        my_exit(EXIT_FAILURE);
    }
}

static void el_tidy() {
    el_end(el_struct);
    history_end(el_history);
}

char the_prompt[] = "> ";

static char *get_prompt(EditLine *el)
{   return the_prompt;
}

void setup_prompt() {
    stdin_tty = isatty(fileno(stdin)) && isatty(fileno(stdout));
    if (stdin_tty) {
        el_struct = el_init("vsl", stdin, stdout, stderr);
        el_history = history_init();

        atexit(el_tidy);
        history(el_history, &el_history_event, H_SETSIZE, 1000);
        el_set(el_struct, EL_PROMPT, get_prompt);
        el_set(el_struct, EL_HIST, history, el_history);
    }
}

int main(int argc, char *argv[])
{
    setup_prompt();
    const char *inputfilename = NULL;
    void *pool;
//@@#ifdef DEBUG
    setvbuf(stdout, NULL, _IONBF, 0);
//@@#endif // DEBUG
//
// The "+16" here is to allow for aliging up memory to be at addresses
// that are multiples of 8.
    pool = allocate_memory(sizeof(block_header) +
                           (2*64 + 5)*HALFBITMAPSIZE + 16);
    if (pool == NULL)
    {   printf("Not enough memory available: Unable to proceed\n");
        my_exit(EXIT_FAILURE);
    }
// I only fill in one entry in the memory block at this stage.
    ((block_header *)pool)->halfbitmapsize = HALFBITMAPSIZE;
    blocks[0] = blocks_by_age[0] = (uintptr_t)pool;
// All others point to the top of virtual memory.
    for (size_t i=1; i<16; i++) blocks[i] = blocks_by_age[i] = (uintptr_t)(-1);
    nblocks = 1;
    C_stackbase = (LispObject *)((intptr_t)&inputfilename &
                                    -sizeof(LispObject));
    coldstart = 0;
    interactive = 1;
#ifdef DEBUG
    logfile = fopen("vsl.log", "w");
#endif // DEBUG
#ifdef __WIN32__
    size_t i = strlen(argv[0]);
    if (strcmp(argv[0]+i-4, ".exe") == 0) i -= 4;
    sprintf(imagename, "%.*s.img", i, argv[0]);
#else // __WIN32__
    sprintf(imagename, "%s.img", argv[0]);
#endif // __WIN32__
    for (int i=1; i<argc; i++)
    {
// I have some VERY simple command-line options here.
//        -z         do a "cold start".
//        -ifilename use that as image file
//        filename   read from that file rather than from the standard input.
        if (strcmp(argv[i], "-z") == 0) coldstart = 1;
        else if (strncmp(argv[i], "-i", 2) == 0) strcpy(imagename, argv[i]+2);
        else if (argv[i][0] != '-') inputfilename = argv[i], interactive = 0;
    }
    printf("VSL version %d.%.3d\n", IVERSION, FVERSION); fflush(stdout);
    linepos = 0;
    for (size_t i=0; i<MAX_LISPFILES; i++) lispfiles[i] = 0;
    lispfiles[0] = stdin;   lispfiles[1] = stdout;
    lispfiles[2] = stderr;  lispfiles[3] = stdin;
    file_direction = (1<<1) | (1<<2); // 1 bits for writable files.
    lispin = 3; lispout = 1;
    if (inputfilename != NULL)
    {   FILE *in = fopen(inputfilename, "r");
        if (in == NULL)
            printf("Unable to read from %s, so using standard input\n",
                   inputfilename);
        else lispfiles[3] = in;
    }
    boffop = 0;
    for (;;) // This loop is for restart-lisp and preserve.
    {   allocateheap();
// A warm start will read an image file which it expects to have been
// made by a previous use of vsl.
        if (coldstart) cold_start();
        else
        {   gzFile f = gzopen(imagename, "rb");
            int i, errcode;
            if (f == NULL)
            {   printf("Error: unable to open image for reading\n");
                my_exit(EXIT_FAILURE);
            }
            if ((i = warm_start(f, &errcode)) != 0)
            {
                gzerror(f, &errcode);
                gzclose(f);
// First case is when gzread has not reported any problems but when the
// internal logic in warm_start has detected some inconsiency.
                if (errcode == Z_OK)
                    printf("+++ Error parsing file (code=%d)\n", i);
// Second case is if the operating system reported trouble reading the
// image file.
                else if (errcode == Z_ERRNO)
                    printf("+++ Error reading image file (code=%d/%d)\n",
                           errno, i);
// Third case is when gzread finds data in a format that it objects to.
                else printf("+++ Error decompressing image file (code=%d/%d)\n",
                            errcode, i);
                my_exit(EXIT_FAILURE);
            }
        }
// Any predefined specified on the command-line using -Dxx=yy are
// instated or re-instated here so they apply even after restart!-lisp.
        for (int i=1; i<argc; i++)
        {   if (argv[i][0] == '-' && argv[i][1] == 'D')
            {   const char *d1 = strchr(argv[i], '=');
                if (d1 == NULL) continue;
// In general through setup (and I count this as still being setup)
// I will code on the basis that there will not be any garbage collection
// so I do not need to think about the effects of data movement during GC.
                qvalue(lookup(argv[i]+2, (d1-argv[i])-2, 3)) =
                    makestring(d1+1, strlen(d1+1));
            }
        }
        fflush(stdout);
        curchar = '\n'; symtype = '?'; cursym = nil;
        if (boffop == 0) // Use standard restart function from image.
        {   if (restartfn == nil) readevalprint(0);
            else Lapply(nil, restartfn, nil);
        }
        else
        {   LispObject x, data = makestring(boffo, boffop);
            data = Lcompress(nil, Lexplodec(nil, data));
            x = qcar(data);   // 'fn or '(module fn)
            if (isCONS(x))
            {   Lload_module(nil, qcar(x));
                x = qcar(qcdr(x));
            }
            Lapply(nil, x, qcdr(data));
        }
        if ((unwindflag & unwindPRESERVE) != 0)
        {   gzFile f = gzopen(imagename, "wbT");
            if (f == NULL)
                printf("\n+++ Unable to open %s for writing\n", imagename);
            else write_image(f);

// A cautious person would have checked for error codes returned by the
// above calls to write and close. I omit that here to be concise.
        }
        if ((unwindflag & unwindRESTART) == 0) break;
        unwindflag = unwindNONE;
        boffop = 0;
        if (qcar(work1) == nil) coldstart = 1;
        else if (qcar(work1) == lisptrue) coldstart = 0;
        else
        {   int save = lispout;
            lispout = -2;
            internalprint(work1);
            wrch(0);
            lispout = save;
            coldstart = 0;
        }
    }
    return 0;
}

// end of main source file.

