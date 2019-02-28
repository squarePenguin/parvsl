// Things to think about


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

#include <iostream>

#include <list>
#include <fstream>
#include <functional>
#include <unordered_map>
#include <vector>

// Multi-threading support
#include <mutex>
#include <thread>

// I want libedit for local editing and history.
#include <histedit.h>

#ifdef WIN32
#define popen _popen
#endif

// This version is an extension of the minimal vsl system. It uses
// a conservative garbage collector and will support a fuller and
// higher performance Lisp.

#include "common.hpp"
#include "thread_data.hpp"

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

static inline uintptr_t search(uintptr_t x, uintptr_t h0, uintptr_t h1)
{   if (x < h1) return h0;
    else return h1;
}

static inline uintptr_t search(uintptr_t x,
    uintptr_t h0, uintptr_t h1, uintptr_t h2, uintptr_t h3)
{   if (x < h2) return search(x, h0, h1);
    else return search(x, h2, h3);
}

static inline uintptr_t search(uintptr_t x,
    uintptr_t h0, uintptr_t h1, uintptr_t h2, uintptr_t h3,
    uintptr_t h4, uintptr_t h5, uintptr_t h6, uintptr_t h7)
{   if (x < h4) return search(x, h0, h1, h2, h3);
    else return search(x, h4, h5, h6, h7);
}

static inline uintptr_t search(uintptr_t x,
    uintptr_t h0, uintptr_t h1, uintptr_t h2, uintptr_t h3,
    uintptr_t h4, uintptr_t h5, uintptr_t h6, uintptr_t h7,
    uintptr_t h8, uintptr_t h9, uintptr_t h10, uintptr_t h11,
    uintptr_t h12, uintptr_t h13, uintptr_t h14, uintptr_t h15)
{   if (x < h8) return search(x, h0, h1, h2, h3, h4, h5, h6, h7);
    else return search(x, h8, h9, h10, h11, h12, h13, h14, h15);
}

// The variants that come next do the same sort of search but return
// results that can be specified separately from the key values.

static inline int search(uintptr_t x,
    uintptr_t h0, uintptr_t h1,
    int v0, int v1)
{   if (x < h1) return v0;
    else return v1;
}

static inline int search(uintptr_t x,
    uintptr_t h0, uintptr_t h1, uintptr_t h2, uintptr_t h3,
    int v0, int v1, int v2, int v3)
{   if (x < h2) return search(x, h0, h1, v0, v1);
    else return search(x, h2, h3, v2, v3);
}

static inline int search(uintptr_t x,
    uintptr_t h0, uintptr_t h1, uintptr_t h2, uintptr_t h3,
    uintptr_t h4, uintptr_t h5, uintptr_t h6, uintptr_t h7,
    int v0, int v1, int v2, int v3,
    int v4, int v5, int v6, int v7)
{   if (x < h4) return search(x, h0, h1, h2, h3, v0, v1, v2, v3);
    else return search(x, h4, h5, h6, h7, v4, v5, v6, v7);
}

static inline int search(uintptr_t x,
    uintptr_t h0, uintptr_t h1, uintptr_t h2, uintptr_t h3,
    uintptr_t h4, uintptr_t h5, uintptr_t h6, uintptr_t h7,
    uintptr_t h8, uintptr_t h9, uintptr_t h10, uintptr_t h11,
    uintptr_t h12, uintptr_t h13, uintptr_t h14, uintptr_t h15,
    int v0, int v1, int v2, int v3,
    int v4, int v5, int v6, int v7,
    int v8, int v9, int v10, int v11,
    int v12, int v13, int v14, int v15)
{   if (x < h8) return search(x, h0, h1, h2, h3, h4, h5, h6, h7,
                                 v0, v1, v2, v3, v4, v5, v6, v7);
    else return search(x, h8, h9, h10, h11, h12, h13, h14, h15,
                          v8, v9, v10, v11, v12, v13, v14, v15);
}

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
    return; //@@
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
    uintptr_t block = search(x,
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
//    0     0    unused space, or one of the non-inital cells of a
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

static inline uintptr_t ALIGN8(uintptr_t a)
{   return (a + 7) & ~(uintptr_t)7;
}

void allocateheap()
{
    uintptr_t i;
    for (i=0; i<nblocks; i++)
    {   uintptr_t bi = blocks[i];
        block_header *b = (block_header *)bi;
        b->hbase = b->h1base = ALIGN8(bi + sizeof(block_header));
        b->h1top = b->h2base = b->h1base + 64*b->halfbitmapsize;
        b->h2top = b->htop = b->h2base + 64*b->halfbitmapsize;
        b->h1starts = b->hstarts = (uint32_t *)b->htop;
        b->h2starts = (uint32_t *)((uintptr_t)b->hstarts + b->halfbitmapsize);
        b->h1fp = b->hfp = (uint32_t *)((uintptr_t)b->hstarts + 2*b->halfbitmapsize);
        b->h2fp = (uint32_t *)((uintptr_t)b->hfp + b->halfbitmapsize);
        b->pinned = (uint32_t *)((uintptr_t)b->hfp + 2*b->halfbitmapsize);
        clearheapstarts(b);
        clearheapfp(b);
    }
    block1 = block2 = 0;
    fringe1 = ((block_header *)blocks_by_age[0])->h1base;
    fringe2 = ((block_header *)blocks_by_age[0])->h2base;
    limit1  = ((block_header *)blocks_by_age[0])->h1top;
    limit2  = ((block_header *)blocks_by_age[0])->h2top;
    heap1_pinchain = heap2_pinchain = packfixnum(0);
    par::reset_segments();
}

// Now I have enough to let me define various allocation functions.

extern void reclaim(int line);
extern LispObject error1(const char *s, LispObject a);
extern LispObject error0(const char *s);
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
    setheapstarts(par::thread_data.segment_fringe);
    qcar(par::thread_data.segment_fringe) = a;
    qcdr(par::thread_data.segment_fringe) = b;
    a = par::thread_data.segment_fringe;
    par::thread_data.segment_fringe += 2*sizeof(LispObject);
    return a;
}

static inline LispObject list2star(LispObject a, LispObject b, LispObject c)
{   // (cons a (cons b c))
    check_space(4*sizeof(LispObject), __LINE__);
    setheapstarts(par::thread_data.segment_fringe);
    qcar(par::thread_data.segment_fringe) = a;
    qcdr(par::thread_data.segment_fringe) = par::thread_data.segment_fringe + 2*sizeof(LispObject);
    a = par::thread_data.segment_fringe;
    par::thread_data.segment_fringe += 2*sizeof(LispObject);
    setheapstarts(par::thread_data.segment_fringe);
    qcar(par::thread_data.segment_fringe) = b;
    qcdr(par::thread_data.segment_fringe) = c;
    par::thread_data.segment_fringe += 2*sizeof(LispObject);
    return a;
}

static inline LispObject acons(LispObject a, LispObject b, LispObject c)
{   // (cons (cons a b) c)
    check_space(4*sizeof(LispObject), __LINE__);
    setheapstarts(par::thread_data.segment_fringe);
    qcar(par::thread_data.segment_fringe) = par::thread_data.segment_fringe + 2*sizeof(LispObject);
    qcdr(par::thread_data.segment_fringe) = c;
    c = par::thread_data.segment_fringe;
    par::thread_data.segment_fringe += 2*sizeof(LispObject);
    setheapstarts(par::thread_data.segment_fringe);
    qcar(par::thread_data.segment_fringe) = a;
    qcdr(par::thread_data.segment_fringe) = b;
    par::thread_data.segment_fringe += 2*sizeof(LispObject);
    return c;
}

static inline LispObject boxfloat(double a)
{   if (!std::isfinite(a)) return error0("floating point error");
    LispObject r;
    check_space(8, __LINE__);
    setheapstartsandfp(par::thread_data.segment_fringe);
    r = par::thread_data.segment_fringe + tagFLOAT;
    qfloat(r) = a;
    par::thread_data.segment_fringe += 8;
    return r;
}

// The code here does not fill in ANY of the fields within the symbol. That
// needs to be done promptly.

static inline LispObject allocatesymbol(LispObject pname)
{   LispObject r;
    check_space(SYMSIZE*sizeof(LispObject), __LINE__);
    setheapstarts(par::thread_data.segment_fringe);
    r = par::thread_data.segment_fringe + tagSYMBOL;
    qflags(r) = tagHDR + typeSYM;
    qvalue(r) = packfixnum(par::allocate_symbol());
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
    par::thread_data.segment_fringe += SYMSIZE*sizeof(LispObject);
    return r;
}

// This one allocates an atom that is n bytes long (plus its header
// word) and again does not fill in ANY of the fields.

static inline LispObject allocateatom(size_t n)
{   LispObject r;
// The actual amount of space allocated must include a word for the
// header and must then be rounded up to be a multiple of 8.
    int nn = ALIGN8(sizeof(LispObject) + n);
    check_space(nn, __LINE__);
    setheapstarts(par::thread_data.segment_fringe);
    r = par::thread_data.segment_fringe + tagATOM;
// I mark the new vector as being a string so that it is GC safe
    qheader(r) = tagHDR + typeSTRING + packlength(n);
    par::thread_data.segment_fringe += nn;
    return r;
}

static inline LispObject makestring(const char *s, int len)
{
    LispObject r = allocateatom(len);
//  qheader(r) = tagHDR + typeSTRING + packlength(len); // already done!
    memcpy(qstring(r), s, len);
    return r;
}

static inline LispObject &elt(LispObject v, size_t n)
{   return ((LispObject *)(v-tagATOM+sizeof(LispObject)))[n];
}

static inline LispObject makevector(int maxindex)
{   int i, len = (maxindex+1)*sizeof(LispObject);
    LispObject r = allocateatom(len);
    qheader(r) = tagHDR + typeVEC + packlength(len);
    for (i=0; i<=maxindex; i++) elt(r, i) = nil;
    return r;
}

static inline LispObject boxint64(int64_t a)
{   if (a >= MIN_FIXNUM && a <= MAX_FIXNUM) return packfixnum(a);
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

extern LispObject lookup(const char *s, size_t n, int flags);

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

INLINE constexpr size_t BOFFO_SIZE = 4096;
char boffo[BOFFO_SIZE+4];
size_t boffop;

using std::swap;

static inline LispObject copy(LispObject x);
static inline LispObject copycontent(LispObject s);

int gccount = 1;

// The version of this implemented at present only supports a single block
// of memory. I really need to look things up in the nice table of memory
// blocks that I have using search...

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
    return p;
}

/*
* [par::reclaim] copies over the thread-local data for garbage collection
*/
void par_reclaim() {
    for (auto x: par::thread_table) {
        auto td = x.second;

        *(td.work1) = copy(*(td.work1));
        *(td.work2) = copy(*(td.work2));
    }

    for (auto& x: par::thread_returns) {
        // copy the values waiting to be joined
        x.second = copy(x.second);
    }
}


extern void ensureheap2space(uintptr_t len);
static uintptr_t space_used = 0;

void inner_reclaim()
{
// The strategy here is due to C J Cheyney ("A Nonrecursive List Compacting
// Algorithm". Communications of the ACM 13 (11): 677-678, 1970),
// adapted to be "conservative". C_stack to C_stackbase will be viewed as
// a set of ambiguous roots.
    uintptr_t s;
    size_t o;
    uintptr_t i;
    LispObject pp;
    npins = heap2_pads = space_used = 0;
    printf("+++ GC number %d ", gccount++);
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
//
// Now scan the stack (ie the ambiguous bases) looking for a reference
// to the start of an object in heap1 that has not alread been pinned.
// When I find one I pin it and add it to heap2_pinchain.

// We have to scan the stacks of all threads
    for (auto t: par::thread_table) {
        auto& td = t.second;

        for (s=(uintptr_t)td.C_stackhead;
            s<(uintptr_t)td.C_stackbase;
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
            {
                a &= ~(LispObject)TAGBITS;
    // The next line is going to assume that the very first location in any heap
    // section has its "starts" bit set, and so the loop can never zoom down and
    // drop beyond the bottom of a segment.
                block_header *block_a = find_block(a);
                LispObject initial_a = a;
                while (!getheapstarts(a))
                {
                    a -= 8;
                    block_header *block_b = find_block(a);
                    assert(block_a == block_b && (uintptr_t)block_a != (uintptr_t)(-1));
                    assert(block_a->h1base <= (uintptr_t)a && (uintptr_t)a < block_a->h1top);
                }
    if (initial_a == 123456) printf("ook\n"); // to get it used.
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
    }
// I will not want to treat anything in the private data structure
// heap2_pinchains as a candidate for pinning, since it will want to
// be discarded at the end of this garbage collection. Well it may have
// just been pinned, so here I will go through an unpin it!
//-    for (pp=heap2_pinchain; isCONS(pp); pp=qcdr(pp))
//-    {   resetpinned(pp);
//-    }
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
// Next I copy all objects directly accessible from proper list list bases.
    for (o=0; o<BASES_SIZE; o++) listbases[o] = copy(listbases[o]);
    for (o=0; o<OBHASH_SIZE; o++)
        obhash[o] = copy(obhash[o]);

    par_reclaim();

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
        swap(b->h1starts, b->h2starts);
        swap(b->h1fp, b->h2fp);
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

//
// @@@ Well I will disable this just for now...
//
        if (false && used > avail/2 && nblocks < 16)
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
    printf("- collection complete\n");
    printf("Pins = %" PRIuPTR
           " heap1_pads = %" PRIuPTR
           " heap2_pads = %" PRIuPTR " total used = %" PRIuPTR "\n",
           npins, heap1_pads, heap2_pads, space_used);
    heap1_pads = 0;
    fflush(stdout);
}

// This force sets the stackhead of the gc thread.
// Useful when inner_reclaim is called directly, e.g during preserve.
void inner_reclaim(LispObject *C_stack) {
    par::thread_data.C_stackhead = C_stack;
    inner_reclaim();
}

volatile int volatile_variable = 12345;

// VB: For now I naively lock this. Will probably very slow.
std::mutex check_space_mutex;

// simple call to check if gc has started and wait
void guard_gc() {
    if (par::gc_on) {
        par::Gc_guard guard;
    }
}

void check_space(int len, int line)
{
// The value passed will always be a multiple of 8. Ensure that that
// many bytes are available at fringe1, calling the garbage collector if
// necessary. This has to allow for the obstruction that pinned items
// lead to. There is some code rather similar to this that is used within
// the garbage collector while copying into heap2. The treatment of large
// memory blocks here will be a little tedious and coule be improved by
// checking the bitmap word at a time not bit at a time.

    guard_gc();

    intptr_t i;
    for (;;) // loop for when pinned items intrude.
    {
        // Check if we can just fit in the current segment
        if (par::thread_data.segment_fringe + len >= par::thread_data.segment_limit) {

            std::lock_guard<std::mutex> lock(check_space_mutex);
            int a = par::Thread_data::SEGMENT_SIZE; // Doesn't compile without the indirection
            uintptr_t size = std::max(a, len);

            if (fringe1 + size >= limit1) {
                reclaim(line);
                if (fringe1 + size >= limit1) {
                    // not enough memory
                    my_exit(137);
                }
                continue;
            } else {
                // printf("segfringe %llu seglimit %llu size %d \n", par::thread_data.segment_fringe, par::thread_data.segment_limit, size);
                par::thread_data.segment_fringe = fringe1;
                par::thread_data.segment_limit = par::thread_data.segment_fringe + size;
                fringe1 += size;
            }
        }

        // printf("%lld %lld %lld %lld\n", thread_data.segment_fringe, fringe1, thread_data.segment_limit, limit1);
        // VB: if a segment has been assigned we make these assertions
        assert(par::thread_data.segment_fringe < par::thread_data.segment_limit);
        assert(par::thread_data.segment_limit <= limit1);

// here thread_data.segment_fringe+len < thread_data.segment_limit
        for (i=0; i<len; i+=8)
            if (getheapstarts(par::thread_data.segment_fringe+i)) break;
        if (i >= len) return; // success
// a block that looks like a string will serve as a padder...
        if (i > 0)
        {   qcar(par::thread_data.segment_fringe) =
                tagHDR + typeSTRING + packlength(i-sizeof(LispObject));
            setheapstarts(par::thread_data.segment_fringe);
            par::thread_data.segment_fringe += i;
            heap1_pads += i;
        }
        while (getheapstarts(par::thread_data.segment_fringe))
        {   LispObject h;
// I now need to skip over the pinned item. If it is floating point,
// a cons cell or a symbol I have to detect that, otherwise its
// header gives its length explicitly.
            if (getheapfp(par::thread_data.segment_fringe)) par::thread_data.segment_fringe += 8;
            else if (!isHDR(h = qcar(par::thread_data.segment_fringe)))
                par::thread_data.segment_fringe += 2*sizeof(LispObject);
            else if ((h & TYPEBITS) == typeSYM)
                 par::thread_data.segment_fringe += SYMSIZE*sizeof(LispObject);
            else par::thread_data.segment_fringe += ALIGN8(sizeof(LispObject) + veclength(h));
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
    par::reset_segments();
}

void reclaim(int line)
{
    bool expected = false;
    if (!par::gc_on.compare_exchange_strong(expected, true)) {
        // any later thread ends up here and only waits for gc to finish
        par::Gc_guard guard;
        return;
    }

    par::Gc_lock gc_lock;

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
            space_used += 2*sizeof(LispObject);
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
            space_used += SYMSIZE*sizeof(LispObject);
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
            space_used += o;
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
                space_used += 8;
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

void copy_symval(LispObject x) {
    assert(isSYMBOL(x));

    if (is_global(x)) {
        qvalue(x) = copy(qvalue(x));
    } else {
        int loc = qfixnum(qvalue(x));
        for (auto x: par::thread_table) {
            auto td = x.second;
            // TODO: need local_symbol here!
            auto& val = td.local_symbol(loc);
            val = copy(val);
        }

        auto& global = par::fluid_globals[loc];
        global = copy(global);
    }
}

static inline LispObject copycontent(LispObject s)
{   LispObject h = s, w;
    assert(inheap(h) && getheapstarts(h));
    if (getheapfp(h)) return h + 8;
    h = qcar(s);
// I call copycontent with pointers into heap2 and there should never
// be forwarding pointers there. The only case where I call copycontent
// on something in heap1 is when it is pinned, and in that case it should
// not be forwarded either.
// @@@ But if I had allowed the old pinchain there to get itself pinned
// I could be in trouble and find a forwarding pointer, so in the degenerate
// case I see that I will suppose I can "do nothing" and set the wild location
// to nil..
    if (isFORWARD(h)) return nil;
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
            // par::symval(w) = copy(par::symval(w));
            copy_symval(w);
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

#include "printing.hpp"

include "reading.hpp"

#include "eval.hpp"

#include "specforms.hpp"

#include "basic.hpp"

#include "arith.hpp"

#include "miscfns.hpp"

LispObject Lthread(LispObject lits, LispObject x) {
    auto f = [=]() {
        return eval(x);
    };

    int tid = par::start_thread(f);
    return packfixnum(tid);
}

/**
 * thread2 allows passing a function and a list of arguments
 * */
LispObject Lthread2(LispObject lits, LispObject func, LispObject arg) {
    auto f = [=]() {
        return Lapply(nil, func, arg);
    };

    int tid = par::start_thread(f);
    return packfixnum(tid);
}

LispObject Ljoin_thread(LispObject lits, LispObject x) {
    int tid = qfixnum(x);
    LispObject result = par::join_thread(tid);
    return result;
}

LispObject Lmutex(LispObject _data) {
    return packfixnum(par::mutex());
}

LispObject Lmutex_lock(LispObject lits, LispObject x) {
    int id = qfixnum(x);
    par::mutex_lock(id);
    return nil;
}

LispObject Lmutex_unlock(LispObject lits, LispObject x) {
    int id = qfixnum(x);
    par::mutex_unlock(id);
    return nil;
}

LispObject Lcondvar(LispObject _data) {
    return packfixnum(par::condvar());
}

LispObject Lcondvar_wait(LispObject lits, LispObject cv, LispObject m) {
    int cvid = qfixnum(cv);
    int mid = qfixnum(m);
    par::condvar_wait(cvid, mid);
    return nil;
}

LispObject Lcondvar_notify_one(LispObject lits, LispObject cv) {
    int cvid = qfixnum(cv);
    par::condvar_notify_one(cvid);
    return nil;
}

LispObject Lcondvar_notify_all(LispObject lits, LispObject cv) {
    int cvid = qfixnum(cv);
    par::condvar_notify_all(cvid);
    return nil;
}

LispObject Lhardware_threads(LispObject _data) {
    return packfixnum(std::thread::hardware_concurrency());
}

LispObject Lthread_id(LispObject _data) {
    return packfixnum(par::thread_data.id);
}

#include "tables.hpp"

#include "setup.hpp"

#include "main.hpp"


// end of main source file.
