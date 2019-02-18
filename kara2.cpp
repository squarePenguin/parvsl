// Karatsuba multiplication                                A C Norman, 2019


/**************************************************************************
 * Copyright (C) 2019, Codemist.                         A C Norman       *
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



#include <cstdio>
#include <cstring>
#include <cstdint>
#include <cctype>
#include <cinttypes>
#include <cstdlib>
#include <cstdarg>
#include <random>
#include <iostream>
#include <iomanip>
#include <thread>
#include <ctime>
#include <chrono>

#define OLD 1

#include "arithlib.hpp"

using namespace arithlib;

#ifdef LOOK_AT_CODE_FOR_BASIC_OPERATIONS

// I want a version of add_with_carry() that leads to the generation of
// good code, so I have these small and rather stupid functions here so that
// I can compile this stuff into machine code and see what is generated.
// Using g++ on x86_64 and -O3 the expansions of add_with_carry here end up
// jump-free and at least close to as slick as I could manage in hand-crafted
// assembly code. Whew!
// I use volatile variables A, B etc to ensure that the compiler will actually
// perform the operations shown, and to seprate compilation of the
// add_with_carry from everything to do with argument passing.

volatile uint64_t A=1, B=1, C_IN=1, C_OUT=0, R=0;

void test_add_with_carry1()
{   uint64_t r1;
    C_OUT = add_with_carry(A, B, r1);
    R = r1;
}

void test_add_with_carry2()
{   uint64_t r1;
    C_OUT = add_with_carry(A, B, C_IN, r1);
    R = r1;
}

#endif

#ifdef __cpp_inline_variables

// For versions of C++ up to C++17 I will put constant values in header
// files using something along the line of "static const int VAR = VAL;".
// This should give the compiler a chance to replace the name with its value
// throughout the compilation unit, and if the compiler is clever enough it
// will avoid leaving a word of memory with the value stored if all uses
// have been dealt with more directly. However it will tend to lead to a
// lot of "static variable defined but not used" warnings.
// From C++17 onwards (and C++ mandates the __cpp_inline_variables macro to
// indicate if the feature is in place) I will use
// "inline const int VAR = VAL;" and now if memory is allocated for the
// variable it will only be allocated once, and I hope that compilers will
// not feel entitled to moan about cases where there are no references.
//
#define INLINE_VAR inline
#else
#define INLINE_VAR static
#endif


//                       Karatsuba multiplication.
//                       =========================

// The multiplication code has a signature something like
//   void mult(uint64_t *a, size_t lena,
//             uint64_t *b, size_t lenb,
//             uint64_t *c,
//             uint64_t *work_vector=NULL)
// where a and b are vectors with lena and lenb words in then respectively.
// c is a vector and lena+lenb words of a product will be written into it.
// Note that sometimes the top digit will end up as either 0 or -1.
// w must be a workspace vector of length lenb+2*log(lenb) [the log is to
// the base 2]. It is not needed if lenb is very short.
//

// For use within the multiplication code I need variants on my
// addition and subtraction code. 

// I want:
//    kadd(a, lena, r, lenr);          // r += a; and return a carry
//    kadd(a, lena, b, lenb, r, lenr); // r := a + b; and return a carry
// and correspondingly for subtraction. Note that these have lenr set on
// input and will propagate carries or extend a result all the way up to
// lenr words even if lena is a lot shorter.

uint64_t kadd(uint64_t *a, size_t lena, uint64_t *r, size_t lenr)
{   uint64_t carry = 0;
    size_t i;
    for (i=0; i<lena; i++)
        carry = add_with_carry(a[i], r[i], carry, r[i]);
    while (carry!=0 && i<lenr)
    {   carry = add_with_carry(r[i], carry, r[i]);
        i++;
    }
    return carry;
}

// For the 2-input addition I want lena >= lenb.

uint64_t kadd(uint64_t *a, size_t lena, uint64_t *b, size_t lenb,
              uint64_t *r, size_t lenr)
{   uint64_t carry = 0;
    size_t i;
    for (i=0; i<lenb; i++)
        carry = add_with_carry(a[i], b[i], carry, r[i]);
    while (i<lena)
    {   carry = add_with_carry(a[i], carry, r[i]);
        i++;
    }
    if (i < lenr)
    {   r[i++] = carry;
        carry = 0;
    }
    while (i < lenr) r[i++] = 0;
    return carry;
}

// r = r - a;

uint64_t ksub(uint64_t *a, size_t lena, uint64_t *r, size_t lenr)
{   uint64_t borrow = 0;
    size_t i;
    for (i=0; i<lena; i++)
        borrow = subtract_with_borrow(r[i], a[i], borrow, r[i]);
    while (borrow!=0 && i<lenr)
    {   borrow = subtract_with_borrow(r[i], borrow, r[i]);
        i++;
    }
    return borrow;
}

// c = a*b;

inline void classical_multiply(uint64_t *a, size_t lena,
                               uint64_t *b, size_t lenb,
                               uint64_t *r)
{   uint64_t hi=0, lo;
    for (size_t j=0; j<lenb; j++)
        multiplyadd64(a[0], b[j], hi, hi, r[j]);
    r[lenb] = hi;
    for (size_t i=1; i<lena; i++)
    {   hi = 0;
        for (size_t j=0; j<lenb; j++)
        {   multiplyadd64(a[i], b[j], hi, hi, lo);
            hi += add_with_carry(lo, r[i+j], r[i+j]);
        }
        r[i+lenb] = hi;
    }
}

// c = c + a*b and return any carry.

inline uint64_t classical_multiply_and_add(uint64_t *a, size_t lena,
                                           uint64_t *b, size_t lenb,
                                           uint64_t *r)
{   uint64_t hi=0, lo, carry=0;
    for (size_t i=0; i<lena; i++)
    {   hi = 0;
        for (size_t j=0; j<lenb; j++)
        {   multiplyadd64(a[i], b[j], hi, hi, lo);
            hi += add_with_carry(lo, r[i+j], r[i+j]);
        }
        carry = add_with_carry(hi, r[i+lenb], carry, r[i+lenb]);
    }
    return carry;
}

// Now variants that use just a single digit first argument. These may be seen
// as optimized cases.

inline void classical_multiply(uint64_t a,
                               uint64_t *b, size_t lenb,
                               uint64_t *r)
{   uint64_t hi=0, lo;
    for (size_t j=0; j<lenb; j++)
        multiplyadd64(a, b[j], hi, hi, r[j]);
    r[lenb] = hi;
}

// c = c + a*b and return any carry.

inline uint64_t classical_multiply_and_add(uint64_t a,
                                           uint64_t *b, size_t lenb,
                                           uint64_t *r)
{   uint64_t hi=0, lo, carry=0;
    for (size_t j=0; j<lenb; j++)
    {   multiplyadd64(a, b[j], hi, hi, lo);
        hi += add_with_carry(lo, r[j], r[j]);
    }
    return add_with_carry(hi, r[lenb], carry, r[lenb]);
}

// The cutoff here is potentially system-dependent, however the exact value
// is not incredibly critical because for quite some range around it the
// classical and Karatsuba methods will have pretty similar costs. The
// value here was chosen after a few tests on x86_64 both using cygwin-64
// and Linux. As a sanity check the corresponding threshold as used by
// gmp was checked - there depending on exactly what computer is used they
// move to the more complicated method at somewhere between 10 and 35 word
// (well their term is "limb") numbers.

INLINE_VAR size_t KARATSUBA_CUTOFF = 5;

inline void kara_and_add2(uint64_t *a, size_t lena,
                  uint64_t *b, size_t lenb,
                  uint64_t *c,
                  uint64_t *w);

inline void kara_and_add1(uint64_t *a, size_t lena,
                  uint64_t *b, size_t lenb,
                  uint64_t *c,
                  uint64_t *w)
{   if (lena<KARATSUBA_CUTOFF)
        classical_multiply_and_add(a, lena, b, lenb, c);
    else
    {   if (lena>lenb && lenb%2==0)
        {   kara_and_add2(a, lenb, b, lenb, c, w);
            c[2*lenb] = 0;
            classical_multiply_and_add(a[lena-1], b, lenb, c+lenb);
        }
        else kara_and_add2(a, lena, b, lenb, c, w);
    }
}

inline void kara1(uint64_t *a, size_t lena,
                  uint64_t *b, size_t lenb,
                  uint64_t *c, uint64_t *w);

inline void kara_and_add2(uint64_t *a, size_t lena,
                  uint64_t *b, size_t lenb,
                  uint64_t *c,
                  uint64_t *w)
{
// The all the cases that are supported here the next line sets n to a
// suitably rounded up half length.
    size_t n = (lena+1)/2;
    uint64_t c1 = kadd(a, n, a+n, lena-n, w, n);     // a0+a1
    uint64_t c2 = kadd(b, n, b+n, lenb-n, w+n, n);   // b0+b1
    kara_and_add1(w, n, w+n, n, c+n, w+2*n);         // (a0+a1)*(b0+b1)
    size_t lenc = lena+lenb;
    if (c1 != 0)
    {   kadd(w+n, n, c+2*n, lenc-2*n);               // fix for overflow
        if (c2 != 0)
        {   kadd(w, n, c+2*n, lenc-2*n);             // fix for overflow
            for (size_t i=3*n; c2!=0 && i<lenc; i++)
                c2 = add_with_carry(c[i], c2, c[i]);
        }
    }
    else if (c2 != 0) kadd(w, n, c+2*n, lenc-2*n);   // fix for overflow
    for (size_t i=0; i<2*n; i++) w[i] = 0;
    kara1(a, n, b, n, w, w+2*n);                // a0*b0
    kadd(w, 2*n, c, lenc);                           // add in at bottom..
    ksub(w, 2*n, c+n, lenc-n);                       // and subtract 1 digit up
    for (size_t i=0; i<lena+lenb-2*n; i++) w[i] = 0;
    kara1(a+n, lena-n, b+n, lenb-n, w, w+2*n);
    kadd(w, lena+lenb-2*n, c+2*n, lenc-2*n);         // a1*b1 may be shorter
    ksub(w, lena+lenb-2*n, c+n, lenc-n);
}


// This code is where the main recursion happens. The main complication
// within it is dealing with unbalanced length operands.

inline void kara_and_add(uint64_t *a, size_t lena,
                 uint64_t *b, size_t lenb,
                 uint64_t *c,
                 uint64_t *w)
{   if (lena < lenb)
    {   std::swap(a, b);
        std::swap(lena, lenb);
    }
// Now b is the shorter operand. If it is not too big I can just do
// simple classical long multiplication.
    if (lenb < KARATSUBA_CUTOFF)
    {   classical_multiply_and_add(a, lena, b, lenb, c);
        return;
    }
// For equal lengths I can do just one call to Karatsuba.
    if (lena == lenb)
    {   kara_and_add1(a, lena, b, lenb, c, w);
        return;
    }
// If the two inputs are unbalanced in length I will perform multiple
// balanced operations each of which can be handled specially. I will
// try to make each subsidiary multiplication as big as possible.
// This will be lenb rounded up to an even number.
    size_t len = lenb + (lenb & 1);
    while (lena>=len)
    {   kara_and_add1(a, len, b, lenb, c, w);
        a += len;
        lena -= len;
        c += len;
    }
    if (lena != 0) kara_and_add(b, lenb, a, lena, c, w);
}

inline void kara2(uint64_t *a, size_t lena,
                  uint64_t *b, size_t lenb,
                  uint64_t *c, uint64_t *w);

inline void kara1(uint64_t *a, size_t lena,
                  uint64_t *b, size_t lenb,
                  uint64_t *c, uint64_t *w)
{   if (lena<KARATSUBA_CUTOFF) classical_multiply(a, lena, b, lenb, c);
    else if (lena>lenb && lenb%2==0)
    {
// In the case that the product is (2n+1)*(2n) I will not want to round up to
// a (2n+1)*(2n+1) Karatsuba because then the numbers do not split neatly, and
// I could end up padding to (2n+2)*(2n+2). So I do a (2n)*(2n) fast multiply
// and then deal with the "+1" classically.
        kara2(a, lena-1, b, lenb, c, w);
// I write the classical multiplication by a single digit out by hand
// here: it adds its product into the result I have got already, except that
// the final top digit goes into space that has not previously been used.
        uint64_t hi=0, lo;
        for (size_t j=0; j<lenb; j++)
        {   multiplyadd64(a[lena-1], b[j], hi, hi, lo);
            hi += add_with_carry(lo, c[lena+j-1], c[lena+j-1]);
        }
        c[lena+lenb-1] = hi;
    }
// here I have a nicely balanced case to work with.
    else kara2(a, lena, b, lenb, c, w);
}


// The key function here multiplies two numbers that are at least almost
// the same length. The cases that can arise here are
//      2n   2n       Easy and neat sub-division
//      2n   2n-1     Treat the second number as if it has a padding zero
//      2n-1 2n       Treat first number as padded
//      2n-1 2n-1     Treat both numbers as if padded to size 2n
// Observe that if the two numbers have different lengths then the longer
// one is an even length, so the case (eg) 2n+1,2n will not arise.

inline void kara2(uint64_t *a, size_t lena,
                  uint64_t *b, size_t lenb,
                  uint64_t *c, uint64_t *w)
{
// The all the cases that are supported here the next line sets n to a
// suitably rounded up half length.
    size_t n = (lena+1)/2;
    uint64_t c1 = kadd(a, n, a+n, lena-n, w, n);     // a0+a1
    uint64_t c2 = kadd(b, n, b+n, lenb-n, w+n, n);   // b0+b1
    kara1(w, n, w+n, n, c+n, w+2*n);                 // (a0+a1)*(b0+b1)
// I have now multiplied (a0+a1) truncated to n words by (b0+b1) similarly
// truncated. However when I added a0 to a1 I might have had a carry into
// the next word up. That means that the full product intrudes into just
// one more word...
    size_t lenc = lena + lenb;
    c[3*n] = 0;
    if (c1 != 0)
    {   c[3*n] = kadd(w+n, n, c+2*n, n);
        if (c2 != 0) c[3*n] += kadd(w, n, c+2*n, n) + 1;
    }
    else if (c2 != 0) c[3*n] = kadd(w, n, c+2*n, n);
// Now form the product of the high halves of a and b. This can be shorter
// than 2*n.
    kara1(a+n, lena-n, b+n, lenb-n, w, w+2*n);
// The top half of the product (less just one word!) can be copied into
// previously unused space in c...
    for (size_t i=n+1; i<lena+lenb-2*n; i++) c[2*n+i] = w[i];
// Next I subtract the high product off. This is not liable to underflow
// (and in doing so call for a long chain of borrows) because it is basically
// subtracting a value from 2^64 times the same value.
    ksub(w, lena+lenb-2*n, c+n, lenc-n);
// Now add in the bottom part.
    kadd(w, n+1, c+2*n, lenc-2*n);
// I now need to do something similar with the product of the low parts of
// a and b.
    kara1(a, n, b, n, w, w+2*n);                     // a0*b0
// I can just copy the low n words across.
    for (size_t i=0; i<n; i++) c[i] = w[i];
    kadd(w+n, n, c+n, lenc-n);                       // add in at bottom..
    ksub(w, 2*n, c+n, lenc-n);                       // and subtract 1 digit up
}


// This code is where the main recursion happens. The main complication
// within it is dealing with unbalanced length operands.

inline void kara(uint64_t *a, size_t lena,
                 uint64_t *b, size_t lenb,
                 uint64_t *c, uint64_t *w)
{   if (lena < KARATSUBA_CUTOFF || lenb < KARATSUBA_CUTOFF)
    {   classical_multiply(a, lena, b, lenb, c);
        return;
    }
// For equal lengths I can do just one call to Karatsuba.
    if (lena == lenb)
    {   kara2(a, lena, b, lenb, c, w);
        return;
    }
    if (lena < lenb)
    {   std::swap(a, b);
        std::swap(lena, lenb);
    }
// Now b is the shorter operand. The case (2n)*(2n-1) will be handled
// using Karatsuba merely by treating the smaller number as if padded with
// a leading zero.
    if (lena == lenb+1 && lena%2==0)
    {   kara2(a, lena, b, lenb, c, w);
        return;
    }
// If the two inputs are unbalanced in length I will perform multiple
// balanced operations each of which can be handled specially. I will
// try to make each subsidiary multiplication as big as possible.
// This will be lenb rounded up to an even number.
// This is where I need a version of the code that adds the product in
// on top of an existing partial result.
// I will be willing to do chunks that are of an even size that is
// either lenb or lenb+1. 
    size_t len = lenb + (lenb & 1);
    uint64_t *a1 = a, *c1 = c;
    size_t lena1 = lena;
    for (;;)
    {   kara1(a1, len, b, lenb, c1, w);
        c1 += 2*len;
        if (lenb < len) c1[-1] = 0; // pad because b was short
        if (lena1 <= 3*len) break;
        a1 += 2*len;
        lena1 -= 2*len;
    }
    while (lena>=len)
    {   kara1(a, len, b, lenb, c, w);
        a += len; lena -= len;
        c += len;
    }
    if (lena != 0) kara(b, lenb, a, lena, c, w);
}

// Finally I can provide the top-level entrypoint that accepts signed
// integers that may not be the same size.

INLINE_VAR const size_t KARA_FIXED_WORKSPACE_SIZE = 200;
INLINE_VAR const size_t KARA_WORKSPACE_SIZE = 408;
INLINE_VAR uint64_t kara_workspace[KARA_WORKSPACE_SIZE];

// First some written out code that multiplies 2-digit numbers together.
// One version treats them as unsigned, the second as signed.

inline void mul2x2(uint64_t ahi, uint64_t alo,
                   uint64_t bhi, uint64_t blo,
                   uint64_t &r3, uint64_t &r2, uint64_t &r1, uint64_t &r0)
{   uint64_t r1a;
    multiply64(alo, blo, r1a, r0);
    uint64_t r1b, r2a;
    multiplyadd64(alo, bhi, r1a, r2a, r1a);
    uint64_t r2b;
    multiplyadd64(ahi, blo, r1a, r2b, r1);
    uint64_t r3a;
    multiplyadd64(ahi, bhi, r2a, r3a, r2a);
    r3a += add_with_carry(r2a, r2b, r2);
    r3 = r3a;
}

inline void mul2x2S(int64_t ahi, uint64_t alo,
                    int64_t bhi, uint64_t blo,
                    int64_t &r3, uint64_t &r2, uint64_t &r1, uint64_t &r0)
{   uint64_t r1a;
    multiply64(alo, blo, r1a, r0);
    uint64_t r1b, r2a;
    multiplyadd64(alo, (uint64_t)bhi, r1a, r2a, r1a);
    uint64_t r2b;
    multiplyadd64((uint64_t)ahi, blo, r1a, r2b, r1);
    int64_t r3a;
    signed_multiplyadd64(ahi, bhi, r2a, r3a, r2a);
    r3a = (int64_t)((uint64_t)r3a + add_with_carry(r2a, r2b, r2a));
// Do the arithmetic in unsigned mode in case of overflow problems.
    if (ahi < 0) r3a = (int64_t)((uint64_t)r3a -
                                 subtract_with_borrow(r2a, blo, r2a));
    if (bhi < 0) r3a = (int64_t)((uint64_t)r3a -
                                 subtract_with_borrow(r2a, alo, r2a));
    r2 = r2a;
    r3 = r3a;
}

inline void kmultiply(uint64_t *a, size_t lena,
                      uint64_t *b, size_t lenb,
                      uint64_t *r, size_t &lenr)
{
// If a and/or be are negative then I can treat their true values as
//    a = sa + va      b = sb + vb
// where sa and sb and the signs - represented here as 0 for a positive
// number and -2^(64*len) for a negative one. va and vb are then the simple
// bit-patterns for a and b but now interpreted as unsigned values. So if
// instead of using 64-bit digits I was using 8 bit ones, the value -3
// would be stored as 0xfd and that would be spit up as -128 + 253.
// Then a*b = sa*sb + sa*vb + sb*va + va*vb.
// The last item there is just the product of a and b when treated as
// unsigned values, and so is what I compute first here rather simply.
// If sa and/or sb is non-zero it is just the negative of a power of 2^64,
// and so I can correct the unsigned product into a signed one by (sometimes)
// subtracting a shifted version of a or b from it.
// If both arguments are tiny I write out the code in-line believing that
// any cost savings there will be valuable.
    if (lena + lenb <= 5) switch (lena + 4*lenb)
    {
    case 1+4*1:
        {   int64_t hi;
            signed_multiply64(a[0], b[0], hi, r[0]);
            r[1] = (int64_t)hi;
            lenr = 2;
            return;
        }
    case 1+4*2:
        {   int64_t hi1;
            uint64_t hi0, lo1;
            multiply64(a[0], b[0], hi0, r[0]);
            signed_multiplyadd64(a[0], b[1], hi0, hi1, lo1);
            if (negative(a[0]))
                hi1 = (int64_t)((uint64_t)hi1 -
                                subtract_with_borrow(lo1, b[0], lo1));
            r[1] = lo1;
            r[2] = hi1;
            lenr = 3;
            return;
        }
    case 2+4*1:
        {   int64_t hi1;
            uint64_t hi0, lo1;
            multiply64(b[0], a[0], hi0, r[0]);
            signed_multiplyadd64(b[0], a[1], hi0, hi1, lo1);
            if (negative(b[0]))
                hi1 = (int64_t)((uint64_t)hi1 -
                                subtract_with_borrow(lo1, a[0], lo1));
            r[1] = lo1;
            r[2] = hi1;
            lenr = 3;
            return;
        }
      case 2+4*2:
          {   int64_t top;
              mul2x2S((int64_t)a[1], a[0],
                      (int64_t)b[1], b[0],
                      top, r[2], r[1], r[0]);
              r[3] = (uint64_t)top;
              lenr = 4;
              return;
          }
// Just for now I have this framework in place but I have not implemented
// and of the other special cases!
    case 1+4*3:
    case 3+4*1:

    case 1+4*4:
    case 4+4*1:

    case 2+4*3:
    case 3+4*2:
        // Drop through these cases for now and just use the generic scheme.
    default:
        ;
    }

// If the smaller input is reasonably small I will merely use classical
// multiplication.
    if (lenb < KARATSUBA_CUTOFF ||
        lenb < KARATSUBA_CUTOFF) classical_multiply(a, lena, b, lenb, r);
    else
    {
// For many smaller cases I will just use some static pre-allocated workspace
// and hence avoid potential storage management overheads.
        if (lena <= KARA_FIXED_WORKSPACE_SIZE ||
            lenb <= KARA_FIXED_WORKSPACE_SIZE)
            kara(a, lena, b, lenb, r, kara_workspace);
        else
        {   push(a); push(b);
            size_t lenw;
            if (lena < lenb) lenw = lena;
            else lenw = lenb;
            for (size_t i=lenw; i>8; i=i/2) lenw++;
            uint64_t *w = reserve(2*lenw);
            pop(b); pop(a);
            kara(a, lena, b, lenb, r, w);
            abandon(w);
        }
    }
// Now adapt for the situation where one or both of the inputs had been
// negative.
    if (negative(a[lena-1]))
    {   uint64_t carry = 1;
        for (size_t i=0; i<lenb; i++)
            carry = add_with_carry(r[i+lena], ~b[i], carry, r[i+lena]);
    }
    if (negative(b[lenb-1]))
    {   uint64_t carry = 1;
        for (size_t i=0; i<lena; i++)
            carry = add_with_carry(r[i+lenb], ~a[i], carry, r[i+lenb]);
    }
// The actual value may be 1 word shorter than this. So test the top
// digit of r and if necessary reduce lenr. But note that I can only
// ever need to shrink the result by 1 word. Also note that the pending
// result is at least of length 2 here.
    lenr = lena + lenb;
    if (r[lenr-1] == 0)
    {   if (positive(r[lenr-2])) lenr--;
    }
    else if (r[lenr-1] == -(uint64_t)1)
    {   if (negative(r[lenr-2])) lenr--;
    }
}

//===========================================================================
//===========================================================================
//===========================================================================
//===========================================================================

#ifndef NO_MAIN

// Here will be some test code

#ifndef NO_GMP
#include "gmp.h"
#endif // NO_GMP


// This is extracted from a slightly old arithlib.hpp and is a direct
// classical implementation of multiplication that I have tested reasonably
// thoroughly.

inline void referencemultiply(const uint64_t *a, size_t lena,
                             const uint64_t *b, size_t lenb,
                             uint64_t *r, size_t &lenr)
{   for (size_t i=0; i<lena+lenb; i++) r[i] = 0;
// If a and/or be are negative then I can treat their true values as
//    a = sa + va      b = sb + vb
// where sa and sb and the signs - represented here as 0 for a positive
// number and -2^(64*len) for a negative one. va and vb are then the simple
// bit-patterns for a and b but now interpreted as unsigned values. So if
// instead of using 64-bit digits I was using 8 bit ones, the value -3
// would be stored as 0xfd and that would be spit up as -128 + 253.
// Then a*b = sa*sb + sa*vb + sb*va + va*vb.
// The last item there is just the product of a and b when treated as
// unsigned values, and so is what I compute first here rather simply.
// If sa and/or sb is non-zero it is just the negative of a power of 2^64,
// and so I can correct the unsigned product into a signed one by (sometimes)
// subtracting a shifted version of a or b from it.
    for (size_t i=0; i<lena; i++)
    {   uint64_t hi = 0;
        for (size_t j=0; j<lenb; j++)
        {   uint64_t lo;
// The largest possible value if (hi,lo) here is (0xffffffffffffffff, 0)
// which arises if a[1], b[i] and prev_hi are all at their maximum. That
// means that in all other cases (and in particular unless lo==0) hi ends
// up LESS than the maximum, and so adding one to it can happen without
// overflow.
            multiplyadd64(a[i], b[j], hi, hi, lo);
            hi += add_with_carry(lo, r[i+j], r[i+j]);
        }
        r[i+lenb] = hi;
    }
    if (negative(a[lena-1]))
    {   uint64_t carry = 1;
        for (size_t i=0; i<lenb; i++)
            carry = add_with_carry(r[i+lena], ~b[i], carry, r[i+lena]);
    }
    if (negative(b[lenb-1]))
    {   uint64_t carry = 1;
        for (size_t i=0; i<lena; i++)
            carry = add_with_carry(r[i+lenb], ~a[i], carry, r[i+lenb]);
    }
    lenr = lena + lenb;
// The actual value may be 1 word shorter than this.
//  test top digit or r and if necessary reduce lenr.
    truncate_positive(r, lenr);
    truncate_negative(r, lenr);
}



static const size_t MAX = 3000;

uint64_t a[MAX];
uint64_t b[MAX];
uint64_t c[2*MAX];
uint64_t c1[2*MAX];
uint64_t w[3*MAX];

size_t lena, lenb, lenc, lenc1, lenw;

static uint64_t MULT = 6364136223846793005U;
static uint64_t ADD  = 1442695040888963407U;

int main(int argc, char *argv[])
{   uint64_t seed;
    if (argc > 1) seed = atoi(argv[1]);
    else seed = mersenne_twister() & 0xffff;
    std::cout << "Karatsuba tests " << seed << std::endl;
    reseed(seed);

#ifndef NO_CORRECTNESS

// This generates some random data and then calls the multiplication code
// to multiply each m*n prefix of it. It compares the results from the
// new code here against those from  "referencemultiply".

    for (int run=0; run<40; run++)
    {   for (size_t i=0; i<MAX; i++)
        {   a[i] = mersenne_twister();
            b[i] = mersenne_twister();
        }
        int count = 0;
        int maxlen = 4; // 20;
        for (lena=1; lena<maxlen; lena++)
        {   for (lenb=1; lenb<maxlen; lenb++)
            {   std::cout << " " << lena
                          << "*" << lenb
                          << " ";
                if (++count == 8)
                {   std::cout << std::endl;
                    count = 0;
                }
                referencemultiply(a, lena, b, lenb, c, lenc);
                kmultiply(a, lena, b, lenb, c1, lenc1);
                bool ok=(lenc == lenc1);
                for (size_t i=0; ok && i<lenc; i++)
                    if (c[i] != c1[i]) ok = false;
                if (!ok)
                {   std::cout << std::endl;
                    display("a", a, lena);
                    display("b", b, lenb);
                    display("c ", c,  lenc);
                    display("c1", c1, lenc1);
                    std::cout << "Failed" << std::endl;
                    return 1;
                }
            }
        }
        std::cout << std::endl;
    }

#endif // NO_CORRECTNESS

#ifndef NO_TUNING

// Parameters for a Linear Congruential generator mod 2^64. 
    size_t best = 0;
    double best_time = HUGE_VAL;

// First I will try multiplications (in fact of balanced inputs) for
// a range of cutoffs, and see how long things take. For each potential
// cutoff value I will test a range of different length inputs.

    for (KARATSUBA_CUTOFF=4; KARATSUBA_CUTOFF<35; KARATSUBA_CUTOFF++)
    {   for (size_t i=0; i<MAX; i++)
        {   a[i] = mersenne_twister();
            b[i] = mersenne_twister();
        }
        clock_t cl0 = clock();
        for (lena=4; lena<100; lena++)
        {   lenb = lena;
            for (size_t n = 0; n<100000/(lena*lena); n++)
            {   for (size_t m=0; m<500; m++)
                    kmultiply(a, lena, b, lenb, c1, lenc1);
                for (size_t i=0; i<lena; i++)
                    a[i] = MULT*a[i] + ADD;
                for (size_t i=0; i<lenb; i++)
                    b[i] = MULT*b[i] + ADD;
            }
        }
        clock_t cl1 = clock();
        double t = (cl1-cl0)/(double)CLOCKS_PER_SEC;
        std::cout << KARATSUBA_CUTOFF << " gives time " << t << std::endl;
        if (t < best_time)
        {   best_time = t;
            best = KARATSUBA_CUTOFF;
        }
    }
    std::cout << "Best = " << best << std::endl;
    KARATSUBA_CUTOFF = best;

// When I think I have identified the best cutoff I will measure the
// resulting performance for various small input sizes.

    for (size_t lena=1; lena<2*KARATSUBA_CUTOFF; lena++)
    {   lenb = lena;
        for (size_t i=0; i<MAX; i++)
        {   a[i] = mersenne_twister();
            b[i] = mersenne_twister();
        }
        clock_t cl0 = clock();
        for (size_t n = 0; n<100000/(4+lena*lena); n++)
        {   for (size_t m=0; m<1000; m++)
                kmultiply(a, lena, b, lenb, c1, lenc1);
            for (size_t i=0; i<lena; i++)
                a[i] = MULT*a[i] + ADD;
            for (size_t i=0; i<lenb; i++)
                b[i] = MULT*b[i] + ADD;
        }
        clock_t cl1 = clock();
        double t = (cl1-cl0)/(double)CLOCKS_PER_SEC;
        std::cout << std::setw(10) << lena << std::setw(10) << t << std::endl;
    }


#endif // NO_TUNING


#ifndef NO_GMP
    const size_t table_size = 100;

    size_t size[table_size];
    size_t testcount[table_size];
    double mine[table_size];
    double gmp[table_size];

    uint64_t my_check = 1;
    uint64_t gmp_check = 1;

    KARATSUBA_CUTOFF = 12;   // Use (at least close to) optimal value.

    size_t tests;

    reseed(seed);
    lena = 1;
    for (size_t trial=0; trial<table_size; trial++)
    {   lena = (9*lena+7)/8;
// This arranges to run tests using a sequence of number-lengths
// from 2 to 1000 such that each case is around 20% longer than the
// previous one. This turns out to give about 30 samples.
        size[trial] = 0;
        if (lena >= 2000) break;
        lenb = lena;
// I start by filling my input vectors with random data. I set the same
// seed before trying my code and before trying gmp so that each get the
// same set of test cases.
        for (size_t i=0; i<lena; i++)
        {   a[i] = mersenne_twister();
            b[i] = mersenne_twister();
        }
        clock_t cl0 = clock();

// When using Karatsuba the cost of a multiplication is expected to
// grow as n^1.585, and so to arrange that I tke roughly the same
// absolute time on each number-length I perform my tests a number of
// times scaled inversely by that.
        size_t tests = 2+100000/(int)std::pow((double)lena, 1.585);
        for (size_t n = 0; n<tests; n++)
        {
// The gpm function mpn_mul multiplies unsigned integers, while my
// kmultiply is at a slightly higher level and deals with signed values.
// I want to compare their results, and so forcing all inputs to be positive
// (in my representation) bl clearing most significant bits is necessary.
            a[lena-1] &= 0x7fffffffffffffffU;
            b[lena-1] &= 0x7fffffffffffffffU;
// So that all the administration here does not corrupt my measurement
// I do the actual multiplication of each test case 5000 times.
            for (size_t m=0; m<5000; m++)
                kmultiply(a, lena, b, lenb, c1, lenc1);
// By accumulating a sort of checksum on all the products that I compute
// I will be able to reassure myself that the output from gmp and from my
// own code agrees.
            for (size_t i=0; i<lena+lenb; i++)
                my_check = my_check*MULT + c1[i];
// I alter the inputs using a linear congruential scheme (which is cheap)
// so that for any length inputs I am doing test multiplications of a
// range of varied cases. This is so that stray special cases are less liable
// to corrupt my results.
            for (size_t i=0; i<lena; i++)
                a[i] = MULT*a[i] + ADD;
            for (size_t i=0; i<lenb; i++)
                b[i] = MULT*b[i] + ADD;
        }
        clock_t cl1 = clock();
        double t = (cl1-cl0)/(double)CLOCKS_PER_SEC;
// I store details of this test run in an array for display later on.
        size[trial] = lena;
        testcount[trial] = 5000*tests;
        mine[trial] = t;
        std::cout << ".";
        std::cout.flush();
    }
// Now do just the same sort of thing but using gmp rather then my
// multiplication code.
    reseed(seed);
    lena = 1;
    for (size_t trial=0; trial<table_size; trial++)
    {   lena = (9*lena+7)/8;
        if (lena >= 2000) break;
        lenb = lena;
        for (size_t i=0; i<lena; i++)
        {   a[i] = mersenne_twister();
            b[i] = mersenne_twister();
        }
        clock_t cl0 = clock();
// somewhere around lena=1400 the fraction on the next line reduces
// to zero. So for the last few cases I will take distinctly longer
// than for each of the rest.
        size_t tests = 2+100000/(int)std::pow((double)lena, 1.585);
        for (size_t n = 0; n<tests; n++)
        {   a[lena-1] &= 0x7fffffffffffffffU;
            b[lena-1] &= 0x7fffffffffffffffU;
            for (size_t m=0; m<5000; m++)
                mpn_mul((mp_ptr)c, (mp_srcptr)a, lena, (mp_srcptr)b, lenb);
            for (size_t i=0; i<lena+lenb; i++)
                gmp_check = gmp_check*MULT + c[i];
            for (size_t i=0; i<lena; i++)
                a[i] = MULT*a[i] + ADD;
            for (size_t i=0; i<lenb; i++)
                b[i] = MULT*b[i] + ADD;
        }
        clock_t cl1 = clock();
        double t = (cl1-cl0)/(double)CLOCKS_PER_SEC;
        gmp[trial] = t;
        std::cout << ":";
        std::cout.flush();
    }

// Display the checksum output. Note that this also ensures that the
// result of the multiplication (well more pedantically the result of
// the last of 5000 multiplications!) is used so clever optimizing
// compilers are not allowed to avoid computing it!
    std::cout << std::endl;
    std::cout << (my_check == gmp_check ? "checksums match" :
                                          "checksums disagree") << std::endl;
    std::cout << std::hex << "my checksum:  " << my_check << std::endl; 
    std::cout             << "gmp checksum: " << gmp_check << std::endl;
    std::cout << std::dec;
    std::cout << "Times are reported in microseconds per multiplication"
              << std::endl;
    std::cout << std::setw(10) << "length"
              << std::setw(10) << "my time"
              << std::setw(10) << "gmp time"
              << std::setw(10) << "  ratio mine/gmp"
              << std::fixed << std::setprecision(3)
               << std::endl;
// In the following table times are reported in microseconds per
// multiplication. The ratio is > 1.0 when my code is slower than gmp.
    for (size_t i=0; i<table_size; i++)
    {   if (size[i] == 0) break;
        std::cout << std::setw(10) << size[i]
                  << std::setw(10) << (1.0e6*mine[i]/testcount[i])
                  << std::setw(10) << (1.0e6*gmp[i]/testcount[i])
                  << std::setw(10) << (mine[i]/gmp[i])
                  << std::endl;
    }


#endif // NO_GMP


#ifdef OLD_CODE
    std::cout << "The timing columns are the times in seconds for 500M/n^2" << std::endl;
    std::cout << "unsigned multiplications, where n is the number of words" << std::endl;
    std::cout << "in each of the two numbers being multiplied." << std::endl;
    std::cout << std::setw(10) << std::fixed << " "
                      << "    " << "Classical"
                      << "    " << "Karatsuba"
                      << "    " << "Classic/Kara"
                      << std::endl;
    for (lena=1; lena<table_size; lena++)
    {   lenb = lena;
        uint64_t s = mersenne_twister();
        reseed(s);
        (void)mersenne_twister();
        clock_t cl0 = clock();
        for (int pass=0; pass<100; pass++)
        {   for (size_t i=0; i<lena; i++)
            {   a[i] = mersenne_twister();
                b[i] = mersenne_twister();
            }
            for (size_t i=0; i<5000000/(lena*lena); i++)
                classical_multiply(a, lena, b, lenb, c);
        }
        clock_t cl1 = clock();
        reseed(s);
        (void)mersenne_twister();
        clock_t cl1a = clock();
        for (int pass=0; pass<100; pass++)
        {   for (size_t i=0; i<lena; i++)
            {   a[i] = mersenne_twister();
                b[i] = mersenne_twister();
            }
            for (size_t i=0; i<5000000/(lena*lena); i++)
                kara(a, lena, b, lenb, c1, kara_workspace);
        }
        clock_t cl2 = clock();
        double t1 = cl1-cl0;
        double t2 = cl2-cl1a;
        std::cout << std::setw(10) << std::fixed << lena
                          << "    " << (t1/(double)CLOCKS_PER_SEC)
                          << "    " << (t2/(double)CLOCKS_PER_SEC)
                          << "    " << (t1/t2)
                          << std::endl;
        reseed(s);
        (void)mersenne_twister();
        bool ok = true;
        for (int pass=0; pass<100; pass++)
        {   for (size_t i=0; i<lena; i++)
            {   a[i] = mersenne_twister();
                b[i] = mersenne_twister();
            }
            classical_multiply(a, lena, b, lenb, c);
            kara(a, lena, b, lenb, c1, kara_workspace);
            ok=true;
            for (size_t i=0; ok && i<lena+lenb; i++)
                if (c[i] != c1[i]) ok = false;
            if (!ok) break;
        }
        if (!ok)
        {   display("a", a, lena);
            display("b", b, lenb);
            display("c ", c,  lenc);
            display("c1", c1, lenc1);
            return 1;
        }
    }
#endif // OLD_CODE

    std::cout << "Finished" << std::endl;
    return 0;
}

#endif // NO_MAIN

// end of kara.cpp

