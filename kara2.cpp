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

#ifdef LOOK_AT_CODE_FOR_BASIC_OPERATIONS

// I want a version of add_with_carry() that leads to the generation of
// good code, so I have these small and rather stupid functions here so that
// I can compile this stuff into machine code and see what is generated.
// Using g++ on x86_64 and -O3 the expansions of add_with_carry here end up
// jump-free and at least close to as slick as I could manage in hand-crafted
// assembly code. Whew!
// I use volatile variables A, B etc to ensure that the compiler will actually
// perform the operations shown, and to separate compilation of the
// add_with_carry from everything to do with argument passing.

INLINE_VAR volatile uint64_t A=1, B=1, C_IN=1, C_OUT=0, R=0;

void test_add_with_carry1()
{   uint64_t r1;
// The upper case names here are so I can find them easily in generated
// assembly code.
    C_OUT = add_with_carry(A, B, r1);
    R = r1;
}

void test_add_with_carry2()
{   uint64_t r1;
    C_OUT = add_with_carry(A, B, C_IN, r1);
    R = r1;
}

#endif // LOOK_AT_CODE_FOR_BASIC_OPERATIONS



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
//    kadd(a, lena, r, lenc);          // c += a; and return a carry
//    kadd(a, lena, b, lenb, c, lenc); // c := a + b; and return a carry
// and correspondingly for subtraction. Note that these have lenc set on
// input and will propagate carries or extend a result all the way up to
// lenc words even if lena is a lot shorter.
// Sometimes these are used with lena=lenb=lenc and with carry very relevant.
// On other occasions they are used with lena=lenb<lenc and carries can
// propagate as far as lenc but any overflow beyond there is unimportant.

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

uint64_t kadd(uint64_t *a, size_t lena,
              uint64_t *b, size_t lenb,
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

// Now code that multiplies 2-digit numbers together.
// One version treats them as unsigned, the second as signed.

inline void mul2x2(uint64_t a1, uint64_t a0,
                   uint64_t b1, uint64_t b0,
                   uint64_t &r3, uint64_t &r2, uint64_t &r1, uint64_t &r0)
{   uint64_t r1a, r1b, r2a, r2b, r3a;
    multiply64(a0, b0, r1a, r0);
    multiplyadd64(a0, b1, r1a, r2a, r1a);
    multiplyadd64(a1, b0, r1a, r2b, r1);
    multiplyadd64(a1, b1, r2a, r3a, r2a);
    r3a += add_with_carry(r2a, r2b, r2);
    r3 = r3a;
}

inline void mul2x2S(int64_t a1, uint64_t a0,
                    int64_t b1, uint64_t b0,
                    int64_t &r3, uint64_t &r2, uint64_t &r1, uint64_t &r0)
{   uint64_t r1a;
    multiply64(a0, b0, r1a, r0);
    uint64_t r1b, r2a;
    multiplyadd64(a0, (uint64_t)b1, r1a, r2a, r1a);
    uint64_t r2b;
    multiplyadd64((uint64_t)a1, b0, r1a, r2b, r1);
    int64_t r3a;
    signed_multiplyadd64(a1, b1, r2a, r3a, r2a);
    r3a = (int64_t)((uint64_t)r3a + add_with_carry(r2a, r2b, r2a));
// Do the arithmetic in unsigned mode in case of overflow problems.
    if (a1 < 0) r3a = (int64_t)((uint64_t)r3a -
                                subtract_with_borrow(r2a, b0, r2a));
    if (b1 < 0) r3a = (int64_t)((uint64_t)r3a -
                                subtract_with_borrow(r2a, a0, r2a));
    r2 = r2a;
    r3 = r3a;
}

inline void mul3x2(uint64_t a2, uint64_t a1, uint64_t a0,
                   uint64_t b1, uint64_t b0,
                   uint64_t &r4, uint64_t &r3, uint64_t &r2,
                   uint64_t &r1, uint64_t &r0)
{   uint64_t r4a, r3a;
    mul2x2(a1, a0, b1, b0, r3, r2, r1, r0);
    multiplyadd64(a2, b0, r2, r3a, r2);
    uint64_t carry = add_with_carry(r3, r3a, r3);
    multiplyadd64(a2, b1, r3, r4, r3);
    r4 += carry;
}

inline void mul3x3(uint64_t a2, uint64_t a1, uint64_t a0,
                   uint64_t b2, uint64_t b1, uint64_t b0,
                   uint64_t &r5, uint64_t &r4, uint64_t &r3,
                   uint64_t &r2, uint64_t &r1, uint64_t &r0)
{   uint64_t r4a, r3a;
    mul2x2(a1, a0, b1, b0, r3, r2, r1, r0);
    multiplyadd64(a2, b0, r2, r3a, r2);
    uint64_t carry = add_with_carry(r3, r3a, r3);
    multiplyadd64(a0, b2, r2, r3a, r2);
    carry += add_with_carry(r3, r3a, r3);
    multiplyadd64(a2, b1, r3, r4, r3);
    carry = add_with_carry(r4, carry, r4);
    multiplyadd64(a1, b2, r3, r4a, r3);
    carry = add_with_carry(r4, r4a, r4);
    multiplyadd64((int64_t)a2, (int64_t)b2, r4, r5, r4);
    r5 = (int64_t)((uint64_t)r5 + carry);
}

inline void mul3x3S(uint64_t a2, uint64_t a1, uint64_t a0,
                    uint64_t b2, uint64_t b1, uint64_t b0,
                    int64_t &r5, uint64_t &r4, uint64_t &r3,
                    uint64_t &r2, uint64_t &r1, uint64_t &r0)
{   uint64_t r4a, r3a;
    mul2x2(a1, a0, b1, b0, r3, r2, r1, r0);
    multiplyadd64(a2, b0, r2, r3a, r2);
    uint64_t carry = add_with_carry(r3, r3a, r3);
    multiplyadd64(a0, b2, r2, r3a, r2);
    carry += add_with_carry(r3, r3a, r3);
    multiplyadd64(a2, b1, r3, r4, r3);
    carry = add_with_carry(r4, carry, r4);
    multiplyadd64(a1, b2, r3, r4a, r3);
    carry = add_with_carry(r4, r4a, r4);
    signed_multiplyadd64((int64_t)a2, (int64_t)b2, r4, r5, r4);
    r5 = (int64_t)((uint64_t)r5 + carry);
    if (negative(b2))
    {   uint64_t borrow = subtract_with_borrow(r3, a0, r3);
        borrow = subtract_with_borrow(r4, a1, borrow, r4);
        r5 = (int64_t)((uint64_t)r5 - borrow);
    }
    if (negative(a2))
    {   uint64_t borrow = subtract_with_borrow(r3, b0, r3);
        borrow = subtract_with_borrow(r4, b1, borrow, r4);
        r5 = (int64_t)((uint64_t)r5 - borrow);
    }
}

void mul4x4(uint64_t a3, uint64_t a2, uint64_t a1, uint64_t a0,
            uint64_t b3, uint64_t b2, uint64_t b1, uint64_t b0,
            uint64_t &c7, uint64_t &c6, uint64_t &c5, uint64_t &c4,
            uint64_t &c3, uint64_t &c2, uint64_t &c1, uint64_t &c0)
{   uint64_t w7, w6, w5a, w5b, w5c, w4a, w4b, w4c,
             w3a, w3b, w3c, w2a, w2b, w2c;
    mul2x2(a1, a0, b1, b0, w3a, w2a, c1, c0);
    mul2x2(a1, a0, b3, b2, w5a, w4a, w3b, w2b);
    mul2x2(a3, a2, b1, b0, w5b, w4b, w3c, w2c);
    mul2x2(a3, a2, b3, b2, w7, w6, w5c, w4c);
    uint64_t carry = add_with_carry(w2a, w2b, w2c, c2);
    carry = add_with_carry(w3a, w3b, w3c, carry, c3);
    carry = add_with_carry(w4a, w4b, w4c, carry, c4);
    carry = add_with_carry(w5a, w5b, w5c, carry, c5);
    carry = add_with_carry(w6, carry, c6);
    c7 = w7 + carry; 
}

// c = a*b;

inline void classical_multiply(uint64_t *a, size_t lena,
                               uint64_t *b, size_t lenb,
                               uint64_t *c)
{
// Here I experimented with working with 2 or 3 digits at a time -
// in effect unrolling the loops and rearranging the sequence of memory
// accesses in case I could speed things up. With g++ on x86_64 the
// changes hurt rather than benefitted me. So this has dropped back
// to simple code. Note that this should work with lena==1 or lenb==1 so
// I will not need top make those special cases.
    uint64_t hi=0, lo;
    for (size_t j=0; j<lenb; j++)
        multiplyadd64(a[0], b[j], hi, hi, c[j]);
    c[lenb] = hi;
    for (size_t i=1; i<lena; i++)
    {   hi = 0;
        for (size_t j=0; j<lenb; j++)
        {   multiplyadd64(a[i], b[j], hi, hi, lo);
            hi += add_with_carry(lo, c[i+j], c[i+j]);
        }
        c[i+lenb] = hi;
    }
}

// c = c + a*b. Potentially carry all the way up to lenc.

inline void classical_multiply_and_add(uint64_t *a, size_t lena,
                                       uint64_t *b, size_t lenb,
                                       uint64_t *c, size_t lenc)
{   uint64_t hi=0, lo, carry=0;
    for (size_t i=0; i<lena; i++)
    {   hi = 0;
        for (size_t j=0; j<lenb; j++)
        {   multiplyadd64(a[i], b[j], hi, hi, lo);
            hi += add_with_carry(lo, c[i+j], c[i+j]);
        }
        carry = add_with_carry(hi, c[i+lenb], carry, c[i+lenb]);
    }
    for (size_t i=lena+lenb; carry!=0 && i<lenc; i++)
        carry = add_with_carry(c[i], carry, c[i]);
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
                                           uint64_t *c, size_t lenc)
{   uint64_t hi=0, lo, carry=0;
    for (size_t j=0; j<lenb; j++)
    {   multiplyadd64(a, b[j], hi, hi, lo);
        hi += add_with_carry(lo, c[j], c[j]);
    }
    carry = add_with_carry(hi, c[lenb], carry, c[lenb]);
    for (size_t i=lenb+1; carry!=0 && i<lenc; i++)
        carry = add_with_carry(c[i], carry, c[i]);
}

// The cutoff here is potentially system-dependent, however the exact value
// is not incredibly critical because for quite some range around it the
// classical and Karatsuba methods will have pretty similar costs. The
// value here was chosen after a few tests on x86_64 both using cygwin-64
// and Linux. As a sanity check the corresponding threshold as used by
// gmp was checked - there depending on exactly what computer is used they
// move to the more complicated method at somewhere between 10 and 35 word
// (well their term is "limb") numbers.

#ifndef K
// I provide a default here but can override it at compile time.
#define K 18
#endif

INLINE_VAR size_t KARATSUBA_CUTOFF = K;

inline void kara_and_add2(uint64_t *a, size_t lena,
                  uint64_t *b, size_t lenb,
                  uint64_t *c, size_t lenc,
                  uint64_t *w);

inline void kara_and_add1(uint64_t *a, size_t lena,
                  uint64_t *b, size_t lenb,
                  uint64_t *c, size_t lenc,             
                  uint64_t *w)
{   if (lena<KARATSUBA_CUTOFF)
        classical_multiply_and_add(a, lena, b, lenb, c, lenc);
    else
    {   if (lena>lenb && lenb%2==0)
        {   kara_and_add2(a, lenb, b, lenb, c, lenc, w);
            classical_multiply_and_add(a[lena-1], b, lenb, c+lenb, lenc-lenb);
        }
        else kara_and_add2(a, lena, b, lenb, c, lenc, w);
    }
}

inline void kara1(uint64_t *a, size_t lena,
                  uint64_t *b, size_t lenb,
                  uint64_t *c, uint64_t *w);

inline void kara_and_add2(uint64_t *a, size_t lena,
                  uint64_t *b, size_t lenb,
                  uint64_t *c, size_t lenc,
                  uint64_t *w)
{
// The all the cases that are supported here the next line sets n to a
// suitably rounded up half length.
    size_t n = (lena+1)/2;
    uint64_t c1 = kadd(a, n, a+n, lena-n, w, n);     // a0+a1
    uint64_t c2 = kadd(b, n, b+n, lenb-n, w+n, n);   // b0+b1
    kara_and_add1(w, n, w+n, n, c+n, lenc-n, w+2*n); // (a0+a1)*(b0+b1)
    if (c1 != 0)
    {   kadd(w+n, n, c+2*n, lenc-2*n);               // fix for overflow
        if (c2 != 0)
        {   kadd(w, n, c+2*n, lenc-2*n);             // fix for overflow
            for (size_t i=3*n; c2!=0 && i<lenc; i++)
                c2 = add_with_carry(c[i], c2, c[i]);
        }
    }
    else if (c2 != 0) kadd(w, n, c+2*n, lenc-2*n);   // fix for overflow
    assert(c[lena+lenb] == 0x5555555555555555);
    for (size_t i=0; i<2*n+1; i++) w[i] = 0x5555555555555555;
    assert(w[lena+lenb] == 0x5555555555555555);
    kara1(a, n, b, n, w, w+2*n+1);                // a0*b0
    assert(w[lena+lenb] == 0x5555555555555555);
    kadd(w, 2*n, c, lenc);                           // add in at bottom..
    ksub(w, 2*n, c+n, lenc-n);                       // and subtract 1 digit up
    for (size_t i=0; i<lena+lenb-2*n+1; i++) w[i] = 0x5555555555555555;
    assert(w[lena+lenb-2*n] == 0x5555555555555555);
    kara1(a+n, lena-n, b+n, lenb-n, w, w+2*n+1);
    assert(w[lena+lenb-2*n] == 0x5555555555555555);
    kadd(w, lena+lenb-2*n, c+2*n, lenc-2*n);         // a1*b1 may be shorter
    ksub(w, lena+lenb-2*n, c+n, lenc-n);
}


// This code is where the main recursion happens. The main complication
// within it is dealing with unbalanced length operands.

inline void kara_and_add(uint64_t *a, size_t lena,
                 uint64_t *b, size_t lenb,
                 uint64_t *c, size_t lenc,
                 uint64_t *w)
{   if (lena < lenb)
    {   std::swap(a, b);
        std::swap(lena, lenb);
    }
// Now b is the shorter operand. If it is not too big I can just do
// simple classical long multiplication.
    if (lenb < KARATSUBA_CUTOFF)
    {   classical_multiply_and_add(a, lena, b, lenb, c, lenc);
        return;
    }
// For equal lengths I can do just one call to Karatsuba.
    if (lena == lenb)
    {   kara_and_add1(a, lena, b, lenb, c, lenc, w);
        return;
    }
// If the two inputs are unbalanced in length I will perform multiple
// balanced operations each of which can be handled specially. I will
// try to make each subsidiary multiplication as big as possible.
// This will be lenb rounded up to an even number.
    size_t len = lenb + (lenb & 1);
// Here I am always adding into the existing vector, so there is no
// merit it getting clever with odd and even chunks.
    while (lena>=len)
    {   kara_and_add1(a, len, b, lenb, c, lenc, w);
        a += len;
        lena -= len;
        c += len;
        lenc -= len;
    }
    if (lena != 0) kara_and_add(b, lenb, a, lena, c, lenc, w);
}

inline void kara2(uint64_t *a, size_t lena,
                  uint64_t *b, size_t lenb,
                  uint64_t *c, uint64_t *w);

inline void kara1(uint64_t *a, size_t lena,
                  uint64_t *b, size_t lenb,
                  uint64_t *c, uint64_t *w)
{   assert(c[lena+lenb] == 0x5555555555555555);
    if (lena<KARATSUBA_CUTOFF) classical_multiply(a, lena, b, lenb, c);
    else if (lena>lenb && lenb%2==0)
    {
// In the case that the product is (2n+1)*(2n) I will not want to round up to
// a (2n+1)*(2n+1) Karatsuba because then the numbers do not split neatly, and
// I could end up padding to (2n+2)*(2n+2). So I do a (2n)*(2n) fast multiply
// and then deal with the "+1" classically.
        assert(c[lena+lenb] == 0x5555555555555555);
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
    assert(c[lena+lenb] == 0x5555555555555555);
    size_t n = (lena+1)/2;
    uint64_t c1 = kadd(a, n, a+n, lena-n, w, n);     // a0+a1
    uint64_t c2 = kadd(b, n, b+n, lenb-n, w+n, n);   // b0+b1
    kara1(w, n, w+n, n, c+n, w+2*n);                 // (a0+a1)*(b0+b1)
    assert(c[lena+lenb] == 0x5555555555555555);
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
    assert(c[lena+lenb] == 0x5555555555555555);
// Now form the product of the high halves of a and b. This can be shorter
// than 2*n.
    kara1(a+n, lena-n, b+n, lenb-n, w, w+2*n);
    assert(c[lena+lenb] == 0x5555555555555555);
// The top half of the product (less just one word!) can be copied into
// previously unused space in c...
    for (size_t i=n+1; i<lena+lenb-2*n; i++) c[2*n+i] = w[i];
    assert(c[lena+lenb] == 0x5555555555555555);
// Next I subtract the high product off. This is not liable to underflow
// (and in doing so call for a long chain of borrows) because it is basically
// subtracting a value from 2^64 times the same value.
    ksub(w, lena+lenb-2*n, c+n, lenc-n);
    assert(c[lena+lenb] == 0x5555555555555555);
// Now add in the bottom part.
    kadd(w, n+1, c+2*n, lenc-2*n);
// I now need to do something similar with the product of the low parts of
// a and b.
    kara1(a, n, b, n, w, w+2*n);                     // a0*b0
    assert(c[lena+lenb] == 0x5555555555555555);
// I can just copy the low n words across.
    for (size_t i=0; i<n; i++) c[i] = w[i];
    assert(c[lena+lenb] == 0x5555555555555555);
    kadd(w+n, n, c+n, lenc-n);                       // add in at bottom..
    ksub(w, 2*n, c+n, lenc-n);                       // and subtract 1 digit up
    assert(c[lena+lenb] == 0x5555555555555555);
}


// This code is where the main recursion happens. The main complication
// within it is dealing with unbalanced length operands.

inline void kara(uint64_t *a, size_t lena,
                 uint64_t *b, size_t lenb,
                 uint64_t *c, uint64_t *w)
{   if (lena < KARATSUBA_CUTOFF || lenb < KARATSUBA_CUTOFF)
    {   //if (lena==1) classical_multiply(a[0], b, lenb, c);
        //else if (lenb==1) classical_multiply(b[0], a, lena, c);
        //else
        classical_multiply(a, lena, b, lenb, c);
        return;
    }
// For equal lengths I can do just one call to Karatsuba.
    assert(c[lena+lenb] == 0x5555555555555555);
    if (lena == lenb)
    {   kara2(a, lena, b, lenb, c, w);
        assert(c[lena+lenb] == 0x5555555555555555);
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
    {   assert(c[lena+lenb] == 0x5555555555555555);
        kara2(a, lena, b, lenb, c, w);
        assert(c[lena+lenb] == 0x5555555555555555);
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
// Multiply-and-add will be (slightly) more expensive than just Multiply,
// so I do a sequence of multiplications where their outputs will not overlap
// first.
    for (;;)
    {
        assert(c[lena+lenb] == 0x5555555555555555);
        kara1(a1, len, b, lenb, c1, w);
        assert(c[lena+lenb] == 0x5555555555555555);
        c1 += 2*len;
// I will keep going provided the next multiplication I will do will fully fit.
        if (lena1 < 3*len) break;
// I may have rounded the size of b up by 1, and if I have I would generate
// 2*len-1 digits not 2*len and hence risk leaving a 1-word gap between filled
// in data. I zero that here to avoid trouble. However I must not do this
// if lenb was not rounded up because that could corrupt memory beyond the
// length of the full result! I also only do this if I am about to do another
// multiplication in this loop, so I do not pad anything at the very end of the
// result. The subscript "-1" here obviously looks a bit odd!
        if (len!=lenb) c1[-1] = 0;
        assert(c[lena+lenb] == 0x5555555555555555);
        a1 += 2*len;
        lena1 -= 2*len;
    }
    if (lena1 > 2*len)
    {   a1 += 2*len;
        lena1 -= 2*len;
// Do a shorter nice Multiply (without Add) to reach the end of input a.
        assert(c[lena+lenb] == 0x5555555555555555);
        kara(a1, lena1, b, lenb, c1, w);
        assert(c[lena+lenb] == 0x5555555555555555);
    }
    else if (lena1!=len)
    {
// I may need to pad with zeros when the top digit to be generated will be
// put there using MultiplyAndAdd.
        assert(c[lena+lenb] == 0x5555555555555555);
        for (size_t i=c1-c; i<lena+lenb; i++) c[i] = 0;
        assert(c[lena+lenb] == 0x5555555555555555);
    }
    assert(c[lena+lenb] == 0x5555555555555555);
// Now I need to do much the same for the odd numbered digits of a, but
// adding the products in rather than writing them into place.
    a1 = a + len;
    c1 = c + len;
    size_t lenc1 = lena+lenb-len;
// I know that I had lena>lenb at the start. This means I have at
// least a part block to process here, but there is no guarantee that
// I have a whole one.
    lena1 = lena - len;
    for (;;)
    {   if (lena1 < len) break;
        assert(c[lena+lenb] == 0x5555555555555555);
        kara_and_add1(a1, len, b, lenb, c1, lenc1, w);
        assert(c[lena+lenb] == 0x5555555555555555);
        if (lena1 <= 2*len)
        {   lena1 = 0;
            break;
        }
        c1 += 2*len;
        lenc1 -= 2*len;
        a1 += 2*len;
        lena1 -= 2*len;
    }
    assert(c[lena+lenb] == 0x5555555555555555);
    if (lena1!=0) kara_and_add(a1, lena1, b, lenb, c1, lenc1, w);
    assert(c[lena+lenb] == 0x5555555555555555);
}

// Finally I can provide the top-level entrypoint that accepts signed
// integers that may not be the same size.

INLINE_VAR const size_t KARA_FIXED_WORKSPACE_SIZE = 200;
INLINE_VAR const size_t KARA_WORKSPACE_SIZE = 408;
INLINE_VAR uint64_t kara_workspace[KARA_WORKSPACE_SIZE];

inline constexpr int BY(int m, int n)
{   return m + 4*n;
}

inline void kmultiply(uint64_t *a, size_t lena,
                      uint64_t *b, size_t lenb,
                      uint64_t *c, size_t &lenc)
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
// If both arguments are tiny I write out the code in-line. The timings I
// have taken suggest that this makes a significant difference to costs, and
// I view it as plausible that "rather small" cases will often dominate.
    assert(c[lena+lenb] == 0x5555555555555555);
    if (lena <=4 && lenb <= 4) switch (lena + 4*lenb)
    {
// Length 2 result
    case BY(1, 1):
        {   int64_t r1;
            uint64_t r0;
            signed_multiply64(a[0], b[0], r1, r0);
            c[0] = r0;
            if (shrinkable(r1, r0)) lenc = 1;
            else
            {   c[1] = (uint64_t)r1;
                lenc = 2;
            }
            return;
        }

// Length 3 result
    case BY(1, 2):
        std::swap(a, b);
        // drop through.
    case BY(2, 1):
        {   if (b[0]==0)
            {   c[0] = 0;
                lenc = 1;
                return;
            }
            int64_t r2;
            uint64_t r1, r0;
            multiply64(b[0], a[0], r1, r0);
            c[0] = r0;
            signed_multiplyadd64(b[0], a[1], r1, r2, r1);
            if (negative(b[0]))
                r2 = (int64_t)((uint64_t)r2 -
                                subtract_with_borrow(r1, a[0], r1));
            c[1] = r1;
// If you have an input like 2^63 it will be represented as a 2-word
// bignum {0,2^63} so that the most significant bit of the most significant
// digits is 0 to show that it is positive. If you multiply two such numbers
// the direct result you get is {0, 0, 2^62, 0} and the top non-zero digit
// now does not have its top bit set. So TWO words can be trimmed from the
// top. This issue may not arise in the length-1 by length-2 case here, but
// I leave the test in to feel safe and tidy.
            if (shrinkable(r2, r1))
            {   if (shrinkable(r1, r0)) lenc = 1;
                else lenc = 2;
            }
            else
            {   c[2] = (uint64_t)r2;
                lenc = 3;
            }
            return;
        }

// Length 4 result
    case BY(2, 2):
        {   int64_t r3;
            uint64_t r2, r1;
            mul2x2S((int64_t)a[1], a[0],
                    (int64_t)b[1], b[0],
                    r3, r2, r1, c[0]);
            c[1] = r1;
            c[2] = r2;
            if (shrinkable(r3, r2))
            {   if (shrinkable(r2, r1)) lenc = 2;
                else lenc = 3;
            }
            else
            {   c[3] = (uint64_t)r3;
                lenc = 4;
            }
            return;
        }

    case BY(1, 3):
        std::swap(a, b);
        // drop through.
    case BY(3, 1):
        {   if (b[0]==0)
            {   c[0] = 0;
                lenc = 1;
                return;
            }
            int64_t r3;
            uint64_t r2, r1;
            multiply64(b[0], a[0], r1, c[0]);
            multiplyadd64(b[0], a[1], r1, r2, r1);
            signed_multiplyadd64(b[0], a[2], r2, r3, r2);
            if (negative(b[0]))
            {   uint64_t borrow = subtract_with_borrow(r1, a[0], r1);
                borrow = subtract_with_borrow(r2, a[1], borrow, r2);
                r3 = (int64_t)((uint64_t)r3 - borrow);
            }
            c[1] = r1;
            c[2] = r2;
            if (shrinkable(r3, r2))
            {   if (shrinkable(r2, r1)) lenc = 2;
                else lenc = 3;
            }
            else
            {   c[3] = (uint64_t)r3;
                lenc = 4;
            }
            return;
        }

// Length 5 result
    case BY(1, 4):
        std::swap(a, b);
    case BY(4, 1):
        {   if (b[0]==0)
            {   c[0] = 0;
                lenc = 1;
                return;
            }
            int64_t r4;
            uint64_t r3, r2, r1;
            multiply64(b[0], a[0], r1, c[0]);
            multiplyadd64(b[0], a[1], r1, r2, r1);
            multiplyadd64(b[0], a[2], r2, r3, r2);
            signed_multiplyadd64(b[0], a[3], r3, r4, r3);
            if (negative(b[0]))
            {   uint64_t borrow = subtract_with_borrow(r1, a[0], r1);
                borrow = subtract_with_borrow(r2, a[1], borrow, r2);
                borrow = subtract_with_borrow(r3, a[2], borrow, r3);
                r4 = (int64_t)((uint64_t)r4 - borrow);
            }
            c[1] = r1;
            c[2] = r2;
            c[3] = r3;
            if (shrinkable(r4, r3))
            {   if (shrinkable(r3, r2)) lenc = 3;
                else lenc = 4;
            }
            else
            {   c[4] = (uint64_t)r4;
                lenc = 5;
            }
            return;
        }

    case BY(2, 3):
        std::swap(a, b);
    case BY(3, 2):
        {   int64_t r4;
            uint64_t r3, r3a, r2;
            mul2x2(a[1], a[0], b[1], b[0],
                   r3, r2, c[1], c[0]);
            multiplyadd64(a[2], b[0], r2, r3a, r2);
            uint64_t carry = add_with_carry(r3, r3a, r3);
            signed_multiplyadd64((int64_t)a[2], (int64_t)b[1], r3, r4, r3);
            r4 = (int64_t)((uint64_t)r4 + carry);
            if (negative(b[1]))
            {   uint64_t borrow = subtract_with_borrow(r2, a[0], r2);
                borrow = subtract_with_borrow(r3, a[1], borrow, r3);
                r4 = (int64_t)((uint64_t)r4 - borrow);
            }
            if (negative(a[2]))
            {   uint64_t borrow = subtract_with_borrow(r3, b[0], r3);
                r4 = (int64_t)((uint64_t)r4 - borrow);
            }
            c[2] = r2;
            c[3] = r3;
            if (shrinkable(r4, r3))
            {   if (shrinkable(r3, r2)) lenc = 3;
                else lenc = 4;
            }
            else
            {   c[4] = (uint64_t)r4;
                lenc = 5;
            }
            return;
        }

// Length 6 results
    case BY(2, 4):
    case BY(4, 2):
// I do not have to implement all the cases here - any that I either choose
// not to or have not got around to can merely go "break;" and join in the
// generic path.
        break;

    case BY(3, 3):
        {   int64_t r5;
            uint64_t r4, r3, r2, r1;
            mul3x3S(a[2], a[1], a[0], b[2], b[1], b[0],
                    r5, r4, r3, r2, r1, c[0]);
            c[1] = r1;
            c[2] = r2;
            c[3] = r3;
            c[4] = r4;
            if (shrinkable(r5, r4))
            {   if (shrinkable(r4, r3)) lenc = 4;
                else lenc = 5;
            }
            else
            {   c[5] = (uint64_t)r5;
                lenc = 6;
            }
            return;
        }

// Length 7 results
    case BY(3, 4):
    case BY(4, 3):
// As above, cases that have not been coded here do not cause failure.
        break;


// Length 8 result
    case BY(4, 4):
#ifdef OLD
        {   int64_t r7;
            uint64_t r6, r5, r5a, r4, r4a, r3, r3a, r2, r2a;
            mul2x2(a[1], a[0], b[1], b[0], r3, r2, c[1], c[0]);
            mul2x2(a[3], a[2], b[1], b[0], r5, r4, r3a, r2a);
            uint64_t carry =
                add_with_carry(r3, r3a, add_with_carry(r2, r2a, r2), r3);
            mul2x2(a[1], a[0], b[3], b[2], r5a, r4a, r3a, r2a);
            carry += add_with_carry(r3, r3a, add_with_carry(r2, r2a, r2), r3);
            c[2] = r2;
            c[3] = r3;
            uint64_t carry1 =
                add_with_carry(r5, r5a, add_with_carry(r4, r4a, r4), r5);
            carry1 += add_with_carry(r5, add_with_carry(r4, carry, r4), r5);
            mul2x2S((int64_t)a[3], a[2], (int64_t)b[3], b[2],
                    r7, r6, r5a, r4a);
            carry1 +=
                add_with_carry(r5, r5a, add_with_carry(r4, r4a, r4), r5);
            r7 = (int64_t)((uint64_t)r7 +  add_with_carry(r6, carry1, r6));
#else
        {   int64_t r7;
            uint64_t r6, r5, r4, r3, r2;
            mul4x4(a[3], a[2], a[1], a[0],
                   b[3], b[2], b[1], b[0],
                   r7, r6, r5, r4, c[3], c[2], c[1], c[0]);
#endif
            if (negative(a[3]))
            {   uint64_t borrow = subtract_with_borrow(r4, b[0], r4);
                borrow = subtract_with_borrow(r5, b[1], borrow, r5);
                borrow = subtract_with_borrow(r6, borrow, r6);
                r7 = (int64_t)((uint64_t)r7 - borrow);
            }
            if (negative(b[3]))
            {   uint64_t borrow = subtract_with_borrow(r4, a[0], r4);
                borrow = subtract_with_borrow(r5, a[1], borrow, r5);
                borrow = subtract_with_borrow(r6, borrow, r6);
                r7 = (int64_t)((uint64_t)r7 - borrow);
            }
            c[4] = r4;
            c[5] = r5;
            c[6] = r6;
            if (shrinkable(r7, r6))
            {   if (shrinkable(r6, r5)) lenc = 6;
                else lenc = 7;
            }
            else
            {   c[7] = (uint64_t)r7;
                lenc = 8;
            }
            return;
        }

    default:
// The default label should never be activated!
        ;
    }

// If the smaller input is reasonably small I will merely use classical
// multiplication.
    if (lena < KARATSUBA_CUTOFF ||
        lenb < KARATSUBA_CUTOFF)
    {
// I have to make lena==1 and lenb==1 special cases because a special
// situation within those case is multiplication by zero.
        if (lena==1)
        {   if (a[0]==0)
            {   c[0] = 0;
                lenc = 1;
                return;
            }
            else classical_multiply(a[0], b, lenb, c);
        }
        else if (lenb==1)
        {   if (b[0]==0)
            {   c[0] = 0;
                lenc = 1;
                return;
            }
            else classical_multiply(b[0], a, lena, c);
        }
        else classical_multiply(a, lena, b, lenb, c);
    }
    else
    {
// For many smaller cases I will just use some static pre-allocated workspace
// and hence avoid potential storage management overheads.
        if (lena <= KARA_FIXED_WORKSPACE_SIZE ||
            lenb <= KARA_FIXED_WORKSPACE_SIZE)
        {   assert(c[lena+lenb] == 0x5555555555555555);
            kara(a, lena, b, lenb, c, kara_workspace);
        }
        else
        {   push(a); push(b);
            size_t lenw;
            if (lena < lenb) lenw = lena;
            else lenw = lenb;
            for (size_t i=lenw; i>8; i=i/2) lenw++;
            uint64_t *w = reserve(2*lenw);
            pop(b); pop(a);
            assert(c[lena+lenb] == 0x5555555555555555);
            kara(a, lena, b, lenb, c, w);
            abandon(w);
        }
    }
// Now adapt for the situation where one or both of the inputs had been
// negative.
    if (negative(a[lena-1]))
    {   uint64_t carry = 1;
        for (size_t i=0; i<lenb; i++)
            carry = add_with_carry(c[i+lena], ~b[i], carry, c[i+lena]);
    }
    if (negative(b[lenb-1]))
    {   uint64_t carry = 1;
        for (size_t i=0; i<lena; i++)
            carry = add_with_carry(c[i+lenb], ~a[i], carry, c[i+lenb]);
    }
// The actual value may be 1 or 2 words shorter than this. So test the top
// digit of c and if necessary reduce lenc.
// Also note that the pending  result is at least of length 2 here because
// various small cases had been processed in-line earlier.
// eg {0, 0x8000000000000000} times itself is {0, 0, 0x4000000000000000, 0}
// and both leading zeros can be trimmed.
    lenc = lena + lenb;
    if (shrinkable(c[lenc-1], c[lenc-2]))
    {   lenc--;
        if (shrinkable(c[lenc-1], c[lenc-2])) lenc--;
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
// thoroughly. It is used so that its results can be compared against the
// ones that my more complicated code produce.

inline void referencemultiply(const uint64_t *a, size_t lena,
                             const uint64_t *b, size_t lenb,
                             uint64_t *c, size_t &lenc)
{   for (size_t i=0; i<lena+lenb; i++) c[i] = 0;
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
            hi += add_with_carry(lo, c[i+j], c[i+j]);
        }
        c[i+lenb] = hi;
    }
    if (negative(a[lena-1]))
    {   uint64_t carry = 1;
        for (size_t i=0; i<lenb; i++)
            carry = add_with_carry(c[i+lena], ~b[i], carry, c[i+lena]);
    }
    if (negative(b[lenb-1]))
    {   uint64_t carry = 1;
        for (size_t i=0; i<lena; i++)
            carry = add_with_carry(c[i+lenb], ~a[i], carry, c[i+lenb]);
    }
    lenc = lena + lenb;
// The actual value may be shorter than this.
//  test top digit or c and if necessary reduce lenc.
    truncate_positive(c, lenc);
    truncate_negative(c, lenc);
}



INLINE_VAR const size_t MAX = 3000;

INLINE_VAR uint64_t a[MAX];
INLINE_VAR uint64_t b[MAX];
INLINE_VAR uint64_t c[2*MAX];
INLINE_VAR uint64_t c1[2*MAX];
INLINE_VAR uint64_t w[3*MAX];

INLINE_VAR size_t lena, lenb, lenc, lenc1, lenw;

// The following are parameters for a plausible linear congruential generator.
INLINE_VAR const uint64_t MULT = 6364136223846793005U;
INLINE_VAR const uint64_t ADD  = 1442695040888963407U;

// This program is invoked as
//   ./kara2   -k=N   M
// where N is the transition level between classical and Karatsuba
// multiplication and M is a seed for the random number generator.

int main(int argc, char *argv[])
{   uint64_t seed = 0;
    int LEN = 500;
    int N = 30;
    std::cout << argv[0] << ": [options]" << std::endl;
    std::cout << "   -k={N:" << K << "}    Transition to use Karatsuba" << std::endl;
    std::cout << "   -l={N:500}   For gmp comparison test up to N (64-bit word) case" << std::endl;
    std::cout << "   -n={N:30}    For gmp comparison controls the number of tests per run" << std::endl;
    std::cout << "   {N:<random>} Initial seed for random number generator" << std::endl;
    std::cout << std::endl;
    for (int i=1; i<argc; i++)
    {   char *a = argv[i];
        if (std::strncmp(a, "-k=", 3) == 0)      KARATSUBA_CUTOFF=std::atoi(a+3);
        else if (std::strncmp(a, "-k", 2) == 0)  KARATSUBA_CUTOFF=std::atoi(a+2);
        else if (std::strncmp(a, "-l=", 3) == 0) LEN=std::atoi(a+3);
        else if (std::strncmp(a, "-l", 2) == 0)  LEN=std::atoi(a+2);
        else if (std::strncmp(a, "-n=", 3) == 0) N=std::atoi(a+3);
        else if (std::strncmp(a, "-n", 2) == 0)  N=std::atoi(a+2);
        else                                     seed=std::atoi(a);
    }
// Impose limits on the various input parameters.
    if (KARATSUBA_CUTOFF < 4) KARATSUBA_CUTOFF = 4;
    if (LEN < 20) LEN = 20;
    else if (LEN > 10000) LEN = 10000;
    if (N < 5) N = 5;
    else if (N > 1000) N = 1000;
    if (seed == 0) seed = mersenne_twister() & 0xfffff;
    reseed(seed);
    std::cout << "Karatsuba tests "
              << " cutoff=" << KARATSUBA_CUTOFF
              << "  LEN = " << LEN << "  N = " << N
              << " seed = " << seed << std::endl;

#ifndef NO_CORRECTNESS

// This generates some random data and then calls the multiplication code
// to multiply each m*n prefix of it. It compares the results from the
// new code here against those from  "referencemultiply".

    std::cout << "Correctness test" << std::endl;
    for (int run=0; run<100; run++)
    {   for (size_t i=0; i<MAX; i++)
        {   a[i] = mersenne_twister();
            b[i] = mersenne_twister();
        }
        uint64_t toplen = mersenne_twister();
        int count = 0;
        int maxlen = 64;
        for (lena=1; lena<maxlen; lena++)
        {   for (lenb=1; lenb<maxlen; lenb++)
            {
// I will fudge the lengths of the top words of the two numbers. This is so
// that my correctness test gets both +ve and -ve numbers and ones with
// from 1 to 63 bits in use in the top word of the bignum, and hence it
// tests cases where the result of multiplying numbers of length lena and
// lenb is lena+lenb or lena+lenb-1.
                int s1 = toplen%64;
                int s2 = (toplen>>6)%64;
                uint64_t savea = a[lena-1], saveb = b[lenb-1];
// Here I am IMPROPERLY assuming that right shifts on signed values are
// arithmetic and hence they propagate the sign bit down.
                a[lena-1] = ((int64_t)a[lena-1])>>s1;
                b[lenb-1] = ((int64_t)b[lenb-1])>>s2;
                if (lena > 1)
                {   if (a[lena-1] == 0)  a[lena-2] |= 0x8000000000000000;
                    if (a[lena-1] == -1) a[lena-2] &= 0x7fffffffffffffff;
                }
                if (lenb > 1)
                {   if (b[lenb-1] == 0)  b[lenb-2] |= 0x8000000000000000;
                    if (b[lenb-1] == -1) b[lenb-2] &= 0x7fffffffffffffff;
                }
                for (size_t i=0; i<lena+lenb+1; i++)
                    c[i] = c1[i] = 0x5555555555555555;
                referencemultiply(a, lena, b, lenb, c, lenc);
                kmultiply(a, lena, b, lenb, c1, lenc1);
                bool ok=(lenc == lenc1 &&
                         c[lena+lenb] == 0x5555555555555555 &&
                         c1[lena+lenb] == 0x5555555555555555);
                for (size_t i=0; ok && i<lenc; i++)
                    if (c[i] != c1[i]) ok = false;
                if (!ok)
                {   std::cout << std::endl;
                    std::cout << "Failed at "
                              << lena << "*" << lenb << std::endl;
                    std::cout << std::hex << "c-end = " << c[lena+lenb] << std::endl;
                    std::cout << std::hex << "c1-end = " << c1[lena+lenb] << std::endl;
                    display("a", a, lena);
                    display("b", b, lenb);
                    display("c ", c,  lenc);
                    display("c1", c1, lenc1);
                    std::cout << "Failed" << std::endl;
                    return 1;
                }
                a[lena-1] = savea;
                b[lenb-1] = saveb;
            }
        }
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
            {
// I make my test inputs positive because I do not want any confusion
// with the cost of the extra subtraction needed when multiplying bt
// a negative value.
                a[lena-1] &= 0x7fffffffffffffff;
                b[lenb-1] &= 0x7fffffffffffffff;
                for (size_t i=0; i<lena+lenb+1; i++) c1[i] = 0x5555555555555555;
                for (size_t m=0; m<500; m++)
                    kmultiply(a, lena, b, lenb, c1, lenc1);
                for (size_t i=0; i<lena; i++)
                    a[i] = (MULT*a[i] + ADD);
                for (size_t i=0; i<lenb; i++)
                    b[i] = (MULT*b[i] + ADD);

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
        {   a[lena-1] &= 0x7fffffffffffffff;
            b[lenb-1] &= 0x7fffffffffffffff;
            for (size_t i=0; i<lena+lenb+1; i++) c1[i] = 0x5555555555555555;
            for (size_t m=0; m<1000; m++)
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

    KARATSUBA_CUTOFF = 14;   // Use (at least close to) optimal value.

    size_t tests;

    reseed(seed);
    lena = 1;
    for (size_t trial=0; trial<table_size; trial++)
    {   lena = (21*lena+19)/20;
        size[trial] = 0;
        if (lena >= LEN) break;
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
        size_t tests = 2+(10000*N)/(int)std::pow((double)lena, 1.585);
        for (size_t n = 0; n<tests; n++)
        {
// The gpm function mpn_mul multiplies unsigned integers, while my
// kmultiply is at a slightly higher level and deals with signed values.
// I want to compare their results, and so forcing all inputs to be positive
// (in my representation) by clearing most significant bits is necessary.
// Note that this will almost always lead to numbers that have bits all the
// way up to the limit and hence where the product is as long as it can be.
// cases where multiplying m*n leads to a result of length m*n-1 will not
// be exercised.
            a[lena-1] &= 0x7fffffffffffffffU;
            b[lena-1] &= 0x7fffffffffffffffU;
// So that all the administration here does not corrupt my measurement
// I do the actual multiplication of each test case 500 times.
            for (size_t i=0; i<lena+lenb+1; i++) c1[i] = 0x5555555555555555;
            for (size_t m=0; m<500; m++)
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
        testcount[trial] = 500*tests;
        mine[trial] = t;
        std::cout << ".";
        std::cout.flush();
    }
    std::cout << std::endl;
// Now do just the same sort of thing but using gmp rather then my
// own multiplication code.
    reseed(seed);
    lena = 1;
    for (size_t trial=0; trial<table_size; trial++)
    {   lena = (21*lena+19)/20;
        if (lena >= LEN) break;
        lenb = lena;
        for (size_t i=0; i<lena; i++)
        {   a[i] = mersenne_twister();
            b[i] = mersenne_twister();
        }
        clock_t cl0 = clock();
// somewhere around lena=1400 the fraction on the next line reduces
// to zero. So for the last few cases I will take distinctly longer
// than for each of the rest.
        size_t tests = 2+(10000*N)/(int)std::pow((double)lena, 1.585);
        for (size_t n = 0; n<tests; n++)
        {   a[lena-1] &= 0x7fffffffffffffffU;
            b[lena-1] &= 0x7fffffffffffffffU;
            for (size_t m=0; m<500; m++)
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
// the last of 500 multiplications!) is used so clever optimizing
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

    std::cout << "Finished" << std::endl;
    return 0;
}

#endif // NO_MAIN

// end of kara.cpp

