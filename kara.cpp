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

#include "arithlib.hpp"

using namespace arithlib;



//                       Karatsuba multiplication.
//                       =========================

// It starts to look as if the key versions of multiplication code need
// to have a signature something like
//   void mult(uint64_t *a, size_t lena,
//             uint64_t *b, size_t lenb,
//             uint64_t *c, size_t lenc,
//             uint64_t *work_vector=NULL)
// where lenc >= lena+lenb. The behaviour will be to add a*b into c. If
// lenc > lena+lenb this can involve propagating carry information all
// the way up to lenc.
// For simple multiplication I will just initialize c first. This in fact
// helps (just a fraction) with twos complement treatment! At the top level
// if a and b are both positive I initialize c to zero. If a is positive but
// b is negative I set the low half of c to zero and the high half to -a.
// Similarly for a<0 and b>0. If both a and b are negative I need to
// set the top half of c to -(a+b). Adding in the product then "just works".
//
// Having "multiply and add" means that I can not use the destination as
// workspace in the early stages of my calculation. This means I need a
// longer work-vector. The length of the vector need to be at least 2.25 times
// the length of the shorter input number. The factor of 2 is just because
// that is the amount of space required. The extra bit is because the short
// argument might be an odd length and when one halves it it is necessary
// to round up to the next integer - and this effect can happen at each
// level of recursion. If I only use subdivision on numbers that are at least
// 10 words long the worst expansion is as indicated here.


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

// c += a*b;

inline void classical_multiply(uint64_t *a, size_t lena,
                               uint64_t *b, size_t lenb,
                               uint64_t *r, size_t lenr)
{   uint64_t carry = 0;
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
        carry = add_with_carry(r[i+lenb], hi, carry, r[i+lenb]);
    }
    for (size_t i=lena+lenb; carry!=0 && i<lenr; i++)
        carry = add_with_carry(r[i], carry, r[i]);
}

// The cutoff here is potentially system-dependent, however the exact value
// is not incredibly critical because for quite some range around it the
// classical and Karatsuba methods will have pretty similar costs. The
// value here was chosen after a few tests on x86_64 both using cygwin-64
// and Linux. As a sanity check the corresponding threshold as used by
// gmp was checked - there depending on exactly what computer is used they
// move to the more complicated method at somewhere between 10 and 35 word
// (well their term is "limb") numbers. The value 16 seems reasonably
// consistent to use as a single fixed value.

static const size_t KARATSUBA_CUTOFF = 16;

inline void kara2(uint64_t *a, size_t lena,
                  uint64_t *b, size_t lenb,
                  uint64_t *c, size_t lenc,
                  uint64_t *w);

inline void kara1(uint64_t *a, size_t lena,
                  uint64_t *b, size_t lenb,
                  uint64_t *c, size_t lenc,
                  uint64_t *w)
{   if (lena<KARATSUBA_CUTOFF) classical_multiply(a, lena, b, lenb, c, lenc);
    else
    {   if (lena>lenb && lenb%2==0)
        {   classical_multiply(a, 1, b, lenb, c, lenc);
            a = a + 1;
            lena = lena - 1;
            c = c + 1;
            lenc = lenc - 1;
        }
        kara2(a, lena, b, lenb, c, lenc, w);
    }
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
                  uint64_t *c, size_t lenc,
                  uint64_t *w)
{
// The all the cases that are supported here the next line sets n to a
// suitably rounded up half length.
    size_t n = (lena+1)/2;
    uint64_t c1 = kadd(a, n, a+n, lena-n, w, n);     // a0+a1
    uint64_t c2 = kadd(b, n, b+n, lenb-n, w+n, n);   // b0+b1
    kara1(w, n, w+n, n, c+n, lenc-n, w+2*n);         // (a0+a1)*(b0+b1)
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
    kara1(a, n, b, n, w, 2*n, w+2*n);                // a0*b0
    kadd(w, 2*n, c, lenc);                           // add in at bottom..
    ksub(w, 2*n, c+n, lenc-n);                       // and subtract 1 digit up
    for (size_t i=0; i<lena+lenb-2*n; i++) w[i] = 0;
    kara1(a+n, lena-n, b+n, lenb-n, w, lena+lenb-2*n, w+2*n);
    kadd(w, lena+lenb-2*n, c+2*n, lenc-2*n);         // a1*b1 may be shorter
    ksub(w, lena+lenb-2*n, c+n, lenc-n);
}


// This code is where the main recursion happens. The main complication
// within it is dealing with unbalanced length operands.

inline void kara(uint64_t *a, size_t lena,
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
    {   classical_multiply(a, lena, b, lenb, c, lenc);
        return;
    }
// For equal lengths I can do just one call to Karatsuba.
    if (lena == lenb)
    {   kara1(a, lena, b, lenb, c, lenc, w);
        return;
    }
// If the two inputs are unbalanced in length I will perform multiple
// balanced operations each of which can be handled specially. I will
// try to make each subsidiary multiplication as big as possible.
// This will be lenb rounded up to an even number.
    uint64_t *oc = c;
    size_t olenc = lenc;
    size_t len = lenb + (lenb & 1);
    while (lena>=len)
    {   kara1(a, len, b, lenb, c, lenc, w);
        a += len;
        lena -= len;
        c += len;
        lenc -= len;
    }
    if (lena != 0) kara(b, lenb, a, lena, c, lenc, w);
}

// Finally I can provide the top-level entrypoint that accepts signed
// integers that may not be the same size.

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
    lenr = lena + lenb;
    for (size_t i=0; i<lenr; i++) r[i] = 0;
// If the smaller input is reasonably small I will merely use classical
// multiplication.
    if (lenb < KARATSUBA_CUTOFF)
        classical_multiply(a, lena, b, lenb, r, lenr);
    else
    {   if (lena < lenb)
        {   std::swap(a, b);
            std::swap(lena, lenb);
        }
        push(a); push(b);
        size_t lenw = 2*lenb;
        for (size_t i=lenb; i>8; i=i/2) lenw += 2;
        uint64_t *w = reserve(lenw); // Just enough in worst cases I believe.
        pop(b); pop(a);
        kara(a, lena, b, lenb, r, lenr, w);
        abandon(w);
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
// digit of r and if necessary reduce lenr.
    truncate_positive(r, lenr);
    truncate_negative(r, lenr);
}

//===========================================================================
//===========================================================================
//===========================================================================
//===========================================================================

// Here will be some test code


static const size_t MAX = 1000;

uint64_t a[MAX];
uint64_t b[MAX];
uint64_t c[MAX];
uint64_t c1[MAX];
uint64_t w[MAX];

size_t lena, lenb, lenc, lenc1, lenw;


int main(int argc, char *argv[])
{   std::cout << "Karatsuba tests" << std::endl;
    for (size_t i=0; i<MAX; i++)
    {   a[i] = mersenne_twister();
        b[i] = mersenne_twister();
    }
    for (lena=1; lena<40; lena++)
    {   lenb = lena;
        clock_t cl0 = clock();
        for (size_t i=0; i<200000; i++)
            bigmultiply(a, lena, b, lenb, c, lenc);
        clock_t cl1 = clock();
        for (size_t i=0; i<200000; i++)
            kmultiply(a, lena, b, lenb, c1, lenc1);
        clock_t cl2 = clock();
        double t1 = cl1-cl0;
        double t2 = cl2-cl1;
        if (lena < KARATSUBA_CUTOFF)
        {   t1 = 0.001;
            t2 = 0.001;
        }
        std::cout << std::setw(10) << lena
                          << "    " << (t1/(double)CLOCKS_PER_SEC)
                          << "    " << (t2/(double)CLOCKS_PER_SEC)
                          << "    " << (t1/t2)
                          << std::endl;
        bool ok=(lenc == lenc1);
        for (size_t i=0; ok && i<lenc; i++)
            if (c[i] != c1[i]) ok = false;
        if (!ok)
        {   display("a", a, lena);
            display("b", b, lenb);
            display("c ", c,  lenc);
            display("c1", c1, lenc1);
            return 1;
        }
    }
    std::cout << "Finished" << std::endl;
    return 0;
}

// end of kara.cpp

