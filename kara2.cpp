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

// The OLD option in arithlib.hpp replaces Karatsuba with just
// classical multiplication. I want there here to avoid name clashes and
// also because I want the simplest and most reliable code to compare
// my new stuff with

#define OLD 1
#include "arithlib.hpp"

// Avoid name-clashes...
#define bigmultiply new_bigmultiply
#define KARATSUBA_CUTOFF NEW_KARATSUBA_CUTOFF
#define kadd new_kadd
#define ksub new_ksub

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
//    kadd(a, lena, c, lenc);          // c += a

inline uint64_t kadd(uint64_t *a, size_t lena,
                     uint64_t *c, size_t lenc,
                     uint64_t carry=0)
{   size_t i;
    for (i=0; i<lena; i++)
        carry = add_with_carry(a[i], c[i], carry, c[i]);
    while (carry!=0 && i<lenc)
    {   carry = add_with_carry(c[i], carry, c[i]);
        i++;
    }
    return carry;
}

// c = a - b. In the use I make of
// this I may need to allow for either a or b to be longer than the other.

inline uint64_t ksub(uint64_t *a, size_t lena,
                     uint64_t *b, size_t lenb,
                     uint64_t *c, size_t lenc)
{   size_t shorter = lena < lenb ? lena : lenb;
    uint64_t borrow = 0;
    size_t i;
    for (i=0; i<shorter; i++)
        borrow = subtract_with_borrow(a[i], b[i], borrow, c[i]);
    while (i<lena)
    {   borrow = subtract_with_borrow(a[i], borrow, c[i]);
        i++;
    }
    while (i<lenb)
    {   borrow = subtract_with_borrow(0, a[i], borrow, c[i]);
        i++;
    }
    while (i < lenc) c[i++] = -borrow;
    return borrow;
}

inline void kneg(uint64_t *a, size_t lena)
{   uint64_t carry = 0;
    for (size_t i=0; i<lena; i++)
        a[i] = add_with_carry(~a[i], carry, a[i]);
}

// c = |a - b| and return an indication of which branch of the absolute
// value function was used, ie whether we had a>=b or a<b. If a==b so
// the result is zero the value is not terribly important.

inline bool absdiff(uint64_t *a, size_t lena,
                    uint64_t *b, size_t lenb,
                    uint64_t *c, size_t lenc)
{
// I will do a cheap comparison of a and b first, based on an understanding
// that lena <= lenb;
    if (lena < lenb ||
        a[lena-1]<=b[lenb-1])
    {
// If my cheap test suggests that a is the smaller one then I form (b-a).
// If that generates a borrow my "guess" was wrong, so I negate the
// result. For fully random inputs the cheap test is liable to be reliable.
// for values that have long sequences of 0 bits in their binary
// representation, eg values that are close to a power of 2 or ones that
// have a large power of 2 as a factor, the fallback may be activated
// more frequently.
        if (ksub(b, lenb, a, lena, c, lenc) != 0)
        {   kneg(c, lenc);
            return true;     // Have computed a-b
        }
        else return false;   // have computed b-a
    }
    else
    {   if (ksub(a, lena, b, lenb, c, lenc) != 0)
        {   kneg(c, lenc);
            return false;    // b-a
        }
        else return true;    // a-b
    }
}

// I will have in-line code for a number of very small case on the
// expectation that (a) these will arise expecially often in many
// applications and (b) that the inline code will end up faster
// then general loops.

// Now code that multiplies 2-digit numbers together.
// One version treats them as unsigned, the second as signed.

inline void mul2x2(uint64_t a1, uint64_t a0,
                   uint64_t b1, uint64_t b0,
                   uint64_t &c3, uint64_t &c2, uint64_t &c1, uint64_t &c0)
{   uint64_t c1a, c1b, c2a, c2b, c3a;
    multiply64(a0, b0, c1a, c0);
    multiply64(a0, b1, c1a, c2a, c1a);
    multiply64(a1, b0, c1a, c2b, c1);
    multiply64(a1, b1, c2a, c3a, c2a);
    c3a += add_with_carry(c2a, c2b, c2);
    c3 = c3a;
}

inline void mul2x2S(int64_t a1, uint64_t a0,
                    int64_t b1, uint64_t b0,
                    int64_t &c3, uint64_t &c2, uint64_t &c1, uint64_t &c0)
{   uint64_t c1a;
    multiply64(a0, b0, c1a, c0);
    uint64_t c1b, c2a;
    multiply64(a0, (uint64_t)b1, c1a, c2a, c1a);
    uint64_t c2b;
    multiply64((uint64_t)a1, b0, c1a, c2b, c1);
    int64_t c3a;
    signed_multiply64(a1, b1, c2a, c3a, c2a);
    c3a = (int64_t)((uint64_t)c3a + add_with_carry(c2a, c2b, c2a));
// Do the arithmetic in unsigned mode in case of overflow problems.
    if (a1 < 0) c3a = (int64_t)((uint64_t)c3a -
                                subtract_with_borrow(c2a, b0, c2a));
    if (b1 < 0) c3a = (int64_t)((uint64_t)c3a -
                                subtract_with_borrow(c2a, a0, c2a));
    c2 = c2a;
    c3 = c3a;
}

inline void mul3x2(uint64_t a2, uint64_t a1, uint64_t a0,
                   uint64_t b1, uint64_t b0,
                   uint64_t &c4, uint64_t &c3, uint64_t &c2,
                   uint64_t &c1, uint64_t &c0)
{   uint64_t c4a, c3a;
    mul2x2(a1, a0, b1, b0, c3, c2, c1, c0);
    multiply64(a2, b0, c2, c3a, c2);
    uint64_t carry = add_with_carry(c3, c3a, c3);
    multiply64(a2, b1, c3, c4, c3);
    c4 += carry;
}

inline void mul3x3(uint64_t a2, uint64_t a1, uint64_t a0,
                   uint64_t b2, uint64_t b1, uint64_t b0,
                   uint64_t &c5, uint64_t &c4, uint64_t &c3,
                   uint64_t &c2, uint64_t &c1, uint64_t &c0)
{   uint64_t c4a, c3a;
    mul2x2(a1, a0, b1, b0, c3, c2, c1, c0);
    multiply64(a2, b0, c2, c3a, c2);
    uint64_t carry = add_with_carry(c3, c3a, c3);
    multiply64(a0, b2, c2, c3a, c2);
    carry += add_with_carry(c3, c3a, c3);
    multiply64(a2, b1, c3, c4, c3);
    carry = add_with_carry(c4, carry, c4);
    multiply64(a1, b2, c3, c4a, c3);
    carry = add_with_carry(c4, c4a, c4);
    multiply64((int64_t)a2, (int64_t)b2, c4, c5, c4);
    c5 = (int64_t)((uint64_t)c5 + carry);
}

inline void mul3x3S(uint64_t a2, uint64_t a1, uint64_t a0,
                    uint64_t b2, uint64_t b1, uint64_t b0,
                    int64_t &c5, uint64_t &c4, uint64_t &c3,
                    uint64_t &c2, uint64_t &c1, uint64_t &c0)
{   uint64_t c4a, c3a;
    mul2x2(a1, a0, b1, b0, c3, c2, c1, c0);
    multiply64(a2, b0, c2, c3a, c2);
    uint64_t carry = add_with_carry(c3, c3a, c3);
    multiply64(a0, b2, c2, c3a, c2);
    carry += add_with_carry(c3, c3a, c3);
    multiply64(a2, b1, c3, c4, c3);
    carry = add_with_carry(c4, carry, c4);
    multiply64(a1, b2, c3, c4a, c3);
    carry = add_with_carry(c4, c4a, c4);
    signed_multiply64((int64_t)a2, (int64_t)b2, c4, c5, c4);
    c5 = (int64_t)((uint64_t)c5 + carry);
    if (negative(b2))
    {   uint64_t borrow = subtract_with_borrow(c3, a0, c3);
        borrow = subtract_with_borrow(c4, a1, borrow, c4);
        c5 = (int64_t)((uint64_t)c5 - borrow);
    }
    if (negative(a2))
    {   uint64_t borrow = subtract_with_borrow(c3, b0, c3);
        borrow = subtract_with_borrow(c4, b1, borrow, c4);
        c5 = (int64_t)((uint64_t)c5 - borrow);
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

// There are two versions of the classical multiplication code. One
// computes by scanning the two inputs in the order
//    for i = 0:lena-1 do for j = 0:lenb-1 do c[i+j] += a[i]*b[j]
// while the other arranges to collect all the partial products that
// will go into each position in the output one at a time, in the
// style of   c[i] = sum_{j+k=i} a[j]*b[k]
// Until this code is finished and stable it is not clear to me which
// of these will be faster (or indeed if there will be measurable
// pereformance difference between them.

#ifdef NEWER

// This version, activated via NEWER, forms a product digit by digit.

inline void classical_multiply(uint64_t *a, size_t lena,
                               uint64_t *b, size_t lenb,
                               uint64_t *c)
{   if (lena < lenb)
    {   std::swap(a, b);
        std::swap(lena, lenb);
    }
// (1) do the lowest degree term as a separate step
    uint64_t carry=0, hi, hi1, lo;
    multiply64(b[0], a[0], lo, c[0]);
// Now a sequence of stages where at each the number of terms to
// be combined grows. 
    hi = 0;
    for (int i=1; i<lenb; i++)
    {   carry = 0;
        for (int j=0; j<=i; j++)
        {   multiply64(b[j], a[i-j], lo, hi1, lo);
            carry += add_with_carry(hi, hi1, hi);
        }
        c[i] = lo;
        lo = hi;
        hi = carry;
    }
// If the two inputs are not the same size I demand that lena>=lenb and
// there may be some slices to compute in the middle here.
// if lena==lenb the following loop does not get executed at all.
    for (int i=lenb; i<lena; i++)
    {   carry = 0;  
        for (int j=0; j<lenb; j++)
        {   multiply64(b[j], a[i-j], lo, hi1, lo);
            carry += add_with_carry(hi, hi1, hi);
        }
        c[i] = lo;
        lo = hi;
        hi = carry;
    }
// Now I will have some stages where the number of terms to be combined
// gradually decreases. If lenb==2 the following loop is not executed.
    for (int i=1; i<lenb-1; i++)
    {   carry = 0;
        for (int j=0; j<lenb-i; j++)
        {   multiply64(b[i+j], a[lena-j-1], lo, hi1, lo);
            carry += add_with_carry(hi, hi1, hi);
        }
        c[lena+i-1] = lo;
        lo = hi;
        hi = carry;
    }
// Finally the very top term is computed.
    multiply64(b[lenb-1], a[lena-1], lo, hi1, c[lena+lenb-2]);
    c[lena+lenb-1] = hi + hi1;
}


// c = c + a*b. Potentially carry all the way up to lenc.

inline void classical_multiply_and_add(uint64_t *a, size_t lena,
                                       uint64_t *b, size_t lenb,
                                       uint64_t *c, size_t lenc)
{   if (lena < lenb)
    {   std::swap(a, b);
        std::swap(lena, lenb);
    }
// (1) do the lowest degree term as a separate step
    uint64_t carry=0, carry1, hi, hi1, lo;
    multiply64(b[0], a[0], c[0], lo, c[0]);
// Now a sequence of stages where at each the number of terms to
// be combined grows. 
    hi = 0;
    for (int i=1; i<lenb; i++)
    {   carry = 0;
        for (int j=0; j<=i; j++)
        {   multiply64(b[j], a[i-j], lo, hi1, lo);
            carry += add_with_carry(hi, hi1, hi);
        }
        carry1 = add_with_carry(c[i], lo, c[i]);
        hi = add_with_carry(hi, carry1, lo) + carry;
    }
// If the two inputs are not the same size I demand that lena>=lenb and
// there may be some slices to compute in the middle here.
    for (int i=lenb; i<lena; i++)  //  If lenb==lena this loop is not executed
    {   carry = 0;  
        for (int j=0; j<lenb; j++)
        {   multiply64(b[j], a[i-j], lo, hi1, lo);
            carry += add_with_carry(hi, hi1, hi);
        }
        carry1 = add_with_carry(c[i], lo, c[i]);
        hi = add_with_carry(hi, carry1, lo) + carry;
    }
// Now I will have some stages where the number of terms to be combined
// gradually decreases.
    for (int i=1; i<lenb-1; i++) //  If lenb==2 this loop is not executed
    {   carry = 0;
        for (int j=0; j<lenb-i; j++)
        {   multiply64(b[i+j], a[lena-j-1], lo, hi1, lo);
            carry += add_with_carry(hi, hi1, hi);
        }
        carry1 = add_with_carry(c[lena+i-1], lo, c[lena+i-1]);
        hi = add_with_carry(hi, carry1, lo) + carry;
    }
// Finally the very top term is computed.
    multiply64(b[lenb-1], a[lena-1], lo, hi1, lo);
    carry = add_with_carry(c[lena+lenb-2], lo, c[lena+lenb-2]);
    carry = add_with_carry(c[lena+lenb-1], hi+hi1, carry, c[lena+lenb-1]);
    for (size_t i=lena+lenb; carry!=0 && i<lenc; i++)
        carry = add_with_carry(c[i], carry, c[i]);
}

#else // NEWER

inline void classical_multiply(uint64_t *a, size_t lena,
                               uint64_t *b, size_t lenb,
                               uint64_t *c)
{
// Here I experimented with working with 2 or 3 digits at a time -
// in effect unrolling the loops and rearranging the sequence of memory
// accesses in case I could speed things up. With g++ on x86_64 the
// changes hurt rather than benefitted me. So this has dropped back
// to simple code. Note that this should work with lena==1 or lenb==1 so
// I will not need to make those special cases.
    uint64_t hi=0, lo;
    for (size_t j=0; j<lenb; j++)
        multiply64(a[0], b[j], hi, hi, c[j]);
    c[lenb] = hi;
    for (size_t i=1; i<lena; i++)
    {   hi = 0;
        for (size_t j=0; j<lenb; j++)
        {   multiply64(a[i], b[j], hi, hi, lo);
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
        {   multiply64(a[i], b[j], hi, hi, lo);
            hi += add_with_carry(lo, c[i+j], c[i+j]);
        }
        carry = add_with_carry(hi, c[i+lenb], carry, c[i+lenb]);
    }
    for (size_t i=lena+lenb; carry!=0 && i<lenc; i++)
        carry = add_with_carry(c[i], carry, c[i]);
}

#endif // NEWER

// Now variants that use just a single digit first argument. These may be seen
// as optimized cases.

inline void classical_multiply(uint64_t a,
                               uint64_t *b, size_t lenb,
                               uint64_t *c)
{   uint64_t hi=0, lo;
    for (size_t j=0; j<lenb; j++)
        multiply64(a, b[j], hi, hi, c[j]);
    c[lenb] = hi;
}

// c = c + a*b and return any carry.

inline void classical_multiply_and_add(uint64_t a,
                                       uint64_t *b, size_t lenb,
                                       uint64_t *c, size_t lenc)
{   uint64_t hi=0, lo;
    for (size_t j=0; j<lenb; j++)
    {   multiply64(a, b[j], hi, hi, lo);
        hi += add_with_carry(lo, c[j], c[j]);
    }
    uint64_t carry = add_with_carry(hi, c[lenb], c[lenb]);
    for (size_t i=lenb+1; carry!=0 && i<lenc; i++)
        carry = add_with_carry(c[i], carry, c[i]);
}

#ifndef K
// I provide a default here but can override it at compile time
INLINE_VAR constexpr size_t K=18;
#endif

// When I have completed and measured things I am liable to make this a
// "const", but for now it is a simple variable so I can tinker with the
// value during testing and tuning.

INLINE_VAR size_t KARATSUBA_CUTOFF = K;

inline void small_or_big_multiply(uint64_t *a, size_t lena,
                                  uint64_t *b, size_t lenb,
                                  uint64_t *c, uint64_t *w);

inline void small_or_big_multiply_and_add(uint64_t *a, size_t lena,
                                          uint64_t *b, size_t lenb,
                                          uint64_t *c, size_t lenc,
                                          uint64_t *w);

// The key function here multiplies two numbers that are at least almost
// the same length. The cases that can arise here are
//      2n   2n       Easy and neat sub-division
//      2n   2n-1     Treat the second number as if it has a padding zero
//      2n-1 2n-1     Treat both numbers as if padded to size 2n
// Observe that if the two numbers have different lengths then the longer
// one is an even length, so the case (eg) 2n+1,2n will not arise.
// This will also only be used if lenb >= KARATSUBA_CUTOFF.

// When one multiplies {a1,a0}*{b1,b0} the three sub-multiplications to
// be performed are
//         a1*b1, a0*b0, |a0-a1|*|b0-b1|


inline void karatsuba(uint64_t *a, size_t lena,
                      uint64_t *b, size_t lenb,
                      uint64_t *c, uint64_t *w)
{
    assert(lena == lenb ||
           (lena%2 == 0 && lenb == lena-1));
    assert(lena >= 2);
    size_t n = (lena+1)/2;    // size of a "half-number"
    size_t lenc = lena+lenb;
    if (absdiff(a+n, lena-n, a, n, w, n) !=
        absdiff(b+n, lenb-n, b, n, w+n, n))
    {
// Here I will collect
//    a1*b1    (a1*b0 + b1*a0 - a1*b1 - a0*b0)     a0*b0   
// First write the middle part into place.
        small_or_big_multiply(w, n, w+n, n, c+n, w+2*n);     // (a1-a0)*(b0-b1)
// Now I just need to add back in parts of the a1*b1 and a0*b0
        small_or_big_multiply(a+n, lena-n, b+n, lenb-n, w, w+2*n); // a1*b1
// First insert the copy at the very top. Part can just be copied because I
// have not yet put anything into c there, the low half then has to be added
// in (and carries could propagate all the way up).
        for (size_t i=n; i<lenc-2*n; i++) c[2*n+i] = w[i];
        kadd(w, n, c+2*n, lenc-2*n);
// Now add in the second copy
        kadd(w, lenc-2*n, c+n, lenc-n);
// Now I can deal with the a0*b0.
        small_or_big_multiply(a, n, b, n, w, w+2*n);               // a0*b0
        for (size_t i=0; i<n; i++) c[i] = w[i];
        kadd(w+n, n, c+n, lenc-n);
        kadd(w, 2*n, c+n, lenc-n);
    }
    else
    {
// This case is slightly more awkward because the key parts of the middle
// part are negated. 
//    a1*b1    (-a1*b0 - b1*a0 + a1*b1 + a0*b0)     a0*b0   
        small_or_big_multiply(w, n, w+n, n, c+n, w+2*n);     // (a1-a0)*(b1-b0)
        small_or_big_multiply(a+n, lena-n, b+n, lenb-n, w, w+2*n); // a1*b1
        for (size_t i=n; i<lenc-2*n; i++) c[2*n+i] = w[i];
// Now I will do {c3,c2,c1} = {c3,w0,0} - {0,c2,c1) which has a mere negation
// step for the c1 digit, but is otherwise a reverse subtraction. Note I had
// just done c3 = w1 so that first term on the RHS is "really" {w1,w0,0}.
//      c1 = 0 - c1 [and generate borrow]
//      c2 = w0 - c2 - borrow [and generate borrow]
//      c3 = c3 - borrow
        uint64_t borrow = 0;
        for (size_t i=0; i<n; i++)
            borrow = subtract_with_borrow(0, c[n+i], borrow, c[n+i]);
        for (size_t i=0; i<n; i++)
            borrow = subtract_with_borrow(w[i], c[2*n+i], borrow, c[2*n+i]);
        for (size_t i=0; i<lenc-3*n && borrow!=0; i++)
            borrow = subtract_with_borrow(c[3*n+i], borrow, c[3*n+i]);
// Now I can proceed as before
        kadd(w, lenc-2*n, c+n, lenc-n);
        small_or_big_multiply(a, n, b, n, w, w+2*n);               // a0*b0
        for (size_t i=0; i<n; i++) c[i] = w[i];
        kadd(w+n, n, c+n, lenc-n);
        kadd(w, 2*n, c+n, lenc-n);
    }
}

inline void karatsuba_and_add(uint64_t *a, size_t lena,
                              uint64_t *b, size_t lenb,
                              uint64_t *c, size_t lenc, uint64_t *w)
{   assert(lena == lenb ||
           (lena%2 == 0 && lenb == lena-1));
    assert(lena >= 2);
    size_t n = (lena+1)/2;    // size of a "half-number"
    size_t lenc1 = lena+lenb;
    if (absdiff(a+n, lena-n, a, n, w, n) !=
        absdiff(b+n, lenb-n, b, n, w+n, n))
    {
// Here I will collect
//    a1*b1    (a1*b0 + b1*a0 - a1*b1 - a0*b0)     a0*b0   
        small_or_big_multiply_and_add(w, n, w+n, n, c+n, lenc-n, w+2*n);
        small_or_big_multiply(a+n, lena-n, b+n, lenb-n, w, w+2*n); // a1*b1
        kadd(w, lenc1-2*n, c+2*n, lenc-2*n);
        kadd(w, lenc1-2*n, c+n, lenc-n);
        small_or_big_multiply(a, n, b, n, w, w+2*n);               // a0*b0
        kadd(w, 2*n, c, lenc);
        kadd(w, 2*n, c+n, lenc-n);
    }
    else
    {
// This case is slightly more awkward because the key parts of the middle
// part are negated. 
//    a1*b1    (-a1*b0 - b1*a0 + a1*b1 + a0*b0)     a0*b0   
// To perform c=c-w; I go c=~c; c=c+w; c=~c; [that is a NOT rather than
// a MINUS there!].
        for (size_t i=n; i<lenc; i++) c[i] = ~c[i];
        small_or_big_multiply_and_add(w, n, w+n, n, c+n, lenc-n, w+2*n);
        for (size_t i=n; i<lenc; i++) c[i] = ~c[i];
        small_or_big_multiply(a+n, lena-n, b+n, lenb-n, w, w+2*n); // a1*b1
        kadd(w, lenc1-2*n, c+2*n, lenc-2*n);
        kadd(w, lenc1-2*n, c+n, lenc-n);
        small_or_big_multiply(a, n, b, n, w, w+2*n);               // a0*b0
        kadd(w, 2*n, c, lenc);
        kadd(w, 2*n, c+n, lenc-n);
    }
}

// Here both inputs are of size at least KARATSUBA_CUTOFF. If their sizes
// match exactly I can use Karatsuba directly. I take the view that if
// the two are of sized (2n) and (2n-1) then I will also use Karatsuba
// directly (treating the shorter input as if it had an initial zero padding
// digit). In all other cases I need to do a sequence of multiplications
// rather along the lines of "short" multiplication treating the size of the
// smaller operand as the digit size.

inline void certainly_big_multiply(uint64_t *a, size_t lena,
                                   uint64_t *b, size_t lenb,
                                   uint64_t *c, uint64_t *w)
{   if (lena == lenb)
    {   karatsuba(a, lena, b, lenb, c, w);
        return;
    }
    if (lena < lenb)
    {   std::swap(a, b);
        std::swap(lena, lenb);
    }
// Now b is the shorter operand. The case (2n)*(2n-1) will be handled
// using Karatsuba merely by treating the smaller number as if padded with
// a leading zero.
    if (lena%2==0 && lenb==lena-1)
    {   karatsuba(a, lena, b, lenb, c, w);
        return;
    }
// If the two inputs are unbalanced in length I will perform multiple
// balanced operations each of which can be handled specially. I will
// try to make each subsidiary multiplication as big as possible.
// This will be lenb rounded up to an even number.
// I will be willing to do chunks that are of an even size that is
// either lenb or lenb+1. 
    size_t len = lenb + (lenb & 1);
    uint64_t *a1 = a, *c1 = c;
    size_t lena1 = lena;
// Multiply-and-add will be (slightly) more expensive than just Multiply,
// so I do a sequence of multiplications where their outputs will not overlap
// first, and then do the interleaved multiplications adding in.
    for (;;)
    {
// I may have rounded the size of b up by 1, and if I have I would generate
// 2*len-1 digits not 2*len and hence risk leaving a 1-word gap between filled
// in data. I zero that here to avoid trouble. However I must not do this
// for if the multiplication I am about to do will write in the very top
// digits of the final answer, because if I did that would be a sort of
// buffer overrun.
        if (len < lena1) c1[2*len-1] = 0;
        karatsuba(a1, len, b, lenb, c1, w);
        c1 += 2*len;
// I will keep going provided the next multiplication I will do will fully fit.
        if (lena1 < 3*len) break;
        a1 += 2*len;
        lena1 -= 2*len;
    }
    if (lena1 > 2*len)
    {   a1 += 2*len;
        lena1 -= 2*len;
// Do a shorter nice Multiply (without Add) to reach the end of input a.
        small_or_big_multiply(a1, lena1, b, lenb, c1, w);
    }
    else if (lena1!=len)
    {
// I may need to pad with zeros when the top digit to be generated will be
// put there using multiply_and_add.
        for (size_t i=c1-c; i<lena+lenb; i++) c[i] = 0;
    }
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
        karatsuba_and_add(a1, len, b, lenb, c1, lenc1, w);
        if (lena1 <= 2*len)
        {   lena1 = 0;
            break;
        }
        c1 += 2*len;
        lenc1 -= 2*len;
        a1 += 2*len;
        lena1 -= 2*len;
    }
    if (lena1!=0)
        small_or_big_multiply_and_add(a1, lena1, b, lenb, c1, lenc1, w);
}

inline void certainly_big_multiply_and_add(uint64_t *a, size_t lena,
                                           uint64_t *b, size_t lenb,
                                           uint64_t *c, size_t lenc,
                                           uint64_t *w)
{   if (lena == lenb)
    {   karatsuba_and_add(a, lena, b, lenb, c, lenc, w);
        return;
    }
    if (lena < lenb)
    {   std::swap(a, b);
        std::swap(lena, lenb);
    }
// Now b is the shorter operand. The case (2n)*(2n-1) will be handled
// using Karatsuba merely by treating the smaller number as if padded with
// a leading zero.
    if (lena%2==0 && lenb==lena-1)
    {   karatsuba_and_add(a, lena, b, lenb, c, lenc, w);
        return;
    }
// If the two inputs are unbalanced in length I will perform multiple
// balanced operations each of which can be handled specially. I will
// try to make each subsidiary multiplication as big as possible.
// This will be lenb rounded up to an even number.
// I will be willing to do chunks that are of an even size that is
// either lenb or lenb+1. 
    size_t len = lenb + (lenb & 1);
    uint64_t *a1 = a, *c1 = c;
    size_t lena1 = lena, lenc1 = lenc;
// because this is "certainly big" I know I can do at least one
// Karatsuba stage.
    for (;;)
    {   karatsuba_and_add(a1, len, b, lenb, c1, lenc1, w);
        c1 += len;
        lenc1 -= len;
        a1 += len;
        lena1 -= len;
// I will keep going provided the next multiplication I will do will fully fit.
        if (lena1 < len) break;
    }
// Do a shorter nice Multiply (without Add) to reach the end of input a.
    if (lena1 != 0)
        small_or_big_multiply_and_add(a1, lena1, b, lenb, c1, lenc1, w);
}

// I am going to hope that the compiler turns this into a tail-call to
// either certainly_big_multiply or classical_multiply with very
// little overhead.

inline void small_or_big_multiply(uint64_t *a, size_t lena,
                                  uint64_t *b, size_t lenb,
                                  uint64_t *c, uint64_t *w)
{   if (lena < KARATSUBA_CUTOFF || lenb < KARATSUBA_CUTOFF)
    {   if (lena==1) classical_multiply(a[0], b, lenb, c);
        else if (lenb==1) classical_multiply(b[0], a, lena, c);
        else classical_multiply(a, lena, b, lenb, c);
    }
    else certainly_big_multiply(a, lena, b, lenb, c, w);
}

inline void small_or_big_multiply_and_add(uint64_t *a, size_t lena,
                                          uint64_t *b, size_t lenb,
                                          uint64_t *c, size_t lenc,
                                          uint64_t *w)
{   if (lena < KARATSUBA_CUTOFF || lenb < KARATSUBA_CUTOFF)
    {   if (lena==1) classical_multiply_and_add(a[0], b, lenb, c, lenc);
        else if (lenb==1) classical_multiply_and_add(b[0], a, lena, c, lenc);
        else classical_multiply_and_add(a, lena, b, lenb, c, lenc);
    }
    else certainly_big_multiply_and_add(a, lena, b, lenb, c, lenc, w);
}

// Finally I can provide the top-level entrypoint that accepts signed
// integers that may not be the same size.

INLINE_VAR const size_t KARA_FIXED_WORKSPACE_SIZE = 200;
INLINE_VAR const size_t KARA_WORKSPACE_SIZE = 408;
INLINE_VAR uint64_t kara_workspace[KARA_WORKSPACE_SIZE];

inline constexpr int BY(int m, int n)
{   return m + 4*n;
}

// This is the main entrypoint to the integer multiplication code. It
// takes two signed numbers and forms their product.

inline void bigmultiply(uint64_t *a, size_t lena,
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
    if (lena <= 4 && lenb <= 4) switch (lena + 4*lenb)
    {
// Length 2 result
    case BY(1, 1):
        {   int64_t c1;
            uint64_t c0;
            signed_multiply64(a[0], b[0], c1, c0);
            c[0] = c0;
            if (shrinkable(c1, c0)) lenc = 1;
            else
            {   c[1] = (uint64_t)c1;
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
            int64_t c2;
            uint64_t c1, c0;
            multiply64(b[0], a[0], c1, c0);
            c[0] = c0;
            signed_multiply64(b[0], a[1], c1, c2, c1);
            if (negative(b[0]))
                c2 = (int64_t)((uint64_t)c2 -
                                subtract_with_borrow(c1, a[0], c1));
            c[1] = c1;
// If you have an input like 2^63 it will be represented as a 2-word
// bignum {0,2^63} so that the most significant bit of the most significant
// digits is 0 to show that it is positive. If you multiply two such numbers
// the direct result you get is {0, 0, 2^62, 0} and the top non-zero digit
// now does not have its top bit set. So TWO words can be trimmed from the
// top. This issue may not arise in the length-1 by length-2 case here, but
// I leave the test in to feel safe and tidy.
            if (shrinkable(c2, c1))
            {   if (shrinkable(c1, c0)) lenc = 1;
                else lenc = 2;
            }
            else
            {   c[2] = (uint64_t)c2;
                lenc = 3;
            }
            return;
        }

// Length 4 result
    case BY(2, 2):
        {   int64_t c3;
            uint64_t c2, c1;
            mul2x2S((int64_t)a[1], a[0],
                    (int64_t)b[1], b[0],
                    c3, c2, c1, c[0]);
            c[1] = c1;
            c[2] = c2;
            if (shrinkable(c3, c2))
            {   if (shrinkable(c2, c1)) lenc = 2;
                else lenc = 3;
            }
            else
            {   c[3] = (uint64_t)c3;
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
            int64_t c3;
            uint64_t c2, c1;
            multiply64(b[0], a[0], c1, c[0]);
            multiply64(b[0], a[1], c1, c2, c1);
            signed_multiply64(b[0], a[2], c2, c3, c2);
            if (negative(b[0]))
            {   uint64_t borrow = subtract_with_borrow(c1, a[0], c1);
                borrow = subtract_with_borrow(c2, a[1], borrow, c2);
                c3 = (int64_t)((uint64_t)c3 - borrow);
            }
            c[1] = c1;
            c[2] = c2;
            if (shrinkable(c3, c2))
            {   if (shrinkable(c2, c1)) lenc = 2;
                else lenc = 3;
            }
            else
            {   c[3] = (uint64_t)c3;
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
            int64_t c4;
            uint64_t c3, c2, c1;
            multiply64(b[0], a[0], c1, c[0]);
            multiply64(b[0], a[1], c1, c2, c1);
            multiply64(b[0], a[2], c2, c3, c2);
            signed_multiply64(b[0], a[3], c3, c4, c3);
            if (negative(b[0]))
            {   uint64_t borrow = subtract_with_borrow(c1, a[0], c1);
                borrow = subtract_with_borrow(c2, a[1], borrow, c2);
                borrow = subtract_with_borrow(c3, a[2], borrow, c3);
                c4 = (int64_t)((uint64_t)c4 - borrow);
            }
            c[1] = c1;
            c[2] = c2;
            c[3] = c3;
            if (shrinkable(c4, c3))
            {   if (shrinkable(c3, c2)) lenc = 3;
                else lenc = 4;
            }
            else
            {   c[4] = (uint64_t)c4;
                lenc = 5;
            }
            return;
        }

    case BY(2, 3):
        std::swap(a, b);
    case BY(3, 2):
        {   int64_t c4;
            uint64_t c3, c3a, c2;
            mul2x2(a[1], a[0], b[1], b[0],
                   c3, c2, c[1], c[0]);
            multiply64(a[2], b[0], c2, c3a, c2);
            uint64_t carry = add_with_carry(c3, c3a, c3);
            signed_multiply64((int64_t)a[2], (int64_t)b[1], c3, c4, c3);
            c4 = (int64_t)((uint64_t)c4 + carry);
            if (negative(b[1]))
            {   uint64_t borrow = subtract_with_borrow(c2, a[0], c2);
                borrow = subtract_with_borrow(c3, a[1], borrow, c3);
                c4 = (int64_t)((uint64_t)c4 - borrow);
            }
            if (negative(a[2]))
            {   uint64_t borrow = subtract_with_borrow(c3, b[0], c3);
                c4 = (int64_t)((uint64_t)c4 - borrow);
            }
            c[2] = c2;
            c[3] = c3;
            if (shrinkable(c4, c3))
            {   if (shrinkable(c3, c2)) lenc = 3;
                else lenc = 4;
            }
            else
            {   c[4] = (uint64_t)c4;
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
        {   int64_t c5;
            uint64_t c4, c3, c2, c1;
            mul3x3S(a[2], a[1], a[0], b[2], b[1], b[0],
                    c5, c4, c3, c2, c1, c[0]);
            c[1] = c1;
            c[2] = c2;
            c[3] = c3;
            c[4] = c4;
            if (shrinkable(c5, c4))
            {   if (shrinkable(c4, c3)) lenc = 4;
                else lenc = 5;
            }
            else
            {   c[5] = (uint64_t)c5;
                lenc = 6;
            }
            return;
        }

// Length 7 results
    case BY(3, 4):
    case BY(4, 3):
// As above, cases that have not been coded here do not cause failure,
// they just lead to that case being handled by the general (loopy) code.
        break;


// Length 8 result
    case BY(4, 4):
        {   uint64_t c7, c6, c5, c4;
            mul4x4(a[3], a[2], a[1], a[0],
                   b[3], b[2], b[1], b[0],
                   c7, c6, c5, c4, c[3], c[2], c[1], c[0]);
            if (negative(a[3]))
            {   uint64_t borrow = subtract_with_borrow(c4, b[0], c4);
                borrow = subtract_with_borrow(c5, b[1], borrow, c5);
                borrow = subtract_with_borrow(c6, b[2], borrow, c6);
                c7 = (int64_t)((uint64_t)c7 - b[3] - borrow);
            }
            if (negative(b[3]))
            {   uint64_t borrow = subtract_with_borrow(c4, a[0], c4);
                borrow = subtract_with_borrow(c5, a[1], borrow, c5);
                borrow = subtract_with_borrow(c6, a[2], borrow, c6);
                c7 = (int64_t)((uint64_t)c7 - a[3] - borrow);
            }
            c[4] = c4;
            c[5] = c5;
            c[6] = c6;
            if (shrinkable(c7, c6))
            {   if (shrinkable(c6, c5)) lenc = 6;
                else lenc = 7;
            }
            else
            {   c[7] = (uint64_t)c7;
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
// It is necessary to make a special case for multiplication by a 1-word
// bignum for two reasons. (a) multiplication by zero yields a zero result
// regardless of the magnitude of the second operand, and (b) one of
// my implementations of classical_multiplication must be called in
// a separate overloaded version to multiply by just one digit.
    if (lena < KARATSUBA_CUTOFF || lenb < KARATSUBA_CUTOFF)
    {   if (lena==1)
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
// I do NOT return here because for a non-zero result I will need to adjust
// if one or both of the input numbers were negative.
    }
    else
    {
// For many smaller cases I will just use some static pre-allocated workspace
// and hence avoid potential storage management overheads.
        if (lena <= KARA_FIXED_WORKSPACE_SIZE ||
            lenb <= KARA_FIXED_WORKSPACE_SIZE)
            certainly_big_multiply(a, lena, b, lenb, c, kara_workspace);
        else
        {   push(a); push(b);
            size_t lenw;
            if (lena < lenb) lenw = lena;
            else lenw = lenb;
            for (size_t i=lenw; i>8; i=i/2) lenw++;
// I give myself workspace as long as the shorter input + log of that. The
// extra logarithmic bit is because each time I split a number into its top
// and bottom parts I may have an odd number and so the workspace needed
// gets rounded up by a constant amount for each level of division.
            uint64_t *w = reserve(2*lenw);
            pop(b); pop(a);
            certainly_big_multiply(a, lena, b, lenb, c, w);
            abandon(w);
        }
    }
// Now adapt for the situation where one or both of the inputs had been
// negative.
    if (negative(a[lena-1]))
    {   uint64_t borrow = 0;
        for (size_t i=0; i<lenb; i++)
            borrow = subtract_with_borrow(c[i+lena], b[i], borrow, c[i+lena]);
    }
    if (negative(b[lenb-1]))
    {   uint64_t borrow = 0;
        for (size_t i=0; i<lena; i++)
            borrow = subtract_with_borrow(c[i+lenb], a[i], borrow, c[i+lenb]);
    }
// The actual value may be 1 or 2 words shorter than this. So test the top
// digit of c and if necessary reduce lenc.
// Also note that the pending  result is at least of length 2 here because
// various small cases had been processed in-line earlier.
// eg {0, 0x8000000000000000} times itself is {0, 0, 0x4000000000000000, 0}
// and both leading zeros can be trimmed.
    lenc = lena + lenb;
    if (lenc > 1 && shrinkable(c[lenc-1], c[lenc-2]))
    {   lenc--;
        if (lenc > 1 && shrinkable(c[lenc-1], c[lenc-2])) lenc--;
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
            multiply64(a[i], b[j], hi, hi, lo);
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
//
// With a cutoff at 4 the first case I partition things will be a 4*4
// multiplication that gets split into (2+2)*(2+2). That means that the
// numbers I pass down to the classical multiplication there will always have
// at least 2 digits, and that is necessary for my code to behave properly.
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

#ifdef PRE_TEST
// This is a place where I can put in particular examples that are
// causing trouble. It is not expected to be used other then during
// heavy debugging sessions. If I activate this it forces subsequent
// tests to use a cutoff of 4.

    a[3] = 0x0010000000000000;
    a[2] = 0x0100000000000000;
    a[1] = 0x0000000004000000;
    a[0] = 0x0000010000000000;
    lena = 4;
    b[4] = 0x0000000000010000;
    b[3] = 0x0000000000000200;
    b[2] = 0x0000000000000200;
    b[1] = 0x0000000000004000;
    b[0] = 0x0000000000004000;
    lenb = 5;
    display("a", a, lena);
    display("b", b, lenb);
    KARATSUBA_CUTOFF = 4;
    bigmultiply(a, lena, b, lenb, c, lenc);
    referencemultiply(a, lena, b, lenb, c1, lenc1);
    display("mine  ", c, lenc);
    display("theirs", c1, lenc1);
    for (int i=0; i<9; i++) w[i] = c[i] ^ c1[i];
    display("diff", w, 8);
#endif

#ifndef NO_EASY

// Here I do "easy" tests on numbbers of length 4 and 5. THis is because
// if I set KARATSUBA_CUTOFF to 4 these JUST start to use that algorithm,
// and I wanted extra testing on it.

    for (int i=0; i<10; i++)
    {   a[i] = ((uint64_t)1)<<(mersenne_twister() & 0x3f);
        b[i] = ((uint64_t)1)<<(mersenne_twister() & 0x3f);
    }
    size_t kk = KARATSUBA_CUTOFF;
    KARATSUBA_CUTOFF = 4;
    for (int trial=0; trial<10000000; trial++)
    for (lena=4; lena<5; lena++)
    for (lenb=4; lenb<6; lenb++)
    {   referencemultiply(a, lena, b, lenb, c, lenc);
        bigmultiply(a, lena, b, lenb, c1, lenc1);
        bool ok=(lenc == lenc1);
        for (size_t i=0; i<lenc; i++)
        {   if (c[i] != c1[i])
            {   std::cout << "Differ at " << std::dec << i
                          << " 0x" << std::hex << (c[i] ^ c1[i])
                          << std::endl;
                ok = false;
            }
        }
        if (!ok)
        {   std::cout << "Failed at "
                      << lena << "*" << lenb << std::endl;
            std::cout << std::hex << "c-end  = " << c[lena+lenb] << std::endl;
            std::cout << std::hex << "c1-end = " << c1[lena+lenb] << std::endl;
            display("a  ", a, lena);
            display("b  ", b, lenb);
            display("OK ", c,  lenc);   // Reference result
            display("MY ", c1, lenc1);  // bigmultiply result
            std::cout << "Failed" << std::endl;
            return 1;
        }
    }
    KARATSUBA_CUTOFF = kk;  // restore the "proper" value.
#endif // NO_EASY

#ifndef NO_CORRECTNESS

// This generates some random data and then calls the multiplication code
// to multiply each m*n prefix of it. It compares the results from the
// new code here against those from  "referencemultiply".

    std::cout << "Correctness test" << std::endl;
    for (int run=0; run<500; run++)
    {   if (run%10 == 9)
        {   std::cout << ((char)(((run+1)/10)%10 + '0'));
            std::cout.flush();
        }
        for (size_t i=0; i<MAX; i++)
        {   a[i] = mersenne_twister();
            b[i] = mersenne_twister();
        }
        uint64_t toplen = mersenne_twister();
        int count = 0;
        int maxlen = 100;
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
                referencemultiply(a, lena, b, lenb, c, lenc);
                bigmultiply(a, lena, b, lenb, c1, lenc1);
                bool ok=(lenc == lenc1);
                for (size_t i=0; i<lenc; i++)
                    if (c[i] != c1[i])
                    {   std::cout << "Differ at " << std::dec << i << std::endl;
                        ok = false;
                    }
                if (!ok)
                {   std::cout << std::endl;
                    std::cout << "Failed at "
                              << lena << "*" << lenb << std::endl;
                    std::cout << std::hex << "c-end  = " << c[lena+lenb] << std::endl;
                    std::cout << std::hex << "c1-end = " << c1[lena+lenb] << std::endl;
                    display("a  ", a, lena);
                    display("b  ", b, lenb);
                    display("ref", c,  lenc);   // Reference result
                    display("my ", c1, lenc1);  // bigmultiply result
                    std::cout << "Failed" << std::endl;
                    return 1;
                }
                a[lena-1] = savea;
                b[lenb-1] = saveb;
            }
        }
    }
    std::cout << std::endl;

#endif // NO_CORRECTNESS

#ifndef NO_TUNING

    size_t best = 0;
    double best_time = HUGE_VAL;

// First I will try multiplications (in fact of balanced inputs) for
// a range of cutoffs, and see how long things take. For each potential
// cutoff value I will test a range of different length inputs.

    for (KARATSUBA_CUTOFF=8; KARATSUBA_CUTOFF<36; KARATSUBA_CUTOFF++)
    {   for (size_t i=0; i<MAX; i++)
        {   a[i] = mersenne_twister();
            b[i] = mersenne_twister();
        }
        clock_t cl0 = clock();
        for (lena=4; lena<100; lena++)
        {   lenb = lena;
            for (size_t n = 0; n<200000/(lena*lena); n++)
            {
// I make my test inputs positive because I do not want any confusion
// with the cost of the extra subtraction needed when multiplying bt
// a negative value.
                a[lena-1] &= 0x7fffffffffffffff;
                b[lenb-1] &= 0x7fffffffffffffff;
                for (size_t m=0; m<1000; m++)
                    bigmultiply(a, lena, b, lenb, c1, lenc1);
// I now use a simple linear conguential scheme to give myself different
// inputs. This can have an effect on how many carry operations are
// performed within the big multiplication.
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

    std::cout << "Table of microseconds per multiplication" << std::endl;
    for (size_t lena=1; lena<2*KARATSUBA_CUTOFF; lena++)
    {   lenb = lena;
        for (size_t i=0; i<MAX; i++)
        {   a[i] = mersenne_twister();
            b[i] = mersenne_twister();
        }
        clock_t cl0 = clock();
        size_t trials = (100000/(4+lena*lena));
        for (size_t n = 0; n<trials; n++)
        {   a[lena-1] &= 0x7fffffffffffffff;
            b[lenb-1] &= 0x7fffffffffffffff;
            for (size_t m=0; m<2000; m++)
                bigmultiply(a, lena, b, lenb, c1, lenc1);
            for (size_t i=0; i<lena; i++)
                a[i] = MULT*a[i] + ADD;
            for (size_t i=0; i<lenb; i++)
                b[i] = MULT*b[i] + ADD;
        }
        clock_t cl1 = clock();
        double t = 1.0e6*(cl1-cl0)/(double)trials/2000.0/(double)CLOCKS_PER_SEC;
        std::cout << std::setw(10) << lena << std::setw(10)
                  << std::fixed << std::setprecision(3) << t << std::endl;
    }


#endif // NO_TUNING


#ifndef NO_GMP
    const size_t table_size = 300;

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
// bigmultiply is at a slightly higher level and deals with signed values.
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
            for (size_t m=0; m<500; m++)
                bigmultiply(a, lena, b, lenb, c1, lenc1);
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
