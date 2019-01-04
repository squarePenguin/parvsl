// Big-number arithmetic.                                  A C Norman, 2019


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


// My bignum package is a header-only library, so to use it you arrange
// that the c++ compiler has a directive such as "-Idirectory_for_arith"
// so that it can be found and you then just include the one file that
// contains everything.

#include "arith.hpp"

#include <iostream>
#include <iomanip>
#include <ctime>

using namespace arith;

// The tests come in sections, and these preprocessor symbols can be used
// to select which sections get run.

#define TEST_SOME_BASICS 1
#define TEST_RANDOM 1
#define TEST_BITWISE 1
#define TEST_SHIFTS 1
#define TEST_PLUS_AND_TIMES 1
#define TEST_DIVISION 1
#define TEST_ISQRT 1
#define TEST_FLOAT

// This function is to test if the least significant bit in the representation
// of a floating point value is zero. It is used when verifying the correct
// rounding of cases that fall exactly half way between two representable
// floating point numbers.

bool evenfloat(double d)
{   int x;
    d = std::frexp(d, &x);
    d = std::ldexp(d, 53);
    int64_t i = (int64_t)d;
    return (i&1) == 0;
}

int main(int argc, char *argv[])
{
// If I invoke this without command line arguments it will run with
// a decent randomized sequence. If I give it a command line argument
// that is an integer it will use that to see its random number generator
// and so it will behave deterministically. This is really useful if an
// error is detected.

    uint64_t seed;
    if (argc > 1) seed = atoi(argv[1]);
    else seed = mersenne_twister() & 0xffff;
    std::cout << "seed = " << seed << std::endl;
    reseed(seed);

    int maxbits, ntries;
    clock_t clk;
    int64_t clong;
    double timing;

    const int MILLION = 1000000;


#ifdef TEST_SOME_BASICS

// Do some very simple operations that just verify that printing and
// basic arithmetic is in order.

    std::cout << "Some simple tests involving powers of 10" << std::endl;
    Bignum a = "10000000000000000000000000";
    std::cout << "a = " << a << std::endl;
    std::cout << "a*a = " << (a*a) << std::endl;
    std::cout << "a*100 = " << (a*Bignum(100)) << std::endl;
    std::cout << "100*a = " << (Bignum(100)*a) << std::endl;
    std::cout << "100*100 = " << (Bignum(100)*Bignum(100)) << std::endl;
    std::cout << "End of simple tests" << std::endl << std::endl;

#endif


#ifdef TEST_RANDOM

// For many of my later tests I use random inputs, but the random distribution
// used is a really odd one! It starts by having a uniform distribution over
// the number of bits in the number. It then takes raw random numbers ("a"
// here) generated that way and takes "a" and round down to "x", the largest
// power of 2 less than or equalt to x. It then returns one of
//    a, x-1, x, x+1, -(x-1), -x, -(x+1)
// with the generically random first case (just "a") happening most often.
// The reasoning behind this choice for test cases is that the internal
// representation used here involves a radix of 2^64, and so numbers close
// to powers of 2 (and their negatives) are potential special cases and
// deserve extra testing.
//
// This section of the test code just displays 10 random values each with
// no more than 160 bits and it illustrates printing in both decimal and
// hexadecimal.

    maxbits = 130;
    ntries = 10;

    std::cout << "Print some random numbers in decimal and hex" << std::endl;
    for (int i=1; i<=ntries; i++)
    {   Bignum a = random_upto_bits_bignum(maxbits);
        uint64_t r = mersenne_twister();
        Bignum b = fudge_distribution_bignum(a, (int)r & 0xf);
//      std::cout << a << std::endl;
        std::cout << b << " "
                  << std::hex << b << std::dec
                  << std::endl;
    }
    std::cout << "end of display of random values" << std::endl << std::endl;

#endif // TEST_RANDOM


#ifdef TEST_BITWISE

// I test the bitwise operations by performing two comparisons:
//       ~(a & b) == ~a | ~b
// and   a ^ b == ~a^b | ~b^a
//
// Note that negative numbers are stored in 2s complement and should be
// treated as if they have an infinite number of leading 1 bits beyond the
// most significant bit that they store.
//
// If I observe a discrepancy here (and in later tests) I will display the
// inputs that led to it so that those can be checked further for debugging.

    maxbits = 400;
    ntries = 50*MILLION;

    std::cout << "Start of bitwise operation testing" << std::endl;
    clk = clock(); clong = 0;

    for (int i=1; i<=ntries; i++)
    {
// On some 32-bit systems clock_t is a 32-bit type and CLOCKS_PER_SEC
// is 1000000. The consequence is that clock readings overflow after about
// half an hour of CPU time. The tests here can reasonably be configured
// such that on a slow machine this limit may be exceeded. To work around
// that I unload from clock() into an int64_t value every million times
// round my loop. On a sufficiently slow system this would not cure the
// problem, but in realistic cases it will and the overhead of the test
// here and the extra work every 2^20 iterations is not liable to be severe. 
        if ((i & 0xfffff) == 0)
        {   clock_t now = clock();
            clong += now - clk;
            clk = now;
        }
        Bignum a = random_upto_bits_bignum(maxbits);
        Bignum b = random_upto_bits_bignum(maxbits);
        uint64_t r = mersenne_twister();
        a = fudge_distribution_bignum(a, (int)r & 0xf);
        b = fudge_distribution_bignum(b, (int)(r>>4) & 0xf);
        Bignum c1 = ~(a & b);
        Bignum c2 = (~a) | (~b);
        Bignum c3 = a ^ b;
        Bignum c4 = (a&(~b)) | (b&(~a));
        if (c1==c2 && c3==c4) continue;
        std::cout << "FAILED on test " << i << std::hex << std::endl;
        std::cout << "a            " << a << std::endl;
        std::cout << "b            " << b << std::endl;
        std::cout << "c1 ~(a&b)    " << c1 << std::endl;
        std::cout << "c2 ~a|~b     " << c2 << std::endl;
        std::cout << "c3 a^b       " << c3 << std::endl;
        Bignum nota = ~a;
        Bignum notb = ~b;
        std::cout << "   ~a        " << nota << std::endl;
        std::cout << "   ~b        " << notb << std::endl;
// Sending something to std::cout is the normal way of observing values
// in the code here, but the function display() as used in the diagnostic
// being printed at present shows the internal representation of the
// numbers.
        display("b", b);
        display("nota", nota);
        std::cout << "   a&~b      " << (a&~b) << std::endl;
        Bignum bnota = b & (~a);
        std::cout << "   b&~a      " << bnota << std::endl;
        display("bnota", bnota);
        std::cout << "c4 a&~b|b&~a "  << c4 << std::endl;
        std::cout << "Failed " << std::dec << std::endl;
        return 1;
    }

    timing = (clong + clock() - clk)/(double)CLOCKS_PER_SEC;
    std::cout << "Bitwise operation tests completed in "
              << timing << " sec" << std::endl;

#endif // TEST_BITWISE


#ifdef TEST_SHIFTS

// For testing shifts I will compare the result of a<<n with a*pow(2,n)
// and a>>n with a/pow(2^n) but with the division actually of
// (a & ~(pow(2,n)-1)) rather than just a so that the division is exact
// and the treatment of negative inputs is as desired. I will include cases
// where the shift amount is greater than the bit-length of the input.

    maxbits = 800;
    ntries = 50*MILLION;

    std::cout << "Start of shift testing" << std::endl;
    clk = clock(); clong = 0;

    for (int i=1; i<=ntries; i++)
    {   if ((i & 0xfffff) == 0)
        {   clock_t now = clock();
            clong += now - clk;
            clk = now;
        }
        Bignum a = random_upto_bits_bignum(maxbits);
        uint64_t r = mersenne_twister();
        a = fudge_distribution_bignum(a, (int)r & 0xf);
        r = (r >> 4)%800;
        Bignum c1 = a << r;
        Bignum p = pow(Bignum(2), (int)r);
        Bignum c2 = a * p;
        Bignum c3 = a >> r;
        Bignum w = a & ~(p-1);
        Bignum c4 = (a & ~(p-1))/p;
        if (c1==c2 && c3==c4) continue;
        std::cout << "FAILED on test " << i << std::hex << std::endl;
        std::cout << "a            " << a << std::endl;
        std::cout << "r            " << std::dec << r << std::hex << std::endl;
        std::cout << "divide " << std::dec << w << std::endl;
        display("div", w);
        std::cout << "by " << std::dec << p << std::hex << std::endl;
        display(" by", p);
        std::cout << "p            " << p << std::endl;
        std::cout << "a&~(p-1)     " << (a&~(p-1)) << std::endl;
        std::cout << "c1 a<<r      " << c1 << std::endl;
        std::cout << "c2 a*2^r     " << c2 << std::endl;
        std::cout << "c3 a>>r      " << c3 << std::endl;
        std::cout << "c4 a/2^r     " << c4 << std::endl;
        display("c4", c4);
        std::cout << "Failed " << std::dec << std::endl;
        return 1;
    }

    timing = (clong + clock() - clk)/(double)CLOCKS_PER_SEC;
    std::cout << "Shift tests completed in "
              << timing << " sec" << std::endl;

#endif // TEST_SHIFTS


#ifdef TEST_PLUS_AND_TIMES

// To check that + and - and * behave on the Bignum type I
// generate random numbers a and b and then compute first (a+b)*(a-b)
// and then a*a-b*b. If these match I feel happy - while if I find a case
// where the two values differ I have uncovered a bug. I also check that the
// square() function [which should be a bit faster than doing a simple
// multiplication] yields the same results.

    maxbits = 600;
    ntries = 50*MILLION;

    std::cout << "Start of Plus and Times testing" << std::endl;
    clk = clock(); clong = 0;

    for (int i=1; i<=ntries; i++)
    {   if ((i & 0xfffff) == 0)
        {   clock_t now = clock();
            clong += now - clk;
            clk = now;
        }
        Bignum a = random_upto_bits_bignum(maxbits);
        Bignum b = random_upto_bits_bignum(maxbits);
        uint64_t r = mersenne_twister();
        a = fudge_distribution_bignum(a, (int)r & 0xf);
        b = fudge_distribution_bignum(b, (int)(r>>4) & 0xf);
        Bignum c1 = (a + b)*(a - b);
        Bignum c2 = a*a - b*b;
        Bignum c3 = square(a) - square(b);
        if (c1 == c2 && c2 == c3) continue;
        std::cout << "FAILED on test " << i << std::endl;
        std::cout << "a  = " << a << std::endl;
        std::cout << "b  = " << b << std::endl;
        std::cout << "a+b         = " << a+b << std::endl;
        std::cout << "a-b         = " << a-b << std::endl;
        std::cout << "a*a         = " << a*a << std::endl;
        std::cout << "b*b         = " << b*b << std::endl;
        std::cout << "(a+b)*(a-b) = " << c1 << std::endl;
        std::cout << "(a+b)*(b-a) = " << (a+b)*(b-a) << std::endl;
        std::cout << "a*a-b*b     = " << c2 << std::endl;
        std::cout << "square(a)   = " << square(a) << std::endl;
        std::cout << "square(b)   = " << square(b) << std::endl;
        std::cout << "square(-a)  = " << square(-a) << std::endl;
        std::cout << "square(-b)  = " << square(-b) << std::endl;
        std::cout << "Failed" << std::endl;
        return 1;
    }

    timing = (clong + clock() - clk)/(double)CLOCKS_PER_SEC;
    std::cout << "Plus and Times tests completed in "
              << timing << " sec" << std::endl;

#endif // TEST_PLUS_AND_TIMES


#ifdef TEST_DIVISION

// For division testing I start by generating random numbers that will
// be my divisor, quotient and remainder. I derive a dividend from them
// and then combine those values to get a dividend. I then check that
// the division and remainder operations recover my original values.
// I have a function that returns both quotient and remainder, but I do not
// have an extra test of that here (yet).

    maxbits = 400;
    ntries = 50*MILLION;

    std::cout << "Start of division testing" << std::endl;
    clk = clock(); clong = 0;

    for (int i=1; i<=ntries; i++)
    {   if ((i & 0xfffff) == 0)
        {   clock_t now = clock();
            clong += now - clk;
            clk = now;
        }
        Bignum divisor, remainder, quotient;
        do
        {   divisor = random_upto_bits_bignum(maxbits) + 1;
            remainder = uniform_upto_bignum(divisor);
            quotient = random_upto_bits_bignum(maxbits);
            uint64_t rr = mersenne_twister();
            divisor = fudge_distribution_bignum(divisor, (int)(rr & 0xf));
            remainder = fudge_distribution_bignum(remainder, (int)((rr>>4) & 0xf));
            quotient = fudge_distribution_bignum(quotient, (int)((rr>>8) & 0xf));
// While I still want my strange distribution of numbers for testing, I
// need the sign of my target remainder to be proper, so I will generate
// random inputs until that is so. Also when I adjust the numbers I could
// reduce the divisor more than the remainder so that the remainder was
// invalid in magnitude... so I need to discard those cases too. It is
// plausible that this means I will discard around 75% of the sets of random
// numberfs that I initially generate.
        } while (((quotient ^ remainder ^ divisor) < Bignum(0)) ||
                 (abs(remainder) >= abs(divisor))); 

        Bignum dividend = quotient*divisor + remainder;
        Bignum q1 = dividend / divisor;
        Bignum r1 = 999999;
// If the quotient is incorrect I will not compute the remainder.
        if (q1 == quotient)
        {   r1 = dividend % divisor;
            if (r1 == remainder) continue;
        }
        std::cout << "FAILED on test " << i << std::endl;
        std::cout << "divisor   " << divisor << std::endl;
        std::cout << "remainder " << remainder << std::endl;
        std::cout << "quotient  " << quotient << std::endl;
        std::cout << "dividend  " << dividend << std::endl;
        std::cout << "q1        " << q1 << std::endl;
        std::cout << "r1        " << r1 << std::endl;
        display("dividend ", dividend);
        display("divisor  ", divisor);
        display("remainder", remainder);
        display("quotient ", quotient);
        display("q1       ", q1);
        display("r1       ", r1);
        std::cout << "Failed " << std::endl;
        return 1;
    }

    timing = (clong + clock() - clk)/(double)CLOCKS_PER_SEC;
    std::cout << "Division tests completed in "
              << timing << " sec" << std::endl;

#endif // TEST_DIVISION

#ifdef TEST_ISQRT

// For various inputs I compute isqrt() and verify that the square of
// that value is no greater than my original input, while if I add one
// and square I get something larger.

    maxbits = 900;
    ntries = 50*MILLION;

    std::cout << "Start of isqrt testing" << std::endl;
    clk = clock(); clong = 0;

    for (int i=1; i<=ntries; i++)
    {   if ((i & 0xfffff) == 0)
        {   clock_t now = clock();
            clong += now - clk;
            clk = now;
        }
        Bignum a, b;
        a = random_upto_bits_bignum(maxbits);
        uint64_t r = mersenne_twister();
        a = fudge_distribution_bignum(a, (int)r & 7);
        b = isqrt(a);
        if (square(b) <= a && square(b+1) > a) continue;
        std::cout << "FAILED on test " << i << std::endl;
        std::cout << "a         " << a << std::endl;
        std::cout << "b         " << b << std::endl;
        std::cout << "b^2       " << square(b) << std::endl;
        std::cout << "(b+1)^2   " << square(b+1) << std::endl;
        display("a", a);
        display("b", b);
        std::cout << "Failed " << std::endl;
        return 1;
    }

    timing = (clong + clock() - clk)/(double)CLOCKS_PER_SEC;
    std::cout << "Isqrt tests completed in "
              << timing << " sec" << std::endl;

#endif // TEST_ISQRT

#ifdef TEST_FLOAT

// To test FLOAT and FIX I will generate a random integer and convery to
// floating point. I then look at floating point values just larger and
// just smaller than the one that I obtained, and verify that my result
// is closer to the original input than the other wto. If there is a tie
// I will expect my value to have its least significant bit zero.

    maxbits = 500;
    ntries = 50*MILLION;

// On some systems (notable 32-bit cygwin and at least some older
// 32-bit Linux systems running on x86) floating point arithmetic is
// performed in 80-bit working precision, so some of my attempts here
// to check results are thwarted. I cope with that by forcing important
// values through the following volatile variable.
    volatile double fp_forcer;

    std::cout << "Start of float testing" << std::endl;
    clk = clock(); clong = 0;

    for (int i=1; i<=ntries; i++)
    {   if ((i & 0xfffff) == 0)
        {   clock_t now = clock();
            clong += now - clk;
            clk = now;
        }
        Bignum a, b;
        a = random_upto_bits_bignum(maxbits);
        uint64_t r = mersenne_twister();
        a = fudge_distribution_bignum(a, (int)r & 15);

        double d = double_bignum(a);
        Bignum n = fix_bignum(d);
        if (a == n) continue; // round trip was exact!

        fp_forcer = d + 1.0;
        double dplus = fp_forcer;
        if (dplus == d) dplus = std::nextafter(d, 1.0e300);
        my_assert(dplus != d);
        fp_forcer = d - 1.0;
        double dminus = fp_forcer;
        if (dminus == d) dminus = std::nextafter(d, -1.0e300);
        my_assert(dminus != d);
        Bignum nplus = fix_bignum(dplus);
        Bignum nminus = fix_bignum(dminus);
        Bignum err = a-n;
        Bignum errplus = a-nplus;
        Bignum errminus = a-nminus;
    
        if (nplus != n && nminus != n)
        {   if (abs(err) < abs(errplus) &&
                abs(err) < abs(errminus)) continue;
            if (abs(err) == abs(errplus) &&
                abs(err) < abs(errminus) && evenfloat(d)) continue;
            if (abs(err) < abs(errplus) &&
                abs(err) == abs(errminus) && evenfloat(d)) continue;
        }

        std::cout << "FAILED on test " << i << std::endl;
        std::cout << "a       " << a << std::endl;
        std::cout << "d       " << std::setprecision(19) << d << std::endl;
        std::cout << "d-      " << dminus << std::endl;
        std::cout << "d+      " << dplus << std::endl;
        std::cout << "nminus  " << nminus << std::endl;
        std::cout << "n       " << n << std::endl;
        std::cout << "nplus   " << nplus << std::endl;
        std::cout << "err-    " << errminus << std::endl;
        std::cout << "err     " << err << std::endl;
        std::cout << "err+    " << errplus << std::endl;
        display("a", a);
        display("n", n);
        std::cout << "Failed " << std::endl;
        return 1;
    }

    timing = (clong + clock() - clk)/(double)CLOCKS_PER_SEC;
    std::cout << "Float tests completed in "
              << timing << " sec" << std::endl;

#endif // TEST_FLOAT

    std::cout << "About to exit" << std::endl;
    return 0;    
}

// end of arithtest.cpp
