static void bigmultiply_by_int_in_place(uint64_t *r, size_t lenr,
                                        uint64_t scale, uint64_t &extra)
{   uint64_t carry = 0;
    for (size_t i=0; i<lenr; i++)
    {   uint64_t hi, lo;
        multiply64(r[i], scale, hi, lo);
        uint64_t w = r[i] = lo + carry;
// NB that adding 1 to hi here can never overflow.
        if (w < carry) carry = hi + 1;
        else carry = hi;
    }
    extra = carry;
}

// divide (hi,lo) by divisor and generate a quotient and a remainder. The
// version of the code that is able to use __int128 can serve as clean
// documentation of the intent.

#ifdef __SIZEOF_INT128__

static inline void divide64(uint64_t hi, uint64_t lo, uint64_t divisor,
                            uint64_t &q, uint64_t &r)
{   UINT128 num = ((UINT128)hi << 64) | lo;
    q = num / divisor;
    r = num % divisor;
}

#else // __SIZEOF_INT128__

static uint64_t divide64(uint64_t hi, uint64_t low, uint64_t divisor,
                         uint64_t &q, uint64_t &r)
{   uint64_t u1 = hi;
    uint64_t u0 = lo;
    uint64_t c = divisor;
// See the Hacker's Delight for commentary about what follows. The associated
// web-site explains usage rights:
// "You are free to use, copy, and distribute any of the code on this web
// site (www.hackersdelight.org) , whether modified by you or not. You need
// not give attribution. This includes the algorithms (some of which appear
// in Hacker's Delight), the Hacker's Assistant, and any code submitted by
// readers. Submitters implicitly agree to this." and then "The author has
// taken care in the preparation of this material, but makes no expressed
// or implied warranty of any kind and assumes no responsibility for errors
// or omissions. No liability is assumed for incidental or consequential
// damages in connection with or arising out of the use of the information
// or programs contained herein."
// I may not be obliged to give attribution, but I view it as polite to!
// Any error that have crept in in my adapaptation of the original code
// will be my fault, but you see in the BSD license at the top of this
// file that I disclaim any possible liability for consequent loss or damage.
    const uint64_t base = 0x100000000U; // Number base (32 bits).
    uint64_t un1, un0,        // Norm. dividend LSD's.
             vn1, vn0,        // Norm. divisor digits.
             q1, q0,          // Quotient digits.
             un32, un21, un10,// Dividend digit pairs.
             rhat;            // A remainder.
// I am going to shift both operands left until the divisor has its
// most significant bit set.
    int s = nlz(c);           // Shift amount for norm. 0 <= s <= 63.
    c = c << s;               // Normalize divisor.
// Now I split the divisor from a single 64-bit number into a pair
// of 32-vit values.
    vn1 = c >> 32;            // Break divisor up into
    vn0 = c & 0xFFFFFFFFU;    // two 32-bit digits.
// Shift the dividend... and split it into parts.
    if (s == 0) un32 = u1;
    else un32 = (u1 << s) | (u0 >> (64 - s));
    un10 = u0 << s;           // Shift dividend left.
    un1 = un10 >> 32;         // Break right half of
    un0 = un10 & 0xFFFFFFFFU; // dividend into two digits.
// Predict a 32-bit quotient digit...
    q1 = un32/vn1;            // Compute the first
    rhat = un32 - q1*vn1;     // quotient digit, q1.
again1:
    if (q1 >= base || q1*vn0 > base*rhat + un1)
    {   q1 = q1 - 1;
        rhat = rhat + vn1;
        if (rhat < base) goto again1;
    }
    un21 = un32*base + un1 - q1*c;  // Multiply and subtract.
    q0 = un21/vn1;            // Compute the second
    rhat = un21 - q0*vn1;     // quotient digit, q0.
again2:
    if (q0 >= base || q0*vn0 > base*rhat + un0)
    {   q0 = q0 - 1;
        rhat = rhat + vn1;
        if (rhat < base) goto again2;
    }
    q = (q1 << 32) | q0;      // assemble and return quotient & remainder
    r = (un21*base + un0 - q0*c) >> s;
}

#endif // __SIZE_OF_INT128__

static uint64_t multiply_and_subtract(uint64_t *a, size_t lena,
                                      uint64_t q0,
                                      uint64_t *b, size_t lenb)
{   return 0;
}

static uint64_t add_back_correction(uint64_t *a, size_t lena,
                                    uint64_t *b, size_t lenb)
{   return 0;
}


static inline uint64_t next_quotient_digit(uint64_t atop,
                                           uint64_t *a, size_t &lena,
                                           uint64_t *b, size_t lenb)
{   UINT128 p0 = (UINT128)atop<<64 | a[lena-1];
    uint64_t q0 =  (uint64_t)(p0 / (UINT128)b[lenb-1]);
    uint64_t r0 =  (uint64_t)(p0 % (UINT128)b[lenb-1]);
// At this stage q0 may be correct or it may be an over-estimate by 1 or 2,
// but never any worse than that.
//
// The test on the next line should detect all case where q0 was in error
// by 2 and most when it was in error by 1.
//
    if (q0 == UINT64_C(0x8000000000000000) ||
        (UINT128)q0*(UINT128)b[lenb-2] >
        ((UINT128)r0<<64 | a[lena-2]))
        q0--;
//  trace_printf("Leading quotient digit = %d = %#x\n", q0, q0);
//
// Now I want to go "a = a - b*q0*2^(31*(lena-lenb));" so that a
// is set to an accurate remainder after using q0 as (part of) the
// quotient. This may carry an overshoot into atop and if so I will need
// to reduce q0 again and compensate.
//
    atop += multiply_and_subtract(a, lena, q0, b, lenb);
//  show1("sets a to ", atop, a, lena);
    if ((int64_t)atop < 0)
    {   q0--;
        atop = add_back_correction(a, lena, b, lenb);
// When I add back b I ought to get a carry...
        assert(atop == 1);
    }
    lena--;  // a is now one digit shorter.
    return q0;
}

static void negate_in_place(uint64_t *r, size_t &lenr)
{
}

static void unscale(uint64_t *r, size_t &lenr, uint64_t scale)
{
}

static void fix_up_bignum_length(uint64_t *q, size_t &lenq)
{
}


// I need to make copies of both numerator and denominator here because
// both forced positive and both get scaled. So w is passed as temporary
// workspace, and q and r as places where the quotient and remainder will
// end up - note r has to start of one word longer than the numerator a
// even though by the end it will be shorter than b.

void bigquotrem(uint64_t *a, size_t lena,
                uint64_t *b, size_t lenb,
                uint64_t *w, size_t lenw,   // temp - size lenb
                uint64_t *q, size_t &lenq,  // quotient - size lena-lenb+1
                uint64_t *r, size_t &lenr)  // remainder - size lena+1
{
// I copy the absolute values of a and b to places where it will be
// OK to overwrite them, taking their absolute values as I go. I record
// whether the eventual quotient and/or remainder will need to be negated
// at the end. This leaves the two inputs as rows of unsigned 64-bit digits
// with a[alen] and b[blen] both non-zero.
    bigabsval(a, lena, r, lenr);
    bool quot_sign = false, rem_sign = false;
    if (negative(a[lena-1]))
        quot_sign = rem_sign = true;
    if (negative(b[lenb-1])) quot_sign = !quot_sign;
// Now I want to compute the quotient of r by w, and both are positive.
// Taking absolute values might have changed the lengths, so this is a good
// place to check for a division where the quotient is unquestionably zero.
    if (lenr < lenw ||
        (lenr == lenw && r[lenr-1] < w[lenw-1]))
    {   q[0] = 0;
        lenq = 1;
        memcpy((void *)r, (void *)a, lena*sizeof(uint64_t));
        lenr = lena;
        return;
    } 
// By now a and b both have strictly positive leading digits.
    lenq = lena-lenb; // potential length of quotient.
// I will multiply a and b by a scale factor that gets the top digit of "b"
// reasonably large. The value stored in "a" can become one digit longer,
// but there is space to store that.
//
// The scale factor used here is as per Knuth II edition II. Edition III
// proposed 0x7fffffffU/bignum_digits(b)[lenb] and if you look at just the
// leading digit of b alone that seems OK, but I am concerned that when you
// take lower digits of b into account that multiplying b by it can overflow.
    uint64_t scale = UINT64_C(0x8000000000000000) / (w[lenw-1] + 1);
// When I scale the dividend expands into an extra digit but the scale
// factor has been chosen so that the divisor does not. So beware that
// r now has digits running from 0 to lenr rather than 0 to lenr-1.
    bigmultiply_by_int_in_place(r, lenr, scale, r[lenr]);
    uint64_t wtop;
    bigmultiply_by_int_in_place(w, lenw, scale, wtop);
    assert(wtop == 0);
// @@@@@@
    size_t m = lenq-1;
    for (;;)
    {   uint64_t qd = next_quotient_digit(
            r[lenr], r, lenr,
            b, lenb);
        q[m] = qd;
        if (m == 0) break;
        m--;
    }
// Unscale and correct the signs.
    unscale(r, lenr, scale);
    if (rem_sign) negate_in_place(r, lenr);
// Ensure that the quotient has r prefix zero digit if needbe.
    fix_up_bignum_length(q, lenq);
    if (quot_sign) negate_in_place(q, lenq);
// Now I need to pack the results so that they are suitable for use
// elsewhere in the system. 
//@@        mv_2 = pack_up_result(r, lenr);
//@@        return pack_up_result(q, lenq);
}

