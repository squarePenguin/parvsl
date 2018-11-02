// I need to make copies of both numerator and denominator here because
// both get scaled. So w is passed as temporary workspace, and q and r as
// places where the quotient and remainder will end up - but r has to start
// of one word longer than the numerator a.

static void bigmultiply_by_int_in_place(unsigned64_t *r, size_t lenr,
                                 uint64_t scale, uint64_t &extra);
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
    mv_2 = a;
    bigabsval(a, lena, r, lenr);
    int sign = 0;
    if (negative(a[lena-1]))
        sign = SIGN_QUOTIENT_NEGATIVE | SIGN_REMAINDER_NEGATIVE;
    if (negative(b[lenb-1]))
        sign ^= SIGN_QUOTIENT_NEGATIVE;
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
    size_t m = lenq;
    for (;;)
    {   uint32_t q = next_quotient_digit(
            bignum_digits(a)[lena+1], a, lena,
            b, lenb);
        bignum_digits(big_quotient)[m] = q;
        if (m == 0) break;
        m--;
    }
// Unscale and correct the signs.
    if ((need & QUOTBB_REMAINDER_NEEDED) != 0)
    {   lena = unscale(a, lena+1, scale);
        if (sign & SIGN_REMAINDER_NEGATIVE)
            lena = negate_in_place(a, lena);
    }
    if ((need & QUOTBB_QUOTIENT_NEEDED) != 0)
// Ensure that the quotient has a prefix zero digit if needbe.
    {   lenq = fix_up_bignum_length(big_quotient, lenq);
        if (sign & SIGN_QUOTIENT_NEGATIVE)
            lenq = negate_in_place(big_quotient, lenq);
    }
// Now I need to pack the results so that they are suitable for use
// elsewhere in the system. 
    if ((need & QUOTBB_REMAINDER_NEEDED) != 0)
        mv_2 = pack_up_result(a, lena);
    if ((need & QUOTBB_QUOTIENT_NEEDED) != 0)
        return pack_up_result(big_quotient, lenq);
    else return nil;
}

