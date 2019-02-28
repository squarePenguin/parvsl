#ifdef CRLIBM

// crlibm aims to produce correctly rounded results in all cases.
// The functions from it selected here are the ones that round to
// nearest. Using this should guarantee constent floating point results
// across al platforms.

extern "C"
{
#include "crlibm.h"
}

// sqrt is not provided by crlibm, probably because it believes that
// most other libraries will get that case exactly right anyway.

double SQRT(double a)  { return  sqrt(a); }
double TANH(double x)  { return  tanh(x); }

double SIN(double a)   { return   sin_rn(a); }
double COS(double a)   { return   cos_rn(a); }
double TAN(double a)   { return   tan_rn(a); }
double SINH(double a)  { return  sinh_rn(a); }
double COSH(double a)  { return  cosh_rn(a); }
double ASIN(double a)  { return  asin_rn(a); }
double ACOS(double a)  { return  acos_rn(a); }
double ATAN(double a)  { return  atan_rn(a); }
double EXP(double a)   { return   exp_rn(a); }
double LOG(double a)   { return   log_rn(a); }
double LOG2(double a)  { return  log2_rn(a); }
double LOG10(double a) { return log10_rn(a); }
double POW(double a, double b)   { return   pow_rn(a, b); }

#else // CRLIBM

double SQRT(double a)  { return  sqrt(a); }
double TANH(double x)  { return  tanh(x); }

double SIN(double a)   { return   sin(a); }
double COS(double a)   { return   cos(a); }
double TAN(double a)   { return   tan(a); }
double SINH(double a)  { return  sinh(a); }
double COSH(double a)  { return  cosh(a); }
double ASIN(double a)  { return  asin(a); }
double ACOS(double a)  { return  acos(a); }
double ATAN(double a)  { return  atan(a); }
double EXP(double a)   { return   exp(a); }
double LOG(double a)   { return   log(a); }
double LOG2(double a)  { return  log2(a); }
double LOG10(double a) { return log10(a); }
double POW(double a, double b)   { return   pow(a, b); }

#endif // CRLIBM

// There are a load of elementary functions not provided by standard
// libraries. I deal with that here.

static const double _pi = 3.14159265358979323846;
static const double _log_2 = 0.6931471805599453094;
static const double _half_pi = ((12868.0 - 0.036490896206895257)/8192.0);

double ASINH(double x)
{   bool sign;
    if (x < 0.0) x = -x, sign = true;
    else sign = false;
    if (x < 1.0e-3)
    {   double xx = x*x;
        x = x*(1 - xx*((1.0/6.0) - (3.0/40.0)*xx));
    }
    else if (x < 1.0e9)
    {   x += sqrt(1.0 + x*x);
        x = LOG(x);
    }
    else x = LOG(x) + _log_2;
    if (sign) x = -x;
    return x;
}

static double acosh_coeffs[] =
{   -0.15718655513711019382e-5,          // x^11
    +0.81758779765416234142e-5,          // x^10
    -0.24812280287135584149e-4,          // x^9
    +0.62919005027033514743e-4,          // x^8
    -0.15404104307204835991e-3,          // x^7
    +0.38339903706706128921e-3,          // x^6
    -0.98871347029548821795e-3,          // x^5
    +0.26854094489454297811e-2,          // x^4
    -0.78918167367399344521e-2,          // x^3
    +0.26516504294146930609e-1,          // x^2
    -0.11785113019775570984,             // x
    +1.41421356237309504786              // 1

};

double ACOSH(double x)
{   bool sign;
    if (x < -1.0) x = -x, sign = true;
    else if (1.0 < x) sign = false;
    else return 0.0;
    if (x < 1.5)
    {   int i;
        double r = acosh_coeffs[0];
        x = (x - 0.5) - 0.5;
//
// This is a minimax approximation to acosh(1+x)/sqrt(x) over the
// range x=0 to 0.5
//
        for (i=1; i<=11; i++) r = x*r + acosh_coeffs[i];
        x = sqrt(x)*r;
    }
    else if (x < 1.0e9)
    {   x += sqrt((x - 1.0)*(x + 1.0));
        x = LOG(x);
    }
    else x = LOG(x) + _log_2;
    if (sign) return -x;
    else return x;
}

double ATANH(double z)
{   if (z > -0.01 && z < -0.01)
    {   double zz = z*z;
        return z * (1 + zz*((1.0/3.0) + zz*((1.0/5.0) + zz*(1.0/7.0))));
    }
    z = (1.0 + z) / (1.0 - z);
    if (z < 0.0) z = -z;
    return LOG(z) / 2.0;
}

static const double n180pi = 57.2957795130823208768;   // 180/pi
static const double pi180  =  0.017453292519943295769; // pi/180

double arg_reduce_degrees(double a, int *quadrant)
//
// Reduce argument to the range -45 to 45, and set quadrant to the
// relevant quadant.  Returns arg converted to radians.
//
{   double w = a / 90.0;
    int32_t n = (int)w;
    w = a - 90.0*n;
    while (w < -45.0)
    {   n--;
        w = a - 90.0*n;
    }
    while (w >= 45.0)
    {   n++;
        w = a - 90.0*n;
    }
    *quadrant = (int)(n & 3);
    return pi180*w;
}

double SIND(double a)
{   int quadrant;
    a = arg_reduce_degrees(a, &quadrant);
    switch (quadrant)
    {   default:
        case 0: return SIN(a);
        case 1: return COS(a);
        case 2: return SIN(-a);
        case 3: return -COS(a);
    }
}

double COSD(double a)
{   int quadrant;
    a = arg_reduce_degrees(a, &quadrant);
    switch (quadrant)
    {   default:
        case 0: return COS(a);
        case 1: return SIN(-a);
        case 2: return -COS(a);
        case 3: return SIN(a);
    }
}

double TAND(double a)
{   int quadrant;
    a = arg_reduce_degrees(a, &quadrant);
    switch (quadrant)
    {   default:
        case 0:
        case 2: return TAN(a);
        case 1:
        case 3: return 1.0/TAN(-a);
    }
}

static double COTD(double a)
{   int quadrant;
    a = arg_reduce_degrees(a, &quadrant);
    switch (quadrant)
    {   default:
        case 0:
        case 2: return 1.0/TAN(a);
        case 1:
        case 3: return TAN(-a);
    }
}

double ACOT(double a)
{   if (a >= 0.0)
        if (a > 1.0) return ATAN(1.0/a);
        else return _half_pi - ATAN(a);
    else if (a < -1.0) return _pi - ATAN(-1.0/a);
    else return _half_pi + ATAN(-a);
}

double ACOTD(double a)
{   if (a >= 0.0)
        if (a > 1.0) return n180pi*ATAN(1.0/a);
        else return 90.0 - n180pi*ATAN(a);
    else if (a < -1.0) return 180.0 - n180pi*ATAN(-1.0/a);
    else return 90.0 + n180pi*ATAN(-a);
}

double CSC(double x)     { return 1.0/SIN(x); }
double SEC(double x)     { return 1.0/COS(x); }
double COT(double x)     { return 1.0/TAN(x); }
double CSCH(double x)    { return 1.0/SINH(x); }
double SECH(double x)    { return 1.0/COSH(x); }
double COTH(double x)    { return 1.0/TANH(x); }
double ACSC(double x)    { return ASIN(1.0/x); }
double ASEC(double x)    { return ACOS(1.0/x); }
double ACSCH(double x)   { return ASINH(1.0/x); }
double ASECH(double x)   { return ACOSH(1.0/x); }
double ACOTH(double x)   { return ATANH(1.0/x); }
double ASIND(double x)   { return (180.0/_pi)*ASIN(x); }
double ACOSD(double x)   { return (180.0/_pi)*ACOS(x); }
double ATAND(double x)   { return (180.0/_pi)*ATAN(x); }
double CSCD(double x)    { return 1.0/SIND(x); }
double SECD(double x)    { return 1.0/COSD(x); }
double ACSCD(double x)   { return ASIND(1.0/x); }
double ASECD(double x)   { return ACOSD(1.0/x); }


