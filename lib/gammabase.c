#include "xmath.h"

#define TRUE 1
#define FALSE 0

extern double gamma(), ppnd();
static double gammp(), gser(), gcf(), gnorm(), ppchi2();

gammabase(x, a, p)
     double *x, *a, *p;
{
  *p = gammp(*a, *x);
}

double ppgamma(p, a, ifault)
     double p, a;
     int *ifault;
{
  double x, v, g;

  v = 2.0 * a;
  g = gamma(a);
  x = ppchi2(&p, &v, &g, ifault);
  return(x / 2.0);
}

/*
  Static Routines
*/

/*
  From Numerical Recipes, with normal approximation from Appl. Stat. 239
*/

#define EPSILON 1.0e-14
#define LARGE_A 10000.0
#define ITMAX 1000

static double gammp(a, x)
     double a, x;
{
  double gln, p;

  if (x <= 0.0 || a <= 0.0) p = 0.0;
  else if (a > LARGE_A) p = gnorm(a, x);
  else {
    gln = gamma(a);
    if (x < (a + 1.0)) p = gser(a, x, gln);
    else p = 1.0 - gcf(a, x, gln);
  }
  return(p);
}

/* compute gamma cdf by a normal approximation */
static double gnorm(a, x)
     double a, x;
{
  double p, sx;

  if (x <= 0.0 || a <= 0.0) p = 0.0;
  else {
    sx = sqrt(a) * 3.0 * (pow(x / a, 1.0 / 3.0) + 1.0 / (a * 9.0) - 1.0);
    normbase(&sx, &p);
  }
  return(p);
}  
  
/* compute gamma cdf by its series representation */
static double gser(a, x, gln)
     double a, x, gln;
{
  double p, sum, del, ap;
  int n, done = FALSE;

  if (x <= 0.0 || a <= 0.0) p = 0.0;
  else {
    ap = a;
    del = sum = 1.0 / a;
    for (n = 1; ! done && n < ITMAX; n++) {
      ap += 1.0;
      del *= x / ap;
      sum += del;
      if (fabs(del) < EPSILON) done = TRUE;
    }
    p = sum * exp(- x + a * log(x) - gln);
  }
  return(p);
}

/* compute complementary gamma cdf by its continued fraction expansion */
static double gcf(a, x, gln)
     double a, x, gln;
{
  double gold = 0.0, g, fac = 1.0, b1 = 1.0;
  double b0 = 0.0, anf, ana, an, a1, a0 = 1.0;
  double p;
  int done = FALSE;

  a1 = x;
  p = 0.0;
  for(an = 1.0; ! done && an <= ITMAX; an += 1.0) {
    ana = an - a;
    a0 = (a1 + a0 * ana) * fac;
    b0 = (b1 + b0 * ana) * fac;
    anf = an * fac;
    a1 = x * a0 + anf * a1;
    b1 = x * b0 + anf * b1;
    if (a1 != 0.0) {
      fac = 1.0 / a1;
      g = b1 * fac;
      if (fabs((g - gold) / g) < EPSILON) {
        p = exp(-x + a * log(x) - gln) * g;
        done = TRUE;
      }
      gold = g;
    }
  }
  return(p);
}

static double gammad(x, a, iflag)
     double *x, *a;
     int *iflag;
{
  double cdf;

  gammabase(x, a, &cdf);
  return(cdf);
}

/*
  ppchi2.f -- translated by f2c and modified

  Algorithm AS 91   Appl. Statist. (1975) Vol.24, P.35
  To evaluate the percentage points of the chi-squared
  probability distribution function.
  
  p must lie in the range 0.000002 to 0.999998,
    (but I am using it for 0 < p < 1 - seems to work)
  v must be positive,
  g must be supplied and should be equal to ln(gamma(v/2.0)) 
  
  Auxiliary routines required: ppnd = AS 111 (or AS 241) and gammad.
*/

static double ppchi2(p, v, g, ifault)
     double *p, *v, *g;
     int *ifault;
{
  /* Initialized data */

  static double aa = .6931471806;
  static double six = 6.;
  static double c1 = .01;
  static double c2 = .222222;
  static double c3 = .32;
  static double c4 = .4;
  static double c5 = 1.24;
  static double c6 = 2.2;
  static double c7 = 4.67;
  static double c8 = 6.66;
  static double c9 = 6.73;
  static double e = 5e-7;
  static double c10 = 13.32;
  static double c11 = 60.;
  static double c12 = 70.;
  static double c13 = 84.;
  static double c14 = 105.;
  static double c15 = 120.;
  static double c16 = 127.;
  static double c17 = 140.;
  static double c18 = 1175.;
  static double c19 = 210.;
  static double c20 = 252.;
  static double c21 = 2264.;
  static double c22 = 294.;
  static double c23 = 346.;
  static double c24 = 420.;
  static double c25 = 462.;
  static double c26 = 606.;
  static double c27 = 672.;
  static double c28 = 707.;
  static double c29 = 735.;
  static double c30 = 889.;
  static double c31 = 932.;
  static double c32 = 966.;
  static double c33 = 1141.;
  static double c34 = 1182.;
  static double c35 = 1278.;
  static double c36 = 1740.;
  static double c37 = 2520.;
  static double c38 = 5040.;
  static double zero = 0.;
  static double half = .5;
  static double one = 1.;
  static double two = 2.;
  static double three = 3.;

/*
  static double pmin = 2e-6;
  static double pmax = .999998;
*/
  static double pmin = 0.0;
  static double pmax = 1.0;

  /* System generated locals */
  double ret_val, d_1, d_2;
  
  /* Local variables */
  static double a, b, c, q, t, x, p1, p2, s1, s2, s3, s4, s5, s6, ch;
  static double xx;
  static int if1;


  /* test arguments and initialise */
  ret_val = -one;
  *ifault = 1;
  if (*p <= pmin || *p >= pmax) return ret_val;
  *ifault = 2;
  if (*v <= zero) return ret_val;
  *ifault = 0;
  xx = half * *v;
  c = xx - one;

  if (*v < -c5 * log(*p)) {
    /* starting approximation for small chi-squared */
    ch = pow(*p * xx * exp(*g + xx * aa), one / xx);
    if (ch < e) {
      ret_val = ch;
      return ret_val;
    }
  }
  else if (*v > c3) {
    /* call to algorithm AS 111 - note that p has been tested above. */
    /* AS 241 could be used as an alternative. */
    x = ppnd(*p, &if1);

    /* starting approximation using Wilson and Hilferty estimate */
    p1 = c2 / *v;
    /* Computing 3rd power */
    d_1 = x * sqrt(p1) + one - p1;
    ch = *v * (d_1 * d_1 * d_1);

    /* starting approximation for p tending to 1 */
    if (ch > c6 * *v + six)
      ch = -two * (log(one - *p) - c * log(half * ch) + *g);
  }
  else{
    /* starting approximation for v less than or equal to 0.32 */
    ch = c4;
    a = log(one - *p);
    do {
      q = ch;
      p1 = one + ch * (c7 + ch);
      p2 = ch * (c9 + ch * (c8 + ch));
      d_1 = -half + (c7 + two * ch) / p1;
      d_2 = (c9 + ch * (c10 + three * ch)) / p2;
      t = d_1 - d_2;
      ch -= (one - exp(a + *g + half * ch + c * aa) * p2 / p1) / t;
    } while (fabs(q / ch - one) > c1);
  }

  do {
    /* call to gammad and calculation of seven term Taylor series */
    q = ch;
    p1 = half * ch;
    p2 = *p - gammad(&p1, &xx, &if1);
    if (if1 != 0) {
      *ifault = 3;
      return ret_val;
    }
    t = p2 * exp(xx * aa + *g + p1 - c * log(ch));
    b = t / ch;
    a = half * t - b * c;
    s1 = (c19 + a * (c17 + a * (c14 + a * (c13 + a * (c12 + c11 * a))))) / c24;
    s2 = (c24 + a * (c29 + a * (c32 + a * (c33 + c35 * a)))) / c37;
    s3 = (c19 + a * (c25 + a * (c28 + c31 * a))) / c37;
    s4 = (c20 + a * (c27 + c34 * a) + c * (c22 + a * (c30 + c36 * a))) / c38;
    s5 = (c13 + c21 * a + c * (c18 + c26 * a)) / c37;
    s6 = (c15 + c * (c23 + c16 * c)) / c38;
    d_1 = (s3 - b * (s4 - b * (s5 - b * s6)));
    d_1 = (s1 - b * (s2 - b * d_1));
    ch += t * (one + half * t * s1 - b * c * d_1);
  } while (fabs(q / ch - one) > e);

  ret_val = ch;
  return ret_val;
}
