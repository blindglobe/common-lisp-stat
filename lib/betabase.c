#include "xmath.h"

#define TRUE 1
#define FALSE 0

/* external function */
extern double macheps(), gamma();

/* forward declarations */
static double logbeta(), betai(), xinbta();

betabase(x, a, b, gia, gib, cdf)
     int *gia, *gib;
     double *x, *a, *b, *cdf;
{
  *cdf = betai(*x, *a, *b);
}

double ppbeta(p, a, b, ifault)
     double p, a, b;
     int *ifault;
{
  double lbeta;

  lbeta = gamma(a) + gamma(b) - gamma(a + b);
  return(xinbta(&a, &b, &lbeta, &p, ifault));
}

/*
  Static routines
*/

static double logbeta(p, q)
     double p, q;
{
  return(gamma(p) + gamma(q) - gamma(p + q));
}

#define Min(x,y) (((x) < (y)) ? (x) : (y))
#define Max(x,y) (((x) > (y)) ? (x) : (y))

static double betai(x, pin, qin)
     double x, pin, qin;
{
  /* Translated from FORTRAN
     july 1977 edition.  w. fullerton, c3, los alamos scientific lab.
     based on bosten and battiste, remark on algorithm 179, comm. acm,
     v 17, p 153, (1974).
     
     input arguments --
     x      upper limit of integration.  x must be in (0,1) inclusive.
     p      first beta distribution parameter.  p must be gt 0.0.
     q      second beta distribution parameter.  q must be gt 0.0.
     betai  the incomplete beta function ratio is the probability that a
            random variable from a beta distribution having parameters
            p and q will be less than or equal to x.
  */
  double c, finsum, p, ps, q, term, xb, xi, y, dbetai, p1;
  static double eps = 0.0, alneps = 0.0, sml = 0.0, alnsml = 0.0;
  int i, n, ib;

  /* I'm not sure these tolerances are optimal */
  if (eps == 0.0) {
    eps = macheps();
    alneps = log(eps);
    sml = macheps();
    alnsml = log(sml);
  }

  y = x;
  p = pin;
  q = qin;
  if (q > p || x >= 0.8)
    if (x >= 0.2) {
      y = 1.0 - y;
      p = qin;
      q = pin;
    }

  if ((p + q) * y / (p + 1.0) < eps) {
    dbetai = 0.0;
    xb = p * log(Max(y, sml)) - log(p) - logbeta(p, q);
    if (xb > alnsml && y != 0.0) dbetai = exp(xb);
    if (y != x || p != pin) dbetai = 1.0 - dbetai;
  }
  else {
    /*
     *  evaluate the infinite sum first.  term will equal
     *  y**p/beta(ps,p) * (1.-ps)-sub-i * y**i / fac(i) .
     */
    ps = q - floor(q);
    if (ps == 0.0) ps = 1.0;
    xb = p * log(y) - logbeta(ps, p) - log(p);
    dbetai = 0.0;
    if (xb >= alnsml) {

      dbetai = exp(xb);
      term = dbetai * p;
      if (ps != 1.0) {
        n = Max(alneps / log(y), 4.0);
        for (i = 1; i <= n; i++) {
          xi = i;
          term = term * (xi - ps) * y / xi;
          dbetai = dbetai + term / (p + xi);
        }
      }
    }
    /*
     * now evaluate the finite sum, maybe.
     */
    if (q > 1.0) {

      xb = p * log(y) + q * log(1.0 - y) - logbeta(p,q) - log(q);
      ib = Max(xb / alnsml, 0.0);
      term = exp(xb - ((float) ib) * alnsml);
      c = 1.0 / (1.0 - y);
      p1 = q * c / (p + q - 1.0);

      finsum = 0.0;
      n = q;
      if (q == (float) n) n = n - 1;
      for (i = 1; i <= n; i++) {
        if (p1 <= 1.0 && term / eps <= finsum) break;
        xi = i;
        term = (q - xi + 1.0) * c * term / (p + q - xi);

        if (term > 1.0) ib = ib - 1;
        if (term > 1.0) term = term * sml;

        if (ib==0) finsum = finsum + term;
      }

      dbetai = dbetai + finsum;
    }
    if (y != x || p != pin) dbetai = 1.0 - dbetai;
    dbetai = Max(Min(dbetai, 1.0), 0.0);
  }
  return(dbetai);
}

/*
  xinbta.f -- translated by f2c and modified
  
  algorithm as 109 appl. statist. (1977), vol.26, no.1
  (replacing algorithm as 64  appl. statist. (1973), vol.22, no.3)

  Remark AS R83 has been incorporated in this version.

  Computes inverse of the incomplete beta function
  ratio for given positive values of the arguments
  p and q, alpha between zero and one.
  log of complete beta function, beta, is assumed to be known.

  Auxiliary function required: betai

  SAE below is the most negative decimal exponent which does not
  cause an underflow; a value of -308 or thereabouts will often be
*/

static double xinbta(p, q, beta, alpha, ifault)
     double *p, *q, *beta, *alpha;
     int *ifault;
{
  /* Initialized data */
  static double sae = -30.0; /* this should be sufficient */
  static double zero = 0.0;
  static double one = 1.0;
  static double two = 2.0;
  static double three = 3.0;
  static double four = 4.0;
  static double five = 5.0;
  static double six = 6.0;

  /* System generated locals */
  double ret_val, d_1, d_2;

  /* Local variables */
  static int indx;
  static double prev, a, g, h, r, s, t, w, y, yprev, pp, qq;
  static double sq, tx, adj, acu;
  static int iex;
  static double fpu, xin;

  /* Define accuracy and initialise. */
  fpu = sae * 10.;
  ret_val = *alpha;

  /* test for admissibility of parameters */
  *ifault = 1;
  if (*p <= zero || *q <= zero) return ret_val;
  *ifault = 2;
  if (*alpha < zero || *alpha > one) return ret_val;
  *ifault = 0;
  if (*alpha == zero || *alpha == one) return ret_val;

  /* change tail if necessary */
  if (*alpha <= .5) {
    a = *alpha;
    pp = *p;
    qq = *q;
    indx = FALSE;
  }
  else {
    a = one - *alpha;
    pp = *q;
    qq = *p;
    indx = TRUE;
  }

  /* calculate the initial approximation */
  r = sqrt(-log(a * a));
  y = r - (r * .27061 + 2.30753) / (one + (r * .04481 + .99229) * r);
  if (pp > one && qq > one) {
    r = (y * y - three) / six;
    s = one / (pp + pp - one);
    t = one / (qq + qq - one);
    h = two / (s + t);
    d_1 = y * sqrt(h + r) / h;
    d_2 = (t - s) * (r + five / six - two / (three * h));
    w = d_1 - d_2;
    ret_val = pp / (pp + qq * exp(w + w));
  }
  else {
    r = qq + qq;
    t = one / (qq * 9.);
    /* Computing 3rd power */
    d_1 = one - t + y * sqrt(t);
    t = r * (d_1 * d_1 * d_1);
    if (t <= zero) {
      ret_val = one - exp((log((one - a) * qq) + *beta) / qq);
    }
    else {
      t = (four * pp + r - two) / t;
      if (t <= one) ret_val = exp((log(a * pp) + *beta) / pp);
      else ret_val = one - two / (t + one);
    }
  }


  /* 
    solve for x by a modified newton-raphson method, using the function betai
  */
  r = one - pp;
  t = one - qq;
  yprev = zero;
  sq = one;
  prev = one;
  if (ret_val < 1e-4) ret_val = 1e-4;
  if (ret_val > .9999) ret_val = .9999;
  /* Computing MAX, two 2nd powers */
  d_1 = -5.0 / (pp * pp) - 1.0 / (a * a) - 13.0;
  iex = (sae > d_1) ? sae : d_1;
  acu = pow(10.0, (double) iex);
  do {
    y = betai(ret_val, pp, qq);
    if (*ifault != 0) {
      *ifault = 3;
      return ret_val;
    }
    xin = ret_val;
    y = (y - a) * exp(*beta + r * log(xin) + t * log(one - xin));
    if (y * yprev <= zero) {
      prev = (sq > fpu) ? sq : fpu;
    }
    g = one;
    do {
      adj = g * y;
      sq = adj * adj;
      if (sq < prev) {
        tx = ret_val - adj;
        if (tx >= zero && tx <= one) {
          if (prev <= acu || y * y <= acu) {
            if (indx) ret_val = one - ret_val;
            return ret_val;
          }
          if (tx != zero && tx != one) break;
        }
      }
      g /= three;
    } while (TRUE);
    if (tx == ret_val) {
      if (indx) ret_val = one - ret_val;
      return ret_val;
    }
    ret_val = tx;
    yprev = y;
  } while (TRUE);
  return ret_val;
}
