#include "xmath.h"

extern void normbase(double *,double *);
extern void gammabase(double *,double *, double *);
extern void studentbase(double *,double *, double *);
extern void betabase(double *, double *,double *,int *, int *,double *);
extern int max(int, int);
extern int min(int, int);

extern void xlfail(char *);

extern double ppnd(), gamma(), bivnor(), uni(), ppgamma(), ppbeta(),
  ppstudent();

/* forward declaration */
extern double tdens();

#ifndef PI
#define PI 3.14159265358979323846
#endif /* PI*/
#define TRUE 1
#define FALSE 0

/*
 * Under ULTRIX 3.1 (the cc1.31 compilers in particular) the _G0 math
 * library does not really exist! You either need to figure out how
 * to get tan() and floor() in at load time for kcl, or use the ones
 * here.
 */

#ifdef mips
#ifdef UX31
double tan(x)
     double x;
{
  return(sin(x) / cos(x));
}
double floor(x)
     double x;
{
  long ix = x;
  double dx = ix;
  return((dx <= x) ? dx : dx - 1.0);
}
#endif
#endif

static void
checkflag(int flag)
{
  /* do nothing for now */
}

static void 
checkexp(double a)
{
  if (a <= 0.0) {
    xlfail("non-positive gamma or beta exponent");
  }
}

static void
checkdf(double df)
{
  if (df <= 0.0) { 
    xlfail("non-positive degrees of freedom");
  }
}

static void 
checkprob(double p, int zerostrict, int onestrict)
{
  if (zerostrict) {
    if (p <= 0.0) xlfail("non-positive probability argument");
  } else {
    if (p < 0.0) xlfail("negative probability argument");
  }
  if (onestrict) {
    if (p >= 1.0) xlfail("probability argument not less than one");
  } else {
    if (p > 1.0) xlfail("probability argument greater than one");
  }
}

static void
checkrho(double r)
{
  if (r < -1 || r > 1) {
    xlfail("correlation out of range");
  }
}

static void
checkpoisson(double L)
{
  if (L < 0.0) { 
    xlfail("negative Poisson mean");
  }
}

static void
checkbinomial(int n, double p)
{
  if (p < 0.0 || p > 1.0) xlfail("binomial p out of range");
  if (n < 1) xlfail("non-positive binomial n");
}

/*****************************************************************************/
/*****************************************************************************/
/**                                                                         **/
/**                          Uniform Distribution                           **/
/**                                                                         **/
/*****************************************************************************/
/*****************************************************************************/

/* uniform generator - avoids zero and one */
double unirand()
{
  double u;
  do {
    u = uni();
  } while ((u <= 0.0) || (u >= 1.0));
  return(u);
}

/*****************************************************************************/
/*****************************************************************************/
/**                                                                         **/
/**                          Normal Distribution                            **/
/**                                                                         **/
/*****************************************************************************/
/*****************************************************************************/

/* standard normal cdf */
double normalcdf(x)
     double x;
{
  double p;
  normbase(&x, &p);
  return(p);
}

/* standard normal quantile function */
double normalquant(p)
     double p;
{
  int flag;
  double x;

  checkprob(p, TRUE, TRUE);
  x = ppnd(p, &flag);
  checkflag(flag);
  return(x);
}

/* standard normal density */
double normaldens(x)
     double x;
{
  return(exp(- 0.5 * x * x) / sqrt(2.0 * PI));
}

/* standard normal generator */
double normalrand()
{
  double x, y, u, u1, v;
  static double c = -1.0;
   
  if (c < 0.0) c = sqrt(2.0 / exp(1.0));
   
  /* ratio of uniforms with linear pretest */
  do {
    u = unirand();
    u1 = unirand();
    v = c * (2 * u1 - 1);
    x = v / u;
    y = x * x / 4.0;
  } while(y > (1 - u) && y > - log(u));
  return(x);
}

/* bivariate normal cdf */
double bnormcdf(x, y, r)
  double x, y, r;
{
  checkrho(r);
  return(bivnor(-x, -y, r));
}

/*****************************************************************************/
/*****************************************************************************/
/**                                                                         **/
/**                          Cauchy Distribution                            **/
/**                                                                         **/
/*****************************************************************************/
/*****************************************************************************/

double
cauchycdf(double dx)
{
  return((atan(dx) + PI / 2) / PI);
}

double
cauchyquant(double p)
{
  checkprob(p, TRUE, TRUE);
  return(tan(PI * (p - 0.5)));
}

double
cauchydens(double dx)
{
  return(tdens(dx, 1.0));
}

/* cauchy generator */
double
cauchyrand()
{
  double u1, u2, v1, v2;
   
  /* ratio of uniforms on half disk */
  do {
    u1 = unirand();
    u2 = unirand();
    v1 = 2.0 * u1 - 1.0;
    v2 = u2;
  } while(v1 * v1 + v2 * v2 > 1.0);
  return(v1 / v2);
}

/*****************************************************************************/
/**                                                                         **/
/**                           Gamma Distribution                            **/
/**                                                                         **/
/*****************************************************************************/

double 
gammacdf(double x, double a)
{
  double p;

  checkexp(a);
  if (x <= 0.0) p = 0.0;
  else gammabase(&x, &a, &p);
  return(p);
}

double 
gammaquant(double p, double a)
{
  int flag;
  double x;

  checkexp(a);
  checkprob(p, FALSE, TRUE);
  x = ppgamma(p, a, &flag);
  checkflag(flag);
  return(x);
}

/* gamma density */
double gammadens(double x, double a)
{
  double dens;

  checkexp(a);
  if (x <= 0.0) { 
    dens = 0.0; 
  } else {
    dens = exp(log(x) * (a - 1) - x - gamma(a));
  }
  return(dens);
}

/* gamma generator */
double
gammarand(double a)
{
  double x, u0, u1, u2, v, w, c, c1, c2, c3, c4, c5;
  static double e = -1.0;
  int done;
  
  checkexp(a);
  if (e < 0.0) e = exp(1.0);
  
  if (a < 1.0) {
    /* Ahrens and Dieter algorithm */
    done = FALSE;
    c = (a + e) / e;
    do {
      u0 = unirand();
      u1 = unirand();
      v = c * u0;
      if (v <= 1.0) {
        x = exp(log(v) / a);
        if (u1 <= exp(-x)) done = TRUE;
      }
      else {
        x = -log((c - v) / a);
        if (x > 0.0 && u1 < exp((a - 1.0) * log(x))) done = TRUE;
      }
    } while(! done);
  }
  else if (a == 1.0) x = -log(unirand());
  else {
    /* Cheng and Feast algorithm */
    c1 = a - 1.0;
    c2 = (a - 1.0 / (6.0 * a)) / c1;
    c3 = 2.0 / c1;
    c4 = 2.0 / (a - 1.0) + 2.0;
    c5 = 1.0 / sqrt(a);
    do {
      do {
        u1 = unirand();
        u2 = unirand();
        if (a > 2.5) u1 = u2 + c5 * (1.0 - 1.86 * u1);
      } while (u1 <= 0.0 || u1 >= 1.0);
      w = c2 * u2 / u1;
    } while ((c3 * u1 + w + 1.0/w) > c4 && (c3 * log(u1) - log(w) + w) > 1.0);
    x = c1 * w;
  }
  return(x);
}

/*****************************************************************************/
/**                                                                         **/
/**                        Chi-Square Distribution                          **/
/**                                                                         **/
/*****************************************************************************/

double chisqcdf(double x, double df)
{
  double p, a;

  checkdf(df);
  a = 0.5 * df;
  x = 0.5 * x;
  if (x <= 0.0) {
    p = 0.0;
  } else {
    gammabase(&x, &a, &p);
  }
  return(p);
}

double
chisqquant(double p, double df)
{
  double x, a;
  int flag;

  checkdf(df);
  checkprob(p, FALSE, TRUE);
  a = 0.5 * df;
  x = 2.0 * ppgamma(p, a, &flag);
  checkflag(flag);
  return(x);
}

double
chisqdens(double dx, double da)
{
  checkdf(da);
  da = 0.5 * da;
  dx = 0.5 * dx;
  return(0.5 * gammadens(dx, da));
}

double
chisqrand(double df)
{
  checkdf(df);
  return(2.0 * gammarand(df / 2.0));
}

/*****************************************************************************/
/**                                                                         **/
/**                           Beta Distribution                             **/
/**                                                                         **/
/*****************************************************************************/

double
betacdf(double x, double a, double b)
{
  double p;
  int ia, ib;
  
  checkexp(a); checkexp(b);
  ia = a; ib = b;
  if (x <= 0.0) {
    p = 0.0;
  } else {
    if (x >= 1.0) {
      p = 1.0;
    } else {
      betabase(&x, &a, &b, &ia, &ib, &p);
    }
  }
  return(p);
}

double
betaquant(double p, double a, double b)
{
  double x;
  int flag;

  checkexp(a);
  checkexp(b);
  checkprob(p, FALSE, FALSE);
  x = ppbeta(p, a, b, &flag);
  checkflag(flag);
  return(x);
}
  
static double
logbeta(double a, double b)
{
  static double da = 0.0, db = 0.0, lbeta = 0.0;
  
  if (a != da || b != db) { /* cache most recent call */
    da = a; db = b;
    lbeta = gamma(da) + gamma(db) - gamma(da + db);
  }
  return(lbeta);
}

/* beta density */
double
betadens(double x, double a, double b)
{
  double dens;
  
  checkexp(a);
  checkexp(b);
  if (x <= 0.0 || x >= 1.0) {
    dens = 0.0;
  } else { 
    dens = exp(log(x) * (a - 1) + log(1 - x) * (b - 1) - logbeta(a, b));
  }
  return(dens);
}

/* beta generator */
double
betarand(double a, double b)
{
  double x, y;

  checkexp(a);
  checkexp(b);
  x = gammarand(a);
  y = gammarand(b);
  return(x / (x + y));
}

/*****************************************************************************/
/**                                                                         **/
/**                            t Distribution                               **/
/**                                                                         **/
/*****************************************************************************/

/* t cdf */
double
tcdf(double x, double df)
{
  double p;
  
  checkdf(df);
  studentbase(&x, &df, &p);
  return(p);
}

double
tquant(double p, double df)
{
  double x;
  int flag;
  
  checkdf(df);
  checkprob(p, TRUE, TRUE);
  x = ppstudent(p, df, &flag);
  checkflag(flag);
  return(x);
}

double
tdens(double x, double a)
{
  double dens;
  
  checkdf(a);
  dens = (1.0 / sqrt(a * PI)) 
       * exp(gamma(0.5 * (a + 1)) - gamma(0.5 * a) 
             - 0.5 * (a + 1) * log(1.0 + x * x / a));
  return(dens);
}

double trand(double df)
{
  checkdf(df);
  return(normalrand() / sqrt(chisqrand(df) / df));
}

/*****************************************************************************/
/**                                                                         **/
/**                            F Distribution                               **/
/**                                                                         **/
/*****************************************************************************/

double fcdf(double x, double ndf, double ddf)
{
  double p, a, b;

  checkdf(ndf); checkdf(ddf);
  a = 0.5 * ddf;
  b = 0.5 * ndf;
  if (x <= 0.0) {
    p = 0.0;
  } else {
    x = a / (a + b * x);
    p = 1.0 - betacdf(x, a, b);
  }
  return(p);
}

double fquant(double p, double ndf, double  ddf)
{
  double x, a, b;
  int flag;

  checkdf(ndf); checkdf(ddf);
  checkprob(p, FALSE, TRUE);
  a = 0.5 * ddf;
  b = 0.5 * ndf;
  if (p == 0.0) {
    x = 0.0;
  } else {
    p = 1.0 - p;
    x = ppbeta(p, a, b, &flag);
    checkflag(flag);
    x = a * (1.0 / x - 1.0) / b;
  }
  return(x);
}

double
fdens(double dx, double da, double db)
{
  double dens;

  checkdf(da);
  checkdf(db);
  if (dx <= 0.0) {
    dens = 0.0;
  } else {
    dens = exp(0.5 * da * log(da) + 0.5 * db *log(db)
	       + (0.5 * da - 1.0) * log(dx)
	       - logbeta(0.5 * da, 0.5 * db)
	       - 0.5 * (da + db) * log(db + da * dx));
  }
  return(dens);
}

/* f generator */
double frand(double ndf, double ddf)
{
  checkdf(ndf);
  checkdf(ddf);
  return((ddf * chisqrand(ndf)) / (ndf * chisqrand(ddf)));
}

/*****************************************************************************/
/**                                                                         **/
/**                         Poisson Distribution                            **/
/**                                                                         **/
/*****************************************************************************/

static double
poisson_cdf(int k, double L)
{
  double dp, dx;

  if (k < 0) {
    dp = 0.0;
  } else {
    if (L == 0.0) {
      dp = (k < 0) ? 0.0 : 1.0;
    } else {
      dx = k + 1.0;
      gammabase(&L, &dx, &dp);
      dp = 1.0 - dp;
    }
  }
  return(dp);
}

double
poissoncdf(double k, double L)
{
  checkpoisson(L);  
  return(poisson_cdf((int) floor(k), L));
}

int
poissonquant(double x, double L)
{
  int k, k1, k2, del, ia;
  double m, s, p1, p2, pk;
  
  checkpoisson(L);
  checkprob(x, FALSE, TRUE);
  m = L;
  s = sqrt(L);
  del = max(1, (int) (0.2 * s));
  
  if (x == 0.0) {
    k = 0.0;
  } else {
    k = m + s * ppnd(x, &ia);
  }
  k1 = k;
  k2 = k;
  
  do {
    k1 = k1 - del; k1 = max(0, k1);
    p1 = poisson_cdf(k1, L);
  } while (k1 > 0 && p1 > x);
  
  if (k1 == 0 && p1 >= x) {
    return(k1);
  }
  
  do {
    k2 = k2 + del;
    p2 = poisson_cdf(k2, L);
  } while (p2 < x);
  
  while (k2 - k1 > 1) {
    k = (k1 + k2) / 2;
    pk = poisson_cdf(k, L);
    if (pk < x) { 
      k1 = k; p1 = pk;
    } else { 
      k2 = k; p2 = pk;
    }
  }
  return(k2);
}

double
poissonpmf(int k, double L)
{
  double dx, dp;

  checkpoisson(L);
  dx = k;
  if (L == 0.0) {
    dp = (k == 0) ? 1.0 : 0.0;
  }  else {
    if (dx < 0.0) {
      dp = 0.0;
    } else {
      dp = exp(dx * log(L) - L - gamma(dx + 1.0));
    }
  }
  return(dp);
}

/* poisson random generator from Numerical Recipes */
int poissonrand(double xm)
{
  static double sqrt2xm, logxm, expxm, g, oldxm = -1.0;
  double t, y;
  int k;

  checkpoisson(xm);
  if (xm < 12.0) {
    if (xm != oldxm) { expxm = exp(-xm); oldxm = xm; }
    k = -1;
    t = 1.0;
    do {
      k++;
      t *= uni();
    } while (t > expxm);
  } else {
    if (xm != oldxm) {
      oldxm = xm;
      sqrt2xm = sqrt(2.0 * xm);
      logxm = log(xm);
      g = xm * logxm - gamma(xm + 1.0);
    }
    do {
      do {
        y = tan(PI * uni());
        k = floor(sqrt2xm * y + xm);
      } while (k < 0);
      t = 0.9 * (1.0 + y * y) * exp(k * logxm - gamma(k + 1.0) - g);
    } while (uni() > t);
  }
  return (k);
}

/*****************************************************************************/
/**                                                                         **/
/**                        Binomial Distribution                            **/
/**                                                                         **/
/*****************************************************************************/

static double
binomial_cdf(int k, int n, double p)
{
  double da, db, dp;
  int ia, ib;

  if (k < 0) {
    dp = 0.0;
  } else {
    if (k >= n) {
      dp = 1.0;
    } else {
      if (p == 0.0) {
	dp = (k < 0) ? 0.0 : 1.0;
      } else {
	if (p == 1.0) {
	  dp = (k < n) ? 0.0 : 1.0;
	} else {
	  da = k + 1.0;
	  db = n - k;
	  ia = floor(da);
	  ib = floor(db);
	  betabase(&p, &da, &db, &ia, &ib, &dp);
	  dp = 1.0 - dp;
	}
      }
    }
  }
  return(dp);
}

double
binomialcdf(double k, int n, double p)
{
  checkbinomial(n, p);
  return(binomial_cdf((int) floor(k), n, p));

}

int
binomialquant(double x, int n, double p)
{
  int k, k1, k2, del, ia;
  double m, s, p1, p2, pk;

  checkbinomial(n, p);
  checkprob(x, FALSE, FALSE);

  m = n * p;
  s = sqrt(n * p * (1 - p));
  del = max(1, (int) (0.2 * s));
  
  if (x == 0.0) k = 0.0;
  else if (x == 1.0) k = n;
  else k = m + s * ppnd(x, &ia);
  k1 = k; k2 = k;
  
  do {
    k1 = k1 - del; k1 = max(0, k1);
    p1 = binomial_cdf(k1, n, p);
  } while (k1 > 0 && p1 > p);
  if (k1 == 0 && p1 >= x) return(k1);
  
  do {
    k2 = k2 + del; k2 = min(n, k2);
    p2 = binomial_cdf(k2, n, p);
  } while (k2 < n && p2 < x);
  if (k2 == n && p2 <= x) return(k2);
  
  while (k2 - k1 > 1) {
    k = (k1 + k2) / 2;
    pk = binomial_cdf(k, n, p);
    if (pk < x) { k1 = k; p1 = pk; }
    else { k2 = k; p2 = pk; }
  }
  return(k2);
}

double binomialpmf(k, n, p)
     int k, n;
     double p;
{
  double dx, dp;

  checkbinomial(n, p);
  dx = k;
  if (p == 0.0) dp = (k == 0) ? 1.0 : 0.0;
  else if (p == 1.0) dp = (k == n) ? 1.0 : 0.0;
  else if (dx < 0.0 || dx > n) dp = 0.0;
  else dp = exp(gamma(n + 1.0) - gamma(dx + 1.0) - gamma(n - dx + 1.0)
		+ dx * log(p) + (n - dx) * log(1.0 - p));
  return(dp);
}

/* binomial random generator from Numerical Recipes */
int binomialrand(n, pp)
	int n;
	double pp;
{
  int j, k;
  static int nold = -1;
  double am, em, g, p, sq, t, y;
  static double pold = -1.0, pc, plog, pclog, en, oldg;
  
  checkbinomial(n, pp);

  p = (pp <= 0.5) ? pp : 1.0 - pp;
  
  am = n * p;
  if (p == 0.0) k = 0;
  else if (p == 1.0) k = n;
  else if (n < 50) {
    k = 0;
    for (j = 0; j < n; j++) if (uni() < p) k++;
  }
  else if (am < 1.0) {
    g = exp(-am);
    t = 1.0;
    k = -1;
    do {
      k++;
      t *= uni();
    } while (t > g);
    if (k > n) k = n;
  }
  else {
    if (n != nold) {
      en = n;
      oldg = gamma(en + 1.0);
      nold = n;
    }
    if (p != pold) {
      pc = 1.0 - p;
      plog = log(p);
      pclog = log(pc);
      pold = p;
    }
    sq = sqrt(2.0 * am * pc);
    do {
      do {
        y = tan(PI * uni());
        em = sq * y + am;
      } while (em < 0.0 || em >= en + 1.0);
      em = floor(em);
      t = 1.2 * sq * (1.0 + y * y)
        * exp(oldg - gamma(em + 1.0) - gamma(en - em + 1.0)
              + em * plog + (en - em) * pclog);
    } while (uni() > t);
    k = em;
  }
  if (p != pp) k = n - k;
  return(k);
}
