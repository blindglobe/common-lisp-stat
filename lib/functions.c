/* functions - callbacks for Bayes routines in XLISP-STAT and S        */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */

#ifdef SBAYES
# include <math.h>
#define PRINTSTR(s) printf(s)
#else
# include "xmath.h"
#define PRINTSTR(s) stdputstr(s)
#endif SBAYES

extern char *S_alloc(), *calloc(), *realloc();
extern double macheps();
char *minresultstring();

/************************************************************************/
/**                                                                    **/
/**                      Definitions and Globals                       **/
/**                                                                    **/
/************************************************************************/

#define nil 0L
#define NULL 0L
#define TRUE 1
#define FALSE 0

#define ROOT2PI 2.50662827463100050241
#define PI_INV .31830988618379067153

#define GRADTOL_POWER 1.0 / 3.0
#define H_POWER 1.0 / 6.0

typedef double **RMatrix, *RVector;

typedef struct{
  char *f, **sf, **g;
  int n, k;
  int change_sign, fderivs;
  int *gderivs;
  double typf, h, dflt;
  RVector typx, fsum, cvals, ctarget;
  RMatrix gfsum;
} Fundata;

static Fundata func, gfuncs, cfuncs;

/************************************************************************/
/**                                                                    **/
/**                         Memory Utilities                           **/
/**                                                                    **/
/************************************************************************/

/* this function is used to maintain a statically allocated piece of    */
/* memory of a specified size. If a larger piece is needed the pointer  */
/* is realloced. This allows functions using memory allocation to be    */
/* called repeatedly (but not recursively) from within the same call    */
/* from S. It attempts to avoid the danger of dangling callocs.         */

static makespace(pptr, size)
     char **pptr;
     int size;
{
  if (size <= 0) return;
  if (*pptr == nil) *pptr = calloc(size, 1);
  else *pptr = realloc(*pptr, size);
  if (size > 0 && *pptr == nil) Recover("memory allocation failed", NULL);
}

/************************************************************************/
/**                                                                    **/
/**                    Functions Evaluation Routines                   **/
/**                                                                    **/
/************************************************************************/

/*
 * All Hessianevaluations by numerical derivatives assume the gradient is
 * evaluated first at the same location. The results are cached away.
 */

/* install log posterior function */
static install_func(f, sf, n, change_sign, typf, h, typx, dflt)
     char *f, **sf;
     int n;
     double typf, h, dflt;
     RVector typx;
{
  int i;
  static int inited = FALSE;

  if (! inited) {
    func.typx = nil;
    func.fsum = nil;
    inited = TRUE;
  }
  makespace(&func.typx, n * sizeof(double));
  makespace(&func.fsum, n * sizeof(double));

  func.f = f;
  func.sf = sf;
  func.n = n;
  func.change_sign = change_sign;
  func.typf = (typf > 0.0) ? typf : 1.0;
  func.h = (h > 0.0) ? h : pow(macheps(), H_POWER);
  for (i = 0; i < n; i++) 
    func.typx[i] = (typx != nil && typx[i] > 0.0) ? typx[i] : 1.0;
  func.dflt = dflt;
  func.fderivs = 0;
}

/* install tilt functions */
static install_gfuncs(g, n, k, change_sign, h, typx)
     char **g;
     int n, k, change_sign;
     double h;
     RVector typx;
{
  int i;
  static int inited = FALSE;
  static double *gfsumdata = nil;

  if (! inited) {
    gfuncs.typx = nil;
    gfuncs.gfsum = nil;
    gfuncs.gderivs = nil;
    inited = TRUE;
  }
  makespace(&gfuncs.typx, n * sizeof(double));
  makespace(&gfuncs.gfsum, k * sizeof(double *));
  makespace(&gfsumdata, k * n * sizeof(double));
  makespace(&gfuncs.gderivs, k *sizeof(int));

  gfuncs.g = g;
  gfuncs.n = n;
  gfuncs.k = k;
  gfuncs.change_sign = change_sign;
  gfuncs.h = (h > 0.0) ? h : pow(macheps(), H_POWER);
  for (i = 0; i < n; i++)
    gfuncs.typx[i] = (typx != nil && typx[i] > 0.0) ? typx[i] : 1.0;
  for (i = 0; i < k; i++) gfuncs.gfsum[i] = gfsumdata + i * n;
}

/* install constraint functions */
static install_cfuncs(g, n, k, ctarget, h, typx)
     char **g;
     int n, k;
     double h;
     RVector typx, ctarget;
{
  int i;
  static int inited = FALSE;

  if (! inited) {
    cfuncs.typx = nil;
    cfuncs.fsum = nil;
    cfuncs.gderivs = nil;
    inited = TRUE;
  }
  makespace(&cfuncs.typx, n * sizeof(double));
  makespace(&cfuncs.fsum, n * sizeof(double));
  makespace(&cfuncs.gderivs, k * sizeof(int));

  cfuncs.g = g;
  cfuncs.n = n;
  cfuncs.k = k;
  cfuncs.h = (h > 0.0) ? h : pow(macheps(), H_POWER);
  for (i = 0; i < n; i++)
    cfuncs.typx[i] = (typx != nil && typx[i] > 0.0) ? typx[i] : 1.0;
  cfuncs.ctarget = ctarget;
}

/* callback to test if x is in the support of the posterior */
static in_support(ff, n, x)
     char **ff;
     int n;
     double *x;
{
  char *args[1], *values[1];
  int *result;
  char *mode[1];
  long length[1];
  
  if (ff == nil || ff[0] == nil) return(TRUE);
  else {
    mode[0] = "double";
    length[0] =n;
    args[0] = (char *) x;
    call_S(ff[0], 1L, args, mode, length, 0L, 1L, values);
    result = (int *) values[0];
    return(result[0]);
  }
}

/* callback for logposterior evaluation */
static evalfunc(x, pval, grad, hess)
     RVector x, grad;
     double *pval;
     RMatrix hess;
{
  char *args[1], *values[3];
  double *result, val;
  char *mode[1];
  long length[1];
  int i, j;

  for (i = 0; i < 3; i++) values[i] = nil;

  if (in_support(func.sf, func.n, x)) {
    if (pval != nil || func.fderivs > 0 || hess != nil) {
      mode[0] = "double";
      length[0] = func.n;
      args[0] = (char *) x;
      call_S(func.f, 1L, args, mode, length, 0L, 3L, values);
      result = (double *) values[0];
	  val = (! func.change_sign) ? result[0] : -result[0];
      if (pval != nil) *pval = val;
      if (values[2] != nil) func.fderivs = 2;
      else if (values[1] != nil) func.fderivs = 1;
      else func.fderivs = 0;
    }
    if (grad != nil) {
      if (func.fderivs > 0) {
	result = (double *) values[1];
	for (i = 0; i < func.n; i++)
	  grad[i] = (! func.change_sign) ? result[i] : -result[i];
      }
      else {
	numergrad(func.n, x, grad, func.fsum, evalfunc, func.h, func.typx);
      }
    }
    if (hess != nil) {
      if (func.fderivs == 2) {
	result = (double *) values[2];
	for (i = 0; i < func.n; i++) 
	  for (j = 0; j < func.n; j++)
	    hess[i][j] = (! func.change_sign) ? result[i + j * func.n]
	                                      : -result[i + j * func.n];
      }
      else {
	if (func.fderivs == 1) /* kludge to get fsum for analytic gradients */
	  numergrad(func.n, x, func.fsum, func.fsum,
		    evalfunc, func.h, func.typx);
	numerhess(func.n, x, hess, val, func.fsum, evalfunc, func.h, func.typx);
      }
    }
    return(TRUE);
  }
  else {
    if (pval != nil) *pval = func.dflt;
    return(FALSE);
  }
}


/* callback for tilt function evaluation */
static int which_gfunc;

static evalgfunc(x, pval, grad, hess)
     RVector x, grad;
     double *pval;
     RMatrix hess;
{
  char *args[1], *values[3];
  double *result, val;
  char *mode[1];
  long length[1];
  int i, j;

  for (i = 0; i < 3; i++) values[i] = nil;

  if (pval != nil || gfuncs.gderivs[which_gfunc] > 0 || hess != nil) {
    mode[0] = "double";
    length[0] = gfuncs.n;
    args[0] = (char *) x;
    call_S(gfuncs.g[which_gfunc], 1L, args, mode, length, 0L, 3L, values);
    result = (double *) values[0];
	val = result[0];
    if (pval != nil) *pval = result[0];
    if (values[2] != nil) gfuncs.gderivs[which_gfunc] = 2;
    else if (values[1] != nil) gfuncs.gderivs[which_gfunc] = 1;
    else gfuncs.gderivs[which_gfunc] = 0;
  }
  if (grad != nil) {
    if (gfuncs.gderivs[which_gfunc] > 0) {
      result = (double *) values[1];
      for (i = 0; i < gfuncs.n; i++) grad[i] = result[i];
    }
    else {
      numergrad(gfuncs.n, x, grad, gfuncs.gfsum[which_gfunc], evalgfunc, 
		gfuncs.h, gfuncs.typx);
    }
  }
  if (hess != nil) {
    if (gfuncs.gderivs[which_gfunc] == 2) {
      result = (double *) values[2];
      for (i = 0; i < gfuncs.n; i++) 
	for (j = 0; j < gfuncs.n; j++)
	  hess[i][j] = result[i + j * gfuncs.n];
    }
    else {
      /* kludge to get fsum if analytic gradient used */
      if (gfuncs.gderivs[which_gfunc] == 1)
	numergrad(gfuncs.n, x, gfuncs.gfsum[which_gfunc],
		  gfuncs.gfsum[which_gfunc], evalgfunc, gfuncs.h, gfuncs.typx);
      numerhess(gfuncs.n, x, hess, val, gfuncs.gfsum[which_gfunc], evalgfunc,
		gfuncs.h, gfuncs.typx);
    }
  }
}

/* callback for constraint function evaluation */
static int which_cfunc;

static evalcfunc(x, pval, grad, hess)
     RVector x, grad;
     double *pval;
     RMatrix hess;
{
  char *args[1], *values[3];
  double *result, val;
  char *mode[1];
  long length[1];
  int i, j;

  if (pval != nil || cfuncs.gderivs[which_cfunc] > 0 || hess != nil) {
    mode[0] = "double";
    length[0] = cfuncs.n;
    args[0] = (char *) x;
    call_S(cfuncs.g[which_cfunc], 1L, args, mode, length, 0L, 3L, values);
    result = (double *) values[0];
	val = result[0];
    if (pval != nil) {
      *pval = result[0];
      if (cfuncs.ctarget != nil) *pval -= cfuncs.ctarget[which_cfunc];
    }
    if (values[2] != nil) cfuncs.gderivs[which_cfunc] = 2;
    else if (values[1] != nil) cfuncs.gderivs[which_cfunc] = 1;
    else cfuncs.gderivs[which_cfunc] = 0;
  }
  if (grad != nil) {
    if (cfuncs.gderivs[which_cfunc] > 0) {
      result = (double *) values[1];
      for (i = 0; i <cfuncs.n; i++) grad[i] = result[i];
    }
    else {
      numergrad(cfuncs.n, x, grad, cfuncs.fsum, evalcfunc, 
		cfuncs.h, cfuncs.typx);
    }
  }
  if (hess != nil) {
    if (cfuncs.gderivs[which_cfunc] == 2) {
      result = (double *) values[2];
      for (i = 0; i <cfuncs.n; i++)
	for (j = 0; j <cfuncs.n; j++)
	  hess[i][j] = result[i + j * cfuncs.n];
    }
    else {
      /* kludge to get fsum if analytic gradient used */
      if (cfuncs.gderivs[which_cfunc] == 1)
	numergrad(cfuncs.n, x, cfuncs.fsum, cfuncs.fsum, evalcfunc, 
		  cfuncs.h, cfuncs.typx);
      numerhess(cfuncs.n, x, hess, val, cfuncs.fsum, evalcfunc,
		cfuncs.h, cfuncs.typx);
    }
  }
}

/* S front end for logposterior evaluation */
evalfront(ff, n, x, val, grad, phess, h, typx)
     char **ff;
     int *n;
     double *x, *val, *grad, *phess, *typx, *h;
{
  int i;
  static RMatrix hess = nil;

  install_func(ff[0], nil, *n, FALSE, 1.0, *h, typx, 0.0);
  if (phess == nil) hess = nil;
  else {
    makespace(&hess, *n * sizeof(double *));
    for (i = 0; i < *n; i++, phess += *n) hess[i] = phess;
  }
  evalfunc(x, val, grad, hess);
}

#ifdef SBAYES
/* S front end for tilt function evaluation */
gevalfront(gg, n, m, x, h, typx, val, grad)
     char **gg;
     int *n, *m;
     double *x, *h, *typx, *val, *grad;
{
  int i;

  install_gfuncs(gg, *n, *m, FALSE, *h, typx);
  for (i = 0; i < *m; i++, val++) {
    which_gfunc = i;
    evalgfunc(x, val, grad, nil);
    if (grad != nil) grad += *n;
  }
}

/************************************************************************/
/**                                                                    **/
/**                     Derivative Scaling Routines                    **/
/**                                                                    **/
/************************************************************************/

static check_derivs(x, drvtol)
     RVector x;
     double drvtol;
{
  static RVector grad = nil, work = nil;
  static RMatrix hess = nil;
  int i, error;

  grad = (RVector) S_alloc(func.n, sizeof(double));
  hess = (RMatrix) S_alloc(func.n, sizeof(double *));
  work = (RVector) S_alloc(func.n + func.n * func.n, sizeof(double));

  for (i = 0; i < func.n; i++) {
    hess[i] = work;
    work += func.n;
  }

  error = derivscale(func.n, x, grad, hess, func.fsum, evalfunc, 
		     &func.h, func.typx, drvtol, work);
  return(error);
}

derivscalefront(ff, n, x, h, typx, tol, info)
     char **ff;
     int *n, *info;
     double *x, *h, *typx, *tol;
{
  int i;

  if (*tol <= 0.0) *tol = pow(macheps(), GRADTOL_POWER);
  
  install_func(ff[0], nil, *n, TRUE, 1.0, *h, typx, 0.0);
  *info = check_derivs(x, *tol);

  *h = func.h;
  for (i = 0; i < *n; i++) typx[i] = func.typx[i];
}

/************************************************************************/
/**                                                                    **/
/**                    Importance Sampling Routines                    **/
/**                                                                    **/
/************************************************************************/

/* joint density of normal-cauchy mixture */
static double dncmix(x, n, p)
     double *x, p;
     int n;
{
  int i;
  double dens;

  for (i = 0, dens = 1.0; i < n; i++) {
    dens *= p * exp(-0.5 * x[i] * x[i]) / ROOT2PI
      + (1 - p) * PI_INV / (1.0 + x[i] * x[i]);
  }
  return(dens);
}

/*
 * S front end for computing sample from transformed normal-cauchy
 * mixture and importance sampling weights
 */
samplefront(ff, sf, rf, p, n, x, ch, N, y, w)
     char **ff, **sf, **rf;
     int *n, *N;
     double *p,  *x, *ch, *y, *w;
{
  double val;
  int i, j, k;
  char *args[2], *values[1];
  double *result, mval, c, dens, m;
  char *mode[2];
  long length[2];
  
  /* get the random variables */
  mode[0] = "double"; mode[1] = "double";
  length[0] = 1; length[1] = 1;
  m = *N * *n; args[0] = (char *) &m; args[1] = (char *) p;
  call_S(rf[0], 2L, args, mode, length, 0L, 1L, values);
  result = (double *) values[0];
  for (i = 0; i < m; i++) y[i] = result[i];

  /* construct the sample and the weights */
  install_func(ff[0], sf, *n, FALSE, 1.0, -1.0, nil, 0.0);
  c = 1.0 / pow(ROOT2PI, (double) *n);
  evalfunc(x, &mval, nil, nil);
  for (i = 0; i < *N; i++, y += *n) {
    dens = dncmix(y, *n, *p);
    for (j = 0; j < *n; j++) {
      val = x[j];
      for (k = j; k < *n; k++) val += y[k] * ch[j + *n * k];
      y[j] = val;
    }
    if (evalfunc(y, &val, nil, nil)) w[i] = exp(val - mval) * c / dens;
    else w[i] = 0.0;
  }
}
#endif SBAYES
/************************************************************************/
/**                                                                    **/
/**                       Maximization Routines                        **/
/**                                                                    **/
/************************************************************************/

typedef struct {
  int n, m, k, itnlimit, backtrack, verbose, vals_suppl, exptilt;
  int count, termcode;
} MaxIPars;

typedef struct {
  double typf, h, gradtol, steptol, maxstep, dflt, tilt, newtilt, hessadd;
} MaxDPars;

struct {
  double tilt;
  RVector gval;
  RMatrix  ggrad, ghess;
  int exptilt;
  RVector tscale;
} tiltinfo;

static set_tilt_info(n, m, tilt, exptilt, tscale)
     int n, m;
     double tilt, *tscale;
     int exptilt;
{
  static double *hessdata = nil, *graddata = nil;
  int i;
  static int inited = FALSE;

  if (! inited) {
    tiltinfo.gval = nil;
    tiltinfo.ggrad = nil;
    tiltinfo.ghess = nil;
    inited = TRUE;
  }
  makespace(&tiltinfo.gval, n * sizeof(double));
  makespace(&tiltinfo.ggrad, m * sizeof(double *));
  makespace(&tiltinfo.ghess, n * sizeof(double *));
  makespace(&graddata, n * m * sizeof(double));
  makespace(&hessdata, n * n * sizeof(double));

  tiltinfo.tilt = tilt;
  tiltinfo.exptilt = exptilt;
  for (i = 0; i < m; i++) tiltinfo.ggrad[i] = graddata + i * n;
  for (i = 0; i < n; i++) tiltinfo.ghess[i] = hessdata + i * n;
  tiltinfo.tscale = tscale;
}

static minfunc(x, pval, grad, hess)
     RVector x, grad;
     double *pval;
     RMatrix hess;
{
  int k = gfuncs.k;

  if (evalfunc(x, pval, grad, hess) && (k > 0))
    add_tilt(x, pval, grad, hess, tiltinfo.tilt, tiltinfo.exptilt);
}

constfunc(x, vals, jac, hess)
     RVector x, vals;
     RMatrix jac, hess;
{
  int i, k = cfuncs.k;
  double *pvali, *jaci;

  for (i = 0; i < k; i++) {
    pvali = (vals != nil) ? vals + i : nil;
    jaci = (jac != nil) ? jac[i] : nil;
    which_cfunc = i;
    evalcfunc(x, pvali, jaci, nil);
  }
}

static add_tilt(x, pval, grad, hess, tilt, exptilt)
     RVector x, grad;
     double *pval, tilt;
     RMatrix hess;
     int exptilt;
{
  int i, j, k, n = func.n, m = gfuncs.k;
  double *gval, *ggrad, **ghess, etilt;

  if (m == 0) return;

  if (gfuncs.change_sign) tilt = -tilt;

  for (k = 0; k < m; k++) {
    gval = (pval != nil) ? tiltinfo.gval + k : nil;
    ggrad = (grad != nil) ? tiltinfo.ggrad[k] : nil;
    ghess = (hess != nil) ? tiltinfo.ghess : nil;

    which_gfunc = k;
    evalgfunc(x, gval, ggrad, ghess);
    
    if (exptilt) {
      etilt = (tiltinfo.tscale != nil) ? tilt / tiltinfo.tscale[k] : tilt;
      if (pval != nil) *pval += etilt * *gval;
      if (grad != nil) 
	for (i = 0; i < n; i++) grad[i] += etilt * ggrad[i];
      if (hess != nil)
	for (i = 0; i < n; i++) 
	  for (j = 0; j < n; j++) hess[i][j] += etilt * ghess[i][j];
    }
    else {
      gval = tiltinfo.gval;
      ggrad = tiltinfo.ggrad[k];
      ghess = tiltinfo.ghess;
      if (gval[k] <= 0.0) Recover("nonpositive function value", NULL);
      if (pval != nil) *pval += tilt * log(gval[k]);
      if (grad != nil) 
	for (i = 0; i < n; i++) grad[i] += tilt * ggrad[i] / gval[k];
      if (hess != nil)
        for (i = 0; i < n; i++)
          for (j = 0; j < n; j++)
	    hess[i][j] +=
	      tilt * (ghess[i][j] / gval[k] 
		      - (ggrad[i] / gval[k]) * (ggrad[j] / gval[k]));
    }
  }
}

maxfront(ff, gf, cf, x, typx, fvals, gvals, cvals, ctarget, ipars, dpars, 
	 tscale, msg)
     char **ff, **gf, **cf;
     double *x, *typx, *fvals, *gvals, *cvals, *ctarget, *tscale;
     MaxIPars *ipars;
     MaxDPars *dpars;
     char **msg;
{
  static char *work = nil;
  static RMatrix H = nil, cJ = nil;
  double *pf, *grad, *c;
  int i, n, m, k;
  int (*cfun)();

  if (ipars->verbose > 0) PRINTSTR("maximizing...\n");

  n = ipars->n;
  m = ipars->m;
  k = ipars->k;
  if (k >= n) Recover("too many constraints", NULL);

  makespace(&H, n * sizeof(double *));
  makespace(&work, minworkspacesize(n, k));

  pf = fvals; fvals++;
  grad = fvals; fvals += n;
  for (i = 0; i < n; i++, fvals += n) H[i] = fvals;
  set_tilt_info(n, m, dpars->newtilt, ipars->exptilt, tscale);

  if (k == 0) {
    c = nil;
    cJ = nil;
    cfun = nil;
  }
  else {
    c = cvals;
    cvals += k;
    makespace(&cJ, k * sizeof(double *));
    for (i = 0; i < k; i++) cJ[i] = cvals + i * n;
    cfun = constfunc;
  }

  install_func(ff[0], nil, n, TRUE, dpars->typf, dpars->h, typx, dpars->dflt);
  install_gfuncs(gf, n, m, TRUE, dpars->h, typx);
  install_cfuncs(cf, n, k, ctarget, dpars->h, typx);

  minsetup(n, k, minfunc, cfun, x, dpars->typf, typx, work);
  minsetoptions(dpars->gradtol, dpars->steptol, dpars->maxstep,
		ipars->itnlimit, ipars->verbose, ipars->backtrack, TRUE);

  if (ipars->vals_suppl) {
    for (i = 0; i < k; i++) c[i] -= ctarget[i];
    if (dpars->newtilt != dpars->tilt) {
      add_tilt(x, pf, grad, H, dpars->newtilt - dpars->tilt, ipars->exptilt);
      dpars->tilt = dpars->newtilt;
    }
    minsupplyvalues(*pf, grad, H, c, cJ);
  }

  minimize();
  minresults(x, pf, nil, grad, H, c, cJ, &ipars->count, &ipars->termcode,
	     &dpars->hessadd);
  msg[0] = minresultstring(ipars->termcode);

  for (i = 0; i < k; i++) c[i] += ctarget[i];
  ipars->vals_suppl = TRUE;
}

/************************************************************************/
/**                                                                    **/
/**                     Log Laplace Approximation                      **/
/**                                                                    **/
/************************************************************************/

loglapdet(fvals, cvals, ipars, dpars, val, detonly)
     double *fvals, *cvals;
     MaxIPars *ipars;
     MaxDPars *dpars;
     double *val;
     int *detonly;
{
  int i, j, l, n = ipars->n, k = ipars->k;
  double f = -fvals[0], *hessdata = fvals + n + 1, *cgraddata = cvals + k;
  double ldL, ldcv, maxadd;
  static RMatrix hess = nil, cgrad = nil;

  if (k >= n) Recover("too many constraints", NULL);

  makespace(&hess, n * sizeof(double *));
  makespace(&cgrad, k * sizeof(double *));

  for (i = 0; i < n; i++) hess[i] = hessdata + i * n;
  for (i = 0; i < k; i++) cgrad[i] = cgraddata + i * n;

  choldecomp(hess, n, 0.0, &maxadd);
  /**** do something if not pos. definite ****/
  
  for (i = 0, ldL = 0.0; i < n; i++) ldL += log(hess[i][i]);

  if (k > 0) {
    /* forward solve for (L^-1) cgrad^T */
    for (l = 0; l < k; l++) {
      for (i = 0; i < n; i++) {
	if (hess[i][i] != 0.0) cgrad[l][i] /= hess[i][i];
	for (j = i + 1; j < n; j++) cgrad[l][j] -= hess[j][i] * cgrad[l][i];
      }
    }

    /* compute sigma and stdev */
    for (i = 0; i < k; i++) {
      for (j = i; j < k; j++) {
	for (l = 0, hess[i][j] = 0.0; l < n; l++)
	  hess[i][j] += cgrad[i][l] * cgrad[j][l];
	hess[j][i] = hess[i][j];
      }
    }

    choldecomp(hess, k, 0.0, &maxadd);
    /**** do something if not pos. definite ****/
    for (i = 0, ldcv = 0.0; i < k; i++) ldcv += log(hess[i][i]);
  }
  else ldcv = 0.0;

  *val = (n - k) * log(ROOT2PI) - ldL - ldcv;
  if (! *detonly) *val += f;
}

#ifdef SBAYES

loglapfront(fvals, cvals, ipars, dpars, val)
     double *fvals, *cvals;
     MaxIPars *ipars;
     MaxDPars *dpars;
     double *val;
{
  int detonly = FALSE;

  loglapdet(fvals, cvals, ipars, dpars, val, &detonly);
}

/************************************************************************/
/**                                                                    **/
/**                        First Order Moments                         **/
/**                                                                    **/
/************************************************************************/

moms1front(gf, n, m, x, hessdata, mean, stdev, sigmadata, h, typx)
     char *gf;
     int *n, *m;
     double *x, *hessdata, *mean, *stdev, *sigmadata, *h, *typx;
{
  int i, j, k;
  RMatrix hess, sigma, delg;
  double *delgdata, maxadd;

  hess = (RMatrix) S_alloc(*n, sizeof(double *));
  sigma = (RMatrix) S_alloc(*m, sizeof(double *));
  delg = (RMatrix) S_alloc(*m, sizeof(double *));
  delgdata = (double *) S_alloc(*m * *n, sizeof(double));

  for (i = 0; i < *n; i++) hess[i] = hessdata + i * *n;
  for (i = 0; i < *m; i++) sigma[i] = sigmadata + i * *m;
  for (i = 0; i < *m; i++) delg[i] = delgdata + i * *n;

  gevalfront(gf, n, m, x, h, typx, mean, delgdata);

  /* get the cholesky decomposition L of the hessian */
  choldecomp(hess, *n, 0.0, &maxadd);
  
  /* forward solve for (L^-1) delg^T */
  for (k = 0; k < *m; k++) {
    for (i = 0; i < *n; i++) {
      if (hess[i][i] != 0.0) delg[k][i] /= hess[i][i];
      for (j = i + 1; j < *n; j++) delg[k][j] -= hess[j][i] * delg[k][i];
    }
  }

  /* compute sigma and stdev */
  for (i = 0; i < *m; i++) {
    for (j = i; j < *m; j++) {
      for (k = 0, sigma[i][j] = 0.0; k < *n; k++)
	sigma[i][j] += delg[i][k] * delg[j][k];
      sigma[j][i] = sigma[i][j];
    }
    stdev[i] = sqrt(sigma[i][i]);
  }
}

/************************************************************************/
/**                                                                    **/
/**                        Second Order Moments                        **/
/**                                                                    **/
/************************************************************************/

typedef struct {
  MaxIPars max;
  int full, covar;
} MomIPars;

typedef struct {
  MaxDPars max;
  double mgfdel;
} MomDPars;

moms2front(ff, gf, gnum, x, typx, fvals, gvals, ipars, dpars, 
	   mean, stdev, sigmadata)
     char **ff, **gf;
     int *gnum;
     double *x, *typx, *fvals, *gvals, *mean, *stdev, *sigmadata;
     MomIPars *ipars;
     MomDPars *dpars;
{
  char *msg;
  double h, loglap0, loglap1, loglap2;
  double *tilts, *fvals1, *gvals1, *x1;
  MomDPars dp1, *dpars1 = &dp1;
  MomIPars ip1, *ipars1 = &ip1;
  int i, n, m;

  n = ipars->max.n;
  m = *gnum;
  h = dpars->max.h;

  tilts = (double *) S_alloc(m, sizeof(double));
  x1 = (double *) S_alloc(n, sizeof(double));
  fvals1 = (double *) S_alloc(1 + n + n * n, sizeof(double));
  gvals1 = (double *) S_alloc(m + n * m, sizeof(double));
  
  maxfront(ff, nil, nil, x, typx, fvals, gvals, nil, nil, 
	   ipars, dpars, nil, &msg);
  copypars(x, fvals, gvals, ipars, dpars, x1, fvals1, gvals1, ipars1, dpars1);
  loglapfront(fvals1, nil, ipars1, dpars1, &loglap0);
  copypars(x, fvals, gvals, ipars, dpars, x1, fvals1, gvals1, ipars1, dpars1);
  moms1front(gf, &n, &m, x1, fvals1 + n + 1, mean, stdev, sigmadata, &h, typx);

  if (ipars->full) {
    for (i = 0; i < m; i++) {
      copypars(x, fvals, gvals, ipars, dpars,
               x1, fvals1, gvals1, ipars1, dpars1);
      ipars1->max.m = 1;
      ipars1->max.exptilt = FALSE;
      dpars1->max.newtilt = 1.0;
      maxfront(ff, gf + i, nil, x1, typx, fvals1, gvals1, nil, nil, 
	       ipars1, dpars1, nil, &msg);
      loglapfront(fvals1, nil, ipars1, dpars1, &loglap1);
      loglap1 -= loglap0;

      copypars(x, fvals, gvals, ipars, dpars,
               x1, fvals1, gvals1, ipars1, dpars1);
      ipars1->max.m = 1;
      ipars1->max.exptilt = FALSE;
      dpars1->max.newtilt = 2.0;
      maxfront(ff, gf + i, nil, x1, typx, fvals1, gvals1, nil, nil, 
	       ipars1, dpars1, nil, &msg);
      loglapfront(fvals1, nil, ipars1, dpars1, &loglap2);
      loglap2 -= loglap0;      

      mean[i] = exp(loglap1);
      stdev[i] = sqrt(exp(loglap2) - exp(2.0 * loglap1));
      if (ipars->covar) sigmadata[i + i * m] = stdev[i] * stdev[i];
    }
    if (ipars->covar) {
      char *cgf[2];
      int j;

      for (i = 0; i < m; i++) {
	for (j = i + 1; j < m; j++) {
	  cgf[0] = gf[i];
	  cgf[1] = gf[j];
	  copypars(x, fvals, gvals, ipars, dpars,
		   x1, fvals1, gvals1, ipars1, dpars1);
	  ipars1->max.m = 2;
	  ipars1->max.exptilt = FALSE;
	  dpars1->max.newtilt = 1.0;
	  maxfront(ff, gf + i, nil, x1, typx, fvals1, gvals1, nil, nil, 
		   ipars1, dpars1, nil, &msg);
	  loglapfront(fvals1, nil, ipars1, dpars1, &loglap1);
	  loglap1 -= loglap0;
	  
	  sigmadata[i + j * m] = exp(loglap1) - mean[i] * mean[j];
	  sigmadata[j + i * m] = sigmadata[i + j * m];
	}
      }
    }
  }
  else {
    for (i = 0; i < m; i++) 
      tilts[i] = (stdev[i] > 0.0) ? dpars->mgfdel / stdev[i] : dpars->mgfdel;
    
    for (i = 0; i < m; i++) {
      copypars(x, fvals, gvals, ipars, dpars, 
	       x1, fvals1, gvals1, ipars1, dpars1);
      ipars1->max.m = 1;
      ipars1->max.exptilt = TRUE;
      dpars1->max.newtilt = tilts[i];
      maxfront(ff, gf + i, nil, x1, typx, fvals1, gvals1, nil, nil, 
	       ipars1, dpars1, nil, &msg);
      loglapfront(fvals1, nil, ipars1, dpars1, &loglap1);
      loglap1 -= loglap0;

      copypars(x, fvals, gvals, ipars, dpars,
               x1, fvals1, gvals1, ipars1, dpars1);
      ipars1->max.m = 1;
      ipars1->max.exptilt = TRUE;
      dpars1->max.newtilt = -tilts[i];
      maxfront(ff, gf + i, nil, x1, typx, fvals1, gvals1, nil, nil,
               ipars1, dpars1, nil, &msg);
      loglapfront(fvals1, nil, ipars1, dpars1, &loglap2);
      loglap2 -= loglap0;

      mean[i] = (loglap1 - loglap2) / (2.0 * tilts[i]);
      stdev[i] = sqrt((loglap1 + loglap2) / (tilts[i] * tilts[i]));
      if (ipars->covar) sigmadata[i + i * m] = stdev[i] * stdev[i];
    }
    if (ipars->covar) {
      char *cgf[2];
      double ctilt, tscale[2];
      int j;

      ctilt = dpars->mgfdel;
      for (i = 0; i < m; i++) {
	for (j = i + 1; j < m; j++) {
	  cgf[0] = gf[i];
	  cgf[1] = gf[j];
	  tscale[0] = stdev[i] > 0 ? stdev[i] : 1.0;
	  tscale[1] = stdev[j] > 0 ? stdev[j] : 1.0;

	  copypars(x, fvals, gvals, ipars, dpars, 
		   x1, fvals1, gvals1, ipars1, dpars1);
	  ipars1->max.m = 2;
	  ipars1->max.exptilt = TRUE;
	  dpars1->max.newtilt = ctilt;
	  maxfront(ff, cgf, nil, x1, typx, fvals1, gvals1, nil, nil, 
		   ipars1, dpars1, tscale, &msg);
	  loglapfront(fvals1, nil, ipars1, dpars1, &loglap1);
	  loglap1 -= loglap0;

	  copypars(x, fvals, gvals, ipars, dpars,
		   x1, fvals1, gvals1, ipars1, dpars1);
	  ipars1->max.m = 2;
	  ipars1->max.exptilt = TRUE;
	  dpars1->max.newtilt = -ctilt;
	  maxfront(ff, cgf, nil, x1, typx, fvals1, gvals1, nil, nil,
		   ipars1, dpars1, tscale, &msg);
	  loglapfront(fvals1, nil, ipars1, dpars1, &loglap2);
	  loglap2 -= loglap0;

	  sigmadata[i + j * m] = stdev[i] * stdev[j]
	    * ((loglap2 + loglap1) /(2 * ctilt * ctilt) - 1.0);
	  sigmadata[j + i * m] = sigmadata[i + j * m];
	}
      }
    }
  }
}

static copypars(x, f, g, ip, dp, x1, f1, g1, ip1, dp1)
     double *x, *f, *g, *x1, *f1, *g1;
     MomIPars *ip, *ip1;
     MomDPars *dp, *dp1;
{
  int i, n, m, nf, ng;

  n = ip->max.n;
  m = ip->max.m;
  nf = 1 + n + n * n;
  ng = m + n * m;

  for (i = 0; i < n; i++) x1[i] = x[i];
  for (i = 0; i < nf; i++) f1[i] = f[i];
  for (i = 0; i < ng; i++) g1[i] = g[i];
  *ip1 = *ip;
  *dp1 = *dp;
}

/************************************************************************/
/**                                                                    **/
/**                          Laplace Margins                           **/
/**                                                                    **/
/************************************************************************/

lapmar1front(ff, gf, x, typx, fvals, ipars, dpars, xmar, ymar, nmar)
     char **ff, **gf;
     double *x, *typx, *fvals, *xmar, *ymar;
     MaxIPars *ipars;
     MaxDPars *dpars;
     int *nmar;
{
  char *msg;
  int i, n, m, mindex;
  double h, loglap0, loglap1, xmode, stdev, sigmadata, ctarget[1];
  double *fvals1, *x1, *cvals, *cvals1, *fvals2, *x2, *cvals2;
  MaxDPars dp1, dp2, *dpars1 = &dp1, *dpars2 = &dp2;
  MaxIPars ip1, ip2, *ipars1 = &ip1, *ipars2 = &ip2;

  n = ipars->n;
  m = 1;
  h = dpars->h;

  x1 = (double *) S_alloc(n + 1, sizeof(double));
  fvals1 = (double *) S_alloc(1 + n + n * n, sizeof(double));
  cvals = (double *) S_alloc(m + n * m, sizeof(double));
  cvals1 = (double *) S_alloc(m + n * m, sizeof(double));
  x2 = (double *) S_alloc(n + 1, sizeof(double));
  fvals2 = (double *) S_alloc(1 + n + n * n, sizeof(double));
  cvals2 = (double *) S_alloc(m + n * m, sizeof(double));

  maxfront(ff, nil, nil, x, typx, fvals, nil, nil, nil,
	   ipars, dpars, nil, &msg);
  cpmarpars(x, fvals, cvals, ipars, dpars, x1, fvals1, cvals1, ipars1, dpars1);
  loglapfront(fvals1, nil, ipars1, dpars1, &loglap0);
  cpmarpars(x, fvals, cvals, ipars, dpars, x1, fvals1, cvals1, ipars1, dpars1);
  moms1front(gf, &n, &m, x1, fvals1 + n + 1, &xmode, &stdev, &sigmadata,
	     &h, typx);
  
  ipars->k = 1;
  ipars->vals_suppl = FALSE;
  ctarget[0] = xmode;
  maxfront(ff, nil, gf, x, typx, fvals, nil, cvals, ctarget,
	   ipars, dpars, nil, &msg);

  for (mindex = 0; mindex < *nmar && xmar[mindex] <= xmode; mindex++);

  cpmarpars(x, fvals, cvals, ipars, dpars, x1, fvals1, cvals1, ipars1, dpars1);
  for (i = mindex; i >= 0; i--) {
    ctarget[0] = xmar[i];
    maxfront(ff, nil, gf, x1, typx, fvals1, nil, cvals1, ctarget,
	     ipars1, dpars1, nil, &msg);
    cpmarpars(x1, fvals1, cvals1, ipars1, dpars1, x2, 
	      fvals2, cvals2, ipars2, dpars2);
    loglapfront(fvals2, cvals2, ipars2, dpars2, &loglap1);
    ymar[i] = exp(loglap1 - loglap0);
  }
  cpmarpars(x, fvals, cvals, ipars, dpars, x1, fvals1, cvals1, ipars1, dpars1);
  for (i = mindex + 1; i < *nmar; i++) {
    ctarget[0] = xmar[i];
    maxfront(ff, nil, gf, x1, typx, fvals1, nil, cvals1, ctarget,
	     ipars1, dpars1, nil, &msg);
    cpmarpars(x1, fvals1, cvals1, ipars1, dpars1, x2, 
	      fvals2, cvals2, ipars2, dpars2);
    loglapfront(fvals2, cvals2, ipars2, dpars2, &loglap1);
    ymar[i] = exp(loglap1 - loglap0);
  }
}

static cpmarpars(x, f, g, ip, dp, x1, f1, g1, ip1, dp1)
     double *x, *f, *g, *x1, *f1, *g1;
     MaxIPars *ip, *ip1;
     MaxDPars *dp, *dp1;
{
  int i, n, k, nf, ng;

  n = ip->n;
  k = ip->k;
  nf = 1 + n + n * n;
  ng = k + n * k;

  for (i = 0; i < n; i++) x1[i] = x[i];
  for (i = 0; i < nf; i++) f1[i] = f[i];
  for (i = 0; i < ng; i++) g1[i] = g[i];
  *ip1 = *ip;
  *dp1 = *dp;
}
#endif /* SBAYES */

#ifdef TODO
get hessian from gradiant for analytical gradiants
avoid repeated derivative calls in mimimize.
2d margins
use pos. definiteness info in margins
#endif TODO
