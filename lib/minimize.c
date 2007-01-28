/* minimize - minimization for Bayes routines in XLISP-STAT and S      */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */

/*
 * Nonlinear optimization modules adapted from Dennis and Schnabel, 
 * "Numerical Methods for Unconstrained Optimization and Nonlinear
 * Equations."
 */

#ifdef SBAYES
# include <math.h>
static char buf[200];
#define PRINTSTR(s) printf(s)
#else
# include "xmath.h"
extern char buf[];
#define PRINTSTR(s) stdputstr(s)
#endif SBAYES

extern double macheps();

/************************************************************************/
/**                                                                    **/
/**                      Definitions and Globals                       **/
/**                                                                    **/
/************************************************************************/

# define nil 0L
# define FALSE 0
# define TRUE 1

# define INIT_GRAD_FRAC .001
# define CONSEC_MAX_LIMIT 5
# define ALPHA .0001
# define MAX_STEP_FACTOR 1000
# define GRADTOL_POWER 1.0 / 3.0
# define STEPTOL_POWER 2.0 / 3.0
# define ITNLIMIT 100
# define VERBOSE 0
# define USE_SEARCH TRUE

typedef double **RMatrix, *RVector;

typedef struct {
  int n, k;
  int (*ffun)(), (*gfun)();
  double f, typf, new_f;
  double crit, new_crit;
  RVector x, new_x, sx, delf, new_delf, qnstep, F;
  RMatrix hessf, H, L, new_delg;
  double gradtol, steptol, maxstep;
  int itncount, itnlimit, maxtaken, consecmax, retcode, termcode;
  int use_line_search, verbose, values_supplied, change_sign;
  double diagadd;
} Iteration;

static char *termcodes[] = {"not yet terminated",
                            "gradient size is less than gradient tolerance",
                            "step size is less than step tolerance",
                            "no satisfactory step found in backtracking",
                            "iteration limit exceeded",
                            "maximum size step taken 5 iterations in a row"};
                            
/************************************************************************/
/**                                                                    **/
/**                         Utility Functions                          **/
/**                                                                    **/
/************************************************************************/

static double Max(a, b)
        double a, b;
{
  return(a > b ? a : b);
}

static double Min(a, b)
        double a, b;
{
  return(a > b ? b : a);
}

/************************************************************************/
/**                                                                    **/
/**                    Cholesky Solving Functions                      **/
/**                                                                    **/
/************************************************************************/

/* solve (L L^T) s = -g for s */
static cholsolve(n, g, L, s)
     int n;
     RVector g, s;
     RMatrix L;
{
  int i;

  /* solve Ly = g */
  lsolve(n, g, L, s);
  
  /* solve L^Ts = y */
  ltsolve(n, s, L, s);

  for (i = 0; i < n; i++) s[i] = -s[i];
}

/* solve Ly = b for y */
static lsolve(n, b, L, y)
     int n;
     RVector b, y;
     RMatrix L;
{
  int i, j;

  for (i = 0; i < n; i++) {
    y[i] = b[i];
    for (j = 0; j < i; j++) y[i] -= L[i][j] * y[j];
    if (L[i][i] != 0) y[i] /= L[i][i];
  }
}

/* solve (L^T)x = y for x */
static ltsolve(n, y, L, x)
     int n;
     RVector y, x;
     RMatrix L;
{
  int i, j;

  for (i = n - 1; i >= 0; i--) {
    x[i] = y[i];
    for (j = i + 1; j < n; j++) x[i] -= L[j][i] * x[j];
    if (L[i][i] != 0) x[i] /= L[i][i];
  }
}

static modelhess(n, sx, H, L, diagadd)
     int n;
     RVector sx;
     RMatrix H, L;
     double *diagadd;
{
  int i, j;
  double sqrteps, maxdiag, mindiag, maxoff, maxoffl, maxposdiag, mu,
    maxadd, maxev, minev, offrow, sdd;

  /* scale H on both sides with sx */
  for (i = 0; i < n; i++)
    for (j = 0; j < n; j++) H[i][j] /= sx[i] * sx[j];

  /* 
   * find mu to add to diagonal so no diagonal elements are negative
   * and largest diagonal dominates largest off diagonal element in H 
   */
  sqrteps = sqrt(macheps());
  for (maxdiag = H[0][0], i = 1; i < n; i++) 
    maxdiag = Max(maxdiag, H[i][i]);
  for (mindiag = H[0][0], i = 1; i < n; i++)
    mindiag = Min(mindiag, H[i][i]);
  maxposdiag = Max(0.0, maxdiag);
  
  if (mindiag <= sqrteps * maxposdiag) {
    mu = 2 * (maxposdiag - mindiag) * sqrteps - mindiag;
    maxdiag += mu;
  }
  else mu = 0.0;
  
  if (n > 1) {
    for (maxoff = fabs(H[0][1]), i = 0; i < n; i++) 
      for (j = i + 1; j < n; j++) 
        maxoff = Max(maxoff, fabs(H[i][j]));
  }
  else maxoff = 0.0;

  if (maxoff * (1 + 2 * sqrteps) > maxdiag) {
    mu += (maxoff - maxdiag) + 2 * sqrteps * maxoff;
    maxdiag = maxoff * (1 + 2 * sqrteps);
  }
  
  if (maxdiag == 0.0) {
    mu = 1;
    maxdiag = 1;
  }

  if (mu > 0) for (i = 0; i < n; i++) H[i][i] += mu;

  maxoffl = sqrt(Max(maxdiag, maxoff / n));

  /*
   * compute the perturbed Cholesky decomposition
   */
  for (i = 0; i < n; i++)
    for (j = 0; j < n; j++) L[i][j] = H[i][j];
  choldecomp(L, n, maxoffl, &maxadd);

  /*
   * add something to diagonal, if needed, to make positive definite
   * and recompute factorization
   */
  if (maxadd > 0) {
    maxev = H[0][0];
    minev = H[0][0];
    for (i = 0; i < n; i++) {
      for (offrow = 0.0, j = 0; j < n; j++) 
        if (i != j) offrow += fabs(H[i][j]);
      maxev = Max(maxev, H[i][i] + offrow);
      minev = Min(minev, H[i][i] - offrow);
    }
    sdd = (maxev - minev) * sqrteps - minev;
    sdd = Max(sdd, 0.0);
    mu = Min(maxadd, sdd);
    for (i = 0; i < n; i++) H[i][i] += mu;
    for (i = 0; i < n; i++)
      for (j = 0; j < n; j++) L[i][j] = H[i][j];
    choldecomp(L, n, maxoffl, &maxadd);
    *diagadd = mu;
  }
  else *diagadd = 0.0;

  /* unscale H and L */
  for (i = 0; i < n; i++)
    for (j = 0; j < n; j++) {
      H[i][j] *= sx[i] * sx[j];
      L[i][j] *= sx[i];
    }
}

/************************************************************************/
/**                                                                    **/
/**                        Stopping Criteria                           **/
/**                                                                    **/
/************************************************************************/

static double gradsize(iter, new)
     Iteration *iter;
     int new;
{
  int n, i;
  double size, term, crit, typf;
  RVector x, delf, sx;

  n = iter->n + iter->k;
  crit = iter->crit; 
  typf = iter->typf;
  x = iter->x;
  sx = iter->sx;
  delf = (new) ? iter->new_delf : iter->delf;

  for (i = 0, size = 0.0; i < n; i++) {
    term = fabs(delf[i]) * Max(x[i], 1.0 / sx[i]) / Max(fabs(crit), typf);
    size = Max(size, term);
  }
  return(size);
}

static double incrsize(iter)
     Iteration *iter;
{
  int n, i;
  double size, term;
  RVector x, new_x, sx;

  new_x = iter->new_x;
  n = iter->n + iter->k;
  x = iter->x;
  sx = iter->sx;

  for (i = 0, size = 0.0; i < n; i++) {
    term = fabs(x[i] - new_x[i]) / Max(fabs(x[i]), 1.0 / sx[i]);
        size = Max(size, term);
  }
  return(size);
}

static stoptest0(iter)
     Iteration *iter;
{
  iter->consecmax = 0;

  if (gradsize(iter, FALSE) <= INIT_GRAD_FRAC * iter->gradtol)
    iter->termcode = 1;
  else iter->termcode = 0;

  return(iter->termcode);
}

static stoptest(iter)
     Iteration *iter;
{
  int termcode, retcode, itncount, itnlimit, maxtaken, consecmax;
  double gradtol, steptol;

  retcode = iter->retcode;
  gradtol = iter->gradtol;
  steptol = iter->steptol;
  itncount = iter->itncount;
  itnlimit = iter->itnlimit;
  maxtaken = iter->maxtaken;
  consecmax = iter->consecmax;

  termcode = 0;
  if (retcode == 1) termcode = 3;
  else if (gradsize(iter, TRUE) <= gradtol) termcode = 1;
  else if (incrsize(iter) <= steptol) termcode = 2;
  else if (itncount >= itnlimit) termcode = 4;
  else if (maxtaken) {
    consecmax++;
    if (consecmax >= CONSEC_MAX_LIMIT) termcode = 5;
  }
  else consecmax = 0;
    
  iter->consecmax = consecmax;
  iter->termcode = termcode;

  return(termcode);
}

/************************************************************************/
/**                                                                    **/
/**               Function and Derivative Evaluation                   **/
/**                                                                    **/
/************************************************************************/

static eval_funval(iter)
     Iteration *iter;
{
  int i;

  (*(iter->ffun))(iter->x, &iter->f, nil, nil);
  if (iter->k == 0) iter->crit = iter->f;
  else {
    eval_gradient(iter);
    for (i = 0, iter->crit = 0.0; i < iter->n + iter->k; i++)
      iter->crit += 0.5 * pow(fabs(iter->delf[i]), 2.0);
  }
}

static eval_next_funval(iter)
     Iteration *iter;
{
  int i;

  (*(iter->ffun))(iter->new_x, &iter->new_f, nil, nil);
  if (iter->k == 0) iter->new_crit = iter->new_f;
  else {
    eval_next_gradient(iter);
    for (i = 0, iter->new_crit = 0.0; i < iter->n + iter->k; i++)
      iter->new_crit += 0.5 * pow(fabs(iter->new_delf[i]), 2.0);
  }
}

static eval_gradient(iter)
     Iteration *iter;
{
  int i, j, n, k;

  n = iter->n;
  k = iter->k;

  (*(iter->ffun))(iter->x, nil, iter->delf, nil);
  if (k > 0) {
    (*(iter->gfun))(iter->x, iter->delf + n, nil, nil);
    (*(iter->gfun))(iter->x, nil, iter->new_delg, nil);
    for (i = 0; i < n; i++) {
      for (j = 0; j < k; j++) 
        iter->delf[i] += iter->x[n + j] * iter->new_delg[j][i];
      for (j = 0; j < k; j++) {
        iter->hessf[n + j][i] = iter->new_delg[j][i];
        iter->hessf[i][n + j] = iter->new_delg[j][i];
      }
    }
  }
}

static eval_next_gradient(iter)
     Iteration *iter;
{
  int i, j, n, k;

  n = iter->n;
  k = iter->k;
  (*(iter->ffun))(iter->new_x, nil, iter->new_delf, nil);
  if (k > 0) {
    (*(iter->gfun))(iter->new_x, iter->new_delf + n, nil, nil);
    (*(iter->gfun))(iter->new_x, nil, iter->new_delg, nil);
    for (i = 0; i < n; i++) {
      for (j = 0; j < k; j++)
        iter->new_delf[i] += iter->new_x[n + j] * iter->new_delg[j][i];
    }
  }
}

static eval_hessian(iter)
     Iteration *iter;
{
  int i, j, n, k;

  n = iter->n;
  k = iter->k;
  (*(iter->ffun))(iter->x, nil, nil, iter->hessf);
  for (i = n; i < n + k; i++)
    for (j = n; j < n + k; j++) iter->hessf[i][j] = 0.0;
}

/************************************************************************/
/**                                                                    **/
/**                     Backtracking Line Search                       **/
/**                                                                    **/
/************************************************************************/

static linesearch(iter)
     Iteration *iter;
{
  int i, n;
  double newtlen, maxstep, initslope, rellength, lambda, minlambda, 
    lambdatemp, lambdaprev, a, b, disc, critprev, f1, f2, a11, a12, a21, a22, 
    del;
  RVector qnstep, delf, x, new_x, sx;

  n = iter->n + iter->k;
  if (! iter->use_line_search) {
    iter->maxtaken = FALSE;
    for (i = 0; i < n; i++)
      iter->new_x[i] = iter->x[i] + iter->qnstep[i];
    eval_next_funval(iter);
    iter->retcode = 0;
  }
  else{
    qnstep = iter->qnstep;
    maxstep = iter->maxstep;
    delf = (iter->k == 0) ? iter->delf : iter->F;
    x = iter->x;
    new_x = iter->new_x;
    sx = iter->sx;

    iter->maxtaken = FALSE;
    iter->retcode = 2;
    
    for (i = 0, newtlen = 0.0; i < n; i++)
      newtlen += pow(sx[i] * qnstep[i], 2.0);
    newtlen = sqrt(newtlen);

    if (newtlen > maxstep) {
      for (i = 0; i < n; i++) qnstep[i] *= (maxstep / newtlen);
      newtlen = maxstep;
    }

    for (i = 0, initslope = 0.0; i < n; i++) initslope += delf[i] * qnstep[i];

    for (i = 0, rellength = 0.0; i < n; i++)
      rellength = Max(rellength, fabs(qnstep[i]) / Max(fabs(x[i]), 1.0 / sx[i]));

    minlambda = (rellength == 0.0) ? 2.0 : iter->steptol / rellength;
    
    lambda = 1.0;
	lambdaprev = 1.0; /* to make compilers happy */
	critprev = 1.0;   /* to make compilers happy */
    while (iter->retcode > 1) {
      for (i = 0; i < n; i++) new_x[i] = x[i] + lambda * qnstep[i];
      eval_next_funval(iter);
      if (iter->new_crit <= iter->crit + ALPHA * lambda * initslope) {
        iter->retcode = 0;
        if (lambda == 1.0 && newtlen > 0.99 * maxstep) iter->maxtaken = TRUE;
      }
      else if (lambda < minlambda) {
        iter->retcode = 1;
        iter->new_crit = iter->crit;
        for (i = 0; i < n; i++) new_x[i] = x[i];
      }
      else {
        if (lambda == 1.0) { /* first backtrack, quadratic fit */
          lambdatemp = - initslope 
                     / (2 * (iter->new_crit - iter->crit - initslope));
        }
        else { /* all subsequent backtracks, cubic fit */
          del = lambda - lambdaprev;
          f1 = iter->new_crit - iter->crit - lambda * initslope;
          f2 = critprev - iter->crit - lambdaprev * initslope;
          a11 = 1.0 / (lambda * lambda);
          a12 = -1.0 / (lambdaprev * lambdaprev);
          a21 = -lambdaprev * a11;
          a22 = -lambda * a12;
          a = (a11 * f1 + a12 * f2) / del;
          b = (a21 * f1 + a22 * f2) / del;
          disc = b * b - 3 * a * initslope;
          if (a == 0) { /* cubic is a quadratic */
            lambdatemp = - initslope / (2 * b);
          }
          else { /* legitimate cubic */
            lambdatemp = (-b + sqrt(disc)) / (3 * a);
          }
          lambdatemp = Min(lambdatemp, 0.5 * lambda);
        }
        lambdaprev = lambda;
        critprev = iter->new_crit;
        lambda = Max(0.1 * lambda, lambdatemp);
        if (iter->verbose > 0) {
          sprintf(buf, "Backtracking: lambda = %g\n", lambda);
          PRINTSTR(buf);
        }
      }
    }
  }
}

/************************************************************************/
/**                                                                    **/
/**                   Status Printing Functions                        **/
/**                                                                    **/
/************************************************************************/

static print_header(iter)
     Iteration *iter;
{
  if (iter->verbose > 0) {
    sprintf(buf, "Iteration %d.\n", iter->itncount);
    PRINTSTR(buf);
  }
}

static print_status(iter)
     Iteration *iter;
{
  int i, j;

  if (iter->verbose > 0) {
    sprintf(buf, "Criterion value = %g\n", 
            (iter->change_sign) ? -iter->crit : iter->crit);
    PRINTSTR(buf);
    if (iter->verbose > 1) {
      PRINTSTR("Location = <");
      for (i = 0; i < iter->n + iter->k; i++) {
        sprintf(buf, (i < iter->n + iter->k - 1) ? "%g " : "%g>\n", iter->x[i]);
        PRINTSTR(buf);
      }
    }
    if (iter->verbose > 2) {
      PRINTSTR("Gradient = <");
      for (i = 0; i < iter->n + iter->k; i++) {
        sprintf(buf, (i < iter->n + iter->k - 1) ? "%g " : "%g>\n", 
                (iter->change_sign) ? -iter->delf[i] : iter->delf[i]);
        PRINTSTR(buf);
      }
    }
    if (iter->verbose > 3) {
      PRINTSTR("Hessian:\n");
      for (i = 0; i < iter->n + iter->k; i++) {
        for (j = 0; j < iter->n + iter->k; j++) {
          sprintf(buf, (j < iter->n + iter->k - 1) ? "%g " : "%g\n", 
                  (iter->change_sign) ? -iter->hessf[i][j] : iter->hessf[i][j]);
          PRINTSTR(buf);
        }
      }
    }
  }
  if (iter->termcode != 0 && iter->verbose > 0) {
    sprintf(buf, "Reason for termination: %s.\n", termcodes[iter->termcode]);
    PRINTSTR(buf);
  }
}

/************************************************************************/
/**                                                                    **/
/**                         Iteration Driver                           **/
/**                                                                    **/
/************************************************************************/

static findqnstep(iter)
     Iteration *iter;
{
  int i, j, N, l;

  if (iter->k == 0) {
    modelhess(iter->n, iter->sx, iter->hessf, iter->L, &iter->diagadd);
    cholsolve(iter->n, iter->delf, iter->L, iter->qnstep);
  }
  else {
    N = iter->n + iter->k;
    for (i = 0; i < N; i++) {
      for (l = 0, iter->F[i] = 0.0; l < N; l++)
        iter->F[i] += iter->hessf[i][l] * iter->delf[l];
      for (j = 0; j < N; j++)
        for (l = 0, iter->H[i][j] = 0.0; l < N; l++)
          iter->H[i][j] += iter->hessf[i][l] * iter->hessf[j][l];
    }
    modelhess(N, iter->sx, iter->H, iter->L, &iter->diagadd);
    cholsolve(N, iter->F, iter->L, iter->qnstep);
  }
}

static iterupdate(iter)
     Iteration *iter;
{
  int i, j, n, k;
  
  n = iter->n;
  k = iter->k;
  iter->f = iter->new_f;
  iter->crit = iter->new_crit;
  for (i = 0; i < n + k; i++) {
    iter->x[i] = iter->new_x[i];
    iter->delf[i] = iter->new_delf[i];
  }
  for (i = 0; i < k; i++) {
    for (j = 0; j < n; j++) {
      iter->hessf[n + i][j] = iter->new_delg[i][j];
      iter->hessf[j][n + i] = iter->new_delg[i][j];
    }
  }
}

static mindriver(iter)
     Iteration *iter;
{
  iter->consecmax = 0;
  iter->itncount = 0;
  iter->termcode = 0;
  if (! iter->values_supplied) {
    eval_funval(iter);
    if (iter->k == 0) eval_gradient(iter);
    eval_hessian(iter);
  }

  stoptest0(iter);
  print_header(iter);
  print_status(iter);
  while (iter->termcode == 0) {
    iter->itncount++;
    print_header(iter);
    findqnstep(iter);
    linesearch(iter);
    if (iter->k == 0) eval_next_gradient(iter);
    stoptest(iter);
    iterupdate(iter);
    eval_hessian(iter);
    print_status(iter);
  }
}  

/************************************************************************/
/**                                                                    **/
/**                   External Interface Routines                      **/
/**                                                                    **/
/************************************************************************/

static Iteration myiter;

minworkspacesize(n, k)
     int n, k;
{
  int size;
  
  /* x, new_x, sx, delf, new_delf, qnstep and F */
  size = 7 * sizeof(double) * (n + k);

  /* hessf, H and L */
  size += 3 * (sizeof(double *) * (n + k) 
               + sizeof(double) * (n + k) * (n + k)); 

  /* delg and new_delg */
  size += 2 * (sizeof(double *) * k + sizeof(double) * n * k);

  return(size);
}

char *minresultstring(code)
     int code;
{
  if (code <= 0) return("bad input data");
  else if (code <= 5) return(termcodes[code]);
  else return("unknown return code");
}
  
minsetup(n, k, ffun, gfun, x, typf, typx, work)
     int n, k, (*ffun)(), (*gfun)();
     RVector x, typx;
     double typf;
     char *work;
{
  Iteration *iter = &myiter;
  int i, j;
  double nx0, ntypx;

  n = (n > 0) ? n : 0;
  k = (k > 0) ? k : 0;

  iter->n = n;
  iter->k = k;
  iter->ffun = ffun;
  iter->gfun = gfun;

  iter->x = (RVector) work; work += sizeof(double) * (n + k);
  iter->new_x = (RVector) work; work += sizeof(double) * (n + k);
  iter->sx = (RVector) work; work += sizeof(double) * (n + k);
  iter->delf = (RVector) work; work += sizeof(double) * (n + k);
  iter->new_delf = (RVector) work; work += sizeof(double) * (n + k);
  iter->qnstep = (RVector) work; work += sizeof(double) * (n + k);
  iter->F = (RVector) work; work += sizeof(double) * (n + k);
  for (i = 0; i < n; i++) {
    iter->x[i] = x[i];
    iter->sx[i] = (typx != nil && typx[i] > 0.0) ? 1.0 / typx[i] : 1.0;
  }
  for (i = 0; i < k; i++) {
    iter->x[n + i] = x[n + i];
    iter->sx[n + i] = 1.0;
  }

  iter->hessf = (RMatrix) work; work += sizeof(double *) * (n + k);
  for (i = 0; i < n + k; i++) {
    iter->hessf[i] = (RVector) work;
    work += sizeof(double) * (n + k);
  }
  for (i = 0; i < n + k; i++)
    for (j = 0; j < n + k; j++) iter->hessf[i][j] = 0.0;
  iter->L = (RMatrix) work; work += sizeof(double *) * (n + k);
  for (i = 0; i < n + k; i++) {
    iter->L[i] = (RVector) work;
    work += sizeof(double) * (n + k);
  }
  iter->H = (RMatrix) work; work += sizeof(double *) * (n + k);
  for (i = 0; i < n + k; i++) {
    iter->H[i] = (RVector) work;
    work += sizeof(double) * (n + k);
  }

  iter->new_delg = (k > 0) ? (RMatrix) work : nil;
  work += sizeof(double *) * k;
  for (i = 0; i < k; i++) {
    iter->new_delg[i] = (RVector) work;
    work += sizeof(double) * n;
  }

  iter->typf = (typf > 0.0) ? typf : 1.0;

  iter->verbose = VERBOSE;
  iter->use_line_search = USE_SEARCH;
  iter->itncount = 0;
  iter->itnlimit = ITNLIMIT;
  iter->gradtol = pow(macheps(), GRADTOL_POWER);
  iter->steptol = pow(macheps(), STEPTOL_POWER);
  for (i = 0, nx0 = 0.0, ntypx = 0.0; i < iter->n; i++) {
    nx0 += fabs(iter->new_x[i] / iter->sx[i]);
    ntypx += fabs(1.0 / iter->sx[i]);
  }
  iter->maxstep = MAX_STEP_FACTOR * Max(nx0, ntypx);
  iter->values_supplied = FALSE;
}

minsetoptions(gradtol, steptol, maxstep, itnlimit, verbose, use_search, change_sign)
     double gradtol, steptol, maxstep;
     int itnlimit, verbose, use_search, change_sign;
{
  Iteration *iter = &myiter;

  if (gradtol > 0.0) iter->gradtol = gradtol;
  if (steptol > 0.0) iter->steptol = steptol;
  if (maxstep > 0.0) iter->maxstep = maxstep;
  if (itnlimit >= 0) iter->itnlimit = itnlimit;
  if (verbose >= 0)  iter->verbose = verbose;
  iter->use_line_search = use_search;
  iter->change_sign = change_sign;
}

minsupplyvalues(f, delf, hessf, g, delg)
     double f;
     RVector delf, g;
     RMatrix hessf, delg;
{
  Iteration *iter = &myiter;
  int i, j, n, k;
  
  n = iter->n;
  k = iter->k;
  
  iter->f = f;
  for (i = 0; i < n; i++) {
    iter->delf[i] = delf[i];
    for (j = 0; j < k; j++)
      iter->delf[i] += iter->x[n + j] * delg[j][i];
    for (j = 0; j < n; j++) iter->hessf[i][j] = hessf[i][j];
  }
  for (i = 0; i < k; i++) {
    iter->delf[n + i] = g[i];
    for (j = 0; j < n; j++) {
      iter->hessf[n + i][j] = delg[i][j];
      iter->hessf[j][n + i] = delg[i][j];
    }
  }
  if (k == 0) iter->crit = f;
  else {
    for (i = 0, iter->crit = 0.0; i < n + k; i++) 
      iter->crit += 0.5 * pow(fabs(iter->delf[i]), 2.0);
  }
  iter->values_supplied = TRUE;
}

minimize() { mindriver(&myiter); }

minresults(x, pf, pcrit, delf, hessf, g, delg, pcount, ptermcode, diagadd)
     RVector x, delf, g;
     double *pf, *pcrit, *diagadd;
     RMatrix hessf, delg;
     int *pcount, *ptermcode;
{
  Iteration *iter = &myiter;
  int i, j, n, k;
  
  n = iter->n;
  k = iter->k;
  
  if (pf != nil) *pf = iter->f;
  if (pcrit != nil) *pcrit = iter->crit;
  for (i = 0; i < n; i++) {
    if (x != nil) x[i] = iter->x[i];
    if (delf != nil) delf[i] = iter->delf[i];
    for (j = 0; j < n; j++) if (hessf != nil) hessf[i][j] = iter->hessf[i][j];
  }
  for (i = 0; i < k; i++) {
    if (x != nil) x[n + i] = iter->x[n + i];
    if (g != nil) g[i] = iter->delf[n + i];
    for (j = 0; j < n; j++)
      if (delg != nil) delg[i][j] = iter->hessf[n + i][j];
  }
  if (pcount != nil) *pcount = iter->itncount;
  if (ptermcode != nil) *ptermcode = iter->termcode;
  if (diagadd != nil) *diagadd = iter->diagadd;
}

#ifdef SBAYES
double pdlogdet(n, H)
     int n;
     RMatrix H;
{
  int i;
  double logdet, maxadd;
  
  choldecomp(H, n, 0.0, &maxadd);
  for (i = 0, logdet = 0.0; i < n; i++) logdet += 2.0 * log(H[i][i]);
  return(logdet);
}
#endif /* SBAYES */
#ifdef TODO
return amount added to make pos definite
scaling for constraints
alternate global strategies
callback function for verbose mode
#endif TODO
