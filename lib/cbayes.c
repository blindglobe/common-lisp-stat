/* cbayes - Lisp interface to laplace approximation stuff              */
/* Copyright (c) 1990, by Luke Tierney                                 */
 
#include "linalg.h"

#ifdef INTPTR
typedef int PTR;
#else
typedef char *PTR;
#endif

extern char *calloc(), *realloc();

/************************************************************************/
/**                                                                    **/
/**                      Definitions and Globals                       **/
/**                                                                    **/
/************************************************************************/

#define MAXALLOC 20

static char  *mem[MAXALLOC], memcount;

typedef struct {
  int n, m, k, itnlimit, backtrack, verbose, vals_suppl, exptilt;
  int count, termcode;
} MaxIPars;

typedef struct {
  double typf, h, gradtol, steptol, maxstep, dflt, tilt, newtilt, hessadd;
} MaxDPars;

typedef struct {
  MaxIPars max;
  int full, covar;
} MomIPars;

typedef struct {
  MaxDPars max;
  double mgfdel;
} MomDPars;

/************************************************************************/
/**                                                                    **/
/**                 Fake Replacements for S Interface                  **/
/**                                                                    **/
/************************************************************************/

static meminit() 
{
  static inited = FALSE;
  int i;

  if (! inited) {
    for (i = 0; i < MAXALLOC; i++) mem[i] = nil;
    inited = TRUE;
  }

  memcount = 0;
}

static makespace(pptr, size)
     char **pptr;
     int size;
{
  if (size <= 0) return;
  if (*pptr == nil) *pptr = calloc(size, 1);
  else *pptr = realloc(*pptr, size);
  if (size > 0 && *pptr == nil) xlfail("memory allocation failed");
}

call_S(fun, narg, args, mode, length, names, nvals, values)
     char *fun, **args, **mode, **names, **values;
     long narg, nvals, *length;
{
  int n = length[0], derivs;
  static double *fval = nil, *grad = nil, *hess = nil;

  makespace(&fval, 1 * sizeof(double));
  makespace(&grad, n * sizeof(double));
  makespace(&hess, n * n * sizeof(double));

  callLminfun(n, args[0], fval, grad, hess, &derivs);
  
  values[0] = (char *) fval;
  values[1] = (derivs > 0) ? (char *) grad : nil;
  values[2] = (derivs > 1) ? (char *) hess : nil;
}

Recover(s, w)
     char *s, *w;
{
  xlfail(s);
}

/************************************************************************/
/**                                                                    **/
/**                         Callback Function                          **/
/**                                                                    **/
/************************************************************************/

static callLminfun(n, x, fval, grad, hess, derivs)
     int n, *derivs;
     RVector x, grad, hess;
     double *fval;
{
  maximize_callback(n, (PTR) x, 
		    (PTR) fval, (PTR) grad, (PTR) hess, (PTR) derivs);
}	   
  
/************************************************************************/
/**                                                                    **/
/**                      Numerical Derivatives                         **/
/**                                                                    **/
/************************************************************************/

numgrad_front(n, px, pgrad, h, pscale)
     int n;
     PTR px, pgrad, pscale;
     double h;
{
  LVAL f = nil;
  double fval;

  evalfront(&f, &n, (double *) px,
	    &fval, (double *) pgrad, nil, &h, (double *) pscale);
}

numhess_front(n, px, pf, pgrad, phess, h, pscale)
     int n;
     PTR px, pf, pgrad, phess, pscale;
     double h;
{
  LVAL f = nil;

  evalfront(&f, &n, (double *) px,
	    (double *) pf, (double *) pgrad, (double *) phess,
	    &h, (double *) pscale);
}

/************************************************************************/
/**                                                                    **/
/**                      Maximization Interface                        **/
/**                                                                    **/
/************************************************************************/

/* internals array information */
#define INTLEN 12
#define F_POS 0
#define G_POS 1
#define C_POS 2
#define X_POS 3
#define SCALE_POS 4
#define FVALS_POS 5
#define CVALS_POS 6
#define CTARG_POS 7
#define IPARS_POS 8
#define DPARS_POS 9
#define TSCAL_POS 10
#define MULT_POS 11

static MaxIPars getMaxIPars(ipars)
     int *ipars;
{
  MaxIPars ip;

  ip.n = ipars[0];
  ip.m = ipars[1];
  ip.k = ipars[2];
  ip.itnlimit = ipars[3];
  ip.backtrack = ipars[4];
  ip.verbose = ipars[5];
  ip.vals_suppl = ipars[6];
  ip.exptilt = ipars[7];
  ip.count = ipars[8];
  ip.termcode = ipars[9];

  return(ip);
}

static MaxDPars getMaxDPars(dpars)
     double *dpars;
{
  MaxDPars dp;

  dp.typf = dpars[0];
  dp.h = dpars[1];
  dp.gradtol = dpars[2];
  dp.steptol = dpars[3];
  dp.maxstep = dpars[4];
  dp.dflt = dpars[5];
  dp.tilt = dpars[6];
  dp.newtilt = dpars[7];
  dp.hessadd = dpars[8];

  return(dp);
}

minfo_maximize(px, pfvals, pscale, pip, pdp, verbose)
     PTR px, pfvals, pscale, pip, pdp;
     int verbose;
{
  LVAL f = nil;
  MaxIPars ip;
  MaxDPars dp;
  int n, m, k;
  static double *dx, *typx, *fvals;
  char *msg;

  dx = (double *) px;
  typx = (double *) pscale;
  fvals = (double *) pfvals;

  ip = getMaxIPars((int *) pip);
  dp = getMaxDPars((double *) pdp);

  m = 0;
  k = 0;
  n = ip.n;
  if (verbose >= 0) ip.verbose = verbose;

  meminit();
  maxfront(&f, nil, nil, dx, typx, fvals, nil, nil, nil,
	   &ip, &dp, nil, &msg);

  bufputstr(msg);
}
