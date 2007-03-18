/* cbayes - Lisp interface to laplace approximation stuff              */
/* Copyright (c) 1990, by Luke Tierney                                 */

#include <stdlib.h>  /* for calloc/realloc */
#include "linalg.h"

#ifdef INTPTR
typedef int PTR;
#else
typedef char *PTR;
#endif

extern void maximize_callback(int, PTR, PTR, PTR, PTR, PTR);
extern void evalfront(char **, int *, double *, double *, double *,
		      double *, double *, double *);

extern void maxfront();
extern void bufputstr(char *);

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

static void 
meminit() 
{
  static int inited = FALSE;
  int i;

  if (! inited) {
    for (i = 0; i < MAXALLOC; i++) mem[i] = nil;
    inited = TRUE;
  }

  memcount = 0;
}

static int
makespace(void **pptr, int size) /* why are we using **char? */
{
  if (size <= 0) {
    return(1); /* we've done, by default, what we asked for. */
  }
  if (*pptr == nil) {
    *pptr = calloc(size, 1);
  } else {
    *pptr = realloc(*pptr, size);
  }
  if (size > 0 && *pptr == nil) {
    return(0); /* xlfail("memory allocation failed"); FIXME:AJR xlfail redef.   */
  } 
  return(1);
}


/************************************************************************/
/**                                                                    **/
/**                         Callback Function                          **/
/**                                                                    **/
/************************************************************************/

static void
callLminfun(int n,
	    double *x, double *fval, double *grad, double *hess,
	    int *derivs)
{
  maximize_callback(n, (PTR) x, 
		    (PTR) fval, (PTR) grad, (PTR) hess, (PTR) derivs);
}

void
call_S(char *fun, long narg, char **args, char **mode, long *length,char **names,
       long nvals, char **values)
{
  int n = length[0], derivs;
  static double *fval = nil, *grad = nil, *hess = nil;

  makespace((void **)&fval, 1 * sizeof(double)); /* probably should test the
					   result of this and the next
					   2 to make sure that they are 
					   appropriate */ 
  makespace((void **)&grad, n * sizeof(double));
  makespace((void **)&hess, n * n * sizeof(double));

  callLminfun(n,(double *)args[0], fval, grad, hess, &derivs);
  
  values[0] = (char *) fval;
  values[1] = (derivs > 0) ? (char *) grad : nil;
  values[2] = (derivs > 1) ? (char *) hess : nil;
}

void
Recover(s, w)
     char *s, *w;
{
  /* FIXME:AJR: xlfail(s); */
  return;
}
  
/************************************************************************/
/**                                                                    **/
/**                      Numerical Derivatives                         **/
/**                                                                    **/
/************************************************************************/

void
numgrad_front(n, px, pgrad, h, pscale)
     int n;
     PTR px, pgrad, pscale;
     double h;
{
  LVAL f = nil;
  double fval;

  evalfront((char **)&f, &n, (double *) px,
	    &fval, (double *) pgrad, nil, &h, (double *) pscale);
}

void
numhess_front(n, px, pf, pgrad, phess, h, pscale)
     int n;
     PTR px, pf, pgrad, phess, pscale;
     double h;
{
  LVAL f = nil;

  evalfront((char **)&f, &n, (double *) px,
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

void
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
