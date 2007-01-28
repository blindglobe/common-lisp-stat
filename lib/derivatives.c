/* derivatives - for Bayes code in XLISP-STAT and S                    */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */

# include "xmath.h"
extern double macheps();

typedef double **RMatrix, *RVector;

# define nil 0L
# define FALSE 0
# define TRUE 1

numergrad(n, x, grad, fsum, ffun, h, typx)
     int n;
     RVector x, grad, fsum, typx;
     int (*ffun)();
     double h;
{
  int i;
  double old_xi, f1, f2, hi;

  for (i = 0; i < n; i++) {
    old_xi = x[i];
    hi = (typx != nil) ? typx[i] * h : h;
    x[i] = old_xi + hi;
    (*ffun)(x, &f1, nil, nil);
    x[i] = old_xi - hi;
    (*ffun)(x, &f2, nil, nil);
    x[i] = old_xi;
    grad[i] = (f1 - f2) / (2.0 * hi);
    fsum[i] = f1 + f2;
  }
}

numerhess(n, x, hess, f, fsum, ffun, h, typx)
     int n;
     RVector x, fsum, typx;
     RMatrix hess;
     int (*ffun)();
     double h, f;
{
  int i, j;
  double old_xi, old_xj, f1, f2, hi, hj;

  for (i = 0; i < n; i++) {
    hi = (typx != nil) ? typx[i] * h : h;
    hess[i][i] = (fsum[i] - 2 * f) / (hi * hi);
    for (j = i + 1; j < n; j++) {
      hj = (typx != nil) ? typx[j] * h : h;
      old_xi = x[i];
      old_xj = x[j];
      x[i] = old_xi + hi;
      x[j] = old_xj + hj;
      (*ffun)(x, &f1, nil, nil);
      x[i] = old_xi - hi;
      x[j] = old_xj - hj;
      (*ffun)(x, &f2, nil, nil);
      x[i] = old_xi;
      x[j] = old_xj;
      hess[i][j] = (2 * f + f1 + f2 - fsum[i] - fsum[j]) / (2.0 * hi * hj);
      hess[j][i] = hess[i][j];
    }
  }
}
