/* makerotation - Construct rotation from x to y by alpha.             */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */
 
#include "linalg.h"

static double inner_product(size_t n, RVector x, RVector y)
{
  double result = 0.0;
  
  for (; n > 0; n--, x++, y++) result += *x * *y;
  return(result);
}
  
#define NORM(n, x) (sqrt(inner_product(n, x, x)))

void 
make_rotation(size_t n, double **rot, double *x, double *y, int use_alpha, double alpha)
{
  double nx, ny, xy, c, s;
  size_t i, j;
  
  for (i = 0; i < n; i++) {
    for (j = 0; j < n; j++) rot[i][j] = 0.0;
    rot[i][i] = 1.0;
  }
  
  nx = NORM(n, x);
  ny = NORM(n, y);
  if (nx == 0.0 || ny == 0.0) return;
  
  for (i = 0; i < n; i++) x[i] /= nx;
  for (i = 0; i < n; i++) y[i] /= ny;
  
  xy = inner_product(n, x, y);
  c = (use_alpha) ? cos(alpha) : xy;
  s = (use_alpha) ? sin(alpha) : sqrt(1 - c * c);
  
  for (i = 0; i < n; i++) y[i] -= xy * x[i];

  ny = NORM(n, y);
  if (ny == 0.0) return;
  for (i = 0; i < n; i++) y[i] /= ny;
  
  for (i = 0; i < n; i++) {
    for (j = 0; j < n; j++) 
      rot[i][j] = x[j] * (  x[i] * (c - 1.0) + y[i] * s)
                + y[j] * (- x[i] * s + y[i] * (c - 1.0));
    rot[i][i] += 1.0;
  }
}

