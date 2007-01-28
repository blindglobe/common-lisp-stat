/* choldecomp - Cholesky decomposition  routines.                      */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */
 
#include "linalg.h"

/*
choldecomp(a, n)
	RMatrix a;
	int n;
{
  int i, j, k;
  
  for (i = 0; i < n; i++) {
    for (k = 0; k < i; k++)
      a[i][i] -= a[k][i] * a[k][i];
    a[i][i] = (a[i][i] > 0) ? sqrt(a[i][i]) : 0.0;
    for (j = i + 1; j < n; j++) {
      for (k = 0; k < i; k++) a[i][j] -= a[k][i] * a[k][j];
      a[i][j] = (a[i][i] > 0.0) ? a[i][j] / a[i][i] : 0.0;
    }
  }
  for (i = 0; i < n; i++)
    for (j = 0; j < i; j++)
      a[i][j] = 0.0;
}
*/

static double Max(a, b)
	double a, b;
{
  return(a > b ? a : b);
}

choldecomp(a, n, maxoffl, maxadd)
	RMatrix a;
	int n;
	double maxoffl, *maxadd;
{
  double minl, minljj, minl2;
  int i, j, k;
  
  minl = pow(macheps(), 0.25) * maxoffl;
  minl2 = 0.0;
  
  if (maxoffl == 0.0) {
    for (i = 0; i < n; i++)
      maxoffl = Max(fabs(a[i][i]), maxoffl);
    maxoffl = sqrt(maxoffl);
    minl2 = sqrt(macheps()) * maxoffl;
  }
  
  *maxadd = 0.0;
  for (j = 0; j < n; j++) {
    for (i = 0; i < j; i++) a[j][j] -= a[j][i] * a[j][i];
    
    minljj = 0.0;
    
    for (i = j + 1; i < n; i++) {
      a[i][j] = a[j][i];
      for (k = 0; k < j; k++) a[i][j] -= a[i][k] * a[j][k];
      minljj = Max(fabs(a[i][j]), minljj);
    }
    
    minljj = Max(minljj / maxoffl, minl);
    
    if (a[j][j] > minljj * minljj) a[j][j] = sqrt(a[j][j]);
    else {
      if (minljj < minl2) minljj = minl2;
      *maxadd = Max(*maxadd, minljj * minljj - a[j][j]);
      a[j][j] = minljj;
    }
    
    for (i = j + 1; i < n; i++) a[i][j] /= a[j][j];
  }
  
  for (i = 0; i < n; i++)
    for (j = i + 1; j < n; j++) a[i][j] = 0.0;
}
