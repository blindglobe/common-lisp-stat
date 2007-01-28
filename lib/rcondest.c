/* rcondest - Estimates reciprocal of condition number.                */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */
 
#include "linalg.h"

static double Max(a, b)
	double a, b;
{
  return(a > b ? a : b);
}

double rcondest(a, n)
	RMatrix a;
	int n;
{
  RVector p, pm, x;
  double est, xp, xm, temp, tempm, xnorm;
  int i, j;
  
  for (i = 0; i < n; i++)
    if (a[i][i] == 0.0) return(0.0);
    
  p = rvector(n);
  pm = rvector(n);
  x = rvector(n);
  
  /* Set est to reciprocal of L1 matrix norm of A */
  est = fabs(a[0][0]);
  for (j = 1; j < n; j++) {
    for (i = 0, temp = fabs(a[j][j]); i < j; i++)
      temp += fabs(a[i][j]);
    est = Max(est, temp);
  }
  est = 1.0 / est;
  
  /* Solve A^Tx = e, selecting e as you proceed */
  x[0] = 1.0 / a[0][0];
  for (i = 1; i < n; i++) p[i] = a[0][i] * x[0];
  for (j = 1; j < n; j++) {
    /* select ej and calculate x[j] */
    xp = ( 1.0 - p[j]) / a[j][j];
    xm = (-1.0 - p[j]) / a[j][j];
    temp = fabs(xp);
    tempm = fabs(xm);
    for (i = j + 1; i < n; i++) {
      pm[i] = p[i] + a[j][i] * xm;
      tempm += fabs(pm[i] / a[i][i]);
      p[i] += a[j][i] * xp;
      temp += fabs(p[i] / a[i][i]);
    }
    if (temp >= tempm) x[j] = xp;
    else {
      x[j] = xm;
      for (i = j + 1; i < n; i++) p[i] = pm[i];
    }
  }
  
  for (j = 0, xnorm = 0.0; j < n; j++) xnorm += fabs(x[j]);
  est = est * xnorm;
  backsolve(a, x, n, RE);
  for (j = 0, xnorm = 0.0; j < n; j++) xnorm += fabs(x[j]);
  if (xnorm > 0) est = est / xnorm;
  
  free_vector(p);
  free_vector(pm);
  free_vector(x);
  
  return(est);
}

backsolve(a, b, n, mode)
	Matrix a;
	Vector b;
	int n;
{
  int i, j;
  RMatrix ra = (RMatrix) a;
  RVector rb = (RVector) b;
  CMatrix ca = (CMatrix) a;
  CVector cb = (CVector) b;
  
  switch (mode) {
  case RE:
    for (i = n - 1; i >= 0; i--) {
      if (ra[i][i] != 0.0) rb[i] = rb[i] / ra[i][i];
      for (j = i + 1; j < n; j++) rb[i] -= ra[i][j] * rb[j];
    }
    break;
  case CX:
    for (i = n - 1; i >= 0; i--) {
      if (modulus(ca[i][i]) != 0.0) cb[i] = cdiv(cb[i], ca[i][i]);
      for (j = i + 1; j < n; j++) 
        cb[i] = csub(cb[i], cmul(ca[i][j], cb[j]));
    }
    break;
  }
}
