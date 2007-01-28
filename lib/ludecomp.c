/* ludecomp - LU decomposition and backsolving routines.               */
/* Taken from Numerical Recipies.                                      */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */
 
#include "linalg.h"

crludcmp(mat, n, indx, mode, d)
	Matrix mat;
	IVector indx;
	int n, mode;
	double *d;
{
  int i, imax, j, k, singular = FALSE;
  double big, temp;
  Complex cdum, csum;
  double rdum, rsum;
  CMatrix cmat = (CMatrix) mat;
  RMatrix rmat = (RMatrix) mat;
  RVector vv;
  
  vv = rvector(n);
  *d = 1.0;
  
  /* set up the pivot permutation vector */
  for (i = 0; i < n; i++) indx[i] = i;
  
  /* get scaling information for implicit pivoting */
  for (i = 0; i < n; i++) {
    big = 0.0;
    for (j = 0; j < n; j++) {
      temp = (mode == RE) ? fabs(rmat[i][j]) : modulus(cmat[i][j]);
      if (temp > big) big = temp;
    }
    if (big == 0.0) {
      vv[i] = 1.0;                  /* no scaling for zero rows */
      singular = TRUE;
    }
    else vv[i] = 1.0 / big;
  }
  
  /* loop over columns for Crout's method */
  for (j = 0; j < n; j++) {
    for (i = 0; i < j; i++) {
      if (mode == RE) rsum = rmat[i][j];
      else csum = cmat[i][j];
      
      for (k = 0; k < i; k++) 
        if (mode == RE) rsum -= rmat[i][k] * rmat[k][j];
        else csum = csub(csum, cmul(cmat[i][k], cmat[k][j]));
        
      if (mode == RE) rmat[i][j] = rsum;
      else cmat[i][j] = csum;
    }
    big = 0.0;
    for (i = j; i < n; i++) {
      if (mode == RE) rsum = rmat[i][j];
      else csum = cmat[i][j];
      
      for (k = 0; k < j; k++) 
        if (mode == RE) rsum -= rmat[i][k] * rmat[k][j];
        else csum = csub(csum, cmul(cmat[i][k], cmat[k][j]));
        
      if (mode == RE) rmat[i][j] = rsum;
      else cmat[i][j] = csum;
      
      temp = vv[i] * ((mode == RE) ? fabs(rsum) : modulus(csum));
      if (temp >= big) { big = temp; imax = i; }
    }
    
    /* interchange rows if needed */
    if (j != imax) {
      for (k = 0; k < n; k++) {
        if (mode == RE) {
          rdum = rmat[imax][k];
          rmat[imax][k] = rmat[j][k];
          rmat[j][k] = rdum;
        }
        else {
          cdum = cmat[imax][k];
          cmat[imax][k] = cmat[j][k];
          cmat[j][k] = cdum;
        }
      }
      *d = -(*d);
      vv[imax] = vv[j];
    }
    indx[j] = imax;
    
    /* divide by the pivot element */
    temp = (mode == RE) ? fabs(rmat[j][j]) : modulus(cmat[j][j]);
    if (temp == 0.0) singular = TRUE;
    else if (j < n - 1) {
      if (mode == RE) {
        rdum = 1.0 / rmat[j][j];
        for (i = j + 1; i < n; i++) rmat[i][j] *= rdum;
      }
      else {
        cdum = cdiv(cart2complex(1.0, 0.0), cmat[j][j]);
        for (i = j + 1; i < n; i++) cmat[i][j] = cmul(cmat[i][j], cdum);
      }
    }
  }
  free_vector(vv);
  return(singular);
}

crlubksb(a, n, indx, b, mode)
	Matrix a;
	IVector indx;
	Vector b;
	int n, mode;
{
  int i, ii, ip, j, singular = FALSE;
  CMatrix ca = (CMatrix) a;
  CVector cb = (CVector) b;
  RMatrix ra = (RMatrix) a;
  RVector rb = (RVector) b;
  double rsum;
  Complex csum;

  /* forward substitute using L part */
  for (i = 0, ii = -1; i < n; i++) {
    ip = indx[i];
    if (mode == RE) {
      rsum = rb[ip];
      rb[ip] = rb[i];
    }
    else {
      csum = cb[ip];
      cb[ip] = cb[i];
    }
    if (ii >= 0)
      for (j = ii; j <= i - 1; j++)
        if (mode == RE) rsum -= ra[i][j] * rb[j];
        else csum = csub(csum, cmul(ca[i][j], cb[j]));
    else {
      if (mode == RE) {
        if (rsum != 0.0) ii = i;
      }
      else if (csum.real != 0.0 || csum.imag != 0.0) ii = i;
    }
    if (mode == RE) rb[i] = rsum;
    else cb[i] = csum;
  }

  /* back substitute using the U part */
  for (i = n - 1; i >= 0; i--) {
    if (mode == RE) {
      rsum = rb[i];
      for (j = i + 1; j < n; j++) rsum -= ra[i][j] * rb[j];
      if (ra[i][i] == 0.0) {
        singular = TRUE;
        break;
      }
      else rb[i] = rsum / ra[i][i];
    }
    else {
      csum = cb[i];
      for (j = i + 1; j < n; j++) csum = csub(csum, cmul(ca[i][j], cb[j]));
      if (modulus(ca[i][i]) == 0.0) {
        singular = TRUE;
        break;
      }
      else cb[i] = cdiv(csum, ca[i][i]);
    }    
  }
  
  return(singular);
}

