/* adapted from DQRDC of LINPACK */

#include "linalg.h"

#define SIGN(a, b) ((b) >= 0.0 ? fabs(a) : -fabs(a))

static double NORM2(i, j, n, x)
     int i, j, n;
     double **x;
{
  int k;
  double maxx, sum, temp;

  for (k = i, maxx = 0.0; k < n; k++) {
    temp = fabs(x[k][j]);
    if (maxx < temp) maxx = temp;
  }
  if (maxx == 0.0) return(0.0);
  else {
    for (k = i, sum = 0.0; k < n; k++) {
      temp = x[k][j] / maxx;
      sum += temp * temp;
    }
    return(maxx * sqrt(sum));
  }
}

static double DOT(i, j, k, n, x)
     int i, j, k;
     double **x;
{
  int l;
  double sum;

  for (l = i, sum = 0.0; l < n; l++) sum += x[l][j] * x[l][k];
  return(sum);
}

static AXPY(i, j, k, n, a, x)
     int i, j, k, n;
     double a, **x;
{
  int l;
  for (l = i; l < n; l++) x[l][k] = a * x[l][j] + x[l][k];
}

static SCALE(i, j, n, a, x)
     int i, j, n;
     double a, **x;
{
  int k;
  for (k = i; k < n; k++) x[k][j] *= a;
}

static SWAP(i, j, n, a)
  int i, j, n;
  double **a;
{
  int k;
  double temp;
  for (k = 0; k < n; k++) {
    temp = a[k][i];
    a[k][i] = a[k][j];
    a[k][j] = temp;
  }
}

qrdecomp(x,n,p,v,jpvt,pivot)
     int n, p, pivot;
     int *jpvt;
     double **x, **v;
{
  int i,j,k,jp,l,lp1,lup,maxj;
  double maxnrm,tt,*qraux,*work;
  double nrmxl,t;

  if (n < 0) return;
  work = v[0];
  qraux = rvector(p);

  /*
   * compute the norms of the free columns.
   */
  if (pivot)
    for (j = 0; j < p; j++) {
      jpvt[j] = j;
      qraux[j] = NORM2(0, j, n, x);
      work[j] = qraux[j];
    }
  /*
   * perform the householder reduction of x.
   */
  lup = (n < p) ? n : p;
  for (l = 0; l < lup; l++) {
    if (pivot) {
      /*
       * locate the column of largest norm and bring it
       * into the pivot position.
       */
      maxnrm = 0.0;
      maxj = l;
      for (j = l; j < p; j++) 
	if (qraux[j]>maxnrm) {
	  maxnrm = qraux[j];
	  maxj = j;
	}
      if (maxj!=l) {
	SWAP(l, maxj, n, x);
	qraux[maxj] = qraux[l];
	work[maxj] = work[l];
	jp = jpvt[maxj];
	jpvt[maxj] = jpvt[l];
	jpvt[l] = jp;
      }
    }
    qraux[l] = 0.0;
    if (l != n-1) {
      /*
       * compute the householder transformation for column l.
       */
      nrmxl = NORM2(l, l, n, x);
      if (nrmxl != 0.0) {
	if (x[l][l] != 0.0)
	  nrmxl = SIGN(nrmxl, x[l][l]);
	SCALE(l, l, n, 1.0 / nrmxl, x);
	x[l][l] = 1.0+x[l][l];
	/*
	 * apply the transformation to the remaining columns,
	 * updating the norms.
	 */
	lp1 = l+1;
	for (j = lp1; j < p; j++) {
	  t = -DOT(l, l, j, n, x) / x[l][l];
	  AXPY(l, l, j, n, t, x);
	  if (pivot)
	    if (qraux[j]!=0.0) {
	      tt = 1.0-(fabs(x[l][j])/qraux[j])*(fabs(x[l][j])/qraux[j]);
	      if (tt < 0.0) tt = 0.0;
	      t = tt;
	      tt = 1.0+0.05*tt*(qraux[j]/work[j])*(qraux[j]/work[j]);
	      if (tt!=1.0)
		qraux[j] = qraux[j]*sqrt(t);
	      else {
		qraux[j] = NORM2(l+1, j, n, x);
		work[j] = qraux[j];
	      }
	    }
	}
	/*
	 * save the transformation.
	 */
	qraux[l] = x[l][l];
	x[l][l] = -nrmxl;
      }
    }
  }

  /* copy over the upper triangle of a */
  for (i = 0; i < p; i++) {
    for (j = 0; j < i; j++) v[i][j] = 0.0;
    for (j = i; j < p; j++) {
      v[i][j] = x[i][j];
    }
  }
    
  /* accumulate the Q transformation -- assumes p <= n */
  for (i = 0; i < p; i++) {
    x[i][i] = qraux[i];
    for (k = 0; k < i; k++) x[k][i] = 0.0;
  }
  for (i = p - 1; i >= 0; i--) {
    if (i == n - 1) x[i][i] = 1.0;
    else {
      for (k = i; k < n; k++) x[k][i] = -x[k][i];
      x[i][i] += 1.0;
    }
    for (j = i - 1; j >= 0; j--) {
      if (x[j][j] != 0.0) {
	t = -DOT(j, j, i, n, x) / x[j][j];
	AXPY(j, j, i, n, t, x);
      }
    }
  }

  free_vector(qraux);
}
