/* clinalg - C interface to basic linear algebra routines.             */
/* Copyright (c) 1990, by Luke Tierney                                 */

#include "linalg.h"

#ifdef INTPTR
typedef int PTR;
#else
typedef char *PTR;
#endif

extern double rcondest();

int min (x, y) int x, y; { return((x < y) ? x : y); }
int max (x, y) int x, y; { return((x > y) ? x : y); }

/************************************************************************/
/**                                                                    **/
/**                  Machine Epsilon Determination                     **/
/**                                                                    **/
/************************************************************************/

double macheps()
{
  static int calculated = FALSE;
  static double epsilon = 1.0;
  
  if (! calculated)
    while (1.0 + epsilon / 2.0 != 1.0) epsilon = epsilon / 2.0;
  calculated = TRUE;
  return(epsilon);
}

/************************************************************************/
/**                                                                    **/
/**           Lisp Interfaces to Linear Algebra Routines               **/
/**                                                                    **/
/************************************************************************/

chol_decomp_front(mat, n, dpars)
     PTR mat, dpars;
     int n;
{
  double *dp = (double *) dpars;
  choldecomp((double **) mat, n, *dp, dp + 1);
}

int lu_decomp_front(mat, n, iv, mode, dp)
     PTR mat, iv, dp;
     int n, mode;
{
  return(crludcmp((char **) mat, n, (int *) iv, mode, (double *) dp));
}

int lu_solve_front(a, n, indx, b, mode)
     PTR a, indx, b;
     int n, mode;
{
  return(crlubksb((char **) a, n, (int *) indx, (char *) b, mode));
}

int lu_inverse_front(pmat, n, piv, pv, mode, pinv)
     PTR pmat, piv, pv, pinv;
     int n, mode;
{
  Matrix mat = (Matrix) pmat, inv = (Matrix) pinv;
  IVector iv = (IVector) piv;
  Vector v = (Vector) pv;
  CMatrix cinv;
  RMatrix rinv;
  CVector cv;
  RVector rv;
  double d;
  int i, j, singular;

  singular = crludcmp(mat, n, iv, mode, &d);
  
  if (! singular) {
    rinv = (RMatrix) inv;
    cinv = (CMatrix) inv;
    rv = (RVector) v;
    cv = (CVector) v;
  
    for (j = 0; j < n; j++) {
      for (i = 0; i < n; i++) {
        if (mode == RE) rv[i] = rinv[i][j];
        else cv[i] = cinv[i][j];
      }
      
      singular = singular || crlubksb(mat, n, iv, v, mode);
      
      for (i = 0; i < n; i++) {
        if (mode == RE) rinv[i][j] = rv[i];
        else cinv[i][j] = cv[i];
      }
    }
  }
  return(singular);
}

sv_decomp_front(mat, m, n, w, v)
     PTR mat, w, v;
     int m, n;
{
  return(svdcmp((char **) mat, m, n, (char *) w, (char **) v));
}

qr_decomp_front(mat, m, n, v, jpvt, pivot)
     PTR mat, v, jpvt;
     int m, n, pivot;
{
  qrdecomp((char **) mat, m, n, (char **) v, (char *) jpvt, pivot);
}

double rcondest_front(mat, n)
     PTR mat;
     int n;
{
  return(rcondest((char **) mat, n));
}

make_rotation_front(n, rot, x, y, use_alpha, alpha)
     int n, use_alpha;
     PTR rot, x, y;
     double alpha;
{
  make_rotation(n, (char **) rot, (char *) x, (char *) y, use_alpha, alpha);
}

int eigen_front(a, n, w, z, fv1)
     PTR a, w, z, fv1;
     int n;
{
  int ierr;

  eigen(&n, &n, (char *) a, (char *) w, (char *) z, (char *) fv1, &ierr);
  return(ierr);
}

fft_front(n, x, work, isign)
     int n, isign;
     PTR x, work;
{
  cfft(n, (char *) x, (char *) work, isign);
}

int base_lowess_front(x, y, n, f, nsteps, delta, ys, rw, res)
     PTR x, y, ys, rw, res;
     int n, nsteps;
     double f, delta;
{
  return(lowess((char *) x, (char *) y, n, f, nsteps, delta,
		(char *) ys, (char *) rw, (char *) res));
}

range_to_rseq(n, px, ns, pxs)
     int n, ns;
     PTR px, pxs;
{
  int i;
  double xmin, xmax, *x, *xs;

  x = (double *) px;
  xs = (double *) pxs;
  for (xmax = xmin = x[0], i = 1; i < n; i++) {
    if (x[i] > xmax) xmax = x[i];
    if (x[i] < xmin) xmin = x[i];
  }
  for (i = 0; i < ns; i++)
    xs[i] = xmin + (xmax - xmin) * ((double) i) / ((double) (ns - 1));
}

int spline_front(n, x, y, ns, xs, ys, work)
     PTR x, y, xs, ys, work;
     int n, ns;
{
  return(fit_spline(n, (char *) x, (char *) y, 
		    ns, (char *) xs, (char *) ys, (char *) work));
}

kernel_dens_front(x, n, width, xs, ys, ns, code)
     PTR x, xs, ys;
     int n, ns, code;
     double width;
{
  int ktype;

  if (code == 0) ktype = 'U';
  else if (code == 1) ktype = 'T';
  else if (code == 2) ktype = 'G';
  else ktype = 'B';

  return(kernel_smooth((char *) x, nil, n, width, nil, nil, 
		       (char *) xs, (char *) ys, ns, ktype));
}

int kernel_smooth_front(x, y, n, width, xs, ys, ns, code)
     PTR x, y, xs, ys;
     int n, ns, code;
     double width;
{
  int ktype;

  if (code == 0) ktype = 'U';
  else if (code == 1) ktype = 'T';
  else if (code == 2) ktype = 'G';
  else ktype = 'B';

  return(kernel_smooth((char *) x, (char *) y, n, width, nil, nil, 
		       (char *) xs, (char *) ys, ns, ktype));
}
