/* clinalg - C interface to basic linear algebra routines.             */
/* Copyright (c) 1990, by Luke Tierney                                 */

#include "linalg.h"

#ifdef INTPTR
typedef int PTR;
#else
typedef char *PTR;
#endif

extern double rcondest();

extern void choldecomp();
extern int crludcmp();
extern int crlubksb();
extern int svdcmp();
extern void qrdecomp();
extern void make_rotation();
extern void eigen();
extern void cfft();
extern int lowess();
extern int fit_spline();
extern int kernel_smooth();



int
min (int x, int y)
{
  return((x < y) ? x : y); 
}

int
max (int x, int y)
{
  return((x > y) ? x : y);
}

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

void
chol_decomp_front(PTR mat, int n, PTR dpars)
{
  double *dp = (double *) dpars;
  choldecomp((double **) mat, n, *dp, dp + 1);
}

int
lu_decomp_front(PTR mat, int n, PTR iv, int mode, PTR dp)
{
  return(crludcmp((char **) mat, n, (int *) iv, mode, (double *) dp));
}

int
lu_solve_front(PTR a, int n, PTR indx, PTR b, int mode)
{
  return(crlubksb((char **) a, n, (int *) indx, (char *) b, mode));
}

int
lu_inverse_front(PTR pmat, int n, PTR piv, PTR pv, int mode, PTR pinv)
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

int
sv_decomp_front(PTR mat, int m, int n, PTR w, PTR v)
{
  return(svdcmp((char **) mat, m, n, (char *) w, (char **) v));
}

void
qr_decomp_front(PTR mat, int m, int n, PTR v, PTR jpvt, int pivot)
{
  qrdecomp((char **) mat, m, n, (char **) v, (char *) jpvt, pivot);
}

double
rcondest_front(PTR mat, int n)
{
  return(rcondest((char **) mat, n));
}

void
make_rotation_front(int n, PTR rot, PTR x, PTR y, int use_alpha, double alpha)
{
  make_rotation(n, (char **) rot, (char *) x, (char *) y, use_alpha, alpha);
}

int
eigen_front(PTR a, int n, PTR w, PTR z, PTR fv1)
{
  int ierr;

  eigen(&n, &n, (char *) a, (char *) w, (char *) z, (char *) fv1, &ierr);
  return(ierr);
}

void
fft_front(int n, PTR x, PTR work, int isign)
{
  cfft(n, (char *) x, (char *) work, isign);
}

int
base_lowess_front(PTR x, PTR y, int n, double f,
		  int nsteps, double delta, PTR  ys, PTR rw, PTR res)
{
  return(lowess((char *) x, (char *) y, n, f, nsteps, delta,
		(char *) ys, (char *) rw, (char *) res));
}

void
range_to_rseq(int n, PTR px, int ns, PTR pxs)
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

int
spline_front(int n, PTR x, PTR y, int ns,
	     PTR xs, PTR ys, PTR work)
{
  return(fit_spline(n, (char *) x, (char *) y, 
		    ns, (char *) xs, (char *) ys, (char *) work));
}

int
kernel_dens_front(PTR x, int n, double width, PTR xs, PTR ys, int ns, int code)
{
  int ktype;

  if (code == 0) ktype = 'U';
  else if (code == 1) ktype = 'T';
  else if (code == 2) ktype = 'G';
  else ktype = 'B';

  return(kernel_smooth((char *) x, nil, n, width, nil, nil, 
		       (char *) xs, (char *) ys, ns, ktype));
}

int
kernel_smooth_front(PTR x, PTR y, int n, double width,
		    PTR xs, PTR ys, int ns, int code)
{
  int ktype;

  if (code == 0) ktype = 'U';
  else if (code == 1) ktype = 'T';
  else if (code == 2) ktype = 'G';
  else ktype = 'B';

  return(kernel_smooth((char *) x, (char *) y, n, width, nil, nil, 
		       (char *) xs, (char *) ys, ns, ktype));
}
