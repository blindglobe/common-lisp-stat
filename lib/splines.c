#include "xmath.h"

/* natural cubic spline interpolation based on Numerical Recipes in C */

/* calculate second derivatives; assumes strictly increasing x values */
static find_spline_derivs(x, y, n, y2, u)
     double *x, *y, *y2, *u;
     int n;
{
  int i, k;
  double p, sig;

  y2[0] = u[0] = 0.0;  /* lower boundary condition for natural spline */
  
  /* decomposition loop for the tridiagonal algorithm */
  for (i = 1; i < n - 1; i++) {
    y2[i] = u[i] = 0.0; /* set in case a zero test is failed */
    if (x[i - 1] < x[i] && x[i] < x[i + 1]) {
      sig = (x[i] - x[i - 1]) / (x[i + 1] - x[i - 1]);
      p = sig * y2[i - 1] + 2.0;
      if (p != 0.0) {
        y2[i] = (sig - 1.0) / p;
        u[i] = (y[i + 1] - y[i]) / (x[i + 1] - x[i])
             - (y[i] - y[i - 1]) / (x[i] - x[i - 1]);
        u[i] = (6.0 * u[i] / (x[i + 1] - x[i - 1]) - sig * u[i - 1]) / p;
      }
    }
  }

  /* upper boundary condition for natural spline */
  y2[n - 1] = 0.0;

  /* backsubstitution loop of the tridiagonal algorithm */
  for (k = n - 2; k >= 0; k--)
    y2[k] = y2[k] * y2[k + 1] + u[k];
}

/* interpolate or extrapolate value at x using results of find_spline_derivs */
static spline_interp(xa, ya, y2a, n, x, y)
     double *xa, *ya, *y2a, x, *y;
     int n;
{
  int klo, khi, k;
  double h, b, a;

  if (x <= xa[0]) {
    h = xa[1] - xa[0];
    b = (h > 0.0) ? (ya[1] - ya[0]) / h - h * y2a[1] / 6.0 : 0.0;
    *y = ya[0] + b * (x - xa[0]);
  }
  else if (x >= xa[n - 1]) {
    h = xa[n - 1] - xa[n - 2];
    b = (h > 0.0) ? (ya[n - 1] - ya[n - 2]) / h + h * y2a[n - 2] / 6.0 : 0.0;
    *y = ya[n - 1] + b * (x - xa[n - 1]);
  }
  else {
    /* try a linear interpolation for equally spaced x values */
    k = (n - 1) * (x - xa[0]) / (xa[n - 1] - xa[0]);

    /* make sure the range is right */
    if (k < 0) k = 0;
    if (k > n - 2) k = n - 2;

    /* bisect if necessary until the bracketing interval is found */
    klo = (x >= xa[k]) ? k : 0;
    khi = (x < xa[k + 1]) ? k + 1 : n - 1;
    while (khi - klo > 1) {
      k = (khi + klo) / 2;
      if (xa[k] > x) khi = k;
      else klo = k;
    }

    /* interpolate */
    h = xa[khi] - xa[klo];
    if (h > 0.0) {
      a = (xa[khi] - x) / h;
      b = (x - xa[klo]) / h;
      *y = a * ya[klo] + b * ya[khi]
         + ((a * a * a - a) * y2a[klo] + (b * b * b - b) * y2a[khi]) * (h * h) / 6.0;
    }
    else *y = (ya[klo] + ya[khi]) / 2.0; /* should not be needed */
  }
}

fit_spline(n, x, y, ns, xs, ys, work)
     int n, ns;
     double *x, *y, *xs, *ys, *work;
{
  int i;
  double *y2, *u;

  y2 = work; u = work + n;

  if (n < 2 || ns < 1) return (1); /* signal an error */
  for (i = 1; i < n; i++)
    if (x[i - 1] >= x[i]) return(1); /* signal an error */
  
  find_spline_derivs(x, y, n, y2, u);
  
  for (i = 0; i < ns; i++)
    spline_interp(x, y, y2, n, xs[i], &ys[i]);

  return(0);
}
