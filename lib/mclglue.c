#include "linalg.h"

typedef char *PTR;

extern double unirand(), gamma();
extern double normalcdf(), normalquant(), normaldens(), normalrand();
extern double bnormcdf();
extern double cauchycdf(), cauchyquant(), cauchydens(), cauchyrand();
extern double gammacdf(), gammaquant(), gammadens(), gammarand();
extern double chisqcdf(), chisqquant(), chisqdens(), chisqrand();
extern double betacdf(), betaquant(), betadens(), betarand();
extern double tcdf(), tquant(), tdens(), trand();
extern double fcdf(), fquant(), fdens(), frand();
extern double poissoncdf(), poissonpmf();
extern int poissonquant(), poissonrand();
extern double binomialcdf(), binomialpmf();
extern int binomialquant(), binomialrand();

/***********************************************************************/
/***********************************************************************/
/****                                                               ****/
/****                        Basic Utilities                        ****/
/****                                                               ****/
/***********************************************************************/
/***********************************************************************/

/***********************************************************************/
/**                                                                   **/
/**                    Callback Support Functions                     **/
/**                                                                   **/
/***********************************************************************/

static int ccl_integer_value;
static double ccl_double_value;
static PTR ccl_ptr_value;
ccl_store_integer(x) int x; { ccl_integer_value = x; }
ccl_store_double(x) double x; { ccl_double_value = x; }
ccl_store_ptr(x) PTR x; { ccl_ptr_value = x; }

/***************************************************************************/
/**                                                                       **/
/**                       Lisp-Managed Calloc/Free                        **/
/**                                                                       **/
/***************************************************************************/

#ifdef DODO
static void (*new_ptr)();
static void (*free_ptr)();

register_new_ptr(f) void (*f)(); { new_ptr = f; } 
register_free_ptr(f) void (*f)(); { free_ptr = f; } 

char *calloc(n, m)
	int n, m;
{
  int i, N = n * m;
  char *p;
  
  (*new_ptr)(N);
  p = (char *) ccl_ptr_value;
  for (i = 0; i < N; i++) p[i] = 0;
  return(p);
}

malloc() { xlfail("malloc not available yet"); }
realloc() { xlfail("realloc not available yet"); }

void free(p)
	char *p;
{
  (*free_ptr)(p);
}
#endif DODO

/***************************************************************************/
/**                                                                       **/
/**                     Storage Allocation Functions                      **/
/**                                                                       **/
/***************************************************************************/

PTR la_base_allocate(n, m)
	unsigned n, m;
{
  char *p = calloc(n, m);
  if (p == nil) xlfail("allocation failed");
  return((PTR) p);
}

int la_base_free_alloc(p)
	PTR p;
{
  if (p) free((char *) p);
  return(0);
}

int la_mode_size(mode)
	int mode;
{
  switch (mode) {
  case IN: return(sizeof(int));
  case RE: return(sizeof(double));
  case CX: return(sizeof(Complex));
  }
  return(0);
}

/***************************************************************************/
/**                                                                       **/
/**                    Callbacks for Internal Storage                     **/
/**                                                                       **/
/***************************************************************************/

int (*ccl_la_allocate)(), (*ccl_la_free_alloc)();

register_la_allocate(f) int (*f)(); { ccl_la_allocate = f; }
register_la_free_alloc(f) int (*f)(); { ccl_la_free_alloc = f; }

PTR la_allocate(n, m)
     int n, m;
{
  (*ccl_la_allocate)(n, m);
  return(ccl_ptr_value);
}

la_free_alloc(p)
     PTR p;
{
  (*ccl_la_free_alloc)(p);
}

/***************************************************************************/
/**                                                                       **/
/**                     Storage Access Functions                          **/
/**                                                                       **/
/***************************************************************************/

int la_get_integer(p, i)
     PTR p;
     int i;
{
  return(*(((int *) p) + i));
}

double la_get_double(p, i)
     PTR p;
     int i;
{
  return(*(((double *) p) + i));
}

double la_get_complex_real(p, i)
     PTR p;
     int i;
{
  Complex *c = ((Complex *) p) + i;
  return(c->real);
}

double la_get_complex_imag(p, i)
     PTR p;
     int i;
{
  Complex *c = ((Complex *) p) + i;
  return(c->imag);
}

PTR la_get_pointer(p, i)
     PTR p;
	 int i;
{
  return(*(((PTR *) p) + i));
}

/***************************************************************************/
/**                                                                       **/
/**                     Storage Mutation Functions                        **/
/**                                                                       **/
/***************************************************************************/

int la_put_integer(p, i, x)
     PTR p;
     int i, x;
{
  *(((int *) p) + i) = x;
  return(0);
}

int la_put_double(p, i, x)
     PTR p;
     int i;
     double x;
{
  *(((double *) p) + i) = x;
  return(0);
}

int la_put_complex(p, i, x, y)
     PTR p;
     int i;
     double x, y;
{
  Complex *c = ((Complex *) p) + i;
  c->real = x;
  c->imag = y;
  return(0);
}

int la_put_pointer(p, i, x)
     PTR p, x;
	 int i;
{
  *(((PTR *) p) + i) = x;
  return(0);
}

/***********************************************************************/
/**                                                                   **/
/**               XLISP Internal Error Message Emulation              **/
/**                                                                   **/
/***********************************************************************/

char buf[1000];

static int (*ccl_set_buf_char_fptr)();
register_set_buf_char(f) int (*f)(); { ccl_set_buf_char_fptr = f; }
set_buf_char(n, c) int n, c; { (*ccl_set_buf_char_fptr)(n, c); }

static int (*ccl_print_buffer)();
register_print_buffer(f) int (*f)(); { ccl_print_buffer = f; }
print_buffer(n, m) int n, m; { (*ccl_print_buffer)(n, m); }

static int bufpos = 0;
 
static resetbuf() { bufpos = 0; }

static prbuf(s)
    char *s;
{
  int i, n;
  
  n = strlen(s);
  for (i = 0; i <n; i++, bufpos++) set_buf_char(bufpos, s[i]);
  set_buf_char(bufpos, 0);
}

xlfail(s)
	char *s;
{
  resetbuf();
  prbuf(s);
  print_buffer(bufpos, 1);
}

stdputstr(s)
	char *s;
{
  resetbuf();
  prbuf(s);
  print_buffer(bufpos, 0);
}

bufputstr(s)
	char *s;
{
  resetbuf();
  prbuf(s);
}

/***************************************************************************/
/***************************************************************************/
/****                                                                   ****/
/*****             Lisp Interfaces to Linear Algebra Routines           ****/
/****                                                                   ****/
/***************************************************************************/
/***************************************************************************/

ccl_chol_decomp_front(mat, n, dpars)
     PTR mat, dpars;
     int n;
{
  return(chol_decomp_front(mat, n, dpars));
}

ccl_lu_decomp_front(mat, n, iv, mode, dp)
     PTR mat, iv, dp;
     int n, mode;
{
  return(lu_decomp_front(mat, n, iv, mode, dp));
}

ccl_lu_solve_front(a, n, indx, b, mode)
     PTR a, indx, b;
     int n, mode;
{
  return(lu_solve_front(a, n, indx, b, mode));
}

ccl_lu_inverse_front(pmat, n, piv, pv, mode, pinv)
     PTR pmat, piv, pv, pinv;
     int n, mode;
{
  return(lu_inverse_front(pmat, n, piv, pv, mode, pinv));
}

ccl_sv_decomp_front(mat, m, n, w, v)
     PTR mat, w, v;
     int m, n;
{
  return(sv_decomp_front(mat, m, n, w, v));
}

ccl_qr_decomp_front(mat, m, n, v, jpvt, pivot)
     PTR mat, v, jpvt;
     int m, n, pivot;
{
  return(qr_decomp_front(mat, m, n, v, jpvt, pivot));
}

double ccl_rcondest_front(mat, n)
     PTR mat;
     int n;
{
  return(rcondest_front(mat, n));
}

ccl_make_rotation_front(n, rot, x, y, use_alpha, alpha)
     int n, use_alpha;
     PTR rot, x, y;
     double alpha;
{
  return(make_rotation_front(n, rot, x, y, use_alpha, alpha));
}

ccl_eigen_front(a, n, w, z, fv1)
     PTR a, w, z, fv1;
     int n;
{
  return(eigen_front(a, n, w, z, fv1));
}

ccl_range_to_rseq(n, px, ns, pxs)
     int n, ns;
     PTR px, pxs;
{
  return(range_to_rseq(n, px, ns, pxs));
}

ccl_spline_front(n, x, y, ns, xs, ys, work)
     PTR x, y, xs, ys, work;
     int n, ns;
{
  return(spline_front(n, x, y, ns, xs, ys, work));
}

ccl_kernel_dens_front(x, n, width, xs, ys, ns, code)
     PTR x, xs, ys;
     int n, ns, code;
     double width;
{
  return(kernel_dens_front(x, n, width, xs, ys, ns, code));
}

ccl_kernel_smooth_front(x, y, n, width, xs, ys, ns, code)
     PTR x, y, xs, ys;
     int n, ns, code;
     double width;
{
  return(kernel_smooth_front(x, y, n, width, xs, ys, ns, code));
}

ccl_base_lowess_front(x, y, n, f, nsteps, delta, ys, rw, res)
     PTR x, y, ys, rw, res;
     int n, nsteps;
     double f, delta;
{
  return(base_lowess_front(x, y, n, f, nsteps, delta, ys, rw, res));
}

ccl_fft_front(n, x, work, isign)
     int n, isign;
     PTR x, work;
{
  return(fft_front(n, x, work, isign));
}

static int (*ccl_maximize_callback)();
register_maximize_callback(f) int (*f)(); { ccl_maximize_callback = f; }
maximize_callback(n, px, pfval, pgrad, phess, pderivs)
     int n;
     PTR px, pfval, pgrad, phess, pderivs;
{
  (*ccl_maximize_callback)(n, px, pfval, pgrad, phess, pderivs);
}

ccl_numgrad_front(n, px, pgrad, h, pscale)
     int n;
     PTR px, pgrad, pscale;
     double h;
{
  return(numgrad_front(n, px, pgrad, h, pscale));
}

ccl_numhess_front(n, px, pf, pgrad, phess, h, pscale)
     int n;
     PTR px, pf, pgrad, phess, pscale;
     double h;
{
  return(numhess_front(n, px, pf, pgrad, phess, h, pscale));
}

ccl_minfo_maximize(px, pfvals, pscale, pip, pdp, verbose)
     PTR px, pfvals, pscale, pip, pdp;
     int verbose;
{
  return(minfo_maximize(px, pfvals, pscale, pip, pdp, verbose));
}

/***********************************************************************/
/***********************************************************************/
/****                                                               ****/
/****                   Probability Distributions                   ****/
/****                                                               ****/
/***********************************************************************/
/***********************************************************************/

static int (*ccl_uni_fptr)();
register_uni(f) int (*f)(); { ccl_uni_fptr = f; }
double uni() { (*ccl_uni_fptr)(); return(ccl_double_value); }

double ccl_gamma(x) double x; { return(gamma(x)); }

double ccl_normalcdf(x) double x; { return(normalcdf(x)); }
double ccl_normalquant(x) double x; { return(normalquant(x)); }
double ccl_normaldens(x) double x; { return(normaldens(x)); }
double ccl_normalrand() { return(normalrand()); }
double ccl_bnormcdf(x, y, z) double x, y, z; { return(bnormcdf(x,y,z)); }

double ccl_cauchycdf(x) double x; { return(cauchycdf(x)); }
double ccl_cauchyquant(x) double x; { return(cauchyquant(x)); }
double ccl_cauchydens(x) double x; { return(cauchydens(x)); }
double ccl_cauchyrand() { return(cauchyrand()); }

double ccl_gammacdf(x, y) double x, y; { return(gammacdf(x, y)); }
double ccl_gammaquant(x, y) double x, y; { return(gammaquant(x, y)); }
double ccl_gammadens(x, y) double x, y; { return(gammadens(x, y)); }
double ccl_gammarand(x) double x; { return(gammarand(x)); }

double ccl_chisqcdf(x, y) double x, y; { return(chisqcdf(x, y)); }
double ccl_chisqquant(x, y) double x, y; { return(chisqquant(x, y)); }
double ccl_chisqdens(x, y) double x, y; { return(chisqdens(x, y)); }
double ccl_chisqrand(x) double x; { return(chisqrand(x)); }

double ccl_betacdf(x, y, z) double x, y, z; { return(betacdf(x, y, z)); }
double ccl_betaquant(x, y, z) double x, y, z; { return(betaquant(x, y, z)); }
double ccl_betadens(x, y, z) double x, y, z; { return(betadens(x, y, z)); }
double ccl_betarand(x, y) double x, y; { return(betarand(x, y)); }

double ccl_tcdf(x, y) double x, y; { return(tcdf(x, y)); }
double ccl_tquant(x, y) double x, y; { return(tquant(x, y)); }
double ccl_tdens(x, y) double x, y; { return(tdens(x, y)); }
double ccl_trand(x) double x; { return(trand(x)); }

double ccl_fcdf(x, y, z) double x, y, z; { return(fcdf(x, y, z)); }
double ccl_fquant(x, y, z) double x, y, z; { return(fquant(x, y, z)); }
double ccl_fdens(x, y, z) double x, y, z; { return(fdens(x, y, z)); }
double ccl_frand(x, y) double x, y; { return(frand(x, y)); }

double ccl_poissoncdf(x, y) double x, y; { return(poissoncdf(x, y)); }
int ccl_poissonquant(x, y) double x, y; { return(poissonquant(x, y)); }
double ccl_poissonpmf(x, y) int x; double y; { return(poissonpmf(x, y)); }
int ccl_poissonrand(x) double x; { return(poissonrand(x)); }

double ccl_binomialcdf(x, y, z) double x, z; int y; { return(binomialcdf(x, y, z)); }
int ccl_binomialquant(x, y, z) double x, z; int y; { return(binomialquant(x, y, z)); }
double ccl_binomialpmf(x, y, z) int x, y; double z; { return(binomialpmf(x, y, z)); }
int ccl_binomialrand(x, y) int x; double y; { return(binomialrand(x, y)); }
