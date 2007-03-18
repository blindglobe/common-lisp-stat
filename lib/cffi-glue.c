#include <malloc.h>
#include <string.h>

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

void xlfail(char *);

extern int chol_decomp_front();
extern int lu_decomp_front();
extern int lu_solve_front();
extern int lu_inverse_front();
extern int sv_decomp_front();
extern int qr_decomp_front();
extern int rcondest_front();
extern int make_rotation_front();
extern int eigen_front();
extern int range_to_rseq();
extern int spline_front();
extern int kernel_dens_front();
extern int kernel_smooth_front();
extern int base_lowess_front();
extern int fft_front();
extern int numgrad_front();
extern int numhess_front();
extern int minfo_maximize();

/***********************************************************************/
/****                        Basic Utilities                        ****/
/***********************************************************************/

/***********************************************************************/
/**                    Callback Support Functions                     **/
/***********************************************************************/

static int ccl_integer_value;
static double ccl_double_value;
static PTR ccl_ptr_value;
void ccl_store_integer(int x)  { ccl_integer_value = x; }
void ccl_store_double(double x)  { ccl_double_value = x; }
void ccl_store_ptr(PTR x) { ccl_ptr_value = x; }

/***************************************************************************/
/**                       Lisp-Managed Calloc/Free                        **/
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
#endif /* DODO*/

/***************************************************************************/
/**                                                                       **/
/**                     Storage Allocation Functions                      **/
/**                                                                       **/
/***************************************************************************/

PTR
la_base_allocate(unsigned n, unsigned m)
{
  char *p = calloc(n, m);
  if (p == nil) xlfail("allocation failed");
  return((PTR) p);
}

int
la_base_free_alloc(PTR p)
{
  if (p) free((char *) p);
  return(0);
}

int
la_mode_size(int mode)
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

void register_la_allocate(f) int (*f)(); { ccl_la_allocate = f; }
void register_la_free_alloc(f) int (*f)(); { ccl_la_free_alloc = f; }

PTR
la_allocate(int n, int m)
{
  (*ccl_la_allocate)(n, m);
  return(ccl_ptr_value);
}

void
la_free_alloc(PTR p)
{
  (*ccl_la_free_alloc)(p);
}

/***************************************************************************/
/**                                                                       **/
/**                     Storage Access Functions                          **/
/**                                                                       **/
/***************************************************************************/

int
la_get_integer(PTR p, int i)
{
  return(*(((int *) p) + i));
}

double
la_get_double(PTR p, int i)
{
  return(*(((double *) p) + i));
}

double
la_get_complex_real(PTR p, int i)
{
  Complex *c = ((Complex *) p) + i;
  return(c->real);
}

double
la_get_complex_imag(PTR p, int i)
{
  Complex *c = ((Complex *) p) + i;
  return(c->imag);
}

PTR
la_get_pointer(PTR p, int i)
{
  return(*(((PTR *) p) + i));
}

/***************************************************************************/
/**                                                                       **/
/**                     Storage Mutation Functions                        **/
/**                                                                       **/
/***************************************************************************/

int
la_put_integer(PTR p, int i, int x)
{
  *(((int *) p) + i) = x;
  return(0);
}

int la_put_double(PTR p, int i, double x)
{
  *(((double *) p) + i) = x;
  return(0);
}

int
la_put_complex(PTR p, int i, double x, double y)
{
  Complex *c = ((Complex *) p) + i;
  c->real = x;
  c->imag = y;
  return(0);
}

int
la_put_pointer(PTR p, int i, PTR x)
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
void register_set_buf_char(f) int (*f)(); { ccl_set_buf_char_fptr = f; }
void set_buf_char(int n, int c)  { (*ccl_set_buf_char_fptr)(n, c); }

static int (*ccl_print_buffer)();
void register_print_buffer(f) int (*f)(); { ccl_print_buffer = f; }
void print_buffer(int n, int m) { (*ccl_print_buffer)(n, m); }

static int bufpos = 0;
 
static void
resetbuf()
{
  bufpos = 0;
}

static void
prbuf(char *s)
{
  int i, n;
  
  n = strlen(s);
  for (i = 0; i <n; i++, bufpos++) set_buf_char(bufpos, s[i]);
  set_buf_char(bufpos, 0);
}

void
xlfail(char *s)
{
  resetbuf();
  prbuf(s);
  print_buffer(bufpos, 1);
}

void
stdputstr(char *s)
{
  resetbuf();
  prbuf(s);
  print_buffer(bufpos, 0);
}

void
bufputstr(char *s)
{
  resetbuf();
  prbuf(s);
}

/***************************************************************************/
/****                                                                   ****/
/*****             Lisp Interfaces to Linear Algebra Routines           ****/
/****                                                                   ****/
/***************************************************************************/

int
ccl_chol_decomp_front(PTR mat, int n, PTR dpars)
{
  return(chol_decomp_front(mat, n, dpars));
}

int
ccl_lu_decomp_front(PTR mat, int n, PTR iv, int mode, PTR dp)
{
  return(lu_decomp_front(mat, n, iv, mode, dp));
}

int
ccl_lu_solve_front(PTR a, int n, PTR indx, PTR b, int mode)
{
  return(lu_solve_front(a, n, indx, b, mode));
}

int
ccl_lu_inverse_front(PTR pmat, int n, PTR piv, PTR pv, int mode, PTR pinv)
{
  return(lu_inverse_front(pmat, n, piv, pv, mode, pinv));
}

int 
ccl_sv_decomp_front(PTR mat, int m, int n, PTR w, PTR v)
{
  return(sv_decomp_front(mat, m, n, w, v));
}


int
ccl_qr_decomp_front(PTR mat, int m, int n, PTR v, PTR jpvt, int pivot)
{
  return(qr_decomp_front(mat, m, n, v, jpvt, pivot));
}

double
ccl_rcondest_front(PTR mat, int n)
{
  return(rcondest_front(mat, n));
}

int
ccl_make_rotation_front(int n, PTR rot, PTR x, PTR y, int use_alpha, double alpha)
{
  return(make_rotation_front(n, rot, x, y, use_alpha, alpha));
}

int
ccl_eigen_front(PTR a, int n, PTR w, PTR z, PTR fv1)
{
  return(eigen_front(a, n, w, z, fv1));
}

int
ccl_range_to_rseq(int n, PTR px, int ns, PTR pxs)
{
  return(range_to_rseq(n, px, ns, pxs));
}

int
ccl_spline_front(int n, PTR x, PTR y, int ns, PTR xs, PTR ys, PTR work)
{
  return(spline_front(n, x, y, ns, xs, ys, work));
}

int 
ccl_kernel_dens_front(PTR x, int n, double width, PTR xs, PTR ys, int ns, int code)
{
  return(kernel_dens_front(x, n, width, xs, ys, ns, code));
}

int 
ccl_kernel_smooth_front(PTR x, PTR y, int n, double width, PTR xs, PTR ys, int ns, int code)
{
  return(kernel_smooth_front(x, y, n, width, xs, ys, ns, code));
}

int
ccl_base_lowess_front(PTR x, PTR y, int n, double f,
		      int nsteps, double delta, PTR ys, PTR rw, PTR res)
{
  return(base_lowess_front(x, y, n, f, nsteps, delta, ys, rw, res));
}

int
ccl_fft_front(int n, PTR x, PTR work, int isign)
{
  return(fft_front(n, x, work, isign));
}

static int (*ccl_maximize_callback)();

void
register_maximize_callback(f)
     int (*f)();
{
  ccl_maximize_callback = f;
}

void
maximize_callback(int n, PTR px, PTR pfval, PTR pgrad, PTR phess, PTR pderivs)
{
  (*ccl_maximize_callback)(n, px, pfval, pgrad, phess, pderivs);
}

int 
ccl_numgrad_front(int n, PTR px, PTR pgrad, double h, PTR pscale)
{
  return(numgrad_front(n, px, pgrad, h, pscale));
}

int 
ccl_numhess_front(int n, PTR px, PTR pf, PTR pgrad, PTR phess, double h, PTR pscale)
{
  return(numhess_front(n, px, pf, pgrad, phess, h, pscale));
}

int 
ccl_minfo_maximize(PTR px, PTR pfvals, PTR pscale, PTR pip, PTR pdp, int verbose)
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
void register_uni(f) int (*f)(); { ccl_uni_fptr = f; }
double uni() { (*ccl_uni_fptr)(); return(ccl_double_value); }

double ccl_gamma(double x) { return(gamma(x)); }

double ccl_normalcdf(double x)  { return(normalcdf(x)); }
double ccl_normalquant(double x) { return(normalquant(x)); }
double ccl_normaldens(double x) { return(normaldens(x)); }
double ccl_normalrand() { return(normalrand()); }
double ccl_bnormcdf(double x, double y, double z)  {
  return(bnormcdf(x,y,z));
}

double ccl_cauchycdf(double x)  { return(cauchycdf(x)); }
double ccl_cauchyquant(double x)  { return(cauchyquant(x)); }
double ccl_cauchydens(double x)  { return(cauchydens(x)); }
double ccl_cauchyrand() { return(cauchyrand()); }

double ccl_gammacdf(double x, double y)  { return(gammacdf(x, y)); }
double ccl_gammaquant(double x, double y)  { return(gammaquant(x, y)); }
double ccl_gammadens(double x, double y)  { return(gammadens(x, y)); }
double ccl_gammarand(double x)  { return(gammarand(x)); }

double ccl_chisqcdf(double x, double y)  { return(chisqcdf(x, y)); }
double ccl_chisqquant(double x, double y)  { return(chisqquant(x, y)); }
double ccl_chisqdens(double x, double y)  { return(chisqdens(x, y)); }
double ccl_chisqrand(double x)  { return(chisqrand(x)); }

double ccl_betacdf(double x, double y, double z)  { return(betacdf(x, y, z)); }
double ccl_betaquant(double x, double y, double z)  { return(betaquant(x, y, z)); }
double ccl_betadens(double x, double y, double z)  { return(betadens(x, y, z)); }
double ccl_betarand(double x, double y)  { return(betarand(x, y)); }

double ccl_tcdf(double x, double y)  { return(tcdf(x, y)); }
double ccl_tquant(double x, double y)  { return(tquant(x, y)); }
double ccl_tdens(double x, double y)  { return(tdens(x, y)); }
double ccl_trand(double x)  { return(trand(x)); }

double ccl_fcdf(double x, double y, double z)  { return(fcdf(x, y, z)); }
double ccl_fquant(double x, double y, double z)  { return(fquant(x, y, z)); }
double ccl_fdens(double x, double y, double z)  { return(fdens(x, y, z)); }
double ccl_frand(double x, double y)  { return(frand(x, y)); }

double ccl_poissoncdf(double x, double y)  { return(poissoncdf(x, y)); }
int ccl_poissonquant(double x, double y)  { return(poissonquant(x, y)); }
double ccl_poissonpmf(int x, double y)  { return(poissonpmf(x, y)); }
int ccl_poissonrand(double x)  { return(poissonrand(x)); }

double ccl_binomialcdf(double x, int y, double z) { return(binomialcdf(x, y, z)); }
int ccl_binomialquant(double x, int y, double z) { return(binomialquant(x, y, z)); }
double ccl_binomialpmf(int x, int y, double z)  { return(binomialpmf(x, y, z)); }
int ccl_binomialrand(int x, double y)  { return(binomialrand(x, y)); }
