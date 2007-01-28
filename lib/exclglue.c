#include "linalg.h"

extern double rcondest_front();

extern double unirand(), gamma();
extern double normalcdf(), normalquant(), normaldens(), normalrand(),
  bnormcdf();
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

/***************************************************************************/
/***************************************************************************/
/****                                                                   ****/
/****                         Basic Utilities                           ****/
/****                                                                   ****/
/***************************************************************************/
/***************************************************************************/

/***************************************************************************/
/**                                                                       **/
/**                        Callback Value Storage                         **/
/**                                                                       **/
/***************************************************************************/

static int excl_integer_value;
static double excl_double_value;

excl_set_integer_value(x)
     int x;
{
  excl_integer_value = x;
}

excl_set_double_value(x)
     double x;
{
  excl_double_value = x;
}

/***************************************************************************/
/**                                                                       **/
/**                     Storage Allocation Functions                      **/
/**                                                                       **/
/***************************************************************************/

int la_base_allocate(n, m)
	unsigned n, m;
{
  char *p = calloc(n, m);
  if (p == nil) xlfail("allocation failed");
  return((int) p);
}

int la_base_free_alloc(p)
	int p;
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

int la_allocate_index, la_free_alloc_index;

excl_register_la_allocate(f) int f; { la_allocate_index = f; }
excl_register_la_free_alloc(f) int f; { la_free_alloc_index = f; }

int la_allocate(n, m)
     int n, m;
{
  lisp_call(la_allocate_index, n, m);
  return(excl_integer_value);
}

la_free_alloc(p)
     int p;
{
  lisp_call(la_free_alloc_index, p);
}

/***************************************************************************/
/**                                                                       **/
/**                     Storage Access Functions                          **/
/**                                                                       **/
/***************************************************************************/

int la_get_integer(p, i)
	int p, i;
{
  return(*(((int *) p) + i));
}

double la_get_double(p, i)
	int p, i;
{
  return(*(((double *) p) + i));
}

double la_get_complex_real(p, i)
	int p, i;
{
  Complex *c = ((Complex *) p) + i;
  return(c->real);
}

double la_get_complex_imag(p, i)
	int p, i;
{
  Complex *c = ((Complex *) p) + i;
  return(c->imag);
}

/***************************************************************************/
/**                                                                       **/
/**                     Storage Mutation Functions                        **/
/**                                                                       **/
/***************************************************************************/

int la_put_integer(p, i, x)
	int p, i, x;
{
  *(((int *) p) + i) = x;
  return(0);
}

int la_put_double(p, i, x)
	int p, i;
	double x;
{
  *(((double *) p) + i) = x;
  return(0);
}

int la_put_complex(p, i, x, y)
	int p, i;
	double x, y;
{
  Complex *c = ((Complex *) p) + i;
  c->real = x;
  c->imag = y;
  return(0);
}

/***************************************************************************/
/**                                                                       **/
/**                XLISP internal error message emulation                 **/
/**                                                                       **/
/***************************************************************************/

char buf[1000];

static int excl_set_buf_char_index;
excl_register_set_buf_char(f) int f; { excl_set_buf_char_index = f; }
set_buf_char(n, c) int n, c; { lisp_call(excl_set_buf_char_index, n, c); }

static int excl_print_buffer_index;
excl_register_print_buffer(f) int f; { excl_print_buffer_index = f; }
print_buffer(n, m) int n, m; { lisp_call(excl_print_buffer_index, n, m); }

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
  object buf;

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

excl_chol_decomp_front(x, y, z)
     int x, y, z;
{ return(chol_decomp_front(x, y, z)); }

excl_lu_decomp_front(x, y, z, u, v)
     int x, y, z, u, v;
{ return(lu_decomp_front(x, y, z, u, v)); }
excl_lu_solve_front(x, y, z, u, v)
     int x, y, z, u, v;
{ return(lu_solve_front(x, y, z, u, v)); }
excl_lu_inverse_front(x, y, z, u, v, w)
     int x, y, z, u, v, w;
{ return(lu_inverse_front(x, y, z, u, v, w)); }

excl_sv_decomp_front(x, y, z, u, v)
     int x, y, z, u, v;
{ return(sv_decomp_front(x, y, z, u, v)); }

excl_qr_decomp_front(x, y, z, u, v, w)
     int x, y, z, u, v, w;
{ return(qr_decomp_front(x, y, z, u, v, w)); }

double excl_rcondest_front(x, y) int x, y; { return(rcondest_front(x, y)); }

excl_make_rotation_front(x, y, z, u, v, w)
     int x, y, z, u, v;
     double w;
{ return(make_rotation_front(x, y, z, u, v, w)); }

excl_eigen_front(x, y, z, u, v)
     int x, y, z, u, v;
{ return(eigen_front(x, y, z, u, v)); }

excl_range_to_rseq(x, y, z, u)
     int x, y, z, u;
{ return(range_to_rseq(x, y, z, u)); }
excl_spline_front(x, y, z, u, v, w, a)
     int x, y, z, u, v, w, a;
{ return(spline_front(x, y, z, u, v, w, a)); }

excl_kernel_dens_front(x, y, z, u, v, w, a)
     int x, y, u, v, w, a;
     double z;
{ return(kernel_dens_front(x, y, z, u, v, w, a)); }

excl_kernel_smooth_front(x, y, z, u, v, w, a, b)
     int x, y, z, v, w, a, b;
     double u;
{ return(kernel_smooth_front(x, y, z, u, v, w, a, b)); }

excl_base_lowess_front(x, y, z, u, v, w, a, b, c)
     int x, y, z, v, a, b, c;
     double u, w;
{ return(base_lowess_front(x, y, z, u, v, w, a, b, c)); }

excl_fft_front(x, y, z, u) int x, y, z, u; { return(fft_front(x, y, z, u)); }

static int excl_maximize_callback_index;
excl_register_maximize_callback(f) int f; { excl_maximize_callback_index = f; }
maximize_callback(n, px, pfval, pgrad, phess, pderivs)
     int n, px, pfval, pgrad, phess, pderivs;
{
  lisp_call(excl_maximize_callback_index, n, px, pfval, pgrad, phess, pderivs);
}

excl_numgrad_front(x, y, z, u, v)
     int x, y, z, v;
     double u;
{
  return(numgrad_front(x, y, z, u, v));
}

excl_numhess_front(x, y, z, u, v, w, a)
     int x, y, z, u, v, a;
     double w;
{
  return(numhess_front(x, y, z, u, v, w, a));
}

excl_minfo_maximize(x, y, z, u, v, w)
     int x, y, z, u, v, w;
{
  return(minfo_maximize(x, y, z, u, v, w));
}

/***************************************************************************/
/***************************************************************************/
/****                                                                   ****/
/****                    Probability Distributions                      ****/
/****                                                                   ****/
/***************************************************************************/
/***************************************************************************/

/* Uniform genrator */
static int excl_uni_callback;

excl_register_uni(fun)
     int fun;
{
  excl_uni_callback = fun;
}

double uni()
{ 
  lisp_call(excl_uni_callback);
  return(excl_double_value);
}

double excl_unirand() { return(unirand()); }
double excl_gamma(x) double x; { return(gamma(x)); }

double excl_normalcdf(x) double x; { return(normalcdf(x)); }
double excl_normalquant(x) double x; { return(normalquant(x)); }
double excl_normaldens(x) double x; { return(normaldens(x)); }
double excl_normalrand() { return(normalrand()); }
double excl_bnormcdf(x, y, z) double x, y, z; { return(bnormcdf(x, y, z)); }

double excl_cauchycdf(x) double x; { return(cauchycdf(x)); }
double excl_cauchyquant(x) double x; { return(cauchyquant(x)); }
double excl_cauchydens(x) double x; { return(cauchydens(x)); }
double excl_cauchyrand() { return(cauchyrand()); }

double excl_gammacdf(x, y) double x, y; { return(gammacdf(x, y)); }
double excl_gammaquant(x, y) double x, y; { return(gammaquant(x, y)); }
double excl_gammadens(x, y) double x, y; { return(gammadens(x, y)); }
double excl_gammarand(x) double x; { return(gammarand(x)); }

double excl_chisqcdf(x, y) double x, y; { return(chisqcdf(x, y)); }
double excl_chisqquant(x, y) double x, y; { return(chisqquant(x, y)); }
double excl_chisqdens(x, y) double x, y; { return(chisqdens(x, y)); }
double excl_chisqrand(x) double x; { return(chisqrand(x)); }

double excl_betacdf(x, y, z) double z, y, x; { return(betacdf(x, y, z)); }
double excl_betaquant(x, y, z) double z, y, x; { return(betaquant(x, y, z)); }
double excl_betadens(x, y, z) double z, y, x; { return(betadens(x, y, z)); }
double excl_betarand(x, y) double x, y; { return(betarand(x, y)); }

double excl_tcdf(x, y) double x, y; { return(tcdf(x, y)); }
double excl_tquant(x, y) double x, y; { return(tquant(x, y)); }
double excl_tdens(x, y) double x, y; { return(tdens(x, y)); }
double excl_trand(x) double x; { return(trand(x)); }

double excl_fcdf(x, y, z) double z, y, x; { return(fcdf(x, y, z)); }
double excl_fquant(x, y, z) double z, y, x; { return(fquant(x, y, z)); }
double excl_fdens(x, y, z) double z, y, x; { return(fdens(x, y, z)); }
double excl_frand(x, y) double x, y; { return(frand(x, y)); }

double excl_poissoncdf(x, y) double x, y; { return(poissoncdf(x, y)); }
int excl_poissonquant(x, y) double x, y; { return(poissonquant(x, y)); }
double excl_poissonpmf(x, y) int x; double y; { return(poissonpmf(x, y)); }
int excl_poissonrand(x) double x; { return(poissonrand(x)); }
 
double excl_binomialcdf(x, y, z) double x, z; int y; {return(binomialcdf(x, y, z));}
int excl_binomialquant(x, y, z) double x, z; int y; {return(binomialquant(x, y, z)); }
double excl_binomialpmf(x, y, z) int x, y; double z; {return(binomialpmf(x, y, z)); }
int excl_binomialrand(x, y) int x; double y; { return(binomialrand(x, y)); }
