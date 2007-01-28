;;;; kclglue -- Interface to C library
;;;; 
;;;; Copyright (c) 1991, by Luke Tierney. Permission is granted for
;;;; unrestricted use.

(in-package 'lisp-stat-basics)

(eval-when (compile load eval)
	   (set-macro-character
	    #\%
	    #'(lambda (stream char) (values (read-line stream)))))

(Clines 
%#define IN_KCL_GLUE
%#include "lib/linalg.h"
%extern double rcondest_front();
%extern char *calloc();
%char buf[1000];
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;                           Basic Utilities
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;
;;;;                Storage Allocation Functions
;;;;

(defun null-ptr-p (p) (= p 0))
(defun ptr-eq (p q) (= p q))

(Clines
%int la_base_allocate(n, m)
%	unsigned n, m;
%{
%  char *p = calloc(n, m);
%  if (p == nil) xlfail("allocation failed");
%  return((int) p);
%}
)

(defentry la-base-allocate (int int) (int "la_base_allocate"))

(Clines
%int la_base_free_alloc(p)
%	int p;
%{
%  if (p) free((char *) p);
%  return(0);
%}
)

(defentry la-base-free (int) (int "la_base_free_alloc"))

(Clines
%static int mode_size(mode)
%	int mode;
%{
%  switch (mode) {
%  case IN: return(sizeof(int));
%  case RE: return(sizeof(double));
%  case CX: return(sizeof(Complex));
%  }
%  return(0);
%}
)

(defentry la-mode-size (int) (int "mode_size"))

(defCfun "int la_allocate(n, m) int n, m;" 0
%{
%  int p;
   ((la-allocate (int "n") (int "m")) (int "p"))
%  return(p);
%}
)

(defCfun "la_free_alloc(p) int p;" 0
%{
   (la-free (int "p"))
%}
)

(defentry al (int int) (int "la_allocate"))
(defentry fr (int) (int "la_free_alloc"))

;;;;
;;;;                Storage Access Functions
;;;;

(Clines
%static int get_integer(p, i)
%	int p, i;
%{
%  return(*(((int *) p) + i));
%}
)

(defentry la-get-integer (int int) (int "get_integer"))

(Clines
%static double get_double(p, i)
%	int p, i;
%{
%  return(*(((double *) p) + i));
%}
)

(defentry la-get-double (int int) (double "get_double"))

(Clines
%static double get_complex_real(p, i)
%	int p, i;
%{
%  Complex *c = ((Complex *) p) + i;
%  return(c->real);
%}
)

(defentry la-get-complex-real (int int) (double "get_complex_real"))

(Clines
%static double get_complex_imag(p, i)
%	int p, i;
%{
%  Complex *c = ((Complex *) p) + i;
%  return(c->imag);
%}
)

(defentry la-get-complex-imag (int int) (double "get_complex_imag"))

(defun la-get-complex (p i)
  (complex (la-get-complex-real p i) (la-get-complex-imag p i)))

(defun la-get-pointer (p i) (la-get-integer p i))

;;;;
;;;;                Storage Mutation Functions
;;;;

(Clines
%static int put_integer(p, i, x)
%	int p, i, x;
%{
%  *(((int *) p) + i) = x;
%  return(0);
%}
)

(defentry la-put-integer (int int int) (int "put_integer"))

(Clines
%static int put_double(p, i, x)
%	int p, i;
%	double x;
%{
%  *(((double *) p) + i) = x;
%  return(0);
%}
)

(defentry la-put-double (int int double) (int "put_double"))

(Clines
%static int put_complex(p, i, x, y)
%	int p, i;
%	double x, y;
%{
%  Complex *c = ((Complex *) p) + i;
%  c->real = x;
%  c->imag = y;
%  return(0);
%}
)

(defentry la-put-complex (int int double double) (int "put_complex"))

(defun la-put-pointer (p i x) (la-put-integer p i x))

;;;;
;;;; XLISP internal error message emulation
;;;;

(defvar *buf* (make-string 1000))

(defun set-buf-char (i c) (setf (elt *buf* i) (code-char c)))

(defun get-buf (&optional (n (position (code-char 0) *buf*)))
  (subseq *buf* 0 n))

(Clines
%static int bufpos = 0;
%
%static resetbuf() { bufpos = 0; }
%
)

(defCfun "static prbuf(s) char *s;" 0
%{
%  object ch;
%  int i, n;
%  
%  n = strlen(s);
%  for (i = 0; i <n; i++, bufpos++) {
     (set-buf-char (int "bufpos") (int "(int) s[i]"))
%  }
   (set-buf-char (int "bufpos") (int "(int) 0"))
%}
)

(defCfun "xlfail(s) char *s;" 0
%{
%  object buf;
%
%  resetbuf();
%  prbuf(s);
   ((get-buf (int "bufpos")) "buf")
   (error "buf")
%}
)

(defCfun "stdputstr(s) char *s;" 0
%{
%  object buf;
%
%  resetbuf();
%  prbuf(s);
   ((get-buf (int "bufpos")) "buf")
   (princ "buf")
%}
)

(Clines
%bufputstr(s)
%	char *s;
%{
%  object buf;
%
%  resetbuf();
%  prbuf(s);
%}
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;             Lisp Interfaces to Linear Algebra Routines
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;
;;;; Cholesky Decomposition
;;;;

(defentry chol-decomp-front (int int int) (int "chol_decomp_front"))

;;;;
;;;; LU Decomposition
;;;;

(defentry lu-decomp-front (int int int int int) (int "lu_decomp_front"))
(defentry lu-solve-front (int int int int int) (int "lu_solve_front"))
(defentry lu-inverse-front (int int int int int int) (int "lu_inverse_front"))

;;;;
;;;; SV Decomposition
;;;;

(defentry sv-decomp-front (int int int int int) (int "sv_decomp_front"))

;;;;
;;;; QR Decomposition
;;;;

(defentry qr-decomp-front (int int int int int int) (int "qr_decomp_front"))

;;;;
;;;; Estimate of Condition Number for Lower Triangular Matrix
;;;;

(defentry rcondest-front (int int) (double "rcondest_front"))

;;;;
;;;; Make Rotation Matrix
;;;;

(defentry make-rotation-front
  (int int int int int double)
  (int "make_rotation_front"))

;;;;
;;;; Eigenvalues and Eigenvectors
;;;;

(defentry eigen-front (int int int int int) (int "eigen_front"))

;;;;
;;;; Spline Interpolation
;;;;

(defentry la-range-to-rseq (int int int int) (int "range_to_rseq"))
(defentry spline-front (int int int int int int int) (int "spline_front"))

;;;;
;;;; Kernel Density Estimators and Smoothers
;;;;

(defentry kernel-dens-front
  (int int double int int int int)
  (int "kernel_dens_front"))

(defentry kernel-smooth-front
  (int int int double int int int int)
  (int "kernel_smooth_front"))

;;;;
;;;; Lowess Smoother Interface
;;;;

(defentry base-lowess-front 
  (int int int double int double int int int)
  (int "base_lowess_front"))

;;;;
;;;; FFT
;;;;

(defentry fft-front (int int int int) (int "fft_front"))

;;;;
;;;; Maximization and Numerical Derivatives
;;;;

(defCfun "maximize_callback(n, px, pfval, pgrad, phess, pderivs)
     int n, pderivs, px, pfval, pgrad, phess;" 0
%{
   (maximize-callback (int "n")
		      (int "px")
		      (int "pfval")
		      (int "pgrad")
		      (int "phess")
		      (int "pderivs"))
%}
)

(defentry numgrad-front (int int int double int) (int "numgrad_front"))
(defentry numhess-front (int int int int int double int) (int "numhess_front"))
(defentry base-minfo-maximize (int int int int int int) (int "minfo_maximize"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;                       Probability Distributions
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(Clines
% extern double unirand(), gamma();
% extern double normalcdf(), normalquant(), normaldens(), normalrand();
% extern double bnormcdf();
% extern double cauchycdf(), cauchyquant(), cauchydens(), cauchyrand();
% extern double gammacdf(), gammaquant(), gammadens(), gammarand();
% extern double chisqcdf(), chisqquant(), chisqdens(), chisqrand();
% extern double betacdf(), betaquant(), betadens(), betarand();
% extern double tcdf(), tquant(), tdens(), trand();
% extern double fcdf(), fquant(), fdens(), frand();
% extern double poissoncdf(), poissonpmf();
% extern int poissonquant(), poissonrand();
% extern double binomialcdf(), binomialpmf();
% extern int binomialquant(), binomialrand();
)

(defCfun "double uni()" 0
%{
%  double x;
   ((random (double "1.0")) (double "x"))
%  return(x);
%}
)

(defentry one-uniform-rand () (double "unirand"))
(defentry base-log-gamma (double) (double "gamma"))

(defentry base-normal-cdf (double) (double "normalcdf"))
(defentry base-normal-quant (double) (double "normalquant"))
(defentry base-normal-dens (double) (double "normaldens"))
(defentry one-normal-rand () (double "normalrand"))
(defentry base-bivnorm-cdf (double double double) (double "bnormcdf"))

(defentry base-cauchy-cdf (double) (double "cauchycdf"))
(defentry base-cauchy-quant (double) (double "cauchyquant"))
(defentry base-cauchy-dens (double) (double "cauchydens"))
(defentry one-cauchy-rand () (double "cauchyrand"))

(defentry base-gamma-cdf (double double) (double "gammacdf"))
(defentry base-gamma-quant (double double) (double "gammaquant"))
(defentry base-gamma-dens (double double) (double "gammadens"))
(defentry one-gamma-rand (double) (double "gammarand"))

(defentry base-chisq-cdf (double double) (double "chisqcdf"))
(defentry base-chisq-quant (double double) (double "chisqquant"))
(defentry base-chisq-dens (double double) (double "chisqdens"))
(defentry one-chisq-rand (double) (double "chisqrand"))

(defentry base-beta-cdf (double double double) (double "betacdf"))
(defentry base-beta-quant (double double double) (double "betaquant"))
(defentry base-beta-dens (double double double) (double "betadens"))
(defentry one-beta-rand (double double) (double "betarand"))
 
(defentry base-t-cdf (double double) (double "tcdf"))
(defentry base-t-quant (double double) (double "tquant"))
(defentry base-t-dens (double double) (double "tdens"))
(defentry one-t-rand (double) (double "trand"))
 
(defentry base-f-cdf (double double double) (double "fcdf"))
(defentry base-f-quant (double double double) (double "fquant"))
(defentry base-f-dens (double double double) (double "fdens"))
(defentry one-f-rand (double double) (double "frand"))

(defentry base-poisson-cdf (double double) (double "poissoncdf"))
(defentry base-poisson-quant (double double) (int "poissonquant"))
(defentry base-poisson-pmf (int double) (double "poissonpmf"))
(defentry one-poisson-rand (double) (int "poissonrand"))
 
(defentry base-binomial-cdf (double int double) (double "binomialcdf"))
(defentry base-binomial-quant (double int double) (int "binomialquant"))
(defentry base-binomial-pmf (int int double) (double "binomialpmf"))
(defentry one-binomial-rand (int double) (int "binomialrand"))

