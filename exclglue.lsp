;;;; exclglue -- Interface to C library
;;;; 
;;;; Copyright (c) 1991, by Luke Tierney. Permission is granted for
;;;; unrestricted use.

(in-package 'lisp-stat-basics)

(require :foreign)

(load "lib/exclglue.o"
      :foreign-files '("lib/clib.a")
      :system-libraries #+:mips '("m_G0") #-:mips '("m"))

;;;
;;; FF Macros
;;;

(defmacro defforfun (name arg-types return-type)
  `(ff:defforeign ',name 
		  :arguments ',arg-types 
		  :return-type ,return-type))

(defmacro mkdbl (x) `(float ,x 0.d0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;                           Basic Utilities
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;
;;;;                   Callback Value Storage
;;;;

(defforfun excl_set_integer_value (integer) :void)
(defforfun excl_set_double_value (double-float) :void)

;;;;
;;;;                Storage Allocation Functions
;;;;

(defun null-ptr-p (p) (= p 0))
(defun ptr-eq (p q) (= p q))

(defforfun la_base_allocate (integer integer) :integer)
(defun la-base-allocate (n m) (la_base_allocate n m))

(defforfun la_base_free_alloc (integer) :void)
(defun la-base-free (p) (la_base_free_alloc p))

(defforfun la_mode_size (integer) :integer)
(defun la-mode-size (mode) (la_mode_size mode))

;;;;
;;;;             Callbacks for Internal Storage
;;;;

(ff:defun-c-callable lisp_la_allocate ((n :signed-long) (m :signed-long))
  (excl_set_integer_value (la-allocate n m)))
(defforfun excl_register_la_allocate (integer) :void)
(multiple-value-bind (ptr index) (ff:register-function 'lisp_la_allocate)
  (excl_register_la_allocate index))

(ff:defun-c-callable lisp_la_free_alloc ((p :signed-long))
  (la-free p))
(defforfun excl_register_la_free_alloc (integer) :void)
(multiple-value-bind (ptr index) (ff:register-function 'lisp_la_free_alloc)
  (excl_register_la_free_alloc index))

;;;;
;;;;                Storage Access Functions
;;;;

(defforfun la_get_integer (integer integer) :integer)
(defun la-get-integer (p i) (la_get_integer p i))

(defforfun la_get_double (integer integer) :double-float)
(defun la-get-double (p i) (la_get_double p i))

(defforfun la_get_complex_real (integer integer) :double-float)
(defun la-get-complex-real (p i) (la_get_complex_real p i))

(defforfun la_get_complex_imag (integer integer) :double-float)
(defun la-get-complex-imag (p i) (la_get_complex_imag p i))

(defun la-get-complex (p i)
  (complex (la-get-complex-real p i) (la-get-complex-imag p i)))

(defun la-get-pointer (p i) (la-get-integer p i))

;;;;
;;;;                Storage Mutation Functions
;;;;

(defforfun la_put_integer (integer integer integer) :void)
(defun la-put-integer (p i x) (la_put_integer p i x))

(defforfun la_put_double (integer integer double-float) :void)
(defun la-put-double (p i x) (la_put_double p i (mkdbl x)))

(defforfun la_put_complex (integer integer  double-float double-float) :void)
(defun la-put-complex (p i x y) (la_put_complex p i (mkdbl x) (mkdbl y)))

(defun la-put-pointer (p i x) (la-put-integer p i x))

;;;;
;;;; XLISP internal error message emulation
;;;;

(defvar *buf* (make-string 1000))

(defun set-buf-char (i c) (setf (elt *buf* i) (code-char c)))

(defun get-buf (&optional (n (position (code-char 0) *buf*)))
  (subseq *buf* 0 n))

(ff:defun-c-callable excl-set-buf-char ((n :signed-long) (c :signed-long))
  (set-buf-char n c))
(defforfun excl_register_set_buf_char (integer) :void)
(multiple-value-bind (ptr index) (ff:register-function 'excl-set-buf-char)
  (excl_register_set_buf_char index))

(ff:defun-c-callable excl-print-buffer ((n :signed-long) (type :signed-long))
  (case type
    (0 (princ (get-buf n)))
    (1 (error (get-buf n))))
  n)
(defforfun excl_register_print_buffer (integer) :void)
(multiple-value-bind (ptr index) (ff:register-function 'excl-print-buffer)
  (excl_register_print_buffer index))

(defforfun stdputstr (string) :void)
(defforfun xlfail (string) :void)

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

(defforfun excl_chol_decomp_front (integer integer integer) :integer)
(defun chol-decomp-front (x y z) (excl_chol_decomp_front x y z))

;;;;
;;;; LU Decomposition
;;;;

(defforfun excl_lu_decomp_front
  (integer integer integer integer integer)
  :integer)
(defun lu-decomp-front (x y z u v) (excl_lu_decomp_front x y z u v))
(defforfun excl_lu_solve_front
  (integer integer integer integer integer)
  :integer)
(defun lu-solve-front (x y z u v) (excl_lu_solve_front x y z u v))
(defforfun excl_lu_inverse_front
  (integer integer integer integer integer integer)
  :integer)
(defun lu-inverse-front (x y z u v w) (excl_lu_inverse_front x y z u v w))

;;;;
;;;; SV Decomposition
;;;;

(defforfun excl_sv_decomp_front
  (integer integer integer integer integer)
  :integer)
(defun sv-decomp-front (x y z u v) (excl_sv_decomp_front x y z u v))

;;;;
;;;; QR Decomposition
;;;;

(defforfun excl_qr_decomp_front
  (integer integer integer integer integer integer)
  :integer)
(defun qr-decomp-front (x y z u v w) (excl_qr_decomp_front x y z u v w))

;;;;
;;;; Estimate of Condition Number for Lower Triangular Matrix
;;;;

(defforfun excl_rcondest_front (integer integer) :double-float)
(defun rcondest-front (x y) (excl_rcondest_front x y))

;;;;
;;;; Make Rotation Matrix
;;;;

(defforfun excl_make_rotation_front
  (integer integer integer integer integer double-float)
  :integer)
(defun make-rotation-front (x y z u v w)
  (excl_make_rotation_front x y z u v (mkdbl w)))

;;;;
;;;; Eigenvalues and Eigenvectors
;;;;

(defforfun excl_eigen_front
  (integer integer integer integer integer)
  :integer)
(defun eigen-front (x y z u v) (excl_eigen_front x y z u v))

;;;;
;;;; Spline Interpolation
;;;;

(defforfun excl_range_to_rseq
  (integer integer integer integer)
  :integer)
(defun la-range-to-rseq (x y z u) (excl_range_to_rseq x y z u))
(defforfun excl_spline_front
  (integer integer integer integer integer integer integer)
  :integer)
(defun spline-front (x y z u v w a) (excl_spline_front x y z u v w a))

;;;;
;;;; Kernel Density Estimators and Smoothers
;;;;

(defforfun excl_kernel_dens_front
  (integer integer double-float integer integer integer integer)
  :integer)
(defun kernel-dens-front (x y z u v w a)
  (excl_kernel_dens_front x y (mkdbl z) u v w a))

(defforfun excl_kernel_smooth_front
  (integer integer integer double-float integer integer integer integer)
  :integer)
(defun kernel-smooth-front (x y z u v w a b)
  (excl_kernel_smooth_front x y z (mkdbl u) v w a b))

;;;;
;;;; Lowess Smoother Interface
;;;;

(defforfun excl_base_lowess_front
  (integer integer integer double-float integer double-float 
	   integer integer integer)
  :integer)
(defun base-lowess-front (x y z u v w a b c)
  (excl_base_lowess_front x y z (mkdbl u) v (mkdbl w) a b c))

;;;;
;;;; FFT
;;;;

(defforfun excl_fft_front (integer integer integer integer) :integer)
(defun fft-front (x y z u) (excl_fft_front x y z u))

;;;;
;;;; Maximization and Numerical Derivatives
;;;;

(ff:defun-c-callable excl-maximize-callback ((n :signed-long)
					     (px :signed-long)
					     (pfval :signed-long)
					     (pgrad :signed-long)
					     (phess :signed-long)
					     (pderivs :signed-long))
  (maximize-callback n px pfval pgrad phess pderivs))
(defforfun excl_register_maximize_callback (integer) :void)
(multiple-value-bind (ptr index) (ff:register-function 'excl-maximize-callback)
  (excl_register_maximize_callback index))

(defforfun excl_numgrad_front
  (integer integer integer double-float integer)
  :integer)
(defun numgrad-front (x y z u v) (excl_numgrad_front x y z (mkdbl u) v))

(defforfun excl_numhess_front
  (integer integer integer integer integer double-float integer)
  :integer)
(defun numhess-front (x y z u v w a)
  (excl_numhess_front x y z u v (mkdbl w) a))

(defforfun excl_minfo_maximize
  (integer integer integer integer integer integer)
  :integer)
(defun base-minfo-maximize (x y z u v w) (excl_minfo_maximize x y z u v w))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;                       Probability Distributions
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; C-Callable Uniform Generator
(ff:defun-c-callable uni () (excl_set_double_value (random 1.d0)))
(defforfun excl_register_uni (integer) :void)
(multiple-value-bind (ptr index) (ff:register-function 'uni)
  (excl_register_uni index))

(defforfun excl_unirand () :double-float)
(defun one-uniform-rand () (excl_unirand))

;; Log-gamma function
(defforfun excl_gamma (double-float) :double-float)
(defun base-log-gamma (x) (excl_gamma (mkdbl x)))

;; normal distribution
(defforfun excl_normalcdf (double-float) :double-float)
(defun base-normal-cdf (x) (excl_normalcdf (mkdbl x)))
(defforfun excl_normalquant (double-float) :double-float)
(defun base-normal-quant (x) (excl_normalquant (mkdbl x)))
(defforfun excl_normaldens (double-float) :double-float)
(defun base-normal-dens (x) (excl_normaldens (mkdbl x)))
(defforfun excl_normalrand () :double-float)
(defun one-normal-rand () (excl_normalrand))
(defforfun excl_bnormcdf (double-float double-float double-float) :double-float)
(defun base-bivnorm-cdf (x y z) (excl_bnormcdf (mkdbl x) (mkdbl y) (mkdbl z)))

;; cauchy distribution
(defforfun excl_cauchycdf (double-float) :double-float)
(defun base-cauchy-cdf (x) (excl_cauchycdf (mkdbl x)))
(defforfun excl_cauchyquant (double-float) :double-float)
(defun base-cauchy-quant (x) (excl_cauchyquant (mkdbl x)))
(defforfun excl_cauchydens (double-float) :double-float)
(defun base-cauchy-dens (x) (excl_cauchydens (mkdbl x)))
(defforfun excl_cauchyrand () :double-float)
(defun one-cauchy-rand () (excl_cauchyrand))

;; gamma distribution
(defforfun excl_gammacdf (double-float double-float) :double-float)
(defun base-gamma-cdf (x y) (excl_gammacdf (mkdbl x) (mkdbl y)))
(defforfun excl_gammaquant (double-float double-float) :double-float)
(defun base-gamma-quant (x y) (excl_gammaquant (mkdbl x) (mkdbl y)))
(defforfun excl_gammadens (double-float double-float) :double-float)
(defun base-gamma-dens (x y) (excl_gammadens (mkdbl x) (mkdbl y)))
(defforfun excl_gammarand (double-float) :double-float)
(defun one-gamma-rand (x) (excl_gammarand (mkdbl x)))

;; chi-square distribution
(defforfun excl_chisqcdf (double-float double-float) :double-float)
(defun base-chisq-cdf (x y) (excl_chisqcdf (mkdbl x) (mkdbl y)))
(defforfun excl_chisqquant (double-float double-float) :double-float)
(defun base-chisq-quant (x y) (excl_chisqquant (mkdbl x) (mkdbl y)))
(defforfun excl_chisqdens (double-float double-float) :double-float)
(defun base-chisq-dens (x y) (excl_chisqdens (mkdbl x) (mkdbl y)))
(defforfun excl_chisqrand (double-float) :double-float)
(defun one-chisq-rand (x) (excl_chisqrand (mkdbl x)))

;; beta distribution
(defforfun excl_betacdf (double-float double-float double-float) :double-float)
(defun base-beta-cdf (x y z) (excl_betacdf (mkdbl x) (mkdbl y) (mkdbl z)))
(defforfun excl_betaquant (double-float double-float double-float) :double-float)
(defun base-beta-quant (x y z) (excl_betaquant (mkdbl x) (mkdbl y) (mkdbl z)))
(defforfun excl_betadens (double-float double-float double-float) :double-float)
(defun base-beta-dens (x y z) (excl_betadens (mkdbl x) (mkdbl y) (mkdbl z)))
(defforfun excl_betarand (double-float double-float) :double-float)
(defun one-beta-rand (x y) (excl_betarand (mkdbl x) (mkdbl y)))

;; t distribution
(defforfun excl_tcdf (double-float double-float) :double-float)
(defun base-t-cdf (x y) (excl_tcdf (mkdbl x) (mkdbl y)))
(defforfun excl_tquant (double-float double-float) :double-float)
(defun base-t-quant (x y) (excl_tquant (mkdbl x) (mkdbl y)))
(defforfun excl_tdens (double-float double-float) :double-float)
(defun base-t-dens (x y) (excl_tdens (mkdbl x) (mkdbl y)))
(defforfun excl_trand (double-float) :double-float)
(defun one-t-rand (x) (excl_trand (mkdbl x)))

;; F distribution
(defforfun excl_fcdf (double-float double-float double-float) :double-float)
(defun base-f-cdf (x y z) (excl_fcdf (mkdbl x) (mkdbl y) (mkdbl z)))
(defforfun excl_fquant (double-float double-float double-float) :double-float)
(defun base-f-quant (x y z) (excl_fquant (mkdbl x) (mkdbl y) (mkdbl z)))
(defforfun excl_fdens (double-float double-float double-float) :double-float)
(defun base-f-dens (x y z) (excl_fdens (mkdbl x) (mkdbl y) (mkdbl z)))
(defforfun excl_frand (double-float double-float) :double-float)
(defun one-f-rand (x y) (excl_frand (mkdbl x) (mkdbl y)))

;; Poisson distribution
(defforfun excl_poissoncdf (double-float double-float) :double-float)
(defun base-poisson-cdf (x y) (excl_poissoncdf (mkdbl x) (mkdbl y)))
(defforfun excl_poissonquant (double-float double-float) :integer)
(defun base-poisson-quant (x y) (excl_poissonquant (mkdbl x) (mkdbl y)))
(defforfun excl_poissonpmf (integer double-float) :double-float)
(defun base-poisson-pmf (x y) (excl_poissonpmf x (mkdbl y)))
(defforfun excl_poissonrand (double-float) :integer)
(defun one-poisson-rand (x) (excl_poissonrand (mkdbl x)))
 
;; binomial distribution
(defforfun excl_binomialcdf (double-float integer double-float) :double-float)
(defun base-binomial-cdf (x y z) (excl_binomialcdf (mkdbl x) y (mkdbl z)))
(defforfun excl_binomialquant (double-float integer double-float) :integer)
(defun base-binomial-quant (x y z) (excl_binomialquant (mkdbl x) y (mkdbl z)))
(defforfun excl_binomialpmf (integer integer double-float) :double-float)
(defun base-binomial-pmf (x y z) (excl_binomialpmf x y (mkdbl z)))
(defforfun excl_binomialrand (integer double-float) :integer)
(defun one-binomial-rand (x y) (excl_binomialrand x (mkdbl y)))
