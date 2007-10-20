;;  -*- mode: lisp -*-

;;;; cffiglue -- Interface to C library
;;;; 
;;;; Copyright (c) 1991, by Luke Tierney. 
;;;; Copyright (c) 2007, by Carlos Ungil.
;;;; Copyright (c) 2007, by AJ Rossini <blindglobe@gmail.com>.
;;;; Permission is granted for unrestricted use.

;;;; Tested (but the results have not been checked):
;;;;    Probability Distributions 
;;;;    Internal Error Message Emulation
;;;;    Matrix Manipulation

;;;; Untested
;;;;    numgrad numhess minfo-maximize

(defpackage :lisp-stat-ffi-int
    (:use :common-lisp
	  :cffi)
  (:export  

   chol-decomp-front
   lu-decomp-front lu-solve-front
   sv-decomp-front
   qr-decomp-front

   rcondest-front
   make-rotation-front

   eigen-front

   la-range-to-rseq
   spline-front

   kernel-dens-front
   kernel-smooth-front

   base-lowess-front

   numgrad-front
   numhess-front
   base-minfo-maximize

   one-uniform-rand
   base-log-gamma

   base-normal-cdf
   base-normal-quant
   base-normal-dens
   one-normal-rand
   base-bivnorm-cdf

   base-cauchy-cdf
   base-cauchy-quant
   base-cauchy-dens
   one-cauchy-rand

   base-gamma-cdf
   base-gamma-quant
   base-gamma-dens
   one-gamma-rand
   
   base-chisq-cdf
   base-chisq-quant
   base-chisq-dens
   one-chisq-rand

   base-beta-cdf
   base-beta-quant
   base-beta-dens
   one-beta-rand

   base-t-cdf
   base-t-quant
   base-t-dens
   one-t-rand

   base-f-cdf
   base-f-quant
   base-f-dens
   one-f-rand

   base-poisson-cdf
   base-poisson-quant
   base-poisson-dens
   one-poisson-rand

   base-binomial-cdf
   base-binomial-quant
   base-binomial-dens
   one-binomial-rand


))   

(in-package :lisp-stat-ffi-int)

(cffi:load-foreign-library
 (concatenate 'string
	      (namestring cl-user::*lispstat-home-dir*)
	      "lib/liblispstat"
	      #+darwin ".dylib"
	      #-darwin ".so"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;                     Callback Support Functions
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cffi:defcfun ("ccl_store_integer" ccl-store-integer) 
    :void (x :int))
(cffi:defcfun ("ccl_store_double" ccl-store-double) 
    :void (x :double))
(cffi:defcfun ("ccl_store_ptr" ccl-store-ptr) 
    :void (x :pointer))

;;;;
;;;;                  Lisp-Managed Calloc/Free
;;;;

;;;; this section is commented out in mclglue.lsp
;;;; and the relevant fragment in cffi-glue.c is not compiled (ifdef DODO)

;;;;
;;;;                Storage Allocation Functions
;;;;


(defun null-ptr-p (p) (cffi:null-pointer-p p))
(defun ptr-eq (p q) (cffi:pointer-eq p q))

(cffi:defcfun ("la_base_allocate" ccl-la-base-allocate)
    :pointer (n :int) (m :int))
(defun la-base-allocate (n m) 
  (ccl-la-base-allocate n m))

(cffi:defcfun ("la_base_free_alloc" ccl-la-base-free-alloc)
    :void (p :pointer))
(defun la-base-free (p)
  (ccl-la-base-free-alloc p))

(cffi:defcfun ("la_mode_size" ccl-la-mode-size)
    :int (x :int))

(defun la-mode-size (mode)
  (ccl-la-mode-size mode))

;;;;
;;;;             Callbacks for Internal Storage
;;;;

(cffi:defcallback lisp-la-allocate :void ((n :long) (m :long))
		  (ccl-store-ptr (la-allocate n m)))
(cffi:defcfun ("register_la_allocate" register-la-allocate)
    :void (p :pointer))
(register-la-allocate (cffi:callback lisp-la-allocate))
(cffi:defcfun ("la_allocate" la) 
    :pointer (x :int) (y :int))

(cffi:defcallback lisp-la-free-alloc :void ((p :pointer)) 
		  (la-free p))
(cffi:defcfun ("register_la_free_alloc" register-la-free-alloc)
    :void (p :pointer))
(register-la-free-alloc (cffi:callback lisp-la-free-alloc))
(cffi:defcfun ("la_free_alloc" lf)
    :void (p :pointer))

;;;;
;;;;                Storage Access Functions
;;;;

(cffi:defcfun ("la_get_integer" ccl-la-get-integer)
    :int (p :pointer) (i :int))
(defun la-get-integer (p i)
  (ccl-la-get-integer p i))

(cffi:defcfun ("la_get_double" ccl-la-get-double)
    :double (p :pointer) (i :int))
(defun la-get-double (p i)
  (ccl-la-get-double p i))

(cffi:defcfun ("la_get_complex_real" ccl-la-get-complex-real)
    :double (p :pointer) (i :int))
(defun la-get-complex-real (p i)
  (ccl-la-get-complex-real p i))

(cffi:defcfun ("la_get_complex_imag" ccl-la-get-complex-imag)
    :double (p :pointer) (i :int))
(defun la-get-complex-imag (p i)
  (ccl-la-get-complex-imag p i))

(defun la-get-complex (p i)
  (complex (la-get-complex-real p i) (la-get-complex-imag p i)))

(cffi:defcfun ("la_get_pointer" ccl-la-get-pointer)
    :pointer (p :pointer) (i :int))
(defun la-get-pointer (p i)
  (ccl-la-get-pointer p i))

;;;;
;;;;                Storage Mutation Functions
;;;;

(cffi:defcfun ("la_put_integer" ccl-la-put-integer)
    :void (p :pointer) (i :int) (x :int))
(defun la-put-integer (p i x)
  (ccl-la-put-integer p i x))

(cffi:defcfun ("la_put_double" ccl-la-put-double)
    :void (p :pointer) (i :int) (x :double))
(defun la-put-double (p i x) 
  (ccl-la-put-double p i (float x 1d0)))

(cffi:defcfun ("la_put_complex" ccl-la-put-complex) 
    :void (p :pointer) (i :int) (x :double) (y :double))
(defun la-put-complex (p i x y) 
  (ccl-la-put-complex p i (float x 1d0) (float y 1d0)))

(cffi:defcfun ("la_put_pointer" ccl-la-put-pointer)
    :void (p :pointer) (i :int) (q :pointer))
(defun la-put-pointer (p i q) 
  (ccl-la-put-pointer p i q)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;                 XLISP Internal Error Message Emulation
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *buf* (make-string 1000))

(defun set-buf-char (i c) (setf (elt *buf* i) (code-char c)))

(defun get-buf (&optional (n (position (code-char 0) *buf*)))
  (subseq *buf* 0 n))

(cffi:defcfun ("register_set_buf_char" register-set-buf-char)
    :void (p :pointer))
(cffi:defcallback ccl-set-buf-char :void ((n :int) (c :int))
  (set-buf-char n c))
(register-set-buf-char (cffi:callback ccl-set-buf-char))

(cffi:defcfun ("register_print_buffer" register-print-buffer)
    :void (p :pointer))
(cffi:defcallback ccl-print-buffer :void ((n :int) (type :int))
  (case type
    (0 (princ (get-buf n)))
    (1 (error (get-buf n))))
  n)
(register-print-buffer (cffi:callback ccl-print-buffer))

(cffi:defcfun ("stdputstr" stdputstr) 
    :void (string :string))
(cffi:defcfun ("xlfail" xlfail) 
    :void (string :string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;             Lisp Interfaces to Linear Algebra Routines
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; Cholesky Decomposition
;;;

(cffi:defcfun ("ccl_chol_decomp_front" ccl-chol-decomp-front)
    :int (x :pointer) (y :int) (z :pointer))
(defun chol-decomp-front (x y z) 
  (ccl-chol-decomp-front x y z))

;;;;
;;;; LU Decomposition
;;;;

(cffi:defcfun ("ccl_lu_decomp_front" ccl-lu-decomp-front)
  :int (x :pointer) (y :int) (z :pointer) (u :int) (v :pointer))
(defun lu-decomp-front (x y z u v) 
(ccl-lu-decomp-front x y z u v))

(cffi:defcfun ("ccl_lu_solve_front" ccl-lu-solve-front)
    :int (x :pointer) (y :int) (z :pointer) (u :pointer) (v :int))
(defun lu-solve-front (x y z u v) 
  (ccl-lu-solve-front x y z u v))

(cffi:defcfun ("ccl_lu_inverse_front" ccl-lu-inverse-front)
    :int (x :pointer) (y :int) (z :pointer) (u :pointer) (v :int) (w :pointer))
(defun lu-inverse-front (x y z u v w) 
  (ccl-lu-inverse-front x y z u v w))

;;;;
;;;; SV Decomposition
;;;;

(cffi:defcfun ("ccl_sv_decomp_front" ccl-sv-decomp-front)
    :int (x :pointer) (y :int) (z :int) (u :pointer) (v :pointer))
(defun sv-decomp-front (x y z u v)
  (ccl-sv-decomp-front x y z u v))

;;;;
;;;; QR Decomposition
;;;;

(cffi:defcfun ("ccl_qr_decomp_front" ccl-qr-decomp-front)
    :int (x :pointer) (y :int) (z :int) (u :pointer) (v :pointer) (w :int))
(defun qr-decomp-front (x y z u v w) 
  (ccl-qr-decomp-front x y z u v w))

;;;;
;;;; Estimate of Condition Number for Lower Triangular Matrix
;;;;

(cffi:defcfun ("ccl_rcondest_front" ccl-rcondest-front)
    :double (x :pointer) (y :int))
(defun rcondest-front (x y) 
  (ccl-rcondest-front x y))

;;;;
;;;; Make Rotation Matrix
;;;;

(cffi:defcfun ("ccl_make_rotation_front" ccl-make-rotation-front)
    :int (x :int) (y :pointer) (z :pointer) (u :pointer) (v :int) (w :double))
(defun make-rotation-front (x y z u v w)
  (ccl-make-rotation-front x y z u v (float w 1d0)))

;;;;
;;;; Eigenvalues and Eigenvectors
;;;;

(cffi:defcfun ("ccl_eigen_front" ccl-eigen-front)
    :int (x :pointer) (y :int) (z :pointer) (u :pointer) (v :pointer))
(defun eigen-front (x y z u v) 
  (ccl-eigen-front x y z u v))

;;;;
;;;; Spline Interpolation
;;;;

(cffi:defcfun ("ccl_range_to_rseq" ccl-range-to-rseq)
    :int (x :int) (y :pointer) (z :int) (u :pointer))
(defun la-range-to-rseq (x y z u)
  (ccl-range-to-rseq x y z u))

(cffi:defcfun ("ccl_spline_front" ccl-spline-front)
    :int (x :int) (y :pointer) (z :pointer) (u :int) (v :pointer) (w :pointer) (a :pointer))
(defun spline-front (x y z u v w a) 
  (ccl-spline-front x y z u v w a))

;;;;
;;;; Kernel Density Estimators and Smoothers
;;;;

(cffi:defcfun ("ccl_kernel_dens_front" ccl-kernel-dens-front)
    :int (x :pointer) (y :int) (z :double) (u :pointer) (v :pointer) (w :int) (a :int))
(defun kernel-dens-front (x y z u v w a)
  (ccl-kernel-dens-front x y (float z 1d0) u v w a))

(cffi:defcfun ("ccl_kernel_smooth_front" ccl-kernel-smooth-front)
    :int (x :pointer) (y :pointer) (z :int) (u :double) (v :pointer) (w :pointer) (a :int) (b :int))
(defun kernel-smooth-front (x y z u v w a b)
  (ccl-kernel-smooth-front x y z (float u 1d0) v w a b))

;;;;
;;;; Lowess Smoother Interface
;;;;

(cffi:defcfun ("ccl_base_lowess_front" ccl-base-lowess-front)
  :int (x :pointer) (y :pointer) (z :int) (u :double) (v :int) (w :double) (a :pointer) (b :pointer) (c :pointer))
(defun base-lowess-front (x y z u v w a b c)
  (ccl-base-lowess-front x y z (float u 1d0) v (float w 1d0) a b c))

;;;;
;;;; FFT
;;;;

(cffi:defcfun ("ccl_fft_front" ccl-fft-front)
    :int (x :int) (y :pointer) (z :pointer) (u :int))
(defun fft-front (x y z u) 
  (ccl-fft-front x y z u))

;;;;
;;;; Maximization and Numerical Derivatives
;;;;

(cffi:defcallback ccl-maximize-callback :void ((n :int)
					       (px :pointer)
					       (pfval :pointer)
					       (pgrad :pointer)
					       (phess :pointer)
					       (pderivs :pointer))
  (lisp-stat-optimize::maximize-callback n px pfval pgrad phess pderivs))

(cffi:defcfun ("register_maximize_callback" register-maximize-callback)
    :void (x :pointer))
(register-maximize-callback (cffi:callback ccl-maximize-callback))

(cffi:defcfun ("ccl_numgrad_front" ccl-numgrad-front)
    :int (x :int) (y :pointer) (z :pointer) (u :double) (v :pointer))
(defun numgrad-front (x y z u v)
  (ccl-numgrad-front x y z (float u 1d0) v))

(cffi:defcfun ("ccl_numhess_front" ccl-numhess-front)
    :int (x :int) (y :pointer) (z :pointer) (u :pointer) (v :pointer) (w :double) (a :pointer))
(defun numhess-front (x y z u v w a) 
  (ccl-numhess-front x y z u v (float w 1d0) a))

(cffi:defcfun ("ccl_minfo_maximize" ccl-minfo-maximize)
    :int (x :pointer) (y :pointer) (z :pointer) (u :pointer) (v :pointer) (w :int))
(defun base-minfo-maximize (x y z u v w) 
  (ccl-minfo-maximize x y z u v w))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;                     Probability Distributions
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;
;;;; C-callable uniform generator
;;;;

(cffi:defcfun ("register_uni" register-uni)
    :void (f :pointer))
(cffi:defcallback ccl-uni :int () (ccl-store-double (random 1.0)) 0)
(register-uni (cffi:callback ccl-uni))

(defun one-uniform-rand () (random 1.0))

;;;;
;;;; Log-gamma function
;;;;

(cffi:defcfun ("ccl_gamma" ccl-base-log-gamma)
    :double (x :double))
(defun base-log-gamma (x) 
  (ccl-base-log-gamma (float x 1d0)))

;;;;
;;;; Normal distribution
;;;;

(cffi:defcfun ("ccl_normalcdf" ccl-base-normal-cdf) 
    :double (x :double))
(defun base-normal-cdf (x) 
  (ccl-base-normal-cdf (float x 1d0)))

(cffi:defcfun ("ccl_normalquant" ccl-base-normal-quant)
    :double (x :double))
(defun base-normal-quant (x) 
  (ccl-base-normal-quant (float x 1d0)))

(cffi:defcfun ("ccl_normaldens" ccl-base-normal-dens)
        :double (x :double))
(defun base-normal-dens (x) 
  (ccl-base-normal-dens (float x 1d0)))

(cffi:defcfun ("ccl_normalrand" one-normal-rand) 
    :float)

(cffi:defcfun ("ccl_bnormcdf" ccl-base-bivnorm-cdf) 
    :double (x :double) (y :double) (z :double))
(defun base-bivnorm-cdf (x y z)
  (ccl-base-bivnorm-cdf (float x 1d0) (float y 1d0) (float z 1d0)))

;;;;
;;;; Cauchy distribution
;;;;

(cffi:defcfun ("ccl_cauchycdf" ccl-base-cauchy-cdf) 
            :double (x :double))
(defun base-cauchy-cdf (x) 
  (ccl-base-cauchy-cdf (float x 1d0)))

(cffi:defcfun ("ccl_cauchyquant" ccl-base-cauchy-quant)
            :double (x :double))
(defun base-cauchy-quant (x) 
  (ccl-base-cauchy-quant (float x 1d0)))

(cffi:defcfun ("ccl_cauchydens" ccl-base-cauchy-dens)
            :double (x :double))
(defun base-cauchy-dens (x) 
  (ccl-base-cauchy-dens (float x 1d0)))

(cffi:defcfun ("ccl_cauchyrand" one-cauchy-rand)
    :double)

;;;;
;;;; Gamma distribution
;;;;

(cffi:defcfun ("ccl_gammacdf" ccl-base-gamma-cdf)
            :double (x :double) (y :double))
(defun base-gamma-cdf (x y) 
  (ccl-base-gamma-cdf (float x 1d0) (float y 1d0)))

(cffi:defcfun ("ccl_gammaquant" ccl-base-gamma-quant)
            :double (x :double) (y :double))
(defun base-gamma-quant (x y) 
  (ccl-base-gamma-quant (float x 1d0) (float y 1d0)))

(cffi:defcfun ("ccl_gammadens" ccl-base-gamma-dens)
            :double (x :double) (y :double))
(defun base-gamma-dens (x y) 
  (ccl-base-gamma-dens (float x 1d0) (float y 1d0)))

(cffi:defcfun ("ccl_gammarand" ccl-gamma-rand)
            :double (x :double))
(defun one-gamma-rand (x) 
  (ccl-gamma-rand (float x 1d0)))

;;;;
;;;; Chi-square distribution
;;;;

(cffi:defcfun ("ccl_chisqcdf" ccl-base-chisq-cdf)
            :double (x :double) (y :double))
(defun base-chisq-cdf (x y) 
  (ccl-base-chisq-cdf (float x 1d0) (float y 1d0)))

(cffi:defcfun ("ccl_chisqquant" ccl-base-chisq-quant)
            :double (x :double) (y :double))
(defun base-chisq-quant (x y) 
  (ccl-base-chisq-quant (float x 1d0) (float y 1d0)))

(cffi:defcfun ("ccl_chisqdens" ccl-base-chisq-dens)
            :double (x :double) (y :double))
(defun base-chisq-dens (x y) 
  (ccl-base-chisq-dens (float x 1d0) (float y 1d0)))

(cffi:defcfun ("ccl_chisqrand" ccl-chisq-rand)
            :double (x :double))
(defun one-chisq-rand (x) 
  (ccl-chisq-rand (float x 1d0)))

;;;;
;;;; Beta distribution
;;;;

(cffi:defcfun ("ccl_betacdf" ccl-base-beta-cdf)
    :double (x :double) (y :double) (z :double))
(defun base-beta-cdf (x y z) 
  (ccl-base-beta-cdf (float x 1d0) (float y 1d0) (float z 1d0)))

(cffi:defcfun ("ccl_betaquant" ccl-base-beta-quant)
    :double (x :double) (y :double) (z :double))
(defun base-beta-quant (x y z) 
  (ccl-base-beta-quant (float x 1d0) (float y 1d0) (float z 1d0)))

(cffi:defcfun ("ccl_betadens" ccl-base-beta-dens) 
    :double (x :double) (y :double) (z :double))
(defun base-beta-dens (x y z) 
  (ccl-base-beta-dens (float x 1d0) (float y 1d0) (float z 1d0)))

(cffi:defcfun ("ccl_betarand" ccl-beta-rand)
    :double (x :double) (y :double))
(defun one-beta-rand (x y)
  (ccl-beta-rand (float x 1d0) (float y 1d0)))

;;;;
;;;; t distribution
;;;;

(cffi:defcfun ("ccl_tcdf" ccl-base-t-cdf)
    :double (x :double) (y :double))
(defun base-t-cdf (x y)
  (ccl-base-t-cdf (float x 1d0) (float y 1d0)))

(cffi:defcfun ("ccl_tquant" ccl-base-t-quant)
    :double (x :double) (y :double))
(defun base-t-quant (x y) 
  (ccl-base-t-quant (float x 1d0) (float y 1d0)))

(cffi:defcfun ("ccl_tdens" ccl-base-t-dens)
    :double (x :double) (y :double))
(defun base-t-dens (x y) 
  (ccl-base-t-dens (float x 1d0) (float y 1d0)))

(cffi:defcfun ("ccl_trand" ccl-t-rand)
    :double (x :double))
(defun one-t-rand (x) 
  (ccl-t-rand (float x 1d0)))

;;;;
;;;; F distribution
;;;;

(cffi:defcfun ("ccl_fcdf" ccl-base-f-cdf)
    :double (x :double) (y :double) (z :double))
(defun base-f-cdf (x y z) 
  (ccl-base-f-cdf (float x 1d0) (float y 1d0) (float z 1d0)))

(cffi:defcfun ("ccl_fquant" ccl-base-f-quant)
    :double (x :double) (y :double) (z :double))
(defun base-f-quant (x y z) 
  (ccl-base-f-quant (float x 1d0) (float y 1d0) (float z 1d0)))

(cffi:defcfun ("ccl_fdens" ccl-base-f-dens)
    :double (x :double) (y :double) (z :double))
(defun base-f-dens (x y z) 
  (ccl-base-f-dens (float x 1d0) (float y 1d0) (float z 1d0)))

(cffi:defcfun ("ccl_frand" ccl-f-rand)
    :double (x :double) (y :double))
(defun one-f-rand (x y) (ccl-f-rand (float x 1d0) (float y 1d0)))

;;;;
;;;; Poisson distribution
;;;;

(cffi:defcfun ("ccl_poissoncdf" ccl-base-poisson-cdf)
    :double (x :double) (y :double))
(defun base-poisson-cdf (x y)
  (ccl-base-poisson-cdf (float x 1d0) (float y 1d0)))

(cffi:defcfun ("ccl_poissonquant" ccl-base-poisson-quant)
    :int (x :double) (y :double))
(defun base-poisson-quant (x y) 
  (ccl-base-poisson-quant (float x 1d0) (float y 1d0)))

(cffi:defcfun ("ccl_poissonpmf" ccl-base-poisson-pmf)
    :double (x :int) (y :double))
(defun base-poisson-pmf (x y) 
  (ccl-base-poisson-pmf x (float y 1d0)))

(cffi:defcfun ("ccl_poissonrand" ccl-poisson-rand)
    :int (x :double))
(defun one-poisson-rand (x) 
  (ccl-poisson-rand (float x 1d0)))

;;;;
;;;; Binomial distribution
;;;;

(cffi:defcfun ("ccl_binomialcdf" ccl-base-binomial-cdf) 
    :double (x :double) (y :int) (z :double))
(defun base-binomial-cdf (x y z) 
  (ccl-base-binomial-cdf (float x 1d0) y (float z 1d0)))

(cffi:defcfun ("ccl_binomialquant" ccl-base-binomial-quant) 
    :int (x :double) (y :int) (z :double))
(defun base-binomial-quant (x y z) 
  (ccl-base-binomial-quant (float x 1d0) y (float z 1d0)))

(cffi:defcfun ("ccl_binomialpmf" ccl-base-binomial-pmf)
    :double (x :int) (y :int) (z :double))
(defun base-binomial-pmf (x y z) 
  (ccl-base-binomial-pmf x y (float z 1d0)))

(cffi:defcfun ("ccl_binomialrand" ccl-binomial-rand) 
    :int (x :int) (y :double))
(defun one-binomial-rand (x y) 
  (ccl-binomial-rand x (float y 1d0)))
