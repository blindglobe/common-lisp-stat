;;;; mclglue -- Interface to C library
;;;; 
;;;; Copyright (c) 1991, by Luke Tierney. Permission is granted for
;;;; unrestricted use.

(require :ff)
;(require :traps)

(in-package ls-basics)

(import '(ccl:def-logical-directory ccl:ff-load ccl:deffcfun ccl:defccallable))

(def-logical-directory "mclslib;" "mcls;lib:")
(def-logical-directory "clib;" "ccl;:mpw:libraries:clibraries:")
(def-logical-directory "mpwlib;" "ccl;:mpw:libraries:libraries:")

(defvar mcls-libs '("mclslib;clib.o"
                    "clib;CRuntime.o"
                    "mpwlib;Interface.o"
                    "clib;StdCLib.o"
                    "clib;CSANELib.o"
                    "clib;Math.o"
                    "clib;CInterface.o"))
(defvar mcls-libs-881 '("mclslib;clib.o"
                        "clib;CLib881.o"
                        "mpwlib;Interface.o"
                        "clib;StdCLib.o"
                        "clib;CSANELib881.o"
                        "clib;Math881.o"
                        "clib;CInterface.o"))

(ff-load "mclslib;mclglue.c.o"
         :ffenv-name 'mcls1
         :replace t
         :libraries mcls-libs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;                           Basic Utilities
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;                     Callback Support Functions
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deffcfun (ccl-store-integer "ccl_store_integer") (fixnum) :novalue)
(deffcfun (ccl-store-double "ccl_store_double") (float) :novalue)
(deffcfun (ccl-store-ptr "ccl_store_ptr") ((t :ptr)) :novalue)

;;;;
;;;;                  Lisp-Managed Calloc/Free
;;;;

#|
(defccallable lisp-new-ptr ((n :long) (:result :void))
  (ccl-store-ptr (ccl:_NewPtr :d0 n :a0)))
(deffcfun (register-new-ptr "register_new_ptr") ((t :ptr)) :novalue)
(register-new-ptr lisp-new-ptr)

(defccallable lisp-free-ptr ((p :ptr) (:result :void))
  (ccl:_DisposPtr :a0 p :d0))
(deffcfun (register-free-ptr "register_free_ptr") ((t :ptr)) :novalue)
(register-free-ptr lisp-free-ptr)
|#
;;;;
;;;;                Storage Allocation Functions
;;;;

(defun null-ptr-p (p) (ccl:%null-ptr-p p))
(defun ptr-eq (p q) (= (ccl:%ptr-to-int p) (ccl:%ptr-to-int q)))

(deffcfun (ccl-la-base-allocate "la_base_allocate") (fixnum fixnum) :ptr)
(defun la-base-allocate (n m) (ccl-la-base-allocate n m))

(deffcfun (ccl-la-base-free-alloc "la_base_free_alloc") ((t :ptr)) :novalue)
(defun la-base-free (p) (ccl-la-base-free-alloc p))

(deffcfun (ccl-la-mode-size "la_mode_size") (fixnum) :long)
(defun la-mode-size (mode) (ccl-la-mode-size mode))

;;;;
;;;;             Callbacks for Internal Storage
;;;;

(defccallable lisp-la-allocate ((n :long) (m :long) (:result :void))
  (ccl-store-ptr (la-allocate n m)))
(deffcfun (register-la-allocate "register_la_allocate") ((t :ptr)) :novalue)
(register-la-allocate lisp-la-allocate)
(deffcfun (la "la_allocate") (fixnum fixnum) :ptr)

(defccallable lisp-la-free-alloc ((p :ptr) (:result :void)) (la-free p))
(deffcfun (register-la-free-alloc "register_la_free_alloc") ((t :ptr)) :novalue)
(register-la-free-alloc lisp-la-free-alloc)
(deffcfun (lf "la_free_alloc") ((t :ptr)) :novalue)

;;;;
;;;;                Storage Access Functions
;;;;

(deffcfun (ccl-la-get-integer "la_get_integer") ((t :ptr) fixnum) :long)
(defun la-get-integer (p i) (ccl-la-get-integer p i))

(deffcfun (ccl-la-get-double "la_get_double") ((t :ptr) fixnum) :float)
(defun la-get-double (p i) (ccl-la-get-double p i))

(deffcfun (ccl-la-get-complex-real "la_get_complex_real") ((t :ptr) fixnum) :float)
(defun la-get-complex-real (p i) (ccl-la-get-complex-real p i))

(deffcfun (ccl-la-get-complex-imag "la_get_complex_imag") ((t :ptr) fixnum) :float)
(defun la-get-complex-imag (p i) (ccl-la-get-complex-imag p i))

(defun la-get-complex (p i)
  (complex (la-get-complex-real p i) (la-get-complex-imag p i)))

(deffcfun (ccl-la-get-pointer "la_get_pointer") ((t :ptr) fixnum) :ptr)
(defun la-get-pointer (p i) (ccl-la-get-pointer p i))

;;;;
;;;;                Storage Mutation Functions
;;;;

(deffcfun (ccl-la-put-integer "la_put_integer") ((t :ptr) fixnum fixnum) :novalue)
(defun la-put-integer (p i x) (ccl-la-put-integer p i x))

(deffcfun (ccl-la-put-double "la_put_double") ((t :ptr) fixnum float) :novalue)
(defun la-put-double (p i x) (ccl-la-put-double p i (float x)))

(deffcfun (ccl-la-put-complex "la_put_complex") ((t :ptr) fixnum float float) :novalue)
(defun la-put-complex (p i x y) (ccl-la-put-complex p i (float x) (float y)))

(deffcfun (ccl-la-put-pointer "la_put_pointer") ((t :ptr) fixnum (t :ptr)) :novalue)
(defun la-put-pointer (p i q) (ccl-la-put-pointer p i q)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;                 XLISP Internal Error Message Emulation
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *buf* (make-string 1000))

(defun set-buf-char (i c) (setf (elt *buf* i) (code-char c)))

(defun get-buf (&optional (n (position (code-char 0) *buf*)))
  (subseq *buf* 0 n))

(deffcfun (register-set-buf-char "register_set_buf_char") ((t :ptr)) :novalue)
(defccallable ccl-set-buf-char ((n :long) (c :long) (:result :long))
  (set-buf-char n c))
(register-set-buf-char ccl-set-buf-char)

(deffcfun (register-print-buffer "register_print_buffer") ((t :ptr)) :novalue)
(defccallable ccl-print-buffer ((n :long) (type :long) (:result :long))
  (case type
    (0 (princ (get-buf n)))
    (1 (error (get-buf n))))
  n)
(register-print-buffer ccl-print-buffer)

(deffcfun (stdputstr "stdputstr") ((string :by-reference)) :novalue)
(deffcfun (xlfail "xlfail") ((string :by-reference)) :novalue)

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

(deffcfun (ccl-chol-decomp-front "ccl_chol_decomp_front")
  ((t :ptr) fixnum (t :ptr))
  :long)
(defun chol-decomp-front (x y z) (ccl-chol-decomp-front x y z))

;;;;
;;;; LU Decomposition
;;;;

(deffcfun (ccl-lu-decomp-front "ccl_lu_decomp_front")
  ((t :ptr) fixnum (t :ptr) fixnum (t :ptr))
  :long)
(defun lu-decomp-front (x y z u v) (ccl-lu-decomp-front x y z u v))

(deffcfun (ccl-lu-solve-front "ccl_lu_solve_front")
  ((t :ptr) fixnum (t :ptr) (t :ptr) fixnum)
  :long)
(defun lu-solve-front (x y z u v) (ccl-lu-solve-front x y z u v))

(deffcfun (ccl-lu-inverse-front "ccl_lu_inverse_front")
  ((t :ptr) fixnum (t :ptr) (t :ptr) fixnum (t :ptr))
  :long)
(defun lu-inverse-front (x y z u v w) (ccl-lu-inverse-front x y z u v w))

;;;;
;;;; SV Decomposition
;;;;

(deffcfun (ccl-sv-decomp-front "ccl_sv_decomp_front")
  ((t :ptr) fixnum fixnum (t :ptr) (t :ptr))
  :long)
(defun sv-decomp-front (x y z u v) (ccl-sv-decomp-front x y z u v))

;;;;
;;;; QR Decomposition
;;;;

(deffcfun (ccl-qr-decomp-front "ccl_qr_decomp_front")
  ((t :ptr) fixnum fixnum (t :ptr) (t :ptr) fixnum)
  :long)
(defun qr-decomp-front (x y z u v w) (ccl-qr-decomp-front x y z u v w))

;;;;
;;;; Estimate of Condition Number for Lower Triangular Matrix
;;;;

(deffcfun (ccl-rcondest-front "ccl_rcondest_front") ((t :ptr) fixnum) :float)
(defun rcondest-front (x y) (ccl-rcondest-front x y))

;;;;
;;;; Make Rotation Matrix
;;;;

(deffcfun (ccl-make-rotation-front "ccl_make_rotation_front")
  (fixnum (t :ptr) (t :ptr) (t :ptr) fixnum float)
  :long)
(defun make-rotation-front (x y z u v w)
  (ccl-make-rotation-front x y z u v (float w)))

;;;;
;;;; Eigenvalues and Eigenvectors
;;;;

(deffcfun (ccl-eigen-front "ccl_eigen_front")
  ((t :ptr) fixnum (t :ptr) (t :ptr) (t :ptr))
  :long)
(defun eigen-front (x y z u v) (ccl-eigen-front x y z u v))

;;;;
;;;; Spline Interpolation
;;;;

(deffcfun (ccl-range-to-rseq "ccl_range_to_rseq")
  (fixnum (t :ptr) fixnum (t :ptr))
  :long)
(defun la-range-to-rseq (x y z u) (ccl-range-to-rseq x y z u))

(deffcfun (ccl-spline-front "ccl_spline_front")
  (fixnum (t :ptr) (t :ptr) fixnum (t :ptr) (t :ptr) (t :ptr))
  :long)
(defun spline-front (x y z u v w a) (ccl-spline-front x y z u v w a))

;;;;
;;;; Kernel Density Estimators and Smoothers
;;;;

(deffcfun (ccl-kernel-dens-front "ccl_kernel_dens_front")
  ((t :ptr) fixnum float (t :ptr) (t :ptr) fixnum fixnum)
  :long)
(defun kernel-dens-front (x y z u v w a)
  (ccl-kernel-dens-front x y (float z) u v w a))

(deffcfun (ccl-kernel-smooth-front "ccl_kernel_smooth_front")
  ((t :ptr) (t :ptr) fixnum float (t :ptr) (t :ptr) fixnum fixnum)
  :long)
(defun kernel-smooth-front (x y z u v w a b)
  (ccl-kernel-smooth-front x y z (float u) v w a b))

;;;;
;;;; Lowess Smoother Interface
;;;;

(deffcfun (ccl-base-lowess-front "ccl_base_lowess_front")
  ((t :ptr) (t :ptr) fixnum float fixnum float (t :ptr) (t :ptr) (t :ptr))
  :long)
(defun base-lowess-front (x y z u v w a b c)
  (ccl-base-lowess-front x y z (float u) v (float w) a b c))

;;;;
;;;; FFT
;;;;

(deffcfun (ccl-fft-front "ccl_fft_front") (fixnum (t :ptr) (t :ptr) fixnum) :long)
(defun fft-front (x y z u) (ccl-fft-front x y z u))

;;;;
;;;; Maximization and Numerical Derivatives
;;;;

(defccallable ccl-maximize-callback ((n :long)
				     (px :ptr)
				     (pfval :ptr)
				     (pgrad :ptr)
				     (phess :ptr)
				     (pderivs :ptr)
                                     (:result :void))
  (maximize-callback n px pfval pgrad phess pderivs))
(deffcfun (register-maximize-callback "register_maximize_callback")
				      ((t :ptr))
				      :novalue)
(register-maximize-callback ccl-maximize-callback)

(deffcfun (ccl-numgrad-front "ccl_numgrad_front")
  (fixnum (t :ptr) (t :ptr) float (t :ptr))
  :long)
(defun numgrad-front (x y z u v) (ccl-numgrad-front x y z (float u) v))

(deffcfun (ccl-numhess-front "ccl_numhess_front")
  (fixnum (t :ptr) (t :ptr) (t :ptr) (t :ptr) float (t :ptr))
  :long)
(defun numhess-front (x y z u v w a) (ccl-numhess-front x y z u v (float w) a))

(deffcfun (ccl-minfo-maximize "ccl_minfo_maximize")
  ((t :ptr) (t :ptr) (t :ptr) (t :ptr) (t :ptr) fixnum)
  :long)
(defun base-minfo-maximize (x y z u v w) (ccl-minfo-maximize x y z u v w))

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

(deffcfun (register-uni "register_uni") ((t :ptr)) :novalue)
(defccallable ccl-uni ((:result :long)) (ccl-store-double (random 1.0)) 0)
(register-uni ccl-uni)

(defun one-uniform-rand () (random 1.0))

;;;;
;;;; Log-gamma function
;;;;

(deffcfun (ccl-base-log-gamma "ccl_gamma") (float) :float)
(defun base-log-gamma (x) (ccl-base-log-gamma (float x)))

;;;;
;;;; Normal distribution
;;;;

(deffcfun (ccl-base-normal-cdf "ccl_normalcdf") (float) :float)
(defun base-normal-cdf (x) (ccl-base-normal-cdf (float x)))
(deffcfun (ccl-base-normal-quant "ccl_normalquant") (float) :float)
(defun base-normal-quant (x) (ccl-base-normal-quant (float x)))
(deffcfun (ccl-base-normal-dens "ccl_normaldens") (float) :float)
(defun base-normal-dens (x) (ccl-base-normal-dens (float x)))
(deffcfun (one-normal-rand "ccl_normalrand") () :float)
(deffcfun (ccl-base-bivnorm-cdf "ccl_bnormcdf") (float float float) :float)
(defun base-bivnorm-cdf (x y z) (ccl-base-bivnorm-cdf (float x) (float y) (float z)))

;;;;
;;;; Cauchy distribution
;;;;

(deffcfun (ccl-base-cauchy-cdf "ccl_cauchycdf") (float) :float)
(defun base-cauchy-cdf (x) (ccl-base-cauchy-cdf (float x)))
(deffcfun (ccl-base-cauchy-quant "ccl_cauchyquant") (float) :float)
(defun base-cauchy-quant (x) (ccl-base-cauchy-quant (float x)))
(deffcfun (ccl-base-cauchy-dens "ccl_cauchydens") (float) :float)
(defun base-cauchy-dens (x) (ccl-base-cauchy-dens (float x)))
(deffcfun (one-cauchy-rand "ccl_cauchyrand") () :float)

;;;;
;;;; Gamma distribution
;;;;

(deffcfun (ccl-base-gamma-cdf "ccl_gammacdf") (float float) :float)
(defun base-gamma-cdf (x y) (ccl-base-gamma-cdf (float x) (float y)))
(deffcfun (ccl-base-gamma-quant "ccl_gammaquant") (float float) :float)
(defun base-gamma-quant (x y) (ccl-base-gamma-quant (float x) (float y)))
(deffcfun (ccl-base-gamma-dens "ccl_gammadens") (float float) :float)
(defun base-gamma-dens (x y) (ccl-base-gamma-dens (float x) (float y)))
(deffcfun (ccl-gamma-rand "ccl_gammarand") (float) :float)
(defun one-gamma-rand (x) (ccl-gamma-rand (float x)))

;;;;
;;;; Chi-square distribution
;;;;

(deffcfun (ccl-base-chisq-cdf "ccl_chisqcdf") (float float) :float)
(defun base-chisq-cdf (x y) (ccl-base-chisq-cdf (float x) (float y)))
(deffcfun (ccl-base-chisq-quant "ccl_chisqquant") (float float) :float)
(defun base-chisq-quant (x y) (ccl-base-chisq-quant (float x) (float y)))
(deffcfun (ccl-base-chisq-dens "ccl_chisqdens") (float float) :float)
(defun base-chisq-dens (x y) (ccl-base-chisq-dens (float x) (float y)))
(deffcfun (ccl-chisq-rand "ccl_chisqrand") (float) :float)
(defun one-chisq-rand (x) (ccl-chisq-rand (float x)))

;;;;
;;;; Beta distribution
;;;;

(deffcfun (ccl-base-beta-cdf "ccl_betacdf") (float float float) :float)
(defun base-beta-cdf (x y z) (ccl-base-beta-cdf (float x) (float y) (float z)))
(deffcfun (ccl-base-beta-quant "ccl_betaquant") (float float float) :float)
(defun base-beta-quant (x y z) (ccl-base-beta-quant (float x) (float y) (float z)))
(deffcfun (ccl-base-beta-dens "ccl_betadens") (float float float) :float)
(defun base-beta-dens (x y z) (ccl-base-beta-dens (float x) (float y) (float z)))
(deffcfun (ccl-beta-rand "ccl_betarand") (float float) :float)
(defun one-beta-rand (x y) (ccl-beta-rand (float x) (float y)))

;;;;
;;;; t distribution
;;;;

(deffcfun (ccl-base-t-cdf "ccl_tcdf") (float float) :float)
(defun base-t-cdf (x y) (ccl-base-t-cdf (float x) (float y)))
(deffcfun (ccl-base-t-quant "ccl_tquant") (float float) :float)
(defun base-t-quant (x y) (ccl-base-t-quant (float x) (float y)))
(deffcfun (ccl-base-t-dens "ccl_tdens") (float float) :float)
(defun base-t-dens (x y) (ccl-base-t-dens (float x) (float y)))
(deffcfun (ccl-t-rand "ccl_trand") (float) :float)
(defun one-t-rand (x) (ccl-t-rand (float x)))

;;;;
;;;; F distribution
;;;;

(deffcfun (ccl-base-f-cdf "ccl_fcdf") (float float float) :float)
(defun base-f-cdf (x y z) (ccl-base-f-cdf (float x) (float y) (float z)))
(deffcfun (ccl-base-f-quant "ccl_fquant") (float float float) :float)
(defun base-f-quant (x y z) (ccl-base-f-quant (float x) (float y) (float z)))
(deffcfun (ccl-base-f-dens "ccl_fdens") (float float float) :float)
(defun base-f-dens (x y z) (ccl-base-f-dens (float x) (float y) (float z)))
(deffcfun (ccl-f-rand "ccl_frand") (float float) :float)
(defun one-f-rand (x y) (ccl-f-rand (float x) (float y)))

;;;;
;;;; Poisson distribution
;;;;

(deffcfun (ccl-base-poisson-cdf "ccl_poissoncdf") (float float) :float)
(defun base-poisson-cdf (x y) (ccl-base-poisson-cdf (float x) (float y)))
(deffcfun (ccl-base-poisson-quant "ccl_poissonquant") (float float) :long)
(defun base-poisson-quant (x y) (ccl-base-poisson-quant (float x) (float y)))
(deffcfun (ccl-base-poisson-pmf "ccl_poissonpmf") (fixnum float) :float)
(defun base-poisson-pmf (x y) (ccl-base-poisson-pmf x (float y)))
(deffcfun (ccl-poisson-rand "ccl_poissonrand") (float) :long)
(defun one-poisson-rand (x) (ccl-poisson-rand (float x)))

;;;;
;;;; Binomial distribution
;;;;

(deffcfun (ccl-base-binomial-cdf "ccl_binomialcdf") (float fixnum float) :float)
(defun base-binomial-cdf (x y z) (ccl-base-binomial-cdf (float x) y (float z)))
(deffcfun (ccl-base-binomial-quant "ccl_binomialquant") (float fixnum float) :long)
(defun base-binomial-quant (x y z) (ccl-base-binomial-quant (float x) y (float z)))
(deffcfun (ccl-base-binomial-pmf "ccl_binomialpmf") (fixnum fixnum float) :float)
(defun base-binomial-pmf (x y z) (ccl-base-binomial-pmf x y (float z)))
(deffcfun (ccl-binomial-rand "ccl_binomialrand") (fixnum float) :long)
(defun one-binomial-rand (x y) (ccl-binomial-rand x (float y)))
