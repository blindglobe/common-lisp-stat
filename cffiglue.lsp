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
  (:export ccl-store-integer ccl-store-double ccl-store-ptr))
   
;; are there any error message components that need to be exported?
;; More importantly, should we factor them out into a logging package?

;; This package initially loads the liblispstat library for access.


;; formerly exported:

#|
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
|#

(in-package :lisp-stat-ffi-int)

(cffi:load-foreign-library
 (concatenate 'string
	      (namestring cl-user::*lispstat-home-dir*)
	      "lib/liblispstat"
	      #+darwin ".dylib"
	      #-darwin ".so"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                     Callback Support Functions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cffi:defcfun ("ccl_store_integer" ccl-store-integer) 
    :void (x :int))
(cffi:defcfun ("ccl_store_double" ccl-store-double) 
    :void (x :double))
(cffi:defcfun ("ccl_store_ptr" ccl-store-ptr) 
    :void (x :pointer))

;;;
;;;                  Lisp-Managed Calloc/Free
;;;

;;; this section is commented out in mclglue.lsp
;;; and the relevant fragment in cffi-glue.c is not compiled (ifdef DODO)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                 XLISP Internal Error Message Emulation
;;;
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




