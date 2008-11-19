;;  -*- mode: lisp -*-

;;; cffiglue -- Interface to C library.  Based on the range of CL FFI
;;;             "glues" by Luke Tierney.
;;; 
;;; Copyright (c) 1991, by Luke Tierney. 
;;; Copyright (c) 2007, by Carlos Ungil.
;;; Copyright (c) 2007-2008, by AJ Rossini <blindglobe@gmail.com>.
;;; Permission is granted for unrestricted use.

(in-package :lisp-stat-ffi-int)
   
;; are there any error message components that need to be exported?
;; More importantly, should we factor them out into a logging package?

;; This package initially loads the liblispstat library for access.

(cffi:load-foreign-library
 (concatenate 'string
	      (namestring lisp-stat-config::*lispstat-home-dir*)
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
