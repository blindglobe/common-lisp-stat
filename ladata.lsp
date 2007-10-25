;;; -*- mode: lisp -*-
;;; Copyright (c) 2005--2007, by A.J. Rossini <blindglobe@gmail.com>
;;; See COPYRIGHT file for any additional restrictions (BSD license).
;;; Since 1991, ANSI was finally finished.  Edited for ANSI Common Lisp. 

;;;; ladata -- Data handling functions for linear algebra interface
;;;; 
;;;; Copyright (c) 1991, by Luke Tierney. Permission is granted for
;;;; unrestricted use.

;;;
;;; Package Setup
;;;

;;(in-package #:lisp-stat-basics)
;;(in-package :cl-user)

(defpackage :lisp-stat-linalg-data
  (:use :common-lisp
	:cffi
	:lisp-stat-ffi-int
	:lisp-stat-types
	:lisp-stat-sequence
	:lisp-stat-compound-data
	:lisp-stat-matrix)
  (:export ;; more to add
      +mode-in+ +mode-re+ +mode-cx+ mode-of
      
      la-data-mode la-allocate la-free

      la-get-double
      la-matrix la-free-matrix la-matrix-to-data la-data-to-matrix
      la-vector la-free-vector la-vector-to-data la-data-to-vector ))

(in-package :lisp-stat-linalg-data)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                        Data Mode Functions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;
;;;; These constants need to be redefined if IN, RE or CX in linalg.h
;;;; are changed.
;;;;

;;; FIXME:AJR: This is how Luke got around having appropriate
;;; approaches for Linear Algebra.  We want to cheat and instead use
;;; CLEM or MATLISP as the underlying linear algebra package.

(defparameter +mode-in+ 0)
(defparameter +mode-re+ 1)
(defparameter +mode-cx+ 2)

(defun mode-of (x) 
  (etypecase x
	     (fixnum +mode-in+)
	     (rational +mode-re+)
	     (float +mode-re+)
	     (complex +mode-cx+)))

(defun la-data-mode (data)
  (let ((data (compound-data-seq data))
	(mode 0))
    (cond 
     ((vectorp data)
      (let ((n (length data)))
	(declare (fixnum n))
	(dotimes (i n mode)
		 (declare (fixnum i))
		 (setf mode (max mode (mode-of (aref data i)))))))
     ((consp data) (dolist (x data mode) (setf mode (max mode (mode-of x)))))
     (t (error "bad sequence - ~s" data)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;                        Internal Allocation Funcitons
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *la-allocations* nil)

;;;
;;; CFFI glue for...     Storage Allocation Functions
;;; 

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

;;;
;;;             Callbacks for Internal Storage
;;;

(cffi:defcallback lisp-la-allocate :void ((n :long) (m :long))
		  (ccl-store-ptr (la-allocate n m)))
(cffi:defcfun ("register_la_allocate" register-la-allocate)
    :void (p :pointer))
(register-la-allocate (cffi:callback lisp-la-allocate))
(cffi:defcfun ("la_allocate" la) 
    :pointer (x :int) (y :int))

(cffi:defcallback lisp-la-free-alloc
    :void ((p :pointer))
  (la-free p))

(cffi:defcfun ("register_la_free_alloc" register-la-free-alloc)
    :void (p :pointer))
(register-la-free-alloc (cffi:callback lisp-la-free-alloc))
(cffi:defcfun ("la_free_alloc" lf)
    :void (p :pointer))



;;; CFFI glue for...         Storage Access Functions


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


;;; CFFI glue for     Storage Mutation Functions

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


;; User interface (exported)

(defun la-allocate (n m)
  (let ((p (la-base-allocate n m)))
    (if (null-ptr-p p) (error "allocation failed"))
    (if (member p *la-allocations* :test #'ptr-eq)
      (error "pointer is already on the list"))
    (push p *la-allocations*)
    p))

(defun la-free (p)
  (when (and (not (null-ptr-p p)) (member p *la-allocations* :test #'ptr-eq))
	(setf *la-allocations* (delete p *la-allocations* :test #'ptr-eq))
	(la-base-free p)))

(defun la-cleanup-allocations ()
  (let ((allocs (copy-list *la-allocations*)))
    (dolist (p allocs) (la-free p))))
	  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;                       C Vector and Array Allocation
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun la-vector(n mode) (la-allocate n (la-mode-size mode)))
(defun la-free-vector (v) (la-free v))

(defun la-matrix (n m mode)
  (let ((matrix (la-allocate n (la-mode-size +mode-in+))))
    (dotimes (i n)
      (la-put-pointer matrix i (la-allocate m (la-mode-size mode))))
    matrix))

(defun la-free-matrix (matrix n)
  (dotimes (i n)
    (la-free (la-get-pointer matrix i)))
  (la-free matrix))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;                   C to/from Lisp Data Conversion
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun la-data-to-vector (data mode)
  (check-sequence data)
  (let* ((n (length data))
	 (vec (la-vector n mode))
	 (d (make-next-element data)))
    (declare (fixnum n))
    (cond
     ((= mode +mode-in+)
      (dotimes (i n) 
        (declare (fixnum i))
	(la-put-integer vec i (get-next-element d i))))
     ((= mode +mode-re+)
      (dotimes (i n) 
        (declare (fixnum i))
	(la-put-double vec i (get-next-element d i))))
      ((= mode +mode-cx+)
       (dotimes (i n) 
         (declare (fixnum i))
	 (let ((x (get-next-element d i)))
	   (la-put-complex vec i (realpart x) (imagpart x))))))
    vec))

(defun la-data-to-matrix (data mode)
  (check-matrix data)
  (let* ((n (num-rows data))
	 (m (num-cols data))
	 (mat (la-matrix n m mode)))
    (declare (fixnum n m))
    (cond
     ((= mode +mode-in+)
      (dotimes (i n)
        (declare (fixnum i))
	(let ((vec (la-get-pointer mat i)))
	  (dotimes (j m)
	    (la-put-integer vec j (aref data i j))))))
      ((= mode +mode-re+)
       (dotimes (i n) 
         (declare (fixnum i))
	 (let ((vec (la-get-pointer mat i)))
	   (dotimes (j m)
	     (la-put-double vec j (aref data i j))))))
      ((= mode +mode-cx+)
       (dotimes (i n) 
         (declare (fixnum i))
	 (let ((vec (la-get-pointer mat i)))
	   (dotimes (j m)
	     (let ((x (aref data i j)))
	       (la-put-complex vec i (realpart x) (imagpart x))))))))
    mat))

(defun la-vector-to-data (vec n mode data)
  (declare (fixnum n))
  (check-sequence data)
  (let ((d (make-next-element data))
	(gf (cond
	      ((= mode +mode-in+) #'la-get-integer)
	      ((= mode +mode-re+) #'la-get-double)
	      ((= mode +mode-cx+) #'la-get-complex))))
    (dotimes (i n)
      (declare (fixnum i))
      (set-next-element d i (funcall gf vec i))))
  data)

(defun la-matrix-to-data (mat n m mode result)
  (declare (fixnum n m))
  (check-matrix result)
  (let ((gf (cond
	      ((= mode +mode-in+) #'la-get-integer)
	      ((= mode +mode-re+) #'la-get-double)
	      ((= mode +mode-cx+) #'la-get-complex))))
    (dotimes (i n)
      (declare (fixnum i))
      (let ((vec (la-get-pointer mat i)))
	(dotimes (j m)
	  (declare (fixnum j))
	  (setf (aref result i j) (funcall gf vec j))))))
  result)

