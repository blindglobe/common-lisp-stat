;;; -*- mode: lisp -*-
;;; Copyright (c) 2005--2007, by A.J. Rossini <blindglobe@gmail.com>
;;; See COPYRIGHT file for any additional restrictions (BSD license).
;;; Since 1991, ANSI was finally finished.  Edited for ANSI Common Lisp. 

;;;; lsfloat -- Floating point specs and transcendental functions
;;;; 
;;;; Copyright (c) 1991, by Luke Tierney. Permission is granted for
;;;; unrestricted use.
;;;;
;;;; Common Lisp allows for four different floating point types that need
;;;; not be distinct. For statistical work, the type I prefer to use is
;;;; the one that is closest to a C double. This type is named stat-float.
;;;; By setting the variable *read-default-float-format* to this type, you
;;;; insure that data entered as floating point data is read in with this
;;;; type. The problem arises with data read as integers that is passed to
;;;; a transcendental, like sqrt. Floating point contagion rules say these
;;;; integers are to be converted to type single-float. Unless single-float
;;;; is equivalent to C double, as it is in Mac CL and KCL, this is not
;;;; what I want. Hence this file redefines the transcendentals to first 
;;;; coerce their arguments to stat-float before applying the built-in
;;;; functions.
;;;;
;;;; No actual modifications to the transcendentals are needed if
;;;; single-float is the same as stat-float. The fearure 
;;;; :stat-float-is-double-float is used to indicate this.
;;;;
;;;; KCL NOTE:
;;;; In (A)KCL the type short-float corresponds to C float and the types
;;;; single-float, double-float and long-float correspond to C double.
;;;; But in the implementation of the transcendentals (A)KCL coerces
;;;; rationals to short-float, not single-float. CLtL1 is a little vague
;;;; on this (it talks about "single precision") but CLtL2 clarifies that
;;;; rationals should produce single-float results. So (A)KCL is wrong, at
;;;; least relative to the clarification in CLtL2. I therefore decided
;;;; to fix (A)KCL in files c/num_sfun.c and lsp/numlib.lsp. If these
;;;; fixes are applied, the feature :stat-float-is-double-float should be
;;;; defined.


;;; ======== From the CLHS ==========
;;
;; Type SHORT-FLOAT, SINGLE-FLOAT, DOUBLE-FLOAT, LONG-FLOAT 
;;
;; Supertypes:
;;
;; short-float: short-float, float, real, number, t 
;;
;;
;; single-float: single-float, float, real, number, t 
;;
;; double-float: double-float, float, real, number, t 
;;
;; long-float: long-float, float, real, number, t 
;;
;; Description: For the four defined subtypes of type float, it is true
;; that intermediate between the type short-float and the type long-float
;; are the type single-float and the type double-float. The precise
;; definition of these categories is implementation-defined. The
;; precision (measured in ``bits'', computed as p log 2b) and the
;; exponent size (also measured in ``bits,'' computed as log 2(n+1),
;; where n is the maximum exponent value) is recommended to be at least
;; as great as the values in the next figure. Each of the defined
;; subtypes of type float might or might not have a minus zero.
;;
;;
;; Format  Minimum Precision  Minimum Exponent Size  
;; ----------
;; Short   13 bits            5 bits                 
;; Single  24 bits            8 bits                 
;; Double  50 bits            8 bits                 
;; Long    50 bits            8 bits                 
;;
;; Figure 12-12. Recommended Minimum Floating-Point Precision and Exponent Size 
;;
;;  There can be fewer than four internal representations for floats. If
;; there are fewer distinct representations, the following rules apply:
;;
;;  If there is only one, it is the type single-float. In this
;; representation, an object is simultaneously of types single-float,
;; double-float, short-float, and long-float.
;;
;;  Two internal representations can be arranged in either of the
;; following ways:
;;
;;  Two types are provided: single-float and short-float. An object is
;; simultaneously of types single-float, double-float, and long-float.
;;
;;  Two types are provided: single-float and double-float. An object is
;; simultaneously of types single-float and short-float, or double-float
;; and long-float.
;;
;;  Three internal representations can be arranged in either of the
;; following ways:
;;
;;  Three types are provided: short-float, single-float, and
;; double-float. An object can simultaneously be of type double-float and
;; long-float.
;;
;;  Three types are provided: single-float, double-float, and
;; long-float. An object can simultaneously be of types single-float and
;; short-float.


;;; Package Setup


(defpackage :lisp-stat-float
  (:use :common-lisp)
  (:export +stat-float-typing+ +stat-cfloat-typing+ +stat-float-template+
	   machine-epsilon

	   base-float

	   make-base-trans-fun-2 make-base-trans-fun 

	   BASE-LOG BASE-EXP BASE-EXPT BASE-SQRT BASE-SIN BASE-COS
	   BASE-TAN BASE-ASIN BASE-ACOS BASE-ATAN BASE-SINH
	   BASE-COSH BASE-TANH BASE-ASINH BASE-ACOSH BASE-ATANH
	   BASE-ABS BASE-PHASE BASE-FFLOOR BASE-FCEILING BASE-FTRUNCATE
	   BASE-FROUND BASE-SIGNUM BASE-CIS))

(in-package #:lisp-stat-float)

;; This should technically be conditionalized to the Lisp
;; implementation, i.e. 
#+sbcl(pushnew :stat-float-is-double-float *features*)
#+cmu(pushnew :stat-float-is-double-float *features*)
#+clisp(pushnew :stat-float-is-double-float *features*)
;; etc... the above need to be verified!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Constants determining default floating point type for statistical
;;;;  operations. This type generally corresponds to a C double.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Why do we do this instead of defparameter?  mostly to allow for doc
;; defs as well as to minimize dereferencing mistakes. 
(defmacro define-constant (name value &optional doc)
  `(defparameter ,name (if (boundp ',name) (symbol-value ',name) ,value)
    ,@(when doc (list doc))))

(define-constant +stat-float-typing+ 'long-float)
(define-constant +stat-cfloat-typing+ '(complex long-float))
(define-constant +stat-float-template+ 0.d0)

;(defparameter +stat-float-typing+ 'long-float)
;(defparameter +stat-cfloat-typing+ '(complex long-float))
;(defparameter +stat-float-template+ 0.d0)

(deftype stat-float () +stat-float-typing+)
(deftype stat-cfloat () +stat-cfloat-typing+)

(defparameter machine-epsilon
  (do ((epsilon (float 1.0 +stat-float-template+)
		(/ epsilon 2.0)))
      ((= (+ 1.0 (/ epsilon 2.0)) 1.0) epsilon)))

(defmacro declare-double (&rest vars) `(declare (long-float ,@vars)))

(setf *read-default-float-format* +stat-float-typing+)

;;; FIX-BASE-DOC adds note about modification to the end of the
;;; documentation string argument.
(defmacro fix-base-doc (doc)
  `(format nil
    "~a~%Modified to coerce arguments(s) to stat-float or stat-cfloat."
    ,doc))

;;; FIX-BASE-FUN-DOC fixes documentation of SYM and installs in BASE-SYM
(defun fix-base-fun-doc (sym base-sym)
  (let ((doc (documentation sym 'function)))
    (if doc (setf (documentation base-sym 'function) (fix-base-doc doc)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Functions and Macros for modifying functions to coerce to standard
;;;;  floating point type.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; FFIX - coerces its arguments to standard real or complex floating
;;; point number if needed.
#-stat-float-is-double-float
(defmacro ffix (x)
  `(if (complexp ,x) 
    (coerce ,x +stat-cfloat-typing+)
    (float ,x +stat-float-template+)))

#+stat-float-is-double-float
(defmacro ffix (x) x)

;;; MAKEDOUBLE coerces its argument to the standard floating point
;;; type specified by +stat-float-template+

(defun makedouble (x) (float x +stat-float-template+))

;; Why doesn't this optimization work?
;; #+stat-float-is-double-float
;; (eval-when (compile)
;;   (proclaim '(function makedouble (t) long-float)))

;;;
;;; MAKE-BASE-TRANS-FUN Macro for re-defining one argument transcendental
;;; functions
;;;
#-:stat-float-is-double-float
(defmacro make-base-trans-fun (sym)
  (let* ((base-sym (intern (string-upcase (format nil "BASE-~s" sym))))
         (doc (documentation sym 'function))
	 (doc-list (if doc (list (fix-base-doc doc)))))
  `(defun ,base-sym (x)
     ,@doc-list
     (declare (inline ,sym coerce float))
     (,sym (ffix x)))))

#+:stat-float-is-double-float
(defmacro make-base-trans-fun (sym)
  (let* ((base-sym (intern (string-upcase (format nil "BASE-~s" sym))))
         (doc (documentation sym 'function)))
    `(progn (setf (symbol-function ',base-sym) (symbol-function ',sym))
            (if ,doc (setf (documentation ',base-sym 'function) ,doc)))))

;;;
;;; MAKE-BASE-TRANS-FUN-2 Macro for re-defining transcendental functions
;;; with an optional second argument
;;;
#-:stat-float-is-double-float
(defmacro make-base-trans-fun-2 (sym)
  (let* ((base-sym (intern (string-upcase (format nil "BASE-~s" sym))))
         (doc (documentation sym 'function))
	 (doc-list (if doc (list (fix-base-doc doc)))))
  `(defun ,base-sym (x &optional y)
     ,@doc-list
     (declare (inline ,sym coerce float))
     (if y (,sym (ffix x) (ffix y)) (,sym (ffix x))))))

#+:stat-float-is-double-float
(defmacro make-base-trans-fun-2 (sym)
  (let* ((base-sym (intern (string-upcase (format nil "BASE-~s" sym))))
         (doc (documentation sym 'function)))
    `(progn (setf (symbol-function ',base-sym) (symbol-function ',sym))
            (if ,doc (setf (documentation ',base-sym 'function) ,doc)))))
;;;
;;;  Modified base functions to coerce to standard floating point type
;;;


;;; BASE-FLOAT
#-:stat-float-is-double-float
(progn
  (defun base-float (x &optional (y +stat-float-template+)) (float x y))
  (fix-base-fun-doc 'float 'base-float))

#+:stat-float-is-double-float
(make-base-trans-fun float)

;;; BASE-EXPT
#-:stat-float-is-double-float
(progn
  (defun base-expt (x y)
    (declare (inline expt coerce float integerp))
    (if (integerp y) (expt x y) (expt (ffix x) (ffix y))))

  (fix-base-fun-doc 'expt 'base-expt))

#+:stat-float-is-double-float
(make-base-trans-fun expt)

;;; Others
(make-base-trans-fun-2 log)
(make-base-trans-fun exp)
(make-base-trans-fun sqrt)
(make-base-trans-fun sin)
(make-base-trans-fun cos)
(make-base-trans-fun tan)
(make-base-trans-fun asin)
(make-base-trans-fun acos)
(make-base-trans-fun-2 atan)
(make-base-trans-fun sinh)
(make-base-trans-fun cosh)
(make-base-trans-fun tanh)
(make-base-trans-fun asinh)
(make-base-trans-fun acosh)
(make-base-trans-fun atanh)
(make-base-trans-fun abs)
(make-base-trans-fun phase)
(make-base-trans-fun-2 ffloor)
(make-base-trans-fun-2 fceiling)
(make-base-trans-fun-2 ftruncate)
(make-base-trans-fun-2 fround)
(make-base-trans-fun signum)
(make-base-trans-fun cis)
