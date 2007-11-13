;;; -*- mode: lisp -*-
;;; Copyright (c) 2005--2007, by A.J. Rossini <blindglobe@gmail.com>
;;; See COPYRIGHT file for any additional restrictions (BSD license).
;;; Since 1991, ANSI was finally finished.  Edited for ANSI Common Lisp. 

;;;; lsmath -- Install vectorized arithmetic functions
;;;; 
;;;; Copyright (c) 1991, by Luke Tierney. Permission is granted for
;;;; unrestricted use.

;;; Package Setup

(in-package :cl-user)

(defpackage :lisp-stat-math
   (:use :common-lisp
	 :lisp-stat-object-system
	 :lisp-stat-macros
	 :lisp-stat-compound-data
	 :lisp-stat-float)
   (:shadowing-import-from :lisp-stat-object-system
			   slot-value call-method call-next-method)
   (:shadow expt + - * / ** mod rem abs 1+ 1- log exp sqrt sin cos tan
 	   asin acos atan sinh cosh tanh asinh acosh atanh float random
 	   truncate floor ceiling round minusp zerop plusp evenp oddp 
 	   < <= = /= >= > ;; complex
	   conjugate realpart imagpart phase
 	   min max logand logior logxor lognot ffloor fceiling
 	   ftruncate fround signum cis)
   (:export ^ ** expt + - * / mod rem pmin pmax abs 1+ 1- log exp sqrt sin cos 
 	   tan asin acos atan sinh cosh tanh asinh acosh atanh float random
 	   truncate floor ceiling round minusp zerop plusp evenp oddp < <= =
 	   /= >= > ;; complex
	   conjugate realpart imagpart phase min max
 	   logand logior logxor lognot ffloor fceiling ftruncate fround 
 	   signum cis)
   (:documentation "Vectorization of numerical functions"))

(in-package :lisp-stat-math)

;;; Patch up some type definitions

;;(deftype float () 'common-lisp:float)
;;(deftype complex () 'common-lisp:complex)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Install the vectorized math functions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(make-rv-function ^ base-expt x y)
(make-rv-function ** base-expt x y)
(make-rv-function expt base-expt x y)

(make-rv-function + common-lisp:+)
(make-rv-function-1 - common-lisp:-)
(make-rv-function * common-lisp:*)
(make-rv-function-1 / common-lisp:/)
(make-rv-function mod common-lisp:mod x y)
(make-rv-function rem common-lisp:rem x y)
(make-rv-function-1 pmin common-lisp:min)
(make-rv-function-1 pmax common-lisp:max)
(make-rv-function abs base-abs x)
(make-rv-function 1+ common-lisp:1+ x)
(make-rv-function 1- common-lisp:1- x)

(make-rv-function-1 log base-log)
(make-rv-function exp base-exp x)
(make-rv-function sqrt base-sqrt x)

(make-rv-function sin base-sin x)
(make-rv-function cos base-cos x)
(make-rv-function tan base-tan x)
(make-rv-function asin base-asin x)
(make-rv-function acos base-acos x)
(make-rv-function-1 atan base-atan)
(make-rv-function sinh base-sinh x)
(make-rv-function cosh base-cosh x)
(make-rv-function tanh base-tanh x)
(make-rv-function asinh base-asinh x)
(make-rv-function acosh base-acosh x)
(make-rv-function atanh base-atanh x)

(make-rv-function-1 float base-float)
(make-rv-function-1 random common-lisp:random)

(make-rv-function-1 floor common-lisp:floor)
(make-rv-function-1 ceiling common-lisp:ceiling)
(make-rv-function-1 truncate common-lisp:truncate)
(make-rv-function-1 round common-lisp:round)

(make-rv-function zerop common-lisp:zerop x)
(make-rv-function plusp common-lisp:plusp x)
(make-rv-function minusp common-lisp:minusp x)
(make-rv-function oddp common-lisp:oddp x)
(make-rv-function evenp common-lisp:evenp x)

(make-rv-function-1 < common-lisp:<)
(make-rv-function-1 <= common-lisp:<=)
(make-rv-function-1 = common-lisp:=)
(make-rv-function-1 /= common-lisp:/=)
(make-rv-function-1 >= common-lisp:>=)
(make-rv-function-1 > common-lisp:>)

;;(make-rv-function-1 complex common-lisp:complex)
(make-rv-function realpart common-lisp:realpart x)
(make-rv-function imagpart common-lisp:imagpart x)
(make-rv-function conjugate common-lisp:conjugate x)
(make-rv-function phase base-phase x)

(defun min-1 (x)
  (if (numberp x)
    x
    (let* ((seq (compound-data-seq x))
           (first (elt seq 0))
           (result (if (numberp first) first (min-1 first))))
      (if (consp seq)
        (dolist (x (rest seq) result)
          (let ((r (if (numberp x) x (min-1 x))))
            (if (common-lisp:< r result) (setf result r))))
        (let ((n (length seq)))
          (declare (fixnum n))
          (dotimes (i n result)
            (declare (fixnum i))
            (let* ((x (aref seq i))
                   (r (if (numberp x) x (min-1 x))))
              (if (common-lisp:< r result) (setf result r)))))))))

(defun min (x &optional (y nil has-y) &rest args)
  (if (and (null args) (numberp x) (numberp y))
    (common-lisp:min x y)
    (if has-y (min-1 (cons x (cons y args))) (min-1 x))))

(defun max-1 (x)
  (if (numberp x)
    x
    (let* ((seq (compound-data-seq x))
           (first (elt seq 0))
           (result (if (numberp first) first (max-1 first))))
      (if (consp seq)
        (dolist (x (rest seq) result)
          (let ((r (if (numberp x) x (max-1 x))))
            (if (common-lisp:> r result) (setf result r))))
        (let ((n (length seq)))
          (declare (fixnum n))
          (dotimes (i n result)
            (declare (fixnum i))
            (let* ((x (aref seq i))
                   (r (if (numberp x) x (max-1 x))))
              (if (common-lisp:> r result) (setf result r)))))))))

(defun max (x &optional (y nil has-y) &rest args)
  (if (and (null args) (numberp x) (numberp y))
    (common-lisp:max x y)
    (if has-y (max-1 (cons x (cons y args))) (max-1 x))))

(make-rv-function logand common-lisp:logand)
(make-rv-function logior common-lisp:logior)
(make-rv-function logxor common-lisp:logxor)
(make-rv-function lognot common-lisp:lognot x)

(make-rv-function-1 ffloor base-ffloor)
(make-rv-function-1 fceiling base-fceiling)
(make-rv-function-1 ftruncate base-ftruncate)
(make-rv-function-1 fround base-fround)
(make-rv-function signum base-signum x)
(make-rv-function cis base-cis x)
