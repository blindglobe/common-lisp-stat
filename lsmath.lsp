;;;; lsmath -- Install vectorized arithmetic functions
;;;; 
;;;; Copyright (c) 1991, by Luke Tierney. Permission is granted for
;;;; unrestricted use.

(provide "lsmath")

;;;;
;;;; Package Setup
;;;;

#+:CLtL2
(progn
  (defpackage "LISP-STAT"
    (:nicknames "LS" "STATS")
    (:use "COMMON-LISP" "LISP-STAT-BASICS" "LISP-STAT-OBJECT-SYSTEM"))

  (in-package lisp-stat))
#-:CLtL2
(in-package 'lisp-stat 
	    :nicknames '(ls stats)
	    :use '(lisp ls-basics lsos))

(shadowing-import (package-shadowing-symbols 'lisp-stat-object-system))
(shadowing-import (package-shadowing-symbols 'lisp-stat-basics))
(use-package 'lisp-stat-object-system)
(use-package 'lisp-stat-basics)

;;;
;;; Shadow the symbols in the lisp package that will be redefined
;;;

(shadow '(expt + - * / ** mod rem abs 1+ 1- log exp sqrt sin cos tan
	  asin acos atan sinh cosh tanh asinh acosh atanh float random
	  truncate floor ceiling round minusp zerop plusp evenp oddp 
	  < <= = /= >= > complex conjugate realpart imagpart phase
	  min max logand logior logxor lognot ffloor fceiling
	  ftruncate fround signum cis))

(export '(^ ** expt + - * / mod rem pmin pmax abs 1+ 1- log exp sqrt sin cos 
          tan asin acos atan sinh cosh tanh asinh acosh atanh float random
	  truncate floor ceiling round minusp zerop plusp evenp oddp < <= =
	  /= >= > complex conjugate realpart imagpart phase min max
	  logand logior logxor lognot ffloor fceiling ftruncate fround 
	  signum cis))

;;;;
;;;; Import some symbols
;;;;

(import '(ls-basics::make-rv-function ls-basics::make-rv-function-1))

#+(and kcl fast-c-code internal-c-math)
(progn
(import 'ls-basics::install-rv-function)
(import '(ls-basics::rv-expt ls-basics::rv-+ ls-basics::rv--
	  ls-basics::rv-* ls-basics::rv-/ ls-basics::rv-mod
	  ls-basics::rv-rem ls-basics::rv-pmin ls-basics::rv-pmax
	  ls-basics::rv-1+ ls-basics::rv-1- ls-basics::rv-exp
	  ls-basics::rv-log ls-basics::rv-sqrt ls-basics::rv-sin
	  ls-basics::rv-cos ls-basics::rv-tan ls-basics::rv-atan
	  ls-basics::rv-float ls-basics::rv-random ls-basics::rv-floor
	  ls-basics::rv-ceiling ls-basics::rv-truncate ls-basics::rv-round
	  ls-basics::rv-zerop ls-basics::rv-plusp ls-basics::rv-minusp
	  ls-basics::rv-oddp ls-basics::rv-evenp ls-basics::rv-<
	  ls-basics::rv-<= ls-basics::rv-= ls-basics::rv-/=
	  ls-basics::rv->= ls-basics::rv-> ls-basics::rv-complex
	  ls-basics::rv-realpart ls-basics::rv-imagpart 
	  ls-basics::rv-conjugate))
)

(import '(ls-basics::base-expt ls-basics::base-log ls-basics::base-exp
	  ls-basics::base-sqrt ls-basics::base-sin ls-basics::base-cos
	  ls-basics::base-tan ls-basics::base-asin ls-basics::base-acos
	  ls-basics::base-atan ls-basics::base-sinh ls-basics::base-cosh
	  ls-basics::base-tanh ls-basics::base-asinh ls-basics::base-acosh
	  ls-basics::base-atanh ls-basics::base-float ls-basics::base-abs
	  ls-basics::base-phase ls-basics::base-ffloor
	  ls-basics::base-fceiling ls-basics::base-ftruncate
	  ls-basics::base-fround ls-basics::base-signum
	  ls-basics::base-cis))

;;;;
;;;; Patch up some type definitions
;;;;

(deftype float () 'lisp:float)
(deftype complex () 'lisp:complex)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Install the vectorized math functions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(make-rv-function ^ base-expt x y)
(make-rv-function ** base-expt x y)
(make-rv-function expt base-expt x y)

(make-rv-function + lisp:+)
(make-rv-function-1 - lisp:-)
(make-rv-function * lisp:*)
(make-rv-function-1 / lisp:/)
(make-rv-function mod lisp:mod x y)
(make-rv-function rem lisp:rem x y)
(make-rv-function-1 pmin lisp:min)
(make-rv-function-1 pmax lisp:max)
(make-rv-function abs base-abs x)
(make-rv-function 1+ lisp:1+ x)
(make-rv-function 1- lisp:1- x)

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
(make-rv-function-1 random lisp:random)

(make-rv-function-1 floor lisp:floor)
(make-rv-function-1 ceiling lisp:ceiling)
(make-rv-function-1 truncate lisp:truncate)
(make-rv-function-1 round lisp:round)

(make-rv-function zerop lisp:zerop x)
(make-rv-function plusp lisp:plusp x)
(make-rv-function minusp lisp:minusp x)
(make-rv-function oddp lisp:oddp x)
(make-rv-function evenp lisp:evenp x)

(make-rv-function-1 < lisp:<)
(make-rv-function-1 <= lisp:<=)
(make-rv-function-1 = lisp:=)
(make-rv-function-1 /= lisp:/=)
(make-rv-function-1 >= lisp:>=)
(make-rv-function-1 > lisp:>)

(make-rv-function-1 complex lisp:complex)
(make-rv-function realpart lisp:realpart x)
(make-rv-function imagpart lisp:imagpart x)
(make-rv-function conjugate lisp:conjugate x)
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
            (if (lisp:< r result) (setf result r))))
        (let ((n (length seq)))
          (declare (fixnum n))
          (dotimes (i n result)
            (declare (fixnum i))
            (let* ((x (aref seq i))
                   (r (if (numberp x) x (min-1 x))))
              (if (lisp:< r result) (setf result r)))))))))

(defun min (x &optional (y nil has-y) &rest args)
  (if (and (null args) (numberp x) (numberp y))
    (lisp:min x y)
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
            (if (lisp:> r result) (setf result r))))
        (let ((n (length seq)))
          (declare (fixnum n))
          (dotimes (i n result)
            (declare (fixnum i))
            (let* ((x (aref seq i))
                   (r (if (numberp x) x (max-1 x))))
              (if (lisp:> r result) (setf result r)))))))))

(defun max (x &optional (y nil has-y) &rest args)
  (if (and (null args) (numberp x) (numberp y))
    (lisp:max x y)
    (if has-y (max-1 (cons x (cons y args))) (max-1 x))))

(make-rv-function logand lisp:logand)
(make-rv-function logior lisp:logior)
(make-rv-function logxor lisp:logxor)
(make-rv-function lognot lisp:lognot x)

(make-rv-function-1 ffloor base-ffloor)
(make-rv-function-1 fceiling base-fceiling)
(make-rv-function-1 ftruncate base-ftruncate)
(make-rv-function-1 fround base-fround)
(make-rv-function signum base-signum x)
(make-rv-function cis base-cis x)
