;;; -*- mode: lisp -*-
;;; Copyright (c) 2005--2007, by A.J. Rossini <blindglobe@gmail.com>
;;; See COPYRIGHT file for any additional restrictions (BSD license).
;;; Since 1991, ANSI was finally finished.  Edited for ANSI Common Lisp. 

;;;; lspackages -- Lisp-Stat package specifications
;;;; 
;;;; Copyright (c) 1991, by Luke Tierney. Permission is granted for
;;;; unrestricted use.

;;;
;;;                 LISP-STAT-OBJECT-SYSTEM Package
;;; (moved to lsobjects)

;;;                       LISP-STAT-BASICS Package
;;; (moved to lsbasics)

;;;
;;;                         LISP-STAT Package
;;;

(defpackage :lisp-stat
  (:nicknames :ls :stats)
  (:use :common-lisp
	:lisp-stat-object-system
        :lisp-stat-float
        :lisp-stat-math
	:lisp-stat-basics
	:lisp-stat-regression-linear)
  (:shadowing-import-from :lisp-stat-object-system slot-value call-next-method)

;;    ;; statistics.lsp

;;    |base-lowess|

;;    ;; maximize.lsp
;;    new-minfo-internals minfo-maximize

;;    )

;;   (:shadow

;;    ;; lsmath.lsp

;;    expt + - * / ** mod rem abs 1+ 1- log exp sqrt sin cos tan
;;    asin acos atan sinh cosh tanh asinh acosh atanh float random
;;    truncate floor ceiling round minusp zerop plusp evenp oddp 
;;    < <= = /= >= > complex conjugate realpart imagpart phase
;;    min max logand logior logxor lognot ffloor fceiling
;;    ftruncate fround signum cis

;;    )

  (:export

   ;; lsmath.lsp
   ^ ** expt + - * / mod rem pmin pmax abs 1+ 1- log exp sqrt sin cos 
   tan asin acos atan sinh cosh tanh asinh acosh atanh float random
   truncate floor ceiling round minusp zerop plusp evenp oddp < <= =
   /= >= > complex conjugate realpart imagpart phase min max
   logand logior logxor lognot ffloor fceiling ftruncate fround
   signum cis

   ;; statistics.lsp
   open-file-dialog read-data-file read-data-columns load-data 
   load-example *variables* *ask-on-redefine* def variables savevar
   undef standard-deviation quantile median interquartile-range
   fivnum covariance-matrix difference rseq matrix print-matrix solve
   backsolve eigenvalues eigenvectors accumulate cumsum combine
   lowess

   ;; regression.lsp
   regression-model regression-model-proto x y intercept sweep-matrix
   basis weights included total-sum-of-squares residual-sum-of-squares
   predictor-names response-name case-labels

   ;; nonlin.lsp
   nreg-model nreg-model-proto mean-function theta-hat epsilon
   count-limit verbose

   ;; maximize.lsp
   newtonmax nelmeadmax

   ;; bayes.lsp
   bayes-model bayes-model-proto bayes-internals

   ))


;;; Lisp-stat-user package

(defpackage :lisp-stat-user
  (:use :common-lisp
	:lisp-stat))

(in-package :lisp-stat-user)

