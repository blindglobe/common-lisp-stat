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
  (:use ;; :common-lisp
	;; :lisp-stat-object-system
	:lisp-stat-basics)
  (:shadowing-import-from :lisp-stat-object-system slot-value call-next-method)

  ;; (shadowing-import (package-shadowing-symbols 'lisp-stat-object-system))
  ;; (shadowing-import (package-shadowing-symbols 'lisp-stat-basics))

  (:import-from :ls-basics

   ;; lsmath.lsp
   ;; install-rv-function

   rv-expt rv-+ rv--
   rv-* rv-/ rv-mod
   rv-rem rv-pmin rv-pmax
   rv-1+ rv-1- rv-exp
   rv-log rv-sqrt rv-sin
   rv-cos rv-tan rv-atan
   rv-float rv-random rv-floor
   rv-ceiling rv-truncate rv-round
   rv-zerop rv-plusp rv-minusp
   rv-oddp rv-evenp rv-<
   rv-<= rv-= rv-/=
   rv->= rv-> rv-complex
   rv-realpart rv-imagpart 
   rv-conjugate

   base-expt base-log base-exp
   base-sqrt base-sin base-cos
   base-tan base-asin base-acos
   base-atan base-sinh base-cosh
   base-tanh base-asinh base-acosh
   base-atanh base-float base-abs
   base-phase base-ffloor
   base-fceiling base-ftruncate
   base-fround base-signum
   base-cis

   make-rv-function make-rv-function-1

   ;; statistics.lsp

   |base-lowess|

   ;; maximize.lsp
   new-minfo-internals minfo-maximize

   )

  (:shadow

   ;; lsmath.lsp

   expt + - * / ** mod rem abs 1+ 1- log exp sqrt sin cos tan
   asin acos atan sinh cosh tanh asinh acosh atanh float random
   truncate floor ceiling round minusp zerop plusp evenp oddp 
   < <= = /= >= > complex conjugate realpart imagpart phase
   min max logand logior logxor lognot ffloor fceiling
   ftruncate fround signum cis

   )

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

;;(in-package :lisp-stat)


;;;;
;;;; lstoplevel.lsp
;;;;

#+:kcl
(import '(si::*quit-tag* si::*eof* si::*lisp-initialized* 
			 si::reset-stack-limits si::break-current))
