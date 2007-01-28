;;; -*- mode: lisp -*-
;;; Copyright (c) 2005--2007, by A.J. Rossini <blindglobe@gmail.com>
;;; See COPYRIGHT file for any additional restrictions (BSD license).
;;; Since 1991, ANSI was finally finished.  Edited for ANSI Common Lisp. 

;;;; lspackages -- Lisp-Stat package specifications
;;;; 
;;;; Copyright (c) 1991, by Luke Tierney. Permission is granted for
;;;; unrestricted use.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;;                 LISP-STAT-OBJECT-SYSTEM Package
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;
;;;; lsobjects.lsp
;;;;

(defpackage :lisp-stat-object-system
 (:nicknames :ls-objects :lsos)
 (:use :common-lisp)
 (:shadow call-next-method slot-value)
 (:export ls-object objectp *object* kind-of-p make-object *message-hook*
	  *set-slot-hook* slot-value self send call-next-method call-method
	  defmeth defproto instance-slots proto-name))

(in-package :lisp-stat-object-system)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;;                       LISP-STAT-BASICS Package
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;
;;;; lsbasics.lsp
;;;;

(defpackage #:lisp-stat-basics
  (:nicknames #:ls-basics)
  (:use #:common-lisp #:lisp-stat-object-system)
  ;;(:shadowing-import-from (package-shadowing-symbols #:lisp-stat-object-system))
  (:export sequencep copy-vector copy-array iseq which repeat select 
	   permute-array sum prod count-elements mean if-else
	   sample sort-data order rank))

(in-package #:lisp-stat-basics)

;;;;
;;;; kclpatch.lsp
;;;;

#+:kcl
(export '(function-lambda-expression realp fixnump))

;;;;
;;;; compound.lsp
;;;;

(export compound-data-p map-elements compound-data-seq
	compound-data-length element-seq compound-data-proto)

;;;;
;;;; dists.lsp
;;;;

(export log-gamma uniform-rand normal-cdf normal-quant normal-dens
	normal-rand bivnorm-cdf cauchy-cdf cauchy-quant cauchy-dens
	cauchy-rand gamma-cdf gamma-quant gamma-dens gamma-rand
	chisq-cdf chisq-quant chisq-dens chisq-rand beta-cdf beta-quant
	beta-dens beta-rand t-cdf t-quant t-dens t-rand f-cdf f-quant
	f-dens f-rand poisson-cdf poisson-quant poisson-pmf poisson-rand 
	binomial-cdf binomial-quant binomial-pmf binomial-rand)

;;;;
;;;; linalg.lsp
;;;;

(export chol-decomp lu-decomp lu-solve determinant inverse sv-decomp
	qr-decomp rcondest make-rotation spline kernel-dens kernel-smooth
	fft make-sweep-matrix sweep-operator ax+y numgrad numhess
	split-list eigen)

;;;;
;;;; matrices.lsp
;;;;

(export matrixp num-rows num-cols matmult identity-matrix diagonal
	row-list column-list inner-product outer-product cross-product
	transpose bind-columns bind-rows)

;;;; 
;;;; lsfloat.lsp
;;;;

(export +stat-float-type+ +stat-cfloat-type+ +stat-float-template+
	machine-epsilon)

;;;;
;;;; mclglue.lsp
;;;;

#+:mcl
(import '(ccl:def-logical-directory ccl:ff-load ccl:deffcfun ccl:defccallable))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;;                         LISP-STAT Package
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;
;;;; lsmath.lsp
;;;;

(defpackage :lisp-stat
  (:nicknames :ls :stats)
  (:use :common-lisp :lisp-stat-basics :lisp-stat-object-system))

(in-package :lisp-stat)

;; (shadowing-import (package-shadowing-symbols 'lisp-stat-object-system))
;; (shadowing-import (package-shadowing-symbols 'lisp-stat-basics))
;; (use-package 'lisp-stat-object-system)
;; (use-package 'lisp-stat-basics)

(shadow expt + - * / ** mod rem abs 1+ 1- log exp sqrt sin cos tan
	asin acos atan sinh cosh tanh asinh acosh atanh float random
	truncate floor ceiling round minusp zerop plusp evenp oddp 
	< <= = /= >= > complex conjugate realpart imagpart phase
	min max logand logior logxor lognot ffloor fceiling
	ftruncate fround signum cis)

(export ^ ** expt + - * / mod rem pmin pmax abs 1+ 1- log exp sqrt sin cos 
	tan asin acos atan sinh cosh tanh asinh acosh atanh float random
	truncate floor ceiling round minusp zerop plusp evenp oddp < <= =
	/= >= > complex conjugate realpart imagpart phase min max
	logand logior logxor lognot ffloor fceiling ftruncate fround
	signum cis)

(import ls-basics::install-rv-function)
(import ls-basics::rv-expt ls-basics::rv-+ ls-basics::rv--
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
	ls-basics::rv-conjugate)

(import ls-basics::base-expt ls-basics::base-log ls-basics::base-exp
	ls-basics::base-sqrt ls-basics::base-sin ls-basics::base-cos
	ls-basics::base-tan ls-basics::base-asin ls-basics::base-acos
	ls-basics::base-atan ls-basics::base-sinh ls-basics::base-cosh
	ls-basics::base-tanh ls-basics::base-asinh ls-basics::base-acosh
	ls-basics::base-atanh ls-basics::base-float ls-basics::base-abs
	ls-basics::base-phase ls-basics::base-ffloor
	ls-basics::base-fceiling ls-basics::base-ftruncate
	ls-basics::base-fround ls-basics::base-signum
	ls-basics::base-cis)

(import ls-basics::make-rv-function ls-basics::make-rv-function-1)

;;;;
;;;; statistics.lsp
;;;;

(export open-file-dialog read-data-file read-data-columns load-data 
	load-example *variables* *ask-on-redefine* def variables savevar
	undef standard-deviation quantile median interquartile-range
	fivnum covariance-matrix difference rseq matrix print-matrix solve
	backsolve eigenvalues eigenvectors accumulate cumsum combine
	lowess)

(import ls-basics::|base-lowess|)

;;;;
;;;; regression.lsp
;;;;

(export regression-model regression-model-proto x y intercept sweep-matrix
	basis weights included total-sum-of-squares residual-sum-of-squares
	predictor-names response-name case-labels)

;;;;
;;;; nonlin.lsp
;;;;

(export nreg-model nreg-model-proto mean-function theta-hat epsilon
	count-limit verbose)

;;;;
;;;; maximize.lsp
;;;;

(export newtonmax nelmeadmax)

(import ls-basics::new-minfo-internals ls-basics::minfo-maximize)

;;;;
;;;; bayes.lsp
;;;;

(export bayes-model bayes-model-proto bayes-internals)

;;;;
;;;; lstoplevel.lsp
;;;;

#+:kcl
(import '(si::*quit-tag* si::*eof* si::*lisp-initialized* 
			 si::reset-stack-limits si::break-current))
