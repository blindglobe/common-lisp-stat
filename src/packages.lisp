;;; -*- mode: lisp -*-

;;; Time-stamp: <2008-11-07 17:56:48 tony>
;;; Creation:   <2008-03-11 19:18:34 user> 
;;; File:       packages.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2007--2008, AJ Rossini.  BSD, LLGPL, or GPLv2, depending
;;;             on how it arrives.  
;;; Purpose:    package structure description for lispstat

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

;;; This organization and structure is new to the 21st Century
;;; version.

(in-package :cl-user)

;;; LispStat Basics

(in-package :cl-user)

(defpackage :lisp-stat-object-system
 (:nicknames :ls-objects :lsos)
 (:use :common-lisp)
 (:shadow :call-method :call-next-method :slot-value)
 (:export ls-object objectp *object* kind-of-p make-object
	  *message-hook*
	  *set-slot-hook* slot-value self 
	  send call-next-method call-method
	  defmeth defproto instance-slots proto-name))





(defpackage :lisp-stat-types
  (:documentation "Provides some typeing for LispStat, but is clearly
                   a bit incomplete.")
  (:use :common-lisp)
  (:export fixnump
	   check-nonneg-fixnum check-one-nonneg-fixnum
	   check-one-fixnum check-one-real check-one-number))


;;; Package Setup

(in-package :cl-user)

(defpackage :lisp-stat-float
  (:use :common-lisp)
  (:export +stat-float-typing+ +stat-cfloat-typing+ +stat-float-template+
	   machine-epsilon base-float makedouble

	   make-base-trans-fun-2 make-base-trans-fun 

	   base-log base-exp base-expt base-sqrt base-sin base-cos
	   base-tan base-asin base-acos base-atan base-sinh
	   BASE-COSH BASE-TANH BASE-ASINH BASE-ACOSH BASE-ATANH
	   BASE-ABS BASE-PHASE BASE-FFLOOR BASE-FCEILING BASE-FTRUNCATE
	   BASE-FROUND BASE-SIGNUM BASE-CIS))


(defpackage :lisp-stat-compound-data
  (:use :common-lisp
	:lisp-stat-object-system
	:lisp-stat-types)
  (:shadowing-import-from :lisp-stat-object-system
			  slot-value
			  call-next-method call-method)
  (:export compound-data-p *compound-data-proto*
	   compound-object-p
	   compound-data-seq compound-data-length
	   element-list element-seq
	   sort-data order rank
	   recursive-map-elements map-elements repeat
	   check-sequence
	   get-next-element make-next-element set-next-element
	   sequencep iseq ordered-nneg-seq
	   select split-list which
	   difference rseq
	   flatten-list ))

(defpackage :lisp-stat-macros
  (:use :common-lisp
	:lisp-stat-compound-data)
  (:export make-rv-function make-rv-function-1))

(defpackage :lisp-stat-basics
    (:use :common-lisp
	  :lisp-stat-object-system
	  :lisp-stat-types
	  :lisp-stat-float
	  :lisp-stat-macros
	  :lisp-stat-compound-data)
  (:shadowing-import-from :lisp-stat-object-system
			  slot-value call-method call-next-method)
  (:export permute-array sum prod count-elements mean
	   if-else sample))



(defpackage :lisp-stat-float
  (:use :common-lisp)
  (:export +stat-float-typing+ +stat-cfloat-typing+ +stat-float-template+
	   machine-epsilon base-float makedouble

	   make-base-trans-fun-2 make-base-trans-fun 

	   BASE-LOG BASE-EXP BASE-EXPT BASE-SQRT BASE-SIN BASE-COS
	   BASE-TAN BASE-ASIN BASE-ACOS BASE-ATAN BASE-SINH
	   BASE-COSH BASE-TANH BASE-ASINH BASE-ACOSH BASE-ATANH
	   BASE-ABS BASE-PHASE BASE-FFLOOR BASE-FCEILING BASE-FTRUNCATE
	   BASE-FROUND BASE-SIGNUM BASE-CIS))

;;; 

(defpackage :lisp-stat-macros
  (:use :common-lisp
	:lisp-stat-compound-data)
  (:export make-rv-function make-rv-function-1))

;;; NEW CLOS STRUCTURE

(defpackage :lisp-stat-data-clos
  (:use :common-lisp
	:lisp-matrix)
  (:export get-variable-matrix get-variable-vector
	   ;; generic container class for data -- if small enough
	   ;; could be value, otherwise might be reference.
	   data-pointer))

(defpackage :lisp-stat-regression-linear-clos
  (:use :common-lisp
	:lisp-matrix
	:lisp-stat-data-clos)
  (:export regression-model))



;;; USER PACKAGES

(defpackage :lisp-stat-ffi-int
  (:use :common-lisp
	:cffi)
  (:export ccl-store-integer ccl-store-double ccl-store-ptr
	   get-buf ))

(defpackage :lisp-stat-probability
  (:use :common-lisp
	:cffi
	:lisp-stat-ffi-int
	:lisp-stat-macros)
  (:export log-gamma set-seed
	   uniform-rand
	   normal-cdf normal-quant normal-dens normal-rand
	   bivnorm-cdf
	   cauchy-cdf cauchy-quant cauchy-dens cauchy-rand
	   gamma-cdf gamma-quant gamma-dens gamma-rand
	   chisq-cdf chisq-quant chisq-dens chisq-rand
	   beta-cdf beta-quant beta-dens beta-rand
	   t-cdf t-quant t-dens t-rand
	   f-cdf f-quant f-dens f-rand
	   poisson-cdf poisson-quant poisson-pmf poisson-rand 
	   binomial-cdf binomial-quant binomial-pmf binomial-rand))



(defpackage :lisp-stat-matrix
  (:use :common-lisp
	:cffi
	:lisp-stat-compound-data)
  (:export matrixp ;;  matrix -- conflicts!
	   num-rows num-cols matmult identity-matrix diagonal
	   row-list column-list inner-product outer-product
	   cross-product transpose bind-columns bind-rows
	   array-data-vector vector-to-array

	   check-matrix check-square-matrix

	   copy-array copy-vector
	   ))

(defpackage :lisp-stat-linalg-data
  (:use :common-lisp
	:cffi
	:lisp-stat-ffi-int
	:lisp-stat-types
	:lisp-stat-compound-data
	:lisp-stat-matrix)
  (:export ;; more to add
      +mode-in+ +mode-re+ +mode-cx+ mode-of
      
      la-data-mode la-allocate la-free

      la-get-double la-put-double
      la-put-integer
      
      la-matrix la-free-matrix la-matrix-to-data la-data-to-matrix
      la-vector la-free-vector la-vector-to-data la-data-to-vector ))


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


(defpackage :lisp-stat-linalg
  (:use :common-lisp
	:cffi
	:lisp-stat-ffi-int
	:lisp-stat-math
	:lisp-stat-types
	:lisp-stat-float
	:lisp-stat-compound-data
	:lisp-stat-linalg-data
	:lisp-stat-matrix)
  (:shadowing-import-from :lisp-stat-math
	  expt + - * / ** mod rem abs 1+ 1- log exp sqrt sin cos tan
	  asin acos atan sinh cosh tanh asinh acosh atanh float random
	  truncate floor ceiling round minusp zerop plusp evenp oddp 
	  < <= = /= >= > complex conjugate realpart imagpart phase
	  min max logand logior logxor lognot ffloor fceiling
	  ftruncate fround signum cis)
  (:export chol-decomp lu-decomp lu-solve determinant inverse
	   sv-decomp qr-decomp rcondest make-rotation spline
	   kernel-dens kernel-smooth 
	   fft make-sweep-matrix sweep-operator ax+y eigen

	   check-real ;; for optimize

	   covariance-matrix matrix print-matrix solve
	   backsolve eigenvalues eigenvectors accumulate cumsum combine
	   lowess))


(defpackage :lisp-stat-config
  (:use :common-lisp)
  (:export *default-path*
	   *lsos-files* *basic-files* *ls-files*
	   *lispstat-data-dir* *lispstat-examples-dir*))


(defpackage :lisp-stat-data
  (:documentation "Data management, integration, I/O, and other data technologies.")
  (:nicknames :ls-data)
  (:use :common-lisp
	:lisp-stat-object-system
	:lisp-stat-config
	:lisp-stat-types
	:lisp-stat-compound-data)
  (:shadowing-import-from :lisp-stat-object-system
			  slot-value call-method call-next-method)
  (:export open-file-dialog read-data-file read-data-columns load-data
	   load-example *variables* *ask-on-redefine*
	   def variables savevar undef))

(defpackage :lisp-stat-descriptive-statistics
 (:use :common-lisp
       :lisp-stat-data
       :lisp-stat-math
       :lisp-stat-compound-data
       :lisp-stat-matrix
       :lisp-stat-linalg-data
       :lisp-stat-linalg
       :lisp-stat-basics)
   (:shadowing-import-from :lisp-stat-math ;; life is a vector!
      expt + - * / ** mod rem abs 1+ 1- log exp sqrt sin cos tan
      asin acos atan sinh cosh tanh asinh acosh atanh float random
      truncate floor ceiling round minusp zerop plusp evenp oddp 
      < <= = /= >= > ;; complex 
      conjugate realpart imagpart phase
      min max logand logior logxor lognot ffloor fceiling
      ftruncate fround signum cis)
   (:export standard-deviation quantile median interquartile-range
	    fivnum sample))


(defpackage :lisp-stat-regression-linear
  (:use :common-lisp
	:lisp-stat-object-system
	:lisp-stat-basics
	:lisp-stat-compound-data
	:lisp-stat-math
	:lisp-stat-matrix
	:lisp-stat-linalg
	:lisp-stat-descriptive-statistics)
  (:shadowing-import-from :lisp-stat-object-system
			  slot-value call-method call-next-method)
  (:shadowing-import-from :lisp-stat-math
      expt + - * / ** mod rem abs 1+ 1- log exp sqrt sin cos tan
      asin acos atan sinh cosh tanh asinh acosh atanh float random
      truncate floor ceiling round minusp zerop plusp evenp oddp 
      < <= = /= >= > ;; complex
      conjugate realpart imagpart phase
      min max logand logior logxor lognot ffloor fceiling
      ftruncate fround signum cis)
  (:export regression-model regression-model-proto x y intercept sweep-matrix
	   basis weights included total-sum-of-squares residual-sum-of-squares
	   predictor-names response-name case-labels))


(defpackage :lisp-stat
    (:documentation "Experimentation package for LispStat.  Serious
    work should be packaged up elsewhere for reproducibility.  By this
    I mean, creating a data/analytics/analysis package with the
    minimal set of objects required.")   
  (:use :common-lisp
	:lisp-stat-object-system
	:lisp-stat-compound-data
	:lisp-stat-probability
	:lisp-stat-types
        :lisp-stat-float
	:lisp-stat-basics
	:lisp-stat-data
        :lisp-stat-math
	:lisp-stat-matrix
	:lisp-stat-linalg
	:lisp-stat-descriptive-statistics
	:lisp-stat-regression-linear)
  (:shadowing-import-from :lisp-stat-object-system
	slot-value call-method call-next-method)
  (:shadowing-import-from :lisp-stat-math
	expt + - * / ** mod rem abs 1+ 1- log exp sqrt sin cos tan
	asin acos atan sinh cosh tanh asinh acosh atanh float random
	truncate floor ceiling round minusp zerop plusp evenp oddp 
	< <= = /= >= > ;;complex 
	conjugate realpart imagpart phase
	min max logand logior logxor lognot ffloor fceiling
	ftruncate fround signum cis)
  (:export
   ;; lsobjects :
   defproto defproto2
   defmeth send 
  
   ;; lstypes :
   fixnump check-nonneg-fixnum check-one-fixnum
   check-one-real check-one-number

   ;; lsmacros: 
   
   ;; lsfloat :
   machine-epsilon

   ;; compound :
   compound-data-p *compound-data-proto* compound-object-p
   compound-data-seq compound-data-length 
   element-list element-seq
   sort-data order rank
   recursive-map-elements map-elements
   repeat
   check-sequence
   get-next-element make-next-element set-next-element
   sequencep iseq
   ordered-nneg-seq
   select which
   difference rseq

   ;; lsmath.lsp
   ^ ** expt + - * / mod rem pmin pmax abs 1+ 1- log exp sqrt sin cos 
   tan asin acos atan sinh cosh tanh asinh acosh atanh float random
   truncate floor ceiling round minusp zerop plusp evenp oddp < <= =
   /= >= > ;; complex
   conjugate realpart imagpart phase min max
   logand logior logxor lognot ffloor fceiling ftruncate fround
   signum cis

   ;; matrices.lisp
   matrixp num-rows num-cols matmult identity-matrix diagonal row-list
   column-list inner-product outer-product cross-product transpose
   bind-columns bind-rows

   ;; linalg.lisp
   chol-decomp lu-decomp lu-solve determinant inverse
   sv-decomp qr-decomp rcondest make-rotation spline
   kernel-dens kernel-smooth 
   fft make-sweep-matrix sweep-operator ax+y eigen
   check-real
   covariance-matrix matrix print-matrix solve
   backsolve eigenvalues eigenvectors accumulate cumsum combine
   lowess

   ;; in linalg.lisp, possibly not supported by matlisp
   spline kernel-dens kernel-smooth

   ;; optimize.lsp
   newtonmax nelmeadmax

   ;; lispstat-macros
   make-rv-function make-rv-function-1 

   ;; data.lisp
   open-file-dialog read-data-file read-data-columns load-data
   load-example *variables* *ask-on-redefine*
   def variables savevar undef

   ;; statistics.lsp
   standard-deviation quantile median interquartile-range
   fivnum sample

   ;; dists
   log-gamma set-seed
   uniform-rand normal-cdf normal-quant normal-dens
   normal-rand bivnorm-cdf cauchy-cdf cauchy-quant cauchy-dens
   cauchy-rand gamma-cdf gamma-quant gamma-dens gamma-rand
   chisq-cdf chisq-quant chisq-dens chisq-rand beta-cdf beta-quant
   beta-dens beta-rand t-cdf t-quant t-dens t-rand f-cdf f-quant
   f-dens f-rand poisson-cdf poisson-quant poisson-pmf poisson-rand 
   binomial-cdf binomial-quant binomial-pmf binomial-rand

;;; Here is where we have a problem -- lispstat core should be core
;;; data management and config problems, with packages providing
;;; specialized extensions to LispStat, i.e. regression, nonlin
;;; regression, bayesian regression via laplace approximation, etc. 

;;; The following could be considered "recommended packages", similar
;;; to the idea of the recommended packages in R.

   ;; regression.lsp
   regression-model regression-model-proto x y intercept sweep-matrix
   basis weights included total-sum-of-squares residual-sum-of-squares
   predictor-names response-name case-labels

   ;; nonlin.lsp
   nreg-model nreg-model-proto mean-function theta-hat epsilon
   count-limit verbose

   ;; bayes.lsp
   bayes-model bayes-model-proto bayes-internals))


(defpackage :lisp-stat-data-examples
  (:documentation "Example data for unittests, examples, illustrations,")
  (:use :common-lisp
	:lisp-stat)
  (:shadowing-import-from :lisp-stat
      slot-value call-method call-next-method

      expt + - * / ** mod rem abs 1+ 1- log exp sqrt sin cos tan
      asin acos atan sinh cosh tanh asinh acosh atanh float random
      truncate floor ceiling round minusp zerop plusp evenp oddp 
      < <= = /= >= > > ;; complex
      conjugate realpart imagpart phase
      min max logand logior logxor lognot ffloor fceiling
      ftruncate fround signum cis

      <= float imagpart)
  (:export iron aluminum absorbtion
	   diabetes dlabs))


(defpackage :lisp-stat-user
  (:documentation "Experimentation package for LispStat.
Serious work should be placed in a similar package elsewhere for
reproducibility.  But this should hint as to what needs to be
done for a user- or analysis-package.")
  (:nicknames :ls-user)
  (:use :common-lisp
	:lisp-stat
	:lisp-stat-data-examples) ;; this last is to have 'things to play with'
  (:shadowing-import-from :lisp-stat
      slot-value call-method call-next-method

      expt + - * / ** mod rem abs 1+ 1- log exp sqrt sin cos tan
      asin acos atan sinh cosh tanh asinh acosh atanh float random
      truncate floor ceiling round minusp zerop plusp evenp oddp 
      < <= = /= >= > > ;; complex
      conjugate realpart imagpart phase
      min max logand logior logxor lognot ffloor fceiling
      ftruncate fround signum cis

      <= float imagpart))



(defpackage :lisp-stat-unittests
  (:use :common-lisp :lift :lisp-stat)
  (:shadowing-import-from :lisp-stat
	slot-value call-method call-next-method ;; objects
	expt + - * / ** mod rem abs 1+ 1- log exp sqrt sin cos tan ;; lsmath
	asin acos atan sinh cosh tanh asinh acosh atanh float random
	truncate floor ceiling round minusp zerop plusp evenp oddp 
	< <= = /= >= > ;; complex
	conjugate realpart imagpart phase
	min max logand logior logxor lognot ffloor fceiling
	ftruncate fround signum cis)
  (:export run-lisp-stat-tests run-lisp-stat-test scoreboard ; exec
	   almost= almost=lists numerical=)) ; compare
  

(defpackage :lisp-stat-data-clos-example
  (:use :common-lisp
	:lift  :lisp-stat-unittests
	:lisp-stat-data-clos))
