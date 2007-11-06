;;; -*- mode: lisp -*-
;;; Copyright (c) 2005--2007, by A.J. Rossini <blindglobe@gmail.com>
;;; See COPYRIGHT file for any additional restrictions (BSD license).
;;; Since 1991, ANSI was finally finished.  Edited for ANSI Common Lisp. 

(defpackage :lisp-stat
    (:documentation "Experimentation package for LispStat.  Serious work
should be packaged up elsewhere for reproducibility.")
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
	< <= = /= >= > complex conjugate realpart imagpart phase
	min max logand logior logxor lognot ffloor fceiling
	ftruncate fround signum cis)
  (:export

   ;; lsobjects :
   defproto defmeth send 
  
   ;; lstypes
   fixnump check-nonneg-fixnum check-one-fixnum
   check-one-real check-one-number

   ;; lsmacros
   
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
   /= >= > complex conjugate realpart imagpart phase min max
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


   ;; statistics.lsp
   open-file-dialog read-data-file read-data-columns load-data 
   load-example *variables* *ask-on-redefine* def variables savevar
   undef standard-deviation quantile median interquartile-range
   fivnum covariance-matrix difference rseq matrix print-matrix solve
   backsolve eigenvalues eigenvectors accumulate cumsum combine
   lowess

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

(defpackage :lisp-stat-user
  (:documentation "Experimentation package for LispStat.
Serious work should be placed in a similar package elsewhere for
reproducibility.  But this should hint as to what needs to be
done for a user- or analysis-package.")
  (:nicknames :ls-user)
  (:use :common-lisp
	:lisp-stat)
  (:shadowing-import-from :lisp-stat
      slot-value call-method call-next-method

      expt + - * / ** mod rem abs 1+ 1- log exp sqrt sin cos tan
      asin acos atan sinh cosh tanh asinh acosh atanh float random
      truncate floor ceiling round minusp zerop plusp evenp oddp 
      < <= = /= >= > complex conjugate realpart imagpart phase
      min max logand logior logxor lognot ffloor fceiling
      ftruncate fround signum cis

      <= float imagpart))
