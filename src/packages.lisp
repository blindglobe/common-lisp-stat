;;; -*- mode: lisp -*-

;;; Time-stamp: <2012-11-06 08:26:01 tony>
;;; Creation:   <2008-03-11 19:18:34 user> 
;;; File:       packages.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2007--2010, AJ Rossini.  MIT License.  See
;;;             LICENSE.mit in top level directory for details. 
;;; Purpose:    packages for Common Lisp Statistics

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

(in-package :cl-user)

;;; Current structure, dependencies:
#|
 '((:ls-user       :depends-on   lisp-stat)
   (:lisp-stat     :depends-on '(cls-dataframe
                                cls-data
			        lisp-matrix
			        ))
   (:cls-data      :depends-on  )
   (:cls-dataframe :depends-on   cls-data)
   (:cls-dataio    : )
   (:cls-datatrans : )
|#	     
	                 
;;; Basics

(defpackage :lisp-stat-config
  (:documentation "holds Common Lisp Statistics configuration/loading variables and related functions.")
  (:nicknames :cls-config)
  (:use :common-lisp)
  (:export *common-lisp-stat-version* 
	   *default-path*
	   *lsos-files* *basic-files* *ls-files*
	   *cls-installation-home-dir* *cls-data-dir* *cls-examples-dir*))

(defpackage :lisp-stat-object-system
  (:nicknames :ls-objects :lsos)
  (:use :common-lisp)
  (:shadow :call-method :call-next-method)
  (:export ls-object objectp *object* kind-of-p make-object
	   *message-hook*
	   *set-slot-hook* proto-slot-value self 
	   send call-next-method call-method
	   defmeth defproto instance-slots proto-name))

;;; -types and -float probably ought to be moved into a -numerics
;;; package.

(defpackage :lisp-stat-types
  (:documentation "Provides some typeing for LispStat, but is clearly
  a bit incomplete.")
  (:use :common-lisp)
  (:export fixnump
	   check-nonneg-fixnum check-one-nonneg-fixnum
	   check-one-fixnum check-one-real check-one-number))

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

;;; Probably should move into cls-data package.

(defpackage :lisp-stat-compound-data
  (:use :common-lisp
	:lisp-stat-object-system
	:lisp-stat-types)
  (:shadowing-import-from :lisp-stat-object-system
			  call-next-method call-method)
  (:export compound-data-p *compound-data-proto*
	   compound-object-p
	   compound-data-seq compound-data-length
	   element-list element-seq
	   sort-data order rank
	   recursive-map-elements map-elements repeat
	   check-sequence
	   get-next-element make-next-element set-next-element
	   ;; sequencep
	   iseq ordered-nneg-seq
	   select split-list which
	   difference rseq
	   flatten-list))

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
			  call-method call-next-method)
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

(defpackage :lisp-stat-macros
  (:use :common-lisp
	:lisp-stat-compound-data)
  (:export make-rv-function make-rv-function-1))

;;;;;;;; NEw CLS structure

(defpackage :cls-matrix
  (:documentation "basic utilities for using lisp arrays as numerical
  matrices.  Not optimized, and must consider this slow.  Routines
  should be optimized, it is only that we need them first, optimize
  them later.")
  (:use :common-lisp)
  (:export matrixp num-rows num-cols matmult identity-matrix diagonal
	   row-list column-list inner-product outer-product
	   cross-product transpose bind-columns bind-rows
	   array-data-vector vector-to-array))

;;; NEW CLOS STRUCTURE

;; CLS-DATA contains the basic variable structure, classes, support/indexing, and mixins.
;; CLS-DATAFRAME leverages CLS-DATA, XARRAY, and LISP-MATRIX 
;; CLS-DATAIO stores/saves structures
;; CLS-DATATRANS converts between structures, and from DATAFRAMES to MODEL-MATRIXES

(defpackage :cls-data
  (:use :common-lisp)
  (:shadowing-import-from :xarray slice)
  (:export
   ;; must work out what is needed for stat variable metadata, support
   ;; functions, etc
   ))

;;; cls-data... in dataframe, though.
(defpackage :cls-dataframe
  (:use :common-lisp
	:cls-data
	:xarray
	:lisp-matrix)
  (:shadowing-import-from :xarray slice)
  (:export
   ;; generic container class for data -- if small enough
   ;; could be value, otherwise might be reference.
   dataframe-like
   dataframe-array
   make-dataframe

   dataset
   list-of-columns ;; list-of-variables
   list-of-rows ;; list-of-observations

   ;; accessors
   varlabels caselabels  nrows ncols
   dataframe-dimension dataframe-dimensons
   xref xtype xdims xdim xrank slice take carray
 
   ;; support
   make-labels))

(defpackage :cls-dataio
  (:documentation "Data I/O and similar import technologies.")
  (:use :common-lisp
	:fare-csv
	:lisp-stat-object-system
	:cls-data
	:cls-dataframe)
  (:shadowing-import-from :lisp-stat-object-system
			  call-method call-next-method)
  (:export dsvstream->dataframe
	   dsvstream->matrix
	   dsvstream->listoflist

	   filename.dsv->dataframe

	   ;; How should we manage the fare-csv symbols on export?
	   ))

(defpackage :cls-datatrans
  (:documentation "Data I/O and similar import technologies.")
  (:use :common-lisp
	:listoflist
	:lisp-stat-object-system
	:cls-data
	:cls-dataframe)
  (:shadowing-import-from :lisp-stat-object-system
			  call-method call-next-method)
  (:export listoflist->dataframe   dataframe->listoflist
	   listoflist->array	   array->listoflist
	   listoflist->matrix-like matrix-like->listoflist))

;;;; MODELING

(defpackage :lisp-stat-model
  (:documentation "Model management for data analysis.")
  (:use :common-lisp
	:lisp-matrix)
  (:export
   ;; data structures for model and model/data combination
   model statistical-model analysis))

;;; visualization

(defpackage :cls-visualize
  (:use :common-lisp
	:lisp-matrix
	:cls-dataframe)
  (:shadowing-import-from :xarray slice)
  )

#|
(defpackage :cls-visualize-plplot
  (:use :common-lisp
	:lisp-matrix
	:cls-dataframe
	:cl-plplot-system)
  (:export
   ;; examples 
   plot-ex contour-plot-ex fn-contour-plot-ex shade-plot-ex 3D-plot-ex))
|#

;;; USER PACKAGES

;; (defpackage :lisp-stat-ffi-int
;;   (:use :common-lisp
;; 	:cffi)
;;   (:export ccl-store-integer ccl-store-double ccl-store-ptr
;; 	   get-buf ))

;; (defpackage :lisp-stat-probability
;;   (:use :common-lisp
;; 	:cffi
;; 	:lisp-stat-ffi-int
;; 	:lisp-stat-macros)
;;   (:export log-gamma set-seed
;; 	   uniform-rand
;; 	   normal-cdf normal-quant normal-dens normal-rand
;; 	   bivnorm-cdf
;; 	   cauchy-cdf cauchy-quant cauchy-dens cauchy-rand
;; 	   gamma-cdf gamma-quant gamma-dens gamma-rand
;; 	   chisq-cdf chisq-quant chisq-dens chisq-rand
;; 	   beta-cdf beta-quant beta-dens beta-rand
;; 	   t-cdf t-quant t-dens t-rand
;; 	   f-cdf f-quant f-dens f-rand
;; 	   poisson-cdf poisson-quant poisson-pmf poisson-rand 
;; 	   binomial-cdf binomial-quant binomial-pmf binomial-rand))

(defpackage :cls-probability
  (:use common-lisp
	cl-variates
	)
  (:export probability-law distribution
	   density-fcn quantile-fcn survivorship-fcn ))

(defpackage :lisp-stat-math
   (:use :common-lisp
	 :lisp-stat-object-system
	 :lisp-stat-macros
	 :lisp-stat-compound-data
	 :lisp-stat-float)
   (:shadowing-import-from :lisp-stat-object-system
			   call-method call-next-method)
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


#| ;; some of this goes back in, but not all of it?
(defpackage :lisp-stat-linalg
  (:use :common-lisp
	:cffi
	:lisp-matrix
	:lisp-stat-math
	:lisp-stat-types
	:lisp-stat-float
	:lisp-stat-compound-data)
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

|#




(defpackage :lisp-stat-data
  (:documentation "Data management, integration, I/O, and other data technologies.")
  (:nicknames :ls-data)
  (:use :common-lisp
	:lisp-stat-object-system
	:lisp-stat-config
	:lisp-stat-types
	:lisp-stat-compound-data)
  (:shadowing-import-from :lisp-stat-object-system
			  call-method call-next-method)
  (:export
   ;; generic structures
   ;; Variables
   empirical-statistical-variable
   modelbased-statistical-variable
   categorical-statistical-variable
   nominal-statistical-variable
   ordinal-statistical-variable
   continuous-statistical-variable
   
   ordering factor-levels nobs support pdmf draw
   print-object

   ;; Observations
   statistical-observation
   measurement-types record
   ;; XLS compat tools
   open-file-dialog read-data-file read-data-columns load-data
	   load-example *variables* *ask-on-redefine*
	   def variables savevar undef))

(defpackage :lisp-stat-descriptive-statistics
 (:use :common-lisp
       :lisp-matrix
       :lisp-stat-data
       :lisp-stat-math
       :lisp-stat-compound-data
       :lisp-stat-basics)
   (:shadowing-import-from :lisp-stat-math ;; life is a vector!
      expt + - * / ** mod rem abs 1+ 1- log exp sqrt sin cos tan
      asin acos atan sinh cosh tanh asinh acosh atanh float random
      truncate floor ceiling round minusp zerop plusp evenp oddp 
      < <= = /= >= > ;; complex 
      conjugate realpart imagpart phase
      min max logand logior logxor lognot ffloor fceiling
      ftruncate fround signum cis)
   (:export mean standard-deviation variance
	    quantile median interquartile-range
	    fivnum sample))

(defpackage :lisp-stat-regression-linear
  (:use :common-lisp
	:lisp-matrix
	:lisp-stat-basics
	:lisp-stat-compound-data
	:lisp-stat-descriptive-statistics )
  (:shadowing-import-from :lisp-stat-object-system
			  call-method call-next-method)
  (:export regression-model fit-model

	   estimates covariance-matrix
	   ;; functions for helpers
	   lm xtxinv   
	   print-object ;; for method dispatch
	   ))

(defpackage :common-lisp-statistics
  (:documentation "Experimentation package for LispStat.  Serious work
    should be packaged up as a separate but similar package to help
    drive reproducibility.  By this I mean, creating a
    data/analytics/analysis package with the minimal set of
    objects/packages required.")
  (:nicknames :cls :common-lisp-statistics :lisp-stat)
  (:use :common-lisp

	;; extern packages
	:xarray ;; generic reference -- internally supporting array, lol structs
	:listoflist
	:lisp-matrix ;; conversion to a more robust linalg approach

	;; intern packages
	:lisp-stat-config
	:lisp-stat-object-system
	:lisp-stat-compound-data
	;;	:lisp-stat-probability
	:lisp-stat-types
        :lisp-stat-float
	:lisp-stat-basics
	:lisp-stat-data

	:cls-data
	:cls-dataframe
	:cls-dataio
	:cls-datatrans

        :lisp-stat-math
	:lisp-stat-descriptive-statistics
	:lisp-stat-regression-linear
	:cls-visualize
	;; :cls-visualize-plplot
	;; :cls-visualize-cl2d
	)
  (:shadowing-import-from :xarray slice)
  (:shadowing-import-from :lisp-stat-object-system
			  call-method call-next-method)
  (:shadowing-import-from :lisp-stat-math
	expt + - * / ** mod rem abs 1+ 1- log exp sqrt sin cos tan
	asin acos atan sinh cosh tanh asinh acosh atanh float random
	truncate floor ceiling round minusp zerop plusp evenp oddp 
	< <= = /= >= >
	;;complex 
	conjugate realpart imagpart phase
	
	min max
	logand logior logxor lognot
	ffloor fceiling	ftruncate fround
	signum cis)
  (:export
   ;; lisp-stat-config:
   *default-path* *lsos-files* *basic-files* *ls-files*
   *cls-installation-home-dir* *cls-data-dir* *cls-examples-dir*  

   ;; lsobjects :
   defproto defproto2
   defmeth send proto-slot-value
  
   ;; lstypes :
   fixnump check-nonneg-fixnum check-one-fixnum
   check-one-nonneg-fixnum
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
   ;; sequencep
   iseq
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

#| ;; The following need to be re-found in lisp-matrix...

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

|#

   ;; optimize.lsp
;;   newtonmax nelmeadmax

   ;; package LISPSTAT-MACROS
   make-rv-function make-rv-function-1 

   ;; package XARRAY
   xref xtype xdims xdim xdims* 

   ;; package LISTOFLIST
   sublists-of-same-size-p equal-listoflist transpose-listoflist

   ;; package DATA
   ;; need to take this list and make it symbols...  specs could mean
   ;; that we process the strings in different ways?  Create such a
   ;; macro to enable export within this package?
#|
   (let ((lst ()))
     (unlist
       (mapc #'symbol-for-symbol-to-string-or-symbol
	 (do-external-symbols (s (find-package 'lisp-stat-data) lst) (push s lst))))
     lst)
|#
   open-file-dialog read-data-file read-data-columns load-data
   load-example *variables* *ask-on-redefine*
   def variables savevar undef
   ;; dataframe
   dataframe-like dataframe-array make-dataframe
   varlabels caselabels nrows ncols
   dataframe-dimension dataframe-dimensons
   dfref dfref-case dfref-var 
   consistent-dataframe-p
   dataset list-of-columns list-of-rows
   make-labels
   
   ;; listoflist
   listoflist->dataframe listoflist->array listoflist->matrix-like
   sublists-of-same-size-p 

   ;; cls-dataio
   dsvstream->dataframe dsvstream->matrix dsvstream->listoflist
   filename.dsv->dataframe

   ;; cls-datatrans
   listoflist->dataframe   dataframe->listoflist
   listoflist->array	   array->listoflist
   listoflist->matrix-like matrix-like->listoflist


   ;; statistics.lsp  (descriptions, should probably be moved
   ;; later...?
   standard-deviation quantile median interquartile-range
   fivnum sample

   ;; probability (dists.lisp)
   ;; log-gamma set-seed
   ;; uniform-rand normal-cdf normal-quant normal-dens
   ;; normal-rand bivnorm-cdf cauchy-cdf cauchy-quant cauchy-dens
   ;; cauchy-rand gamma-cdf gamma-quant gamma-dens gamma-rand
   ;; chisq-cdf chisq-quant chisq-dens chisq-rand beta-cdf beta-quant
   ;; beta-dens beta-rand t-cdf t-quant t-dens t-rand f-cdf f-quant
   ;; f-dens f-rand poisson-cdf poisson-quant poisson-pmf poisson-rand 
   ;; binomial-cdf binomial-quant binomial-pmf binomial-rand

   ;; Here is where we have a problem -- lispstat core should be core
   ;; data management and config problems, with packages providing
   ;; specialized extensions to LispStat, i.e. regression, nonlin
   ;; regression, bayesian regression via laplace approximation, etc. 
   
   ;; The following could be considered "recommended packages",
   ;; similar to the idea of the recommended packages in R.  Probably
   ;; we want them to do the exporting within that package, therefore
   ;; NOT being able to lock the "data-ish" package, but only the
   ;; subpackages prior to export.

   ;; regression.lsp
   ;; -- linear regressin models.
   regression-model fit-model
   estimates covariance-matrix

   regression-model-proto x y intercept sweep-matrix
   basis weights included total-sum-of-squares residual-sum-of-squares
   predictor-names response-name case-labels
   lm xtxinv

   ;; nonlin.lsp
   ;; -- nonlinear regression models
   nreg-model nreg-model-proto mean-function theta-hat epsilon
   count-limit verbose
   ;; we might need something like xtxinv here?  But should be
   ;; encapsulated, so we use the one in regression.lisp

   ;; bayes.lsp
   bayes-model bayes-model-proto bayes-internals

   ;; plots.lisp
   plot-ex
   contour-plot-ex
   fn-contour-plot-ex
   shade-plot-ex
   3D-plot-ex 
   
   ))


;;;; PACKAGES FOR USEABILITY

(defpackage :lisp-stat-data-examples
  (:documentation "Example data for unittests, examples, illustrations,")
  (:use :common-lisp
	:common-lisp-statistics)
  (:shadowing-import-from :lisp-stat
			  call-method call-next-method

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
  (:documentation "Experimentation package for LispStat.  Serious work
    should be placed in a similar package elsewhere for
    reproducibility.  But this should hint as to what needs to be done
    for a user- or analysis-package.")
  (:nicknames :ls-user :cls-user)
  (:use :common-lisp ; always needed for user playgrounds!
	:lisp-matrix
	:common-lisp-statistics
	:lisp-stat-data-examples) ;; this last is to have 'things to play with'
  (:shadowing-import-from :lisp-stat
			  call-method call-next-method

      expt + - * / ** mod rem abs 1+ 1- log exp sqrt sin cos tan
      asin acos atan sinh cosh tanh asinh acosh atanh float random
      truncate floor ceiling round minusp zerop plusp evenp oddp 
      < <= = /= >= > > ;; complex
      conjugate realpart imagpart phase
      min max logand logior logxor lognot ffloor fceiling
      ftruncate fround signum cis

      <= float imagpart))

(defpackage :lisp-stat-unittests
  (:use :common-lisp
	:lift :lisp-matrix
	:lisp-stat :lisp-stat-data-examples)
  (:shadowing-import-from :lisp-stat
			  call-method call-next-method ;; objects
	expt + - * / ** mod rem abs 1+ 1- log exp sqrt sin cos tan ;; lsmath
	asin acos atan sinh cosh tanh asinh acosh atanh float random
	truncate floor ceiling round minusp zerop plusp evenp oddp 
	< <= = /= >= > ;; complex
	conjugate realpart imagpart phase
	min max logand logior logxor lognot ffloor fceiling
	ftruncate fround signum cis)
  (:export run-lisp-stat-tests run-lisp-stat-test scoreboard ; exec
	   almost= almost=lists numerical=)) ; compare

(defpackage :cls-dataframe-example
  (:use :common-lisp
	:lift  :lisp-stat-unittests
	:lisp-stat-data-examples
	:cls-dataframe)
  (:export absorbtion aluminum iron))


;; (defpackage :lisp-stat-optimize
;;  (:use :common-lisp
;;        :cffi
;;        :lisp-matrix
;;        :lisp-stat-ffi-int
;;        :lisp-stat-object-system
;;        :lisp-stat-types
;;        :lisp-stat-compound-data
;;        :lisp-stat-math
;;        :lisp-stat-float
;;        :lisp-stat-basics
;; #|
;;        :lisp-stat-matrix
;;        :lisp-stat-linalg-data
;;        :lisp-stat-linalg
;; |#
;;        )
;;  (:shadowing-import-from :lisp-stat-object-system
;; 			 call-method call-next-method)
;;  (:shadowing-import-from :lisp-stat-math
;; 	   expt + - * / ** mod rem abs 1+ 1- log exp sqrt sin cos tan
;;  	   asin acos atan sinh cosh tanh asinh acosh atanh float random
;;  	   truncate floor ceiling round minusp zerop plusp evenp oddp 
;;  	   < <= = /= >= > complex conjugate realpart imagpart phase
;;  	   min max logand logior logxor lognot ffloor fceiling
;;  	   ftruncate fround signum cis)
;;  (:export
;;      ;; derivatives
;;      numgrad numhess

;;      ;; optimization
;;      newtonmax nelmeadmax))
