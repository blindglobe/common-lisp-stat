;;; -*- mode: lisp -*-
;;; Copyright (c) 2005--2007, by A.J. Rossini <blindglobe@gmail.com>
;;; See COPYRIGHT file for any additional restrictions (BSD license).
;;; Since 1991, ANSI was finally finished.  Edited for ANSI Common Lisp. 

;;; File:       model.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2007, AJ Rossini.  BSD, LLGPL, or GPLv2, depending
;;;             on how it arrives.
;;; Purpose:    models as a data summarization tools.
;;; Time-stamp: <2006-05-19 12:33:41 rossini> 
;;; Creation:   <2006-05-17 21:34:07 rossini> 

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

;;; Work towards an object system with a comprehensive theory of data
;;; summarization through models.  The basic idea is that models are
;;; used to summarize different aspects of a data generating process,
;;; possibly realized by a dataset.

(in-package :cl-user)

(defpackage :lisp-stat-model
  (:documentation "Model management and other mathematical technologies.")
  (:nicknames :ls-data)
  (:use :common-lisp
	:lisp-stat-config
	:lisp-stat-object-system
	:lisp-stat-types
	:lisp-stat-compound-data
#|	
	:lisp-stat-matrix
	:lisp-stat-linalg
|#
	)
  (:shadowing-import-from :lisp-stat-object-system
			  slot-value call-method call-next-method)
  (:export model))

(in-package :lisp-stat-model)

(defclass model ()
  ((name :initform nil
	 :initarg :name
	 :accessor name
	 :reader model-name)
   (form :initform nil
	 :initarg :formula
	 :accessor form
	 :reader model-formula)
   (parameter-vars :initform nil
		   :initarg :parameter-vars
		   :accessor param-vars
		   :reader model-formula)
   (data-vars :initform nil
	      :initarg :data-vars
	      :accessor data-vars
	      :reader model-formula)
   (fixed-vars :initform nil
	       :initarg :fixed-vars
	       :accessor fixed-vars
	       :reader model-formula)
   (solution :initform nil
	     :initarg :criteriaFunction
	     :accessor critFcn-vars
	     :reader model-formula)

   (done-solution? :initform nil :reader done-setup?)
   (prototypes-initialized? :initform nil :reader prototypes-initialized?)
   (current-values :initform nil :accessor current-values)

   (log-file :initform nil :initarg :log-file :reader log-file)
   (test-data :initform nil :accessor test-data)
   (expected-failure-p :initform nil :initarg :expected-failure-p
		       :reader expected-failure-p)
   (expected-error-p :initform nil :initarg :expected-error-p
		     :reader expected-error-p)
   (expected-problem-p :initform nil :initarg :expected-problem-p
		       :reader expected-problem-p))
  (:documentation "Mathematical Model"))

(defclass result (model)
  ((param-values  )
   (param-uncertainity )
   (param-characterization ))

;; The following are types of models -- in particular, we can consider
;; that these models 

(defclass statistical-model (model) )

(defclass ode-model (model ))

(defclass linear-regression-model (statistical-mode))

(defclass generalized-linear-regression-model (statistical-model))

(defclass nonlinear-linear-regression-model (statistical-model))




;;; garbage follows


;; modelType

root
opt (min / max)

diffeqn

diffeqn + solve

;; dataType

varExtract
tableExtract 
relationExtract


;; mappingType

(setf myFirstModel
      (defineModel normalLikelihood '((* (/ 1 (sqrt (* 2 (pi) sigma)))
					 (exp (/ (- x mu)
						 sigma))))
	:fixed '()
	:param '(mu sigma)
	:data '(x)
	:critFcnSoln '(:max (one-of bfgs nelder-mead conjugate-gradient))))

(defclass model ()
  ((name )
   (vars-fixed :initarg nil :arg fixed)
   (vars-param :initarg nil :arg param)
   (vars-data :initarg nil :arg data)
   (critFcn   ))

(de

   


(defclass meanModel (model) ... ) ;; a macro to map onto Model
(defclass meanVarModel (model)  ... )
(defclass regressionModel (meanModel) )
(defclass mixedModel (regressionModel) ... )
(defclass bayesianModel (model)  ... )
(defclass diffintgleqnModel (model) ) ;;(ODE, PDF, integral equations)



(setf modX      
      (solveModel :model myFirstModel
		  :data myVar 
		  :mapping '((x . myVar))))

;; result structure

(:solution
 :params
 :params-characterisation
 :paradigm '(bayesian frequentist)
 :dataname
 :modelname )


(with-mapping map :model mymod :data mydata
	      (bootstrap mymod mydata))


;; solution should inherit from model and data (and be recomputable)
;; func args override embedded args
;; 
;;  (solveModel firstSoln) "=" firstSoln
;;
;; unless stoch approx used.

;; 

(defclass       
      