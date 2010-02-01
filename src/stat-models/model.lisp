;;; -*- mode: lisp -*-

;;; File:       model.lisp
;;; Time-stamp: <2010-01-25 15:52:30 tony> 
;;; Creation:   <2006-05-17 21:34:07 rossini> 
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2007-- , AJ Rossini.  BSD, LLGPL, or GPLv2, depending
;;;             on how it arrives.
;;; Purpose:    models as a data summarization tools.  Work towards an
;;;             object system with a comprehensive theory of data
;;;             summarization through models.  The basic idea is that
;;;             models are used to summarize different aspects of a
;;;             data generating process, possibly realized by a
;;;             dataset.

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

(in-package :lisp-stat-model)

(defclass model ()
  ((name
    :initform nil
    :initarg :name
    :accessor name
    :reader model-name
    :type string)
   (form
    :initform nil
    :initarg :formula
    :accessor form
    :reader model-formula
    :type list)
   ;; The following might not be all part of the model?!?
   (parameter-vars
    :initform nil
    :initarg :parameter-vars
    :accessor param-vars
    :reader model-formula
    :type list)
   (data-vars
    :initform nil
    :initarg :data-vars
    :accessor data-vars
    :reader model-formula)
   (fixed-vars
    :initform nil
    :initarg :fixed-vars
    :accessor fixed-vars
    :reader model-formula)

   (solution :initform nil
	     :initarg :criteriaFunction
	     :accessor critFcn-vars
	     :reader model-formula)
   (done-solution? :initform nil :reader done-setup?)

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

(defclass result ()
  ((param-values  )
   (param-uncertainity )
   (param-characterization )))

;; The following are types of models -- in particular, we can consider
;; that these models 

(defclass statistical-model (model result) )

(defclass ode-model (model result ))

(defclass regression-model-linear (statistical-mode))

(defclass regression-model-generalized-linear (statistical-model))

(defclass regression-model-nonlinear-linear (statistical-model))

;;;;;;;;;

(defgeneric =model (lhs rhs)
  (:documentation "returns a class describing the model specified")
  (:method ((lhs atom) (rhs list))
	   (if (linear-model rhs)
	       (make-instance 'regression-model-linear
			      lhs
			      rhs)
	     (make-instance 'general-model))))

;;; Centrality
(defgeneric =mean (lhs rhs))
(defgeneric =quantile (lhs rhs quantile))

;;; Variable
(defgeneric =var (lhs rhs)) ; lhs should spec parameter


#| 
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

|#

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

#|

 (with-mapping map :model mymod :data mydata
	      (bootstrap mymod mydata))
|#

;; solution should inherit from model and data (and be recomputable)
;; func args override embedded args
;; 
;;  (solveModel firstSoln) "=" firstSoln
;;
;; unless stoch approx used.

;; 
      






;;; -*- mode: lisp -*-
;;; Copyright (c) 2005--2007, by A.J. Rossini <blindglobe@gmail.com>
;;; See COPYRIGHT file for any additional restrictions (BSD license).
;;; Since 1991, ANSI was finally finished.  Edited for ANSI Common Lisp. 

;;; File:       model-fit.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2007, AJ Rossini.  BSD, LLGPL, or GPLv2, depending
;;;             on how it arrives.
;;; Purpose:    models as a data summarization tools.
;;; Time-stamp: <2006-05-19 12:33:41 rossini> 
;;; Creation:   <2006-05-17 21:34:07 rossini> 

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

;;; The model class is used to map a partial or complete model
;;; specification (partial specification made complete by assumptions
;;; inherent in the type of model used, i.e. linear regression assumes
;;; mean-zero and homoskedasticity) and the model-fit class is used to
;;; complete the model specification into how the model instance and
;;; type will then get computed.  The approach tken is that we
;;; definitiely want to be able to see how we can explicitly try to
;;; characterize the work that we are trying to infere about the data.

(in-package :cl-user)

(defpackage :lisp-stat-model-fit
  (:documentation "Model fitting theory.")
  (:nicknames :ls-model-fit)
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
  (:export fit))

(in-package :lisp-stat-model)

(defclass fit ()
  ((criteria-functin-name :initform nil
	 :initarg :name
	 :accessor name
	 :reader model-name)
   (criteria-function :initform nil
	 :initarg :formula
	 :accessor function
	 :reader fit-criteria)
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

  (:documentation "Mathematical Model Fit approach"))

(defclass optimization (model-fit))


(defclass least-squares (optimization))
(defclass weighted-least-squares (least-squares))

(defclass maximum-likelihood (optimization))
(defclass minimax (optimization))
(defclass maximin (optimization))

(defclass minimum-entropy (optimization))
(defclass lq-norm (optimization))

(defclass root-finding (model-fit))

(defclass method-of-moments (root-finding))
(defclass marginal-models (method-of-moments))
(defclass gee (marginal-models))
(defclass gee2 (marginal-models))



;;; How would this be used?

(setf my-regr-model-1
      (new 'least-squares  '(- y (+ (* beta1 x1) (* beta2 x2)))))

;; and there should be an approach which could provide a mapping to this, i.e. 

(regression-model (list y) (list x1 x2))

;; could map to the above via macros.


;;;;; More misc protos...


(defclass model-specification ()
  ((spec-string :initform nil
		:initarg :specification
		:accessor :specification)
   (spec-form :initform nil
	      :initarg :spec-form
	      :accessor :spec-form)
   (model-class :initform nil))
  (:documentation "container for mathematical structure"))

(defclass bayesian-model-specification (model-specification)
  ((prior-model-class)
   (spec-string :initform nil
		:initarg :specification
		:accessor :specification)
   (spec-form :initform nil
	      :initarg :spec-form
	      :accessor :spec-form))
  (:documentation "adds structure holding priors to the model"))

;;; The following should be self-created based on introspection of
;;; available:
;;; ## inferential technologies (bayesian, frequentist, etc),
;;; ## optimization criteria (likelihood, least-squares, min-entropy,
;;;    minimax, etc) 
;;; ## simplification macros, i.e. mapping directly to linear
;;;    regression and other applications. fast specialized
;;;    algorithms for edge cases and narrow conditions.
;;; ## 

(defparameter *model-class-list*
  '((linear-regression frequentist)
    (generalized-linear-regression  parametric)
    (linear-regression bayesian)
    ()))

;;;;; More mischief from a different time


;; regression-model is the old API, but regression as a generic will
;; be the new API.  We need to distinguish between APIs which enable
;; the user to do clear activities, and APIs which enable developers
;; to do clear extensions and development, and underlying
;; infrastructure to keep everything straight and enabled.

;; There are conflicting theories for how to structure the
;; specification of mathematical models, along with the statistical
;; inference, along with the data which is instantiating the model.
;; 
;; i.e.:  mathematical model for the relationships between components,
;; between a component and a summarizing parameter, and between
;; parameters.
;; 
;; statistical inference describes the general approach for
;; aggregating into a decision and has impliciations for the scale up
;; from the model on a single instance to the generalization.
;;
;; The data represents the particular substantive context that is
;; driving the model/inference combination, and about which we hope to
;; generate knowledge.
;; 
;; numerical analysis selects appropriate algorithms/implementations
;; for combining the above 3.  
;; 
;; the end result is input on the decision being made (which could be
;; specific (decision analysis/testing), risk-analysis (interval
;; estimation) , most likely/appropriate selection (point estimation)
;; 

