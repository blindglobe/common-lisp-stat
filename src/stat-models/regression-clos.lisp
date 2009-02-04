;;; -*- mode: lisp -*-

;;; Time-stamp: <2009-01-31 14:20:14 tony>
;;; Creation:   <2008-10-03 02:07:10 tony>
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2007, AJ Rossini.  BSD, LLGPL, or GPLv2, depending
;;;             on how it arrives.  
;;; Purpose:    redoing regression in a CLOS based framework.  See
;;;             regression.lsp for basis of work.  

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

;;; This organization and structure is new to the 21st Century
;;; version.

(in-package :lisp-stat-regression-linear-clos)

;;; Regresion Model CLOS, data structures

(defclass regression-model-clos (statistical-model)
  ((x :initform nil :initarg :x :accessor x)
   (y :initform nil :initarg :y :accessor y)
   (included :initform nil :initarg :y :accessor y)
   (total-sum-of-squares :initform nil :initarg :y :accessor y)
   (residual-sum-of-squares :initform nil :initarg :y :accessor y)
   (predictor-names :initform nil :initarg :y :accessor y)
   (response-name :initform nil :initarg :y :accessor y)
   (case-labels :initform nil :initarg :y :accessor y)
   (needs-computing :initform T :initarg :compute? :accessor compute?))
  (:documentation "Normal Linear Regression Model through CLOS.
  Historical design based on what was done for LispStat, not modern."))

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



;;; Regression model generics and methods

(defgeneric regression-model (model-spec data-pointer &key debug)
  (:documentation "CLOSy framework for regression, using numerics from "))

(defmethod regression-model
    ((regr-inst regression-model-clos)
     (data-ptr  data-pointer)
     &key debug)
  "Args: (regr-inst regressino-model-clos)

Returns a fitted regression model object. To examine the model further
assign the result to a variable and send it messages.  Example (data
are in file absorbtion.lsp in the sample data directory/folder):

 (def fit-m (fit (new 'regression-model-clos (list iron aluminum) absorbtion)))
 (print fit-m)
 (plot  fit-m :feature 'residuals)
"
  (let ((x (get-variable-matrix (x regr-inst) data-ptr))
	(y (get-variable-vector (y regr-inst) data-ptr)))



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

#|
 (defclass model ()
  ((type structure)))

 (defgeneric regression ;; assumes x/y from lisp-matrix -- start of a set of generics.
    (model dataset)
  "Args: (x y &key (intercept T) (print T) (weights nil)
          included predictor-names response-name case-labels)
X           - list of independent variables or X matrix
Y           - dependent variable.
INTERCEPT   - T to include (default), NIL for no intercept
PRINT       - if not NIL print summary information
WEIGHTS     - if supplied should be the same length as Y; error
              variances are  
              assumed to be inversely proportional to WEIGHTS
PREDICTOR-NAMES, RESPONSE-NAME, CASE-LABELS
            - sequences of strings or symbols.
INCLUDED    - if supplied should be the same length as Y, with
 	      elements nil to skip a in computing estimates (but not
              in residual analysis).
Returns a regression model object. To examine the model further assign the
result to a variable and send it messages.
Example (data are in file absorbtion.lsp in the sample data directory): 
  (def m (regression-model (list iron aluminum) absorbtion))
  (send m :help) (send m :plot-residuals)"
  (let ((m (send regression-model-proto :new)))
    (format t "~%")
    (send m :doc doc)
    (send m :x x)
    (send m :y y)
    (send m :intercept intercept)
    (send m :weights weights)
    (send m :included included)
    (send m :predictor-names predictor-names)
    (send m :response-name response-name)
    (send m :case-labels case-labels)
    (if debug
	(progn
	  (format t "~%")
	  (format t "~S~%" (send m :doc))
	  (format t "X: ~S~%" (send m :x))
	  (format t "Y: ~S~%" (send m :y))))
    (if print (send m :display))
    m))
|#
