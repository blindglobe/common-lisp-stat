;;; -*- mode: lisp -*-

;;; Time-stamp: <2009-10-09 12:17:47 tony>
;;; Creation:   <2009-03-12 17:14:56 tony>
;;; File:       procedures.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2009--, AJ Rossini.  Currently licensed under MIT
;;;             license.  See file LICENSE.mit in top-level directory
;;;             for information.
;;; Purpose:    Classes for statistical procedures, and generics
;;;             supporting use of such procedures.

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

;;; This organization and structure is new to the 21st Century
;;; version..   Think, "21st Century Schizoid Man".

(in-package :cls-statproc)

;;; Statistical procedures can consist of one or more of:

;;; - mathematical forms: linear predictors, splines, functional
;;;   forms) which get spliced together.

;;; - criteria functions (likelihoods, sum-of-squares, information), params and data.
;;; - optimization algorithmsfunctions

;;; - root functions (score functions, influence functions), params and data
;;; - zero-finding algorithms

;;; - point estimate: special case of...
;;; - interval estimation (with uncertainty criteria to support range or point)
;;; - hypothesis test -- which can be thought of as a point estimator
;;;   for a coarsened problem, or as a simple interval-based decision with
;;;   uncertanty criteria.

;;; Estimators are reaasonably straightforward.  Either we are
;;; providing a point estimate, or we provide an interval estimate.
;;; But shouldn't these be characterized in the same manner?

;;; We are currently building up an ontology (ADG/DAG) or
;;; knowledge-base whose leaves are described by the path to them.

(defvar *statistical-procedure-components*
  '((interval-estimation

     (hypothesis-testing ; coarsened interval
      (frequentist
       (fisherian
	(neyman-pearson
	 (wald-test
	  t-test
	  likelihood-ratio-test
	  score-test))))))
    (point-estimation
     maximum-likelihood-estimation)

    ()
    optimization
    root-finding

    criteria-functions
    
    mathematical-forms
    ))

(defclass statistical-ontology-class ()
  ()
  (:documentation "container class for manipulation of structural components."))

(defclass statistical-decision ()
  ((ontology-classification :initform nil
			    :initarg :ontology-classification
			    :type statistical-ontology-class
			    ))
  (:documentation "instance describing the end result, if it is an
  interval/range/region, or point estimate, or a conclusion from a
  test (i.e. hypothesis(es) selected, strength of conclusion)"))

(defclass statistical-dataset (dataframe-like)
  (:documentation "a particular dataset, usually the subset from a
  larger set, which is used as the input to the procedure."))

(defclass statistical-metadata (dataframe-like)
  (:documentation "the description of the dataset's statistical
  properties which are required for the procedure to work or meet
  assumptions."))

(defclass statistical-procedure ()
  ((ontological-spec :initform nil
		     :initarg :ontology-def
		     :type list
		     :accessor ontology
		     :documentation "list of symbols describing
  ontological classification")
   (how-to-fit)
   (how-to-simulate)
   (instance-data)))

;;; 
(defgeneric proc-consistents-data-p (metadata data)
  (:documentation "verify that the metadata required for a procedure
  is present in a particular dataset.  The dataset will usually be a
  subset of the full working dataset."))

(defgeneric process-data (proc data)
  (:documentation "Run the statistical procedure on the dataset and
  report the decision."))