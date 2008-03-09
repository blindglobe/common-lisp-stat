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
	:lisp-stat-matrix
	:lisp-stat-linalg)
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
