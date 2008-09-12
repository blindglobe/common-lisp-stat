;;; -*- mode: lisp -*-

;;; File:       likelihood-clos.lisp
;;; Time-stamp: <2008-09-05 18:56:59 tony>
;;; Creation:   <2008-03-11 19:18:34 user> 
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2008, AJ Rossini.  BSD, LLGPL, or GPLv2, depending
;;;             on how it arrives.  
;;; Purpose:    likelihood package for lispstat

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

;;; This organization and structure is new to the 21st Century
;;; version.

;;; likelihood-clos.lisp
;;; 
;;; redoing likelihood computations in a CLOS based framework.

(in-package :cl-user)

(defpackage :lisp-stat-likelihood-clos
  (:use :common-lisp
	:lisp-stat-optimize)
  (:export likelihood-model ;; data structure
	   maximize-likelihood ;; optimize
	   report)) ;; tell about what we have

(in-package :lisp-stat-regression-linear-clos)


(defclass likelihood-model (statistical-model)
  ((function :initform nil
	     :initarg :function :accessor function)
   (estimable-vars :initform nil
		   :initarg :est-vars :accessor est-vars)
   (data-vars :initform nil
		    :initarg :data-vars :accessor data-vars)
   (description :initform "" :initarg :descr :accessor descr)
   (needs-computing :initform T :initarg :compute? :accessor compute?)
  (:documentation "Likelihood Model through CLOS.")))

(defclass prior (statistical-model)
  ((function :initform nil
	     :initarg :function :accessor function)
   (estimable-vars :initform nil
		   :initarg :est-vars :accessor est-vars)
   (data-vars :initform nil
		    :initarg :data-vars :accessor data-vars)
   (description :initform "" :initarg :descr :accessor descr)
   (needs-computing :initform T :initarg :compute? :accessor compute?)
  (:documentation "Likelihood Model through CLOS.")))

(defclass inference-framework (statistical-model)
  ((inference-type :initform nil
		   :initarg :inf-type :accessor inf-type)
   (priors :initform nil)
   (description :initform nil :initarg :descr :accessor descr))
  (:documentation "inference framework, including priors"))

(defgeneric maximize-likelihood (likelihood inference-framework &optional prior))

(defgeneric maximize-likelihood (likelihood prior inference-framework))


