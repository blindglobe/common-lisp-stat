;;; -*- mode: lisp -*-

;;; File:       data-clos.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2008, AJ Rossini.  BSD, LLGPL, or GPLv2, depending
;;;             on how it arrives.  
;;; Purpose:    data package for lispstat
;;; Time-stamp: <2008-03-12 17:18:42 user>
;;; Creation:   <2008-03-12 17:18:42 user>

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

;;; This organization and structure is new to the 21st Century
;;; version.

;;; data-clos.lisp
;;; 
;;; redoing regression in a CLOS based framework.
;;; See regression.lsp for basis of work.

(in-package :cl-user)

(defpackage :lisp-stat-data-clos
  (:use :common-lisp
	:clem ;;?? or :matlisp , or :...?
	)
  (:export dataset extract set))

(in-package :lisp-stat-data-clos)

(defclass dataset ()
  ((store :initform (array) :initarg :storage :accessor storage)
   (documentation :initform (list) :initarg :doc :accessor documentation)
   (case-labels :initform (list) :initarg :y :accessor y)
   (var-labels :initform (list) :initarg :y :accessor y))
  (:documentation "Standard Cases by Variables Dataset."))

(defgeneric extract  (dataform what into-form))

(defmethod extract ((ds dataset) what into-form)

