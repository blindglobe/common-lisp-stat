;;; -*- mode: lisp -*-
;;; Copyright (c) 2005--2007, by A.J. Rossini <blindglobe@gmail.com>
;;; See COPYRIGHT file for any additional restrictions (BSD license).
;;; Since 1991, ANSI was finally finished.  Edited for ANSI Common Lisp. 

;;; File:       model.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2007, AJ Rossini.  BSD, LLGPL, or GPLv2, depending on how it arrives. 
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
	:lisp-stat-matrix
	:lisp-stat-linalg)
  (:shadowing-import-from :lisp-stat-object-system
			  slot-value call-method call-next-method)
  (:export model))

(in-package :lisp-stat-model)

