;;; -*- mode: lisp -*-
;;;
;;; Copyright (c) 2007, by A.J. Rossini <blindglobe@gmail.com>
;;; See COPYRIGHT file for any additional restrictions (BSD license).

;;;; datatable -- i.e. data.frame for CL.  Extends CLEM 

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.


;;;;
;;;; Package Setup
;;;;

(defpackage :lisp-stat-data-frame
  (:use :common-lisp
	:clem)
  (:export data-frame-p rows cols 
	   row-list column-list 
	   transpose
	   bind-columns bind-rows
	   array-data-vector vector-to-array))

(in-package :lisp-stat-data-frame)


;;; The goal with this data structure is to be able to extract CLEM
;;; types for statistical computations from a more general data type.
;;; The primary difference is that we need a means for ensuring column
;;; "sameness" and this is absolutely critical. 

