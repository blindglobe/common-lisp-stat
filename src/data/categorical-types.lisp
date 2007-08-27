;;; -*- mode: lisp -*-
;;;
;;; Copyright (c) 2007, by A.J. Rossini <blindglobe@gmail.com>
;;; See COPYRIGHT file for any additional restrictions (BSD license).

;;;; categorical types -- statistical typing of data.

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.


;;;;
;;;; Package Setup
;;;;

(defpackage :lisp-stat-data-categorical
  (:use :common-lisp)
  (:export derived-statistical-type ; construct particular type
	   nominal-type ordinal-type  ; types
	   levels  ; metadata
	   categorical->integer ; conversion
	   conformant-p ; type checking
	   ))

(in-package :lisp-stat-data-frame)

;;; Typing for categorical data.

