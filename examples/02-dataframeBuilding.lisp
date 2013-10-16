;;; -*- mode: lisp -*-

;;; Time-stamp: <2013-10-15 12:28:05 tony>
;;; Creation:   <2012-11-02 08:14:38 tony>
;;; File:       02-DSVloading.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2012--2013, AJ Rossini.  Licensed under MIT
;;;             license.  See file LICENSE.mit in top-level directory
;;;             for information.
;;; Purpose:    Example of loading DSV files

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

;;; This organization and structure is new to the 21st Century
;;; version..   Think, "21st Century Schizoid Man".



;;;; BROKEN unless you are running your lisp in the CLS home
;;;; directory.  Until we fix this, might have to edit so that you
;;;; load from the right place.
(load "examples/00-loadingData.lisp")

(in-package :cls-examples)


(progn
  (defparameter *chickwts-column-types* 	(list 'integer 'number 'string))
  (defparameter *chickwts-array* (filename.dsv->array3 (localized-pathto "Data/R-chickwts.csv")
					       *chickwts-column-types*))
  ;; *chickwts-df*
  (xref *chickwts-array* 1 1) ; => 160
  (xref *chickwts-array* 40 2) ; => "sunflower"
  *chickwts-array*)

;; we now have an array, whose columns have associated types, but this
;; typing isn't formally controlled - it was only set on loading.
*chickwts-array*

;;; Now to make a dataframe from this.  
(defparameter *chickwts-df*
  (make-dataframe *chickwts-array*
		  :vartypes *chickwts-column-types*
		  :varlabels (list "id" "weight" "feedtype")))
