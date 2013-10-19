;;; -*- mode: lisp -*-

;;; Time-stamp: <2013-10-18 14:03:46 tony>
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



;;; This file provides examples of building composite data structures
;;; and using them.  An R dataframe is just one example of a
;;; table-like composite structure, though clearly there are other
;;; examples, such as networks as well as semi-structured datasets.



;;;; BROKEN unless you are running your lisp in the CLS home
;;;; directory.  Until we fix this, might have to edit so that you
;;;; load from the right place.
(load "examples/00-loadingData.lisp")

(in-package :cls-examples)

(progn
  (defparameter *chickwts-column-types*
    (list 'integer 'number 'string))
  (defparameter *chickwts-array*
    (filename.dsv->array3
     (localized-pathto "Data/R-chickwts.csv")
     *chickwts-column-types*))
  ;; *chickwts-df*
  (xref *chickwts-array* 1 1) ; => 160
  (xref *chickwts-array* 40 2) ; => "sunflower"
  *chickwts-array*)

;; we now have an array, whose columns have associated types, but this
;; typing is not yet formally controlled - it was only set on loading.
*chickwts-array*

;;; Now to make a dataframe from this.  
(defparameter *chickwts-df*
  (make-dataframe *chickwts-array*
		  :vartypes *chickwts-column-types*
		  :varlabels (list "id" "weight" "feedtype")))

*chickwts-df*

(eql
 (xref *chickwts-df* 1 1)
 (xref *chickwts-array* 1 1))

(equalp 
 (xref *chickwts-df* 0 1)
 (xref *chickwts-array* 0 1))

(equalp 
 (xref *chickwts-df* 0 2)
 (xref *chickwts-array* 0 2))


;;  There are two initial forms for most datasets which are in
;;  cases-by-variables format -- listoflist structure and lisp
;;  arrays.

(defparameter *ex-lol* '((11d0 12d0 13d0 14d0)
			 (21d0 22d0 23d0 24d0)
			 (31d0 32d0 33d0 34d0)))

(defparameter *ex-array* #2A((11d0 12d0 13d0 14d0)
			   (21d0 22d0 23d0 24d0)
			   (31d0 32d0 33d0 34d0)))


(defparameter *ex-lol-array* (listoflist:listoflist->array  *ex-lol*))

;;; Matrices

(defparameter *ex-lol-mat*
  (make-matrix 3 4 :initial-contents *ex-lol*))
(defparameter *ex-array-mat*
  (make-matrix 3 4 :initial-contents *ex-array*))

;;; Dataframes: 3, all the same

(defparameter *ex-array-df*
  (make-dataframe *ex-array*))
(defparameter *ex-lol-array-df*
  (make-dataframe (listoflist:listoflist->array  *ex-lol*)))
(defparameter *ex-lol-df* (make-dataframe *ex-lol*))

;;; Need examples with variables, perhaps case labels.

;;; "make-dataframe2" is the generic and hopefully future looking
;;; version of this -- FIXME: but it is not yet written, let alone
;;; exported!
;; FIXME: (make-dataframe2 *ex-array*)
;; FIXME: (make-dataframe2 *ex-lol*)) ;; FIXME


