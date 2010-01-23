;;; -*- mode: lisp -*-

;;; Time-stamp: <2010-01-22 07:54:51 tony>
;;; Creation:   <2009-03-12 17:14:56 tony>
;;; File:       dataframe-array.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2009--, AJ Rossini. BSD, LLGPL, or GPLv2, depending
;;;             on how it arrives.  
;;; Purpose:    real dataframe class using lisp-arrays as storage.

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

;;; This organization and structure is new to the 21st Century
;;; version..   Think, "21st Century Schizoid Man".

(in-package :cls-dataframe)

;;;;; DATAFRAME-ARRAY

(defclass dataframe-array (dataframe-like)
  ((store :initform nil
	  :initarg :storage
	  :type (array * *)
	  :accessor dataset
	  :documentation "Data storage: typed as array.")
   (store-class :initform 'array
		:accessor store
		:documentation "Storage class is ARRAY."))
  (:documentation "example implementation of dataframe-like using storage
  based on lisp arrays.  An obvious alternative could be a
  dataframe-matrix-like which uses the lisp-matrix classes."))

(defmethod nrows ((df dataframe-array))
  "specializes on inheritance from matrix-like in lisp-matrix."
  (array-dimension (dataset df) 0))

(defmethod ncols ((df dataframe-array))
  "specializes on inheritance from matrix-like in lisp-matrix."
  (array-dimension (dataset df) 1))

(defmethod xref ((df dataframe-array) &rest subscripts)
  "Returns a scalar in array, in the same vein as aref, mref, vref, etc.
idx1/2 is row/col or case/var."
  (assert (>= 2 (length subscripts)))
#| ;; needed?
  (assert (typep (elt subscripts 0) integer))
  (assert (typep (elt subscripts 1) integer))
|#
  (aref (dataset df) (elt subscripts 0) (elt subscripts 1)))

(defmethod (setf xref) (value (df dataframe-array) &rest subscripts)
  "set value for df-ar."
  ;; (check-type val (elt (var-type df) index2))
  (setf (aref (dataset df) (elt subscripts 0) (elt subscripts 1)) value))

(defparameter *default-dataframe-class* 'dataframe-array)

(defmethod dfselect ((df dataframe-array) 
		     &optional cases vars indices)
  "Extract the OR of cases, vars, or have a list of indices to extract"
  (if indices (error "Indicies not used yet"))
  (let ((newdf (make-instance *default-dataframe-class*
		:storage (make-array (list  (length cases) (length vars)))
		:nrows (length cases)
		:ncols (length vars)
#|
		:case-labels (select-list caselist (case-labels df))
		:var-labels (select-list varlist (var-labels df))
		:var-types (select-list varlist (vartypes df))
|#
		)))
    (dotimes (i (length cases))
      (dotimes (j (length vars))
	(setf (xref newdf i j)
	      (xref df
		    (position (elt cases i) (case-labels df))
		    (position (elt vars j) (var-labels df))))))))
