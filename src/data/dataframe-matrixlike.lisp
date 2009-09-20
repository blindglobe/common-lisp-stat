;;; -*- mode: lisp -*-

;;; Time-stamp: <2009-09-19 23:21:30 tony>
;;; Creation:   <2009-03-12 17:14:56 tony>
;;; File:       dataframe-matrixlike.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2009--, AJ Rossini.  BSD, LLGPL, or GPLv2, depending
;;;             on how it arrives.  
;;; Purpose:    Uses the lisp-matrix dataframe instance for storage.
;;;             Useful if we only have numerical data and no missing
;;;             data, strings, or categorical stuff...

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

;;; This organization and structure is new to the 21st Century
;;; version..   Think, "21st Century Schizoid Man".
;;; DATAFRAME-MATRIXLIKE
;;; 
;;; example/implementatin of using lisp-matrix datastructures for
;;; dataframe storage.

(defclass dataframe-matrixlike (dataframe-like)
  ((store :initform nil
	  :initarg :storage
	  :type matrix-like
	  :accessor dataset
	  :documentation "Data storage: typed as matrix-like
  (numerical only)."))
  (:documentation "example implementation of dataframe-like using storage
  based on lisp-matrix structures."))

(defmethod nrows ((df dataframe-matrixlike))
  "specializes on inheritance from matrix-like in lisp-matrix."
  (matrix-dimension (dataset df) 0))

(defmethod ncols ((df dataframe-matrixlike))
  "specializes on inheritance from matrix-like in lisp-matrix."
  (matrix-dimension (dataset df) 1))

;;; *** FIXME: change mref to xref when we establish lisp-matrix
;;; change to use xarray access facility.  Need to dummy-proof the
;;; following. 
(defmethod xref ((df dataframe-matrixlike) &rest subscripts)
  "Returns a scalar in array, in the same vein as aref, mref, vref, etc.
idx1/2 is row/col or case/var."
  (mref (dataset df) (elt subscripts 0) (elt subscripts 1)))

(defmethod (setf xref) (value (df dataframe-matrixlike) &rest subscripts)
  "Sets a value for df-ml."
  ;; NEED TO CHECK TYPE!
  ;; (check-type val (elt (vartype df) index2))
  (setf (mref (dataset df) (elt subscripts 0) (elt subscripts 1)) value))


