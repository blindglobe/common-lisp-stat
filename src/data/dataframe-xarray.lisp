;;; -*- mode: lisp -*-

;;; Time-stamp: <2009-07-13 14:32:21 tony>
;;; Creation:   <2009-06-22 17:09:47 tony>
;;; File:       dataframe-xarray.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2009--, AJ Rossini.  BSD, LLGPL, or GPLv2, depending
;;;             on how it arrives.  
;;; Purpose:    Integration of xarray tools with dataframe-like
;;;             objects. 

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

;;; Tamas Papp's xarray package is his 2nd generation generic array
;;; indexing package, coming after the ideas and thoughts behind his
;;; affi package, which while novel, is somewhat obsolete.


(defmethod xref ((obj data-frame-like)  &rest indices) 
  (apply #'dfref obj indices))
;; better might be:  (xref (dataset obj) indices)

(defmethod (setf xref) (value (object array) &rest subscripts)
  (setf (apply #'dfref object subscripts) value))
  
(defmethod xref ((obj matrix-like) &rest indices))
  
(defmethod xtype ((obj dataframe-like))
  "Unlike the standard xtype, here we need to return a vector of the
  types.  Vectors can have single types, but arrays have single type.
  Dataframe-like have multiple types, variable-like single type,
  case-like has multiple types, and matrix-like has single type.")

(defmethod xdims ((obj dataframe-like))
  (dataframe-dimensions obj))
#|
 ;; use default methods at this point, except for potentially weird DFs
 (defmethod xdims* ())

 (defmethod xdim ((obj dataframe-like) index)
  (dataframe-dimension index))

|#

(defmethod xrank ())

(defmethod slice ())
  
(defmethod take ())

  (defmethod carray ())
