;;; -*- mode: lisp -*-

;;; Time-stamp: <2009-07-13 16:03:15 tony>
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
;;; affi package, which while novel, is somewhat obsolete as a
;;; selector framework, but great as an iteration framework.

(in-package :cls-dataframe)

(defmethod xref ((obj dataframe-like)  &rest subscripts)
  "For data-frame-like, dispatch on storage object."
  (xref (dataset obj) subscripts))

(defmethod (setf xref) (value (obj dataframe-like) &rest subscripts)
  (setf (xref (dataset obj) subscripts) value))
  
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

#|
 (defmethod slice ())
  
 (defmethod take ())

 (defmethod carray ())
|#