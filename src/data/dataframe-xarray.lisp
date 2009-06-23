;;; -*- mode: lisp -*-

;;; Time-stamp: <2009-06-23 08:02:51 tony>
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

(defmethod (setf xref) (value (object array) &rest subscripts)
  (setf (apply #'dfref object subscripts) value))
  
(defmethod xref ((obj matrix-like)  &rest indices))
  
(defmethod xtype ((obj dataframe-like)))

(defmethod xdims ())

(defmethod xdims* ())

(defmethod xdim ((obj dataframe-like) index)
  (dataframe-dimension index))

(defmethod xrank ())

(defmethod slice ())
  
(defmethod take ())

  (defmethod carray ())
