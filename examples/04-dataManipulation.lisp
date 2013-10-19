;;; -*- mode: lisp -*-

;;; Time-stamp: <2013-10-18 09:11:40 tony>
;;; Creation:   <2009-03-12 17:14:56 tony>
;;; File:       data-manipulation.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2009--, AJ Rossini.  BSD, LLGPL, or GPLv2, depending
;;;             on how it arrives.  
;;; Purpose:    demo on dataframe and matrix work.

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.


(load "examples/02-dataframeBuilding.lisp")

;; we do this in the CLS user playground, so we have access to example
;; data.
(in-package :cls-examples)

;; We have the following variables to start from.  -TYPE* describes
;; the structure, they are built in the previous files.

;; Same data
*chickwts-array*
*chickwts-df*

;; Same data (3x4 matrix)

*ex-array*

*ex-lol*

*ex-array-mat*
*ex-lol-mat*

*ex-array-df*
*ex-lol-array-df*
*ex-lol-df*

;; now, to demonstrate manipulations (as well as experiment with
;; non-implemented APIs that deserve to be implemented).

;; we have the following APIs that could be used.  
;;
;; XARRAY works with all, but has some overhead for dispatch and API
;; munging.
;;
;; LISP-MATRIX works with the dataframe-like and matrix-like
;; inheriting structures, which are dataframes, their subtypes, and
;; the many homogeneous typed matrices from LISP-MATRIX.
;;
;; COMMON-LISP functions and methods can be used with the lisp array
;; and list-of-list structures.

;;; Element extraction

(=  (xref *ex-array-df* 1 2) ; dataframes
    (xref *ex-lol-array-df* 1 2)
    (xref *ex-lol-df* 1 2)

    (xref *ex-array-mat* 1 2) ; FIXME
    (xref *ex-lol-mat* 1 2); FIXME
 
    (xref *ex-array* 1 2) ; lisp array
    (xref *ex-lol-array* 1 2) 

    (xref *ex-lol* 1 2)) ; lol ; FIXME


(=  (mref *ex-array-df* 1 2) ; dataframe-like
    (mref *ex-lol-array-df* 1 2)
    (mref *ex-lol-df* 1 2)

    (mref *ex-array-mat* 1 2) ; matrix-like
    (mref *ex-lol-mat* 1 2))

(=  (aref *ex-array* 1 2) ; lisp array
    (aref *ex-lol-array* 1 2))


;;;FIXME: 
;;;  (get-list-of-list-item *ex-lol* 1 2)) ; lol


;;; Column and row extraction


;;; sub-matrix-like, sub-array or sub-dataframe-like extraction
