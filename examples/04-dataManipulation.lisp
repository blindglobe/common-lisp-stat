;;; -*- mode: lisp -*-

;;; Time-stamp: <2013-10-17 08:34:17 tony>
;;; Creation:   <2009-03-12 17:14:56 tony>
;;; File:       data-manipulation.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2009--, AJ Rossini.  BSD, LLGPL, or GPLv2, depending
;;;             on how it arrives.  
;;; Purpose:    demo on dataframe and matrix work.

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.


;; we do this in the CLS user playground, so we have access to example
;; data.
(in-package :cls-user)

;;; Guidelines:

(defpackage :cls-ex-datamanip
  (:use :common-lisp
	:lisp-matrix
	:common-lisp-statistics)

  (:shadowing-import-from :lisp-stat call-method call-next-method
			  
      expt + - * / ** mod rem abs 1+ 1- log exp sqrt sin cos tan
      asin acos atan sinh cosh tanh asinh acosh atanh float random
      truncate floor ceiling round minusp zerop plusp evenp oddp 
      < <= = /= >= > > ;; complex
      conjugate realpart imagpart phase
      min max logand logior logxor lognot ffloor fceiling
      ftruncate fround signum cis

      <= float imagpart))

(in-package :cls-ex-datamanip)

;;  There are two initial forms for most datasets which are in
;;  cases-by-variables format -- listoflist structure and lisp
;;  arrays.

(defparameter *ex-lol* '((11d0 12d0 13d0 14d0)
			 (21d0 22d0 23d0 24d0)
			 (31d0 32d0 33d0 34d0)))

(defparameter *ex-ary* #2A((11d0 12d0 13d0 14d0)
			   (21d0 22d0 23d0 24d0)
			   (31d0 32d0 33d0 34d0)))

;;; Matrices

(make-matrix 3 4 :initial-contents *ex-lol*)
(make-matrix 3 4 :initial-contents *ex-ary*)

;;; Dataframes: 3, all the same

(defparameter *ex-ary-df* (make-dataframe *ex-ary*))
(defparameter *ex-lol-ary-df* (make-dataframe (listoflist:listoflist->array  *ex-lol*)))
(defparameter *ex-lol-df* (make-dataframe *ex-lol*))






;;; "make-dataframe2" is generic -- FIXME: but it is not yet exported!
;; FIXME: (make-dataframe2 *ex-ary*)
;; FIXME: (make-dataframe2 *ex-lol*)) ;; FIXME


