;;; -*- mode: lisp -*-
;;; Copyright (c) 2006-2008, by A.J. Rossini <blindglobe@gmail.com>
;;; See COPYRIGHT file for any additional restrictions (BSD license).
;;; Since 1991, ANSI was finally finished.  Edited for ANSI Common Lisp. 

;;; Time-stamp: <2012-10-03 05:30:37 tony>
;;; Creation:   <2012-07-01 11:29:42 tony>
;;; File:       example.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c) 2012, AJ Rossini.  BSD.
;;; Purpose:    example of possible usage.

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.


;; Load system
(ql:quickload "cls")

;; use the example package...
(in-package :cls-user)


;; or better yet, create a package/namespace for the particular problem being attacked.
(defpackage :my-package-user
  (:documentation "demo of how to put serious work should be placed in
    a similar package elsewhere for reproducibility.  This hints as to
    what needs to be done for a user- or analysis-package.")
  (:nicknames :my-clswork-user)
  (:use :common-lisp ; always needed for user playgrounds!
	:lisp-matrix ; we only need the packages that we need...
	:common-lisp-statistics
	:lisp-stat-data-examples) ;; this ensures access to a data package
  (:export summarize-data summarize-results this-data this-report)
  (:shadowing-import-from :lisp-stat call-method call-next-method

      expt + - * / ** mod rem abs 1+ 1- log exp sqrt sin cos tan
      asin acos atan sinh cosh tanh asinh acosh atanh float random
      truncate floor ceiling round minusp zerop plusp evenp oddp 
      < <= = /= >= > > ;; complex
      conjugate realpart imagpart phase
      min max logand logior logxor lognot ffloor fceiling
      ftruncate fround signum cis

      <= float imagpart)) 

(in-package :my-clswork-user)

;; create some data by hand

(setf *testdata-prespec-2Darray*
      (make-dataframe #2A((1 2 3)(4 5 6))))


;; (setf *testdata-prespec-listoflist*
;;       (make-dataframe  '(list ((1 2 3)(4 5 6)))))

