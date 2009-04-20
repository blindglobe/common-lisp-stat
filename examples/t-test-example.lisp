;;; -*- mode: lisp -*-

;;; Time-stamp: <2009-04-19 09:53:53 tony>
;;; Creation:   <2009-04-19 09:41:09 tony>
;;; File:       t-test-example.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2009--, AJ Rossini.  BSD, MIT, LLGPL, or
;;;             GPLv2+, or GPLv3+ depending on how it arrives.  
;;; Purpose:    Example of basic exploratory data analysis in CLS. 

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.


(in-package :ls-user)

(defparameter *my-df*
  (make-dataframe
   (rsm.string::filestream->string-table
    "/path/to/file.dsv"))
  "Initial read-in of data.")


;; now to build a linear regression model from *my-df*
