;;; -*- mode: lisp -*-

;;; Time-stamp: <2012-10-12 08:55:28 tony>
;;; Creation:   <2009-04-19 09:41:09 tony>
;;; File:       t-test-example.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2009--, AJ Rossini.  MIT
;;; Purpose:    Example of basic exploratory data analysis in CLS. 

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.


;;; If we can't do a t-test, we can't do anything

;;;; BROKEN!!!

(in-package :cls-user)

(defparameter *my-df*
  (make-dataframe
   (rsm.string::filestream->string-table
    "/path/to/file.dsv"))
  "Initial read-in of data.")


