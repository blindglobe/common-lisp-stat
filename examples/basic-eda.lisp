;;; -*- mode: lisp -*-

;;; Time-stamp: <2009-04-19 09:50:21 tony>
;;; Creation:   <2009-04-19 09:41:09 tony>
;;; File:       basic-eda.lisp
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

;; Summarize is the basic EDA tool -- it accepts symbols or lists of
;; symbols, to describe what to do, and how to do it.  The resulting
;; data structure has a means for re-invoking the result as well as
;; partial storage of key results (when appropriate) as well as
;; metadata about the results (time / context if provided).  

(defparameter *my-df-smry-num*
  (summarize *my-df* :type 'numerical :io 'listing)
  "First numerical summary of *my-df-smry*")

(defparameter *my-df-smry-num*
  (summarize *my-df*
	     :type 'numerical
	     :io 'report-pdf
	     :device '(file "output.pdf"))
  "First numerical summary of *my-df-smry*")

(defparameter *my-df-smry-vis*
  (summarize *my-df*
	     :type 'visual
	     :io 'interactive
	     :device 'xwin)
  "visual summary")

(defparameter *my-df-smry-vis*
  (summarize *my-df* :type 'visual :io 'interactive-dynamic)
  "visual summary")

(defparameter *my-df-smry-vis*
  (summarize *my-df* :type 'visual :io 'static)
  "visual summary")

