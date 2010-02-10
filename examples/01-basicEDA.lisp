;;; -*- mode: lisp -*-

;;; Time-stamp: <2010-02-10 14:23:42 tony>
;;; Creation:   <2009-04-19 09:41:09 tony>
;;; File:       basic-eda.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2009--, AJ Rossini.  BSD, MIT, LLGPL, or
;;;             GPLv2+, or GPLv3+ depending on how it arrives.  
;;; Purpose:    Example of basic exploratory data analysis in CLS. 

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

(in-package :cls-examples)

;; We assume that the "loading-data.lisp" code has been run, and one
;; now wants to analyze the data loaded into *chickwts-df*

(load (localized-pathto "loading-data.lisp")
      :verbose t)

*chickwts*

;; Summarize is the basic EDA tool -- it accepts symbols or lists of
;; symbols, to describe what, when, and how to do it.  The resulting
;; data structure has a means for re-invoking the result as well as
;; partial storage of key results (when appropriate) as well as
;; metadata about the results (time / context if provided).

;; numerical: (txt / *ml /  ,  variable / stream / spec'd to file)
;; visual: (static/dynamic, fixed/interactive) (need better term than fixed)

;; context -- using dataset metadata, to drive the resulting summary.
;; dataset metadata:
;; #1 sampling scheme -
;;       retro / prospect  collection
;;       random, biased, convenience sampling;
;; #2 purpose of dataset integration/manipulation
;; #3 sampling/temporal component of variables
;; 

(defparameter *my-df-smry-num*
  (summarize *chickwts* :type 'numerical :io 'listing)
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
