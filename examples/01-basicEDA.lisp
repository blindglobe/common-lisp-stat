;;; -*- mode: lisp -*-

;;; Time-stamp: <2010-03-03 16:22:26 tony>
;;; Creation:   <2009-04-19 09:41:09 tony>
;;; File:       basic-eda.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright: (c)2009--, AJ Rossini.  See LICENSE.mit in top level
;;;            directory for conditions.
;;; Purpose:    Example of basic exploratory data analysis in CLS. 

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

(in-package :cls-examples)

;; We assume that the "loading-data.lisp" code has been run, and one
;; now wants to analyze the data loaded into *chickwts-df*

(load (localized-pathto "loading-data.lisp")
      :verbose t)

*chickwts-df*

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

(defparameter *chkwt-df-depgraph*
 (let ((g  (make-container 'graph-container )))
   (loop for v in (var-list *chickwt-df*)
	(add-vertex g v))
   (loop for (v1 . v2) in (appropriate-pairs-list *chickwt-df*)
	(add-edge-between-vertexes g v1 v2))))

(defparameter *my-df-smry-num*
  (summarize *chickwts-df* :type 'numerical :io 'listing)
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
