;;; -*- mode: lisp -*-

;;; Time-stamp: <2009-12-13 18:02:13 tony>
;;; Creation:   <2009-04-19 09:41:09 tony>
;;; File:       linear-regression.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2009--, AJ Rossini.  BSD, MIT, LLGPL, or
;;;             GPLv2+, or GPLv3+ depending on how it arrives.  
;;; Purpose:    Example of basic linear regression data analysis in CLS.

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

(in-package :ls-user)

;; TODO:
;; - confirm estimates for multivariate case, 
;; - pretty-print output
;; - fix up API -- what do we want this to look like?

(defparameter *m*
  (regression-model (list->vector-like iron)
		    (list->vector-like absorbtion))
  "holding variable.")

(defparameter *m2*
  (lm (list->vector-like iron)
      (list->vector-like absorbtion))
  "holding variable.")

(defparameter *m-fit*
  (fit-model *m*))

(princ *m2*)
(princ *m-fit*)

(estimates *m-fit*)
(covariance-matrix *m-fit*)

(defparameter *m3*
  (regression-model (transpose
		     (listoflist->matrix-like
		      (list iron aluminum)
		      :orientation :row-major))
		    (list->vector-like  absorbtion)))
(princ *m3*)
(defparameter *m3-fit*
  (fit-model *m3*))

#|
  ;; Should the above look something like:
  (defparameter *m3-fit*
                (spec-and-fit-model '(absorbtion = iron aluminum)))
  ;; in which case we split the list before/after the "=" character.
|#
  
(estimates *m3-fit*)
(covariance-matrix *m3-fit*)



;; now to build a linear regression model from an external CSV datafile...

(defparameter *my-df*
  (make-dataframe
   (rsm.string::file->string-table
    (concatenate 'string *cls-data-dir*  "file.dsv"))
  "Initial read-in of data."))

(defparameter *my-resp-var* (slice *my-df* ))

(defparameter *my-pred-vars* (slice *my-df* ))

