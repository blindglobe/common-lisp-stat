;;; -*- mode: lisp -*-

;;; Time-stamp: <2010-11-07 11:38:53 tony>
;;; Creation:   <2009-03-12 17:14:56 tony>
;;; File:       gaussian.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2009--, AJ Rossini.  Currently licensed under MIT
;;;             license.  See file LICENSE.mit in top-level directory
;;;             for information.
;;; Purpose:    Gaussian probability law structure and functions

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

;;; This organization and structure is new to the 21st Century
;;; version..   Think, "21st Century Schizoid Man".


(in-package :cls-probability)

(defun gaussian-probability-density-univariate (x params))

(defclass gaussian-probability-parameters (probability-parameters)
  ((mean :documentation "vector (or value) which denotes the mean structure.")
   (variance :documentation "matrix (or value) which denotes covariance structure."))
  :documentation "standard mean-variance parameterization for gaussian probability law.")

(defclass gaussian-probability-univariate-parameters (gaussian-probability-parameters)
  ((mean :documentation "vector (or value) which denotes the mean structure."
	 :type univariate-value)
   (variance :documentation "matrix (or value) which denotes covariance structure."
	     :type univariate-value))
  :documentation "standard mean-variance parameterization for gaussian probability law.")

(defclass gaussian-probability-multivariate-parameters (gaussian-probability-parameters)
  ((mean :documentation "vector (or value) which denotes the mean structure."
	 :type array-like)
   (variance :documentation "matrix (or value) which denotes covariance structure."
	     :type matrix-like))
  :documentation "standard mean-variance parameterization for gaussian probability law.")

(defgeneric gaussian-probability-density (x params)
  (:documentation "general gaussian density method.")
  (:method ((x xarray-vector)
	    (params gaussian-probability-multivariate-parameters))
    ;; compute multivariate likelihood, and if X is something else, convert it an proceed to this method. 
    )
  (:method ((x number)
	    (params gaussian-probability-univariate-parameters))
    (\ (exp (* -1.0 (/ (- x (mean params))
		       (standard-deviation params ))))
       (sqrt (* 2.0 pi (variance params))))))



(defclass gaussian-probability-law (probability-law)
  (()))



