;;; -*- mode: lisp -*-

;;; Time-stamp: <2010-11-07 11:29:30 tony>
;;; Creation:   <2010-11-06 01:51:20 tony>
;;; File:       probability.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2010--, AJ Rossini.  Currently licensed under MIT
;;;             license.  See file LICENSE.mit in top-level directory
;;;             for information.
;;; Purpose:    Probability functions

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

;;; This organization and structure is new to the 21st Century
;;; version..   Think, "21st Century Schizoid Man".


;;; Current computations are handled by leveraging the cl-variates and
;;; gsll packages, as they have flexibility and the capability to be
;;; reproducible.  This is just a stub for the interface/API that we
;;; would like to be able to use.

(in-package :cls-probability)

(defgeneric density (probability-law-instance value))
(defmethod density ((pli probability-law) value))

(defgeneric distribution (probability-law-instance value))

(defgeneric quantile (probability-law-instance value))

(defgeneric interquartile-range (probability-law))

(defgeneric draw-variates (probability-law-instance n))

(defclass probability-parameters ()
  ()
  :documentation "Virtual class to denote prob parameters")

(defclass probability-law ()
  ((density-function
    :type prob-function
    :documention "density function, if exists")
   (mass-function
    :type prob-function
    :documention "function")
   (support-function
    :documentation "List of values for discrete mass functions, list of pairs denoting for ranges")
   (support-class
    :type symbol
    :documentation "'REAL, 'DISCRETE'")
   (parameters
    :type list)
   (prng-stream
    :type unif-stream
    :documentation "current underlying prng stream object, typically Uniform[0,1]"))
  (:documentation "sufficient data to compute probabilistic
  quantities.  Given the support of the probability law, and a
  function mapping the law to the prob result, we can compute, in an
  expensive manner, most quantities.  When feasible, we can accelerate
  this quite a bit.")
  ())


#|

;; We basically want to support the following style of construct:

 (let ((my-abstract-law (make 'probability-law
			     :density/mass (gaussian-law
					    :parameters '(:mean 5 :variance 3))
			     :seed 1324
			     :name "Gaussian(5,3)"
			     :documentation "model-distribution, used
			     for likelihoods, probabilities of
			     asympts, and other somethings."))
      (my-empirical-law (make 'probability-law
			      :density/mass (empirical-law data-vector-or-data-list)
			      :seed 415
			      :name "Empirical Law from observations"
			      :documentation "based on observations,
			       bootstrap/resampling style
 			       probability.")))
  (mean my-law)
  (variance my-law)
  (standard-deviation my-law)
  (draw-variates my-law 10)
  ;; one of the following would return a number, the other would return a 'nil
  (probability-density-function my-law x)
  (probability-mass-function my-law x)
  (cumulative-distribution-function my-law x)
  (survivorship-function my-law x)
  (hazard-function my-law x)
  (cumulative-hazard-function my-law x)


  (mean my-empirical-law) ; empirical mean
  (draw-variates my-empirical-law 10) ; bootstrap (unweighted)
  ;; the rest would consist of empirical

  ())

|#

