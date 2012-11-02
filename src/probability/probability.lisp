;;; -*- mode: lisp -*-

;;; Time-stamp: <2012-10-29 14:19:07 tony>
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


;;; Current thinking (30-10-2012) is that computations are handled by
;;; leveraging the cl-variates and gsll packages, as they have
;;; flexibility and the capability to be reproducible.  This is just a
;;; stub for the interface/API that we would like to be able to use.

;;; A bit of applicable design theory behind this:
;;; 
;;; We would like to think about where probability fits into
;;; statistics.  For example, consider the simple case of the "mean".
;;; If we treat both empirical distributions and theoretical
;;; distributions as equivalent first-class objects, it leads to some
;;; computationally interesting ways of doing things.  This means that
;;; observed data implies a first-class (discrete) empirical
;;; distribution in its own right, and also implies that we need to
;;; include a means of computing both functionals of 1 distribution as
;;; well as functionals of multiple distributions (think of the
;;; kullback-leibler divergence).
;;;
;;; This file should describe the primary generic functions and data
;;; structures that imply what is feasible in this case.  
;;; 
;;; We will map empirical and theoretical distributions into this
;;; structure in different files, as well as common (and uncommon)
;;; functionals.
;;; 
;;; in addition, probability and bayesian modeling requires a means to
;;; compute with mass functions, densities, and similar quantities.
;;; Some of these are algorithmically computable, though perhaps not
;;; in an optimal framework.  For example, given a prior, likelihood,
;;; and data, we ought to be able to compute a posterior in a number
;;; of ways.
;;;

(in-package :cls-probability)



(defgeneric density (probability-law value))

(defmethod density ((pli probability-law) (value real)))
(defmethod density ((pli probability-law) (value list)))
(defmethod density ((pli probability-law) (value vector-like)))


(defgeneric distribution (probability-law value))

(defgeneric quantile (probability-law value))

(defgeneric interquartile-range (probability-law))

(defgeneric draw-variates (probability-law-instance n))

(defclass probability-parameters ()
  ()
  :documentation "Virtual class to denote prob parameters")

;; TODO: need to understand how to manage multiple parameterizations
;; for the same family of probability laws.  

;; This structure is semi-BROKEN.  what was I thinking?  (but is
;; getting better).  Need a composition function approach as well...?

(defclass probability-law ()
  ((density/mass-function
    :type prob-function
    :documention "mass/density function, if exists.  Should be
    non-negative, and integrate over the support to being 1.  Can take
    values or ranges.  If mass function, results in probabililties.
    If density, only ranges produce valid probabilities.")
   (distn-function
    :type function
    :documention "distribution function.  value from [0,1],
    over the support of the space.")
   (support-function
    :documentation "Range or region where functions are defined, if
    computationally need to be restricted.")
   (support-class
    :type symbol
    :documentation "'REAL, 'DISCRETE', 'MIXED' (eg: zip dist'n model)")
   (parameters
    :type list
    :documentation "dotted list of symbols and ranges that are used
    for specification of the density and distribution functions.  If
    'nil or 'null, then we have a single distribution and not a family
    of distributions.  Ranges could be intervals, spaces, or a
    list (finite, ?infinite?) of discrete values")
   (prng-stream
    :type unif-stream
    :documentation "current underlying prng stream object, typically
    Uniform[0,1], but could be Uniform{0,...,n} for some discrete pRNGs."))
  (:documentation "Sufficient data to compute probabilistic quantities
  and draw from the particularly specified probability law.  Given the
  support of the probability law, and a function mapping the law to
  the prob result, we can compute, in an expensive manner, most
  quantities.  When feasible, we can accelerate this quite a bit.")
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

  (let ((posterior-prob-fcn (bayes-theorem prior likelihood dataX)))
     (mean posterior-prob-fcn))

  ())

|#

