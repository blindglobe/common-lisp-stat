;;; -*- mode: lisp -*-

;;; Time-stamp: <2012-11-02 17:57:23 tony>
;;; Creation:   <2012-11-02 17:30:08 tony>
;;; File:       60-regressionExamples.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2012--, AJ Rossini.  Currently licensed under MIT
;;;             license.  See file LICENSE.mit in top-level directory
;;;             for information.
;;; Purpose:    Template header file

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

;;; This organization and structure is new to the 21st Century
;;; version..   Think, "21st Century Schizoid Man".

(load "00-loadingData.lisp")

(in-package :cls-examples)


;;; Example from Tamas Papp, BUT SIMPLE COPY, DOESN'T WORK
(use-package :lla)
(use-package :cl-random)

;; FYI, LLA has least squares using QR decomposition (it would be
;; trivial to enable SVD, I had that running but commented it our for
;; the time being since I plan to redo the SVD API).

;; Just try

(lla:least-squares y X)

;; CL-RANDOM builds on this to provide LINEAR-REGRESSION, which
;; returns an object you can DRAW from if you are into Bayesian
;; analysis.  eg

(let ((regression (linear-regression y x)))
  (list (mean regression)   ; posterior mean
        (draw regression))) ; random draw from the posterior

;;; Tony remarks:

;; Great Start!  Issues to be addressed in the future: Need to clarify
;; that the so-called Bayesian linear regression is probably using the
;; standard non-informative/Jeffrey's prior, and so needs to clarify
;; that in the medium-term future (and extend with a property
;; pre-spec'd prior, and we need a means to go from a data frame
;; containing non-numerical variables to support using this.
