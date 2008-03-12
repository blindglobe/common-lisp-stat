;;; -*- mode: lisp -*-

;;; File:       random.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2008, AJ Rossini.  BSD, LLGPL, or GPLv2, depending
;;;             on how it arrives.  
;;; Purpose:    random number streams, serial and parallel
;;; Time-stamp: <2008-03-11 19:18:48 user>
;;; Creation:   <2008-03-11 19:18:34 user> 

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

;;; This organization and structure is new to the 21st Century
;;; version.

;;; Lispy implementation of random numbers.  Initially, we leverage
;;; existing code before "improving" (which is an ill-definded action
;;; to do).

(in-package :cl-user)

(defpackage :lisp-stat-random
  (:use :common-lisp)
  (:export *seed* rng
	   variate
	   quantile
	   probability-density-function cumulative-density-function 
	   probability-types))

(in-package :lisp-stat-random)

(defstruct probability-types
  (list (1 . normal)
	(2 . t)
	(3 . cauchy)
	(4 . gamma)))

(defvar *seed* nil
  "Current global state of all pRNGs that might be active.  This needs
to be aware that it should be thread-safe, so might be a list of
indexible triples.")

(defvar *current-rng* nil
  "How do we understand this in a thread-safe manner?")

(defun rng-seed (&opt value specification) 
  "For getting and setting a particular rng with a particular seed,
Returns the current seed, even (especially!) if there is nothing to
set."
  (if (and specification value)
      (progn
	(set-rng-type specification)
	(if value (setf *seed* value)))
      (if value  (setf *seed* value)))
  *seed*)



