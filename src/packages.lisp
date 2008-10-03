;;; -*- mode: lisp -*-

;;; Time-stamp: <2008-10-03 02:25:41 tony>
;;; Creation:   <2008-03-11 19:18:34 user> 
;;; File:       packages.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2007--2008, AJ Rossini.  BSD, LLGPL, or GPLv2, depending
;;;             on how it arrives.  
;;; Purpose:    package structure description for lispstat

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

;;; This organization and structure is new to the 21st Century
;;; version.

(in-package :cl-user)

;;; LispStat Basics

(defpackage :lisp-stat-basics
    (:use :common-lisp
	  :lisp-stat-object-system
	  :lisp-stat-types
	  :lisp-stat-float
	  :lisp-stat-macros
	  :lisp-stat-compound-data)
  (:shadowing-import-from :lisp-stat-object-system
			  slot-value call-method call-next-method)
  (:export permute-array sum prod count-elements mean
	   if-else sample))


;;;



(defpackage :lisp-stat-float
  (:use :common-lisp)
  (:export +stat-float-typing+ +stat-cfloat-typing+ +stat-float-template+
	   machine-epsilon base-float makedouble

	   make-base-trans-fun-2 make-base-trans-fun 

	   BASE-LOG BASE-EXP BASE-EXPT BASE-SQRT BASE-SIN BASE-COS
	   BASE-TAN BASE-ASIN BASE-ACOS BASE-ATAN BASE-SINH
	   BASE-COSH BASE-TANH BASE-ASINH BASE-ACOSH BASE-ATANH
	   BASE-ABS BASE-PHASE BASE-FFLOOR BASE-FCEILING BASE-FTRUNCATE
	   BASE-FROUND BASE-SIGNUM BASE-CIS))

;;; 

(defpackage :lisp-stat-macros
  (:use :common-lisp
	:lisp-stat-compound-data)
  (:export make-rv-function make-rv-function-1))

;;; NEW CLOS STRUCTURE

(defpackage :lisp-stat-data-clos
  (:use :common-lisp
	:lisp-matrix)
  (:export get-variable-matrix get-variable-vector
	   ;; generic container class for data -- if small enough
	   ;; could be value, otherwise might be reference.
	   data-pointer))

(defpackage :lisp-stat-regression-linear-clos
  (:use :common-lisp
	:lisp-matrix
	:lisp-stat-data-clos)
  (:export regression-model regression-model-obj x y intercept sweep-matrix
	   basis weights included total-sum-of-squares residual-sum-of-squares
	   predictor-names response-name case-labels))

