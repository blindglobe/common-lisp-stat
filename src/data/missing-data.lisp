;;; -*- mode: lisp -*-

;;; Time-stamp: <2009-09-04 17:49:46 tony>
;;; Creation:   <2009-03-12 17:14:56 tony>
;;; File:       template.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2009--, AJ Rossini.  BSD, LLGPL, or GPLv2, depending
;;;             on how it arrives.  
;;; Purpose:    Template header file

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

;;; This organization and structure is new to the 21st Century
;;; version..   Think, "21st Century Schizoid Man".



;;; Missing data handling is critical - initial thought for this is to
;;; have a class which provides scalars which have type "missing" and
;;; perhaps the "supposed" statistical-typing, i.e. continuous ratio, 
;;; ordinal, nominal, etc.   Also have metadata to describe where this
;;; came from.
;;; 
;;; Then we can gensym a value per-dataset per-missing type.  
;;;
;;; Different types of missing:
;;; * censored data
;;; * unobserved data
;;; * coarsened measurement data
;;;
;;; but they can be placed into a generalized framework (see the work
;;; of van der Laan, Robins, etc...).
;;;

