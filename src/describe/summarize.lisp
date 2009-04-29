;;; -*- mode: lisp -*-

;;; Time-stamp: <2009-04-20 13:17:34 tony>
;;; Creation:   <2009-03-12 17:14:56 tony>
;;; File:       summarize.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2009--, AJ Rossini.  BSD, LLGPL, or GPLv2, depending
;;;             on how it arrives.  
;;; Purpose:    Summarization strategy

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.


(defparameter *cls-summary-numerical-methods*
  '())

(defparameter *cls-summary-visual-methods*
  '())



(defgeneric summarize (ds &key type io dev)
  (:documentation "general approach to providing summarizes.  Tie in different approachs to hat we want to do."))