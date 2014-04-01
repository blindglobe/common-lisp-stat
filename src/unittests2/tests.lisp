;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

;;; Time-stamp: <2014-04-01 17:10:47 tony>
;;; Creation:   <2014-02-25 12:32:30 tony>
;;; File:       tests.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2014--, AJ Rossini.  Currently licensed under MIT
;;;             license.  See file LICENSE.mit in top-level directory
;;;             for information.
;;; Purpose:    Examples of using CLUNIT for unit testing for CLS

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

;;; This organization and structure is new to the 21st Century
;;; version..   Think, "21st Century Schizoid Man".


;;; This file just provides for a reminder of what to do to get tests run, etc.



(in-package :cls-clunit)


;; :default, :tap, NIL
(setf clunit:*clunit-report-format* :default) 

(run-suite 'common-lisp-stat
	   :use-debugger NIL
	   :report-progress T)

(run-test 'cholesky-decomposition-1
	  :use-debugger NIL
	  :report-progress T)


(run-suite 'NumberSuite :report-progress nil)

(rerun-failed-tests :use-debugger NIL)


