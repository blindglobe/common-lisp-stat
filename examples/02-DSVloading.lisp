;;; -*- mode: lisp -*-

;;; Time-stamp: <2012-11-03 11:56:46 tony>
;;; Creation:   <2012-11-02 08:14:38 tony>
;;; File:       02-DSVloading.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2012--, AJ Rossini.  Currently licensed under MIT
;;;             license.  See file LICENSE.mit in top-level directory
;;;             for information.
;;; Purpose:    Example of loading DSV files

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

;;; This organization and structure is new to the 21st Century
;;; version..   Think, "21st Century Schizoid Man".



;;;; BROKEN!!!
(load "00-loadingData.lisp")

(in-package :cls-examples)

(ql:quickload :cl-csv)
(use-package :cl-csv)


(ql:quickload :cl-csv)
(use-package :cl-csv)


(ql:quickload :fare-csv)
(use-package :fare-csv)




(let ((results (make-dataframe ))))






