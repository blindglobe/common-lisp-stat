;;; -*- mode: lisp -*-

;;; Time-stamp: <2008-11-02 17:54:25 tony>
;;; Creation:   <2008-09-08 08:06:30 tony>
;;; File:       TODO.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c) 2007-2008, AJ Rossini <blindglobe@gmail.com>.  BSD.
;;; Purpose:    demonstrations of how one might use CLS.

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

;;; This file contains the current challenges to solve, including a
;;; description of the setup and the work to solve....
 
;;; SET UP

(in-package :cl-user)
;;(asdf:oos 'asdf:compile-op 'lift :force t)
;;(asdf:oos 'asdf:load-op 'lift)
;;(asdf:oos 'asdf:load-op 'lispstat)

(in-package :lisp-stat-unittests)
(describe (run-tests :suite 'lisp-stat-ut))
(run-tests :suite 'lisp-stat-ut)
;; tests = 50, failures = 6 , errors = 0
(in-package :ls-user)

;;; Example: currently not relevant, yet
#|
(describe 
 (lift::run-test
  :test-case  'lispstat-unittests::strided-matrix-column-access
  :suite 'lispstat-regression))
|#


(defvar m nil "holding variable.")
(def m (regression-model (list iron aluminum) absorbtion :print nil))
(send m :compute)
(send m :sweep-matrix)
(format t "~%~A~%" (send m :sweep-matrix))

;;; FIXME

;; need to get multiple-linear regression working (simple linear regr
;; works)... to do this, we need to redo the whole numeric structure,
;; I'm keeping these in as example of brokenness...

(send m :basis) ;; this should be positive?
(send m :coef-estimates)

