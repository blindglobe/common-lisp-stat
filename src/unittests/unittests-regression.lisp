;;; -*- mode: lisp -*-

;;; File:       unittests-regression.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2008, AJ Rossini.
;;; License:    BSD, see LICENSE.BSD file for details.
;;; Purpose:    unit-tests for regression; also make good examples
;;; Time-stamp: <2008-05-13 17:16:07 tony>
;;; Creation:   <2008-05-13 17:16:07 tony>

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

;;; This organization and structure is new to the 21st Century
;;; version.

(in-package :cl-user)

;; if needed, but need to set the ASDf path first...!
;; (asdf:oos 'asdf:load-op :lift)

(defpackage :lisp-stat-regression-unittests
  (:use :common-lisp
	:lift  :lisp-stat-unittests
	:lisp-stat-types))

(in-package :lisp-stat-regression-unittests)

(defun run-lisp-stat-tests ()
  (run-tests :suite 'lisp-stat))

(defun run-lisp-stat-test (&rest x)
  (run-test x))

(deftestsuite lisp-stat-regression (lisp-stat) ())


;;; setup data tht we need to get this work done.

(defvar regr-testdata nil
  "dataset used for testing")

(defvar regr-testdata-iron nil "testvector one")
(defvar regr-testdata-alu  nil "testvector one")
(defvar regr-testdata-othe nil "testvector one")
(defvar m1 nil "fitted regr model 1")
(defvar m1 nil "fitted regr model 2")


;;; now go to the tests so that we can do the right thing with the
;;; eval.

(addtest (lisp-stat-regression) simp-lin-regr1-fit
	 (ensure
	  (progn
	    (setf m1 (regression-model regr-testdat-iron regr-testdat-alu))
	    (equal (list ...)
		   (send m1 :coefs)))))

(addtest (lisp-stat-regression) lin-regr1-bomb
	 (ensure-error
	   (check-nonneg-fixnum -3)))

;;; 
;; (run-tests)
;; (describe (run-tests))
