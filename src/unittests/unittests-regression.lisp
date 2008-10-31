;;; -*- mode: lisp -*-

;;; File:       unittests-regression.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2008, AJ Rossini.
;;; License:    BSD, see LICENSE.BSD file for details.
;;; Purpose:    unit-tests for regression; also make good examples
;;; Time-stamp: <2008-10-31 17:33:20 tony>
;;; Creation:   <2008-05-13 17:16:07 tony>

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

;;; This organization and structure is new to the 21st Century
;;; version.

;; (in-package :cl-user)
;; if needed, but need to set the ASDf path first...!
;; (asdf:oos 'asdf:load-op :lift)

(in-package :lisp-stat-regression-unittests)

(deftestsuite lisp-stat-regression (lisp-stat) ())

;;; TESTS

(addtest (lisp-stat-regression)
  lin-regr1-fit-basis
  (let ((m1 (regression-model  iron aluminum :print nil)))
    (ensure (> 0 (send m1 :basis)))))

(addtest (lisp-stat-regression)
  lin-regr2-fit-basis
  (let ((m1 (regression-model (list iron aluminum) absorbtion :print nil)))
    (ensure (> 0 (send m1 :basis)))))

(addtest (lisp-stat-regression)
  check-nonneg-fixnum
  (ensure-error
    (check-nonneg-fixnum -3)))

;;; 
;; (run-tests)
;; (describe (run-tests))
