;;; -*- mode: lisp -*-

;;; File:       unittests-regression.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c) 2008--, AJ Rossini.
;;; License:    BSD, see LICENSE.BSD file for details.
;;; Purpose:    unit-tests for regression; also make good examples
;;; Time-stamp: <2009-04-03 07:37:06 tony>
;;; Creation:   <2008-05-13 17:16:07 tony>

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

(in-package :lisp-stat-unittests)

(deftestsuite lisp-stat-ut-regression (lisp-stat-ut) ())

;;; TESTS

(addtest (lisp-stat-ut-regression)
  lin-regr1-fit-basis
  (let ((m1 (regression-model (list->vector-like iron)
			      (list->vector-like aluminum) :print nil)))
    (ensure (> 0 (send m1 :own-slots)))))

(addtest (lisp-stat-ut-regression)
  lin-regr2-fit-basis
  (let ((m1 (regression-model (list iron aluminum) absorbtion :print nil)))
    (ensure (> 0 (send m1 :basis)))))

(addtest (lisp-stat-ut-regression)
  check-nonneg-fixnum
  (ensure-error
    (check-nonneg-fixnum -3)))

;;; 
;; (run-tests)
;; (describe (run-tests))
