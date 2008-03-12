;;; -*- mode: lisp -*-
;;; Copyright (c) 2007, by A.J. Rossini <blindglobe@gmail.com>
;;; See COPYRIGHT file for any additional restrictions (BSD license).
;;; Since 1991, ANSI was finally finished.  Edited for ANSI Common Lisp. 

;;; This is semi-external to lispstat core packages.  The dependency
;;; should be that lispstat packages are dependencies for the unit
;;; tests.  However, where they will end up is still to be
;;; determined. 

(in-package :cl-user)

(defpackage :lisp-stat-unittests-dataclos
  (:use :common-lisp :lift :lisp-stat :lisp-stat-unittests)
  (:shadowing-import-from :lisp-stat
	slot-value call-method call-next-method ;; objects
	expt + - * / ** mod rem abs 1+ 1- log exp sqrt sin cos tan ;; lsmath
	asin acos atan sinh cosh tanh asinh acosh atanh float random
	truncate floor ceiling round minusp zerop plusp evenp oddp 
	< <= = /= >= > ;; complex
	conjugate realpart imagpart phase
	min max logand logior logxor lognot ffloor fceiling
	ftruncate fround signum cis)
  (:export unittest-data))

(in-package :lisp-stat-unittests-dataclos)

;;; TESTS

(defun run-lisp-stat-tests ()
  (run-tests :suite 'lisp-stat))

(defun run-lisp-stat-test (&rest x)
  (run-test x))


(deftestsuite lisp-stat-dataclos (lisp-stat)
  ()
  (:tests
   (initdata (ensure-true ))))



(deftestsuite lisp-stat-testsupport (lisp-stat)
  ()
  (:tests
   (almost=1 (ensure (almost= 3 3.001 :tol 0.01)))



(addtest (lisp-stat-dataclos) testnameData
	 (ensure-same
	  (dataset (list a b c d) :form (list 2 2))
	  #2A((a b) (c d))
	  :test 'eql))


 