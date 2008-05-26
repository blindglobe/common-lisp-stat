;;; -*- mode: lisp -*-
;;; Copyright (c) 2007, by A.J. Rossini <blindglobe@gmail.com>
;;; See COPYRIGHT file for any additional restrictions (BSD license).
;;; Since 1991, ANSI was finally finished.  Edited for ANSI Common Lisp. 

;;; This is semi-external to lispstat core packages.  The dependency
;;; should be that lispstat packages are dependencies for the unit
;;; tests.  However, where they will end up is still to be
;;; determined. 

(in-package :cl-user)

(defpackage :lisp-stat-unittests
  (:use :common-lisp :lift :lisp-stat)
  (:shadowing-import-from :lisp-stat
	slot-value call-method call-next-method ;; objects
	expt + - * / ** mod rem abs 1+ 1- log exp sqrt sin cos tan ;; lsmath
	asin acos atan sinh cosh tanh asinh acosh atanh float random
	truncate floor ceiling round minusp zerop plusp evenp oddp 
	< <= = /= >= > ;; complex
	conjugate realpart imagpart phase
	min max logand logior logxor lognot ffloor fceiling
	ftruncate fround signum cis)
  (:export run-lisp-stat-tests run-lisp-stat-test scoreboard ; exec
	   almost= almost=lists numerical=)) ; compare

(in-package :lisp-stat-unittests)

;;; TESTS

(defun run-lisp-stat-tests ()
  (run-tests :suite 'lisp-stat))

(defun run-lisp-stat-test (&rest x)
  (run-test x))

(deftestsuite lisp-stat-proto (lisp-stat) ())

;;;; Object System tests

;;(deftestsuite lisp-stat-proto-objects (lisp-stat)
;;  ()
;;  (:documentation "Make sure the proto object system is valid.")
;;  (:tests
;;   (create-proto (ensure (object-proto-p (defproto test-me))))
;;   (create-proto2 (ensure (object-proto-p (defproto2 test-me2))))
;;   (instance1 (ensure (send test-me :isnew)))
;;   (instance1-2 (ensure (send test-me2 :isnew)))
;;   (instance2 (ensure (send test-me :has-slot 'new)))
;;   (instance2-2 (ensure (send test-me2 :has-slot 'new)))

;;   (instance5 (ensure (send test-me :has-slot 'new)))
;;   (instance5-2 (ensure (send test-me2 :has-slot 'new)))
;;   (instance5 (ensure (send test-me :own-slots 'new)))
;;   (instance5-2 (ensure (send test-me2 :own-slots 'new)))
;;   (instance5 (ensure (send test-me :has-slot 'new)))
;;   (instance5-2 (ensure (send test-me2 :has-slot 'new)))
;;   (instance5 (ensure (send test-me :has-slot 'new)))
;;   (instance5-2 (ensure (send test-me2 :has-slot 'new)))

;;   ))


   