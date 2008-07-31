;;; -*- mode: lisp -*-

;;; File:       unittests-lstypes.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2008, AJ Rossini.
;;; License:    BSD, see LICENSE.BSD file for details.
;;; Purpose:    unit-tests for lispstat typing.
;;; Time-stamp: <2008-05-28 08:25:28 tony>
;;; Creation:   <2008-05-09 14:16:56 tony>

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

;;; This organization and structure is new to the 21st Century
;;; version.

(in-package :cl-user)

;; if needed, but need to set the ASDf path first...!
;; (asdf:oos 'asdf:load-op :lift)

(defpackage :lisp-stat-types-unittests
  (:use :common-lisp
	:lift  :lisp-stat-unittests
	:lisp-stat-types))

(in-package :lisp-stat-types-unittests)


(deftestsuite lisp-stat-ut-types (lisp-stat-ut) ())

(addtest (lisp-stat-ut-types) check-one-nonneg-fixnum1
	 (ensure-error
	   ;; (check-one-nonneg-fixnum -3) => error
	   (check-one-nonneg-fixnum -3)))

(addtest (lisp-stat-ut-types) check-one-nonneg-fixnum2
	 (ensure
	  ;; (check-one-nonneg-fixnum 3) => 3
	  (= 3
	     (check-one-nonneg-fixnum 3))))



;;; check-one-nonneg-fixnum

(addtest (lisp-stat-ut-types) check-nonneg-fixnum1
	 (ensure
	  ;; (check-one-nonneg-fixnum 3) => 3
	  (= 3
	     (check-nonneg-fixnum 3))))

(addtest (lisp-stat-ut-types) check-nonneg-fixnum2
	 (ensure
	  ;; (check-one-nonneg-fixnum 3) => 3
	  (equal (list 1 2 3)
		 (check-nonneg-fixnum (list 1 2 3)))))

;;; check-nonneg-fixnum

(addtest (lisp-stat-ut-types) check-nonneg-fixnum1
	 (ensure
	  (= 3
	     (check-nonneg-fixnum 3))))

(addtest (lisp-stat-ut-types) check-nonneg-fixnum2
	 (ensure
	  (equal (list 1 2 3)
		 (check-nonneg-fixnum (list 1 2 3)))))

(addtest (lisp-stat-ut-types) check-nonneg-fixnum3
	 (ensure-error
	   (check-nonneg-fixnum -3)))

(addtest (lisp-stat-ut-types) check-nonneg-fixnum4
	 (ensure-error
	   (check-nonneg-fixnum (list 1 2 -3))))

;;; 
;; (run-tests :suite 'lisp-stat-ut-lstypes)
;; (describe (run-tests :suite 'lisp-stat-ut-lstypes))
