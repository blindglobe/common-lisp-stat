;;; -*- mode: lisp -*-

;;; Time-stamp: <2009-07-18 11:50:00 tony>
;;; Creation:   <2008-09-08 08:06:30 tony>
;;; File:       TODO.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c) 2007-2008, AJ Rossini <blindglobe@gmail.com>.  BSD.
;;; Purpose: Stuff that needs to be made working sits inside the
;;;          progns... This file contains the current challenges to
;;;          solve, including a description of the setup and the work
;;;          to solve....
 
;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

;;; SET UP

(in-package :cl-user)

(defun init-CLS ()
  (asdf:oos 'asdf:load-op 'cls)
  (asdf:oos 'asdf:load-op 'cl-pdf)
  (asdf:oos 'asdf:load-op 'cl-typesetting))

(defun init-CLS-graphics ()
  (init-CLS)
  (asdf:oos 'asdf:load-op 'cl-cairo2-x11)
  (asdf:oos 'asdf:load-op 'cl-2d))

(init-CLS-graphics)

;;(asdf:oos 'asdf:load-op 'lisp-matrix)
;;(asdf:oos 'asdf:compile-op 'cls :force t)
;;(asdf:oos 'asdf:load-op 'cls)


;;(asdf:oos 'asdf:load-op 'cl-pdf)
;;(asdf:oos 'asdf:load-op 'cl-typesetting)

(in-package :lisp-stat-unittests)

;; tests = 80, failures = 8, errors = 15
(run-tests :suite 'lisp-stat-ut)
(describe (run-tests :suite 'lisp-stat-ut))

(describe 'lisp-stat-ut)
(documentation 'lisp-stat-ut 'type)

;; FIXME: Example: currently not relevant, yet
;;   (describe (lift::run-test :test-case  'lisp-stat-unittests::create-proto
;;                             :suite 'lisp-stat-unittests::lisp-stat-ut-proto))

(describe (lift::run-tests :suite 'lisp-stat-ut-dataframe))
(lift::run-tests :suite 'lisp-stat-ut-dataframe)

(describe (lift::run-test
	   :test-case  'lisp-stat-unittests::create-proto
	   :suite 'lisp-stat-unittests::lisp-stat-ut-proto))

(in-package :ls-user)

;;; Tasks to do and consider: 
;;;
;;; * must contemplate data as being separate from object system,
;;; working out a new package dependency system.
;;;
;;; * 

#+nil(progn
  ;; REVIEW: general Lisp use guidance

  (fdefinition 'make-matrix)
  (documentation 'make-matrix 'function)

#| Examples from CLHS, a bit of guidance.

  ;; This function assumes its callers have checked the types of the
  ;; arguments, and authorizes the compiler to build in that assumption.
  (defun discriminant (a b c)
   (declare (number a b c))
   "Compute the discriminant for a quadratic equation."
   (- (* b b) (* 4 a c))) =>  DISCRIMINANT
  (discriminant 1 2/3 -2) =>  76/9

  ;; This function assumes its callers have not checked the types of the
  ;; arguments, and performs explicit type checks before making any assumptions. 
  (defun careful-discriminant (a b c)
    "Compute the discriminant for a quadratic equation."
    (check-type a number)
    (check-type b number)
    (check-type c number)
    (locally (declare (number a b c))
      (- (* b b) (* 4 a c)))) =>  CAREFUL-DISCRIMINANT
  (careful-discriminant 1 2/3 -2) =>  76/9
|#
  )


#+nil
(progn
  (asdf:oos 'asdf:load-op 'versioned-objects)
  (asdf:oos 'asdf:load-op 'validations))




#|
  (with-data dataset ((dsvarname1 [usevarname1])
                      (dsvarname2 [usevarname2]))
      @body)
|#



#|
 (defun testme (&key (a 3) (b (+ a 3)))
   b)
 
 (testme)
 (testme :a 2)
 (testme :b 4)
 (testme :a 2 :b (* a 5))
|#

