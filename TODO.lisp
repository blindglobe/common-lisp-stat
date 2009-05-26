;;; -*- mode: lisp -*-

;;; Time-stamp: <2009-05-25 08:11:27 tony>
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
;;(asdf:oos 'asdf:load-op 'lisp-matrix)
;;(asdf:oos 'asdf:compile-op 'lispstat :force t)
;;(asdf:oos 'asdf:load-op 'lispstat)

(in-package :lisp-stat-unittests)

;; tests = 80, failures = 8, errors = 15
(run-tests :suite 'lisp-stat-ut)
(describe (run-tests :suite 'lisp-stat-ut))

;; FIXME: Example: currently not relevant, yet
;;   (describe (lift::run-test :test-case  'lisp-stat-unittests::create-proto
;;                             :suite 'lisp-stat-unittests::lisp-stat-ut-proto))

(describe (lift::run-tests :suite 'lisp-stat-ut-dataframe))
(lift::run-tests :suite 'lisp-stat-ut-dataframe)

(describe 
 (lift::run-test
  :test-case  'lisp-stat-unittests::create-proto
  :suite 'lisp-stat-unittests::lisp-stat-ut-proto))

(describe 'lisp-stat-ut)

(in-package :ls-user)

#+nil
(progn
  ;; Plotting -- need to figure out the core-dump, or change libraries.
  ;; (asdf:oos 'asdf:load-op 'cl-plplot)

  ;; (defparameter *gdev* "xwin")
  (defparameter *gdev* "xcairo")
  ;; (cl-plplot::plsdev *gdev*) ; -- usually handled within call.
  (plot-ex)
  (plot-ex)
  ;; Boom! -- there is currently a loose pointer floating around that
  ;; causes errors the 3rd time that we create a plot (and crashes
  ;; SBCL the 4th time).  Order independent.
  (plot-ex)

  (contour-plot-ex)
  (fn-contour-plot-ex)
  (shade-plot-ex)
  (3D-plot-ex))


(progn
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
(progn ;; experiments with GSL and the Lisp interface.
  (asdf:oos 'asdf:load-op 'gsll)
  (asdf:oos 'asdf:load-op 'gsll-tests) ; requires lisp-unit

  ;; the following should be equivalent
  (defparameter *t1*  (LIST 6.18d0 6.647777777777779d0 6.18d0))
  (defparameter *t2*  (MULTIPLE-VALUE-LIST
	       (LET ((VEC
		      (gsll:make-marray 'DOUBLE-FLOAT
					:INITIAL-CONTENTS '(-3.21d0 1.0d0 12.8d0)))
		     (WEIGHTS
		      (gsll:MAKE-MARRAY 'DOUBLE-FLOAT
					:INITIAL-CONTENTS '(3.0d0 1.0d0 2.0d0))))
		 (LET ((MEAN (gsll:MEAN VEC)))
		   (LIST (gsll:ABSOLUTE-DEVIATION VEC)
			 (gsll:WEIGHTED-ABSOLUTE-DEVIATION VEC WEIGHTS)
			 (gsll:ABSOLUTE-DEVIATION VEC MEAN))))))
  (eql *t1* *t2*)
  (equal *t1* *t2*)

  ;; from (gsll:examples 'gsll::numerical-integration) ...
  (gsll:integration-qng gsll::one-sine 0.0d0 PI)

  (gsll:defun-single axpb (x) (+ (* 2 x) 3)) ;; a<-2, b<-3
  (gsll:integration-qng axpb 1d0 2d0)

  (let ((a 2)
	(b 3))
    (defun-single axpb2 (x) (+ (* a x) b)))
  (gsll:integration-qng axpb2 1d0 2d0)

  ;;   BAD
  ;;   (gsll:integration-qng 
  ;;    (let ((a 2)
  ;; 	 (b 3))
  ;;      (defun-single axpb2 (x) (+ (* a x) b)))
  ;;    1d0 2d0)

  ;; right, but weird expansion...
  (gsll:integration-qng
   (let ((a 2)
	 (b 3))
     (defun axpb2 (x) (+ (* a x) b))
     (gsll:def-single-function axpb2)
     axpb2)
   1d0 2d0)

  ;; Linear least squares

  (gsll:gsl-lookup "gsl_linalg_LU_decomp") ; => gsll:lu-decomposition
  (gsll:gsl-lookup "gsl_linalg_LU_solve") ; => gsll:lu-solve
  )


#+nil
(progn
  (asdf:oos 'asdf:load-op 'versioned-objects)
  (asdf:oos 'asdf:load-op 'validations)

  )
