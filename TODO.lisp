;;; -*- mode: lisp -*-

;;; Time-stamp: <2009-06-18 18:43:02 tony>
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
  (asdf:oos 'asdf:load-op 'xarray)

  *features*
  *package*

  ;; need to consider CLS features:  CFFI, CL-BLAPACK, LISP-MATRIX, XARRAY
  ;; CL-PLPLOT,  CL-CAIRO2/CL-2D.

  ;; SBCL/CCL -> ...
  ;; CLISP/... -> ...
  )

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


;; SETUP FOR PLOT EXAMPLE:

(asdf:oos 'asdf:load-op 'lispstat)
(asdf:oos 'asdf:load-op 'cl-cairo2-x11)
(asdf:oos 'asdf:load-op 'cl-2d)

(defpackage :cl-2d-user-x11
  (:use :cl :cl-cairo2  :cl-2d :cl-numlib :cl-colors :bind))

(in-package :cl-2d-user-x11)

;; PLOT EXAMPLE
#+nil
(progn


  ;; this is how you create an X11 frame.  If you supply a
  ;; background-color to as-frame, each plot will clear the frame with
  ;; this color.

  (defparameter *frame1* (as-frame (create-xlib-image-context 300 300)
				  :background-color +white+))
  
  ;; or netbook size, picture is similar but on a lower-res display window. 
  (defparameter *frame2* (as-frame (create-xlib-image-context 200 200)
				  :background-color +white+))

  (plot-function *frame1*
		 #'exp (interval-of 0 2)
		 :x-title "x"
		 :y-title "exp(x)")
  
  ;; split the frame, and you can draw on the subframes independently.
  ;; I do this a lot.

  (bind ((#2A((f1 f2) (f3 f4))
	     (split-frame *frame2* (percent 50) (percent 50))))
	(defparameter *f1* f1)
	(defparameter *f2* f2)
	(defparameter *f3* f3)
	(defparameter *f4* f4))

  (plot-function *f1* #'sin (interval-of 0 2) :x-title "x" :y-title "sin(x)")
  (plot-function *f2* #'cos (interval-of 0 2) :x-title "x" :y-title "cos(x)")
  (plot-function *f3* #'tan (interval-of 0 2) :x-title "x" :y-title "tan(x)")
  (plot-function *f4* #'/ (interval-of 0 2) :x-title "x" :y-title "1/x")
  
  (clear *frame1*)


  (let* ((n 500)
	 (xs (num-sequence :from 0 :to 10 :length n))
	 (ys (map 'vector #'(lambda (x) (+ x 8 (random 4.0))) xs))
	 (weights (replicate #'(lambda () (1+ (random 10))) n 'fixnum))
	 (da (plot-simple frame (interval-of 0 10) (interval-of 10 20)
			  :x-title "x" :y-title "y")))
    (draw-symbols da xs ys :weights weights))


  )


;; back to normal application
(in-package :ls-user)


