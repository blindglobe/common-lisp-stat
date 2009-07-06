;;; -*- mode: lisp -*-

;;; Time-stamp: <2009-07-06 18:38:42 tony>
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
  (asdf:oos 'asdf:load-op 'lispstat))

(defun init-CLS-graphics ()
  (init-CLS)
  (asdf:oos 'asdf:load-op 'cl-cairo2-x11)
  (asdf:oos 'asdf:load-op 'cl-2d))

(init-CLS-graphics)

;;(asdf:oos 'asdf:load-op 'lisp-matrix)
;;(asdf:oos 'asdf:compile-op 'lispstat :force t)
;;(asdf:oos 'asdf:load-op 'lispstat)

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
(progn
  (asdf:oos 'asdf:load-op 'versioned-objects)
  (asdf:oos 'asdf:load-op 'validations)

  )


;; SETUP FOR PLOT EXAMPLE:


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


  (bind ((#2A((f1 f2) (f3 f4))
	     (split-frame *frame2* (percent 75) (percent 25))))
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
	 (da (plot-simple *frame1* (interval-of 0 10) (interval-of 10 20)
			  :x-title "x" :y-title "y")))
    (draw-symbols da xs ys :weights weights))

  (xlib-image-context-to-png (context *frame1*) "/home/tony/test1.png")
  (xlib-image-context-to-png (context *frame2*) "/home/tony/test2.png")
  )


;;; EXAMPLE FOR DSC2009
  (defparameter *frame2* (as-frame (create-xlib-image-context 400 400)
				  :background-color +white+))

  (bind ((#2A((f1 f2) (f3 f4))
	     (split-frame *frame2* (percent 50) (percent 50))))
	(defparameter *f1* f1)
	(defparameter *f2* f2)
	(defparameter *f3* f3)
	(defparameter *f4* f4))
  (plot-function *f1* #'sin (interval-of 0 2) :x-title "x" :y-title "sin(x)")
  (plot-function *f2* #'cos (interval-of 0 2) :x-title "x" :y-title "cos(x)")
  (plot-function *f3* #'tan (interval-of 0 2) :x-title "x" :y-title "tan(x)")

 (num-sequence :from 0 :to 10 :length 30)

  (let* ((n 500)
	 (xs (num-sequence :from 0 :to 10 :length n))
	 (ys (map 'vector #'(lambda (x) (+ x 8 (random 4.0))) xs))
	 (weights (replicate #'(lambda () (1+ (random 10))) n 'fixnum))
	 (da (plot-simple *f4* (interval-of 0 10) (interval-of 10 20)
			  :x-title "x" :y-title "y")))
    (draw-symbols da xs ys :weights weights))
  (xlib-image-context-to-png (context *f1*) "/home/tony/test1.png")
  (xlib-image-context-to-png (context *frame2*) "/home/tony/test2.png")
  (destroy (context  *frame2*))



;; back to normal application
(in-package :ls-user)



#|
  (with-data dataset ((dsvarname1 [usevarname1])
                      (dsvarname2 [usevarname2]))
      @body)
|#




(defun testme (&key (a 3) (b (+ a 3)))
  b)

(testme)
(testme :a 2)
(testme :b 4)
(testme :a 2 :b (* a 5))
