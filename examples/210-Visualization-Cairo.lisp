;;; -*- mode: lisp -*-

;;; Time-stamp: <2012-10-12 08:54:07 tony>
;;; Creation:   <2009-03-12 17:14:56 tony>
;;; File:       20-visual-2d-cairo.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2009--2012, AJ Rossini.  MIT license.
;;; Purpose:    Example visualizations using cl-2d and cl-cairo2

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

;;; This organization and structure is new to the 21st Century
;;; version..   Think, "21st Century Schizoid Man".

;; SETUP FOR PLOT EXAMPLE:
(in-package :cl-user)

(ql:quickload :cl-cairo2)
(ql:quickload :cl-2d)

(defpackage :cls-2d-user-x11
  ;; do we need all of these for the plotting? not sure about numlib and cairo2?
  (:use :cl :cl-cairo2  :cl-2d :cl-numlib :cl-colors :bind :cls)
  (:shadowing-import-from
   :lisp-stat call-method call-next-method

   expt + - * / ** mod rem abs 1+ 1- log exp sqrt sin cos tan
   asin acos atan sinh cosh tanh asinh acosh atanh float random
   truncate floor ceiling round minusp zerop plusp evenp oddp 
   < <= = /= >= > > ;; complex
   conjugate realpart imagpart phase
   min max logand logior logxor lognot ffloor fceiling
   ftruncate fround signum cis
   ;; ?? where did these come from? <= float imagpart
   )
  ;; (:export objects and data that we want to use post load)
  )

(in-package :cls-2d-user-x11)

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
  (defparameter *frame2* (as-frame (create-xlib-image-context 300 300)
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

;;; End of examples
