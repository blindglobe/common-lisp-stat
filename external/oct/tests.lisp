;;;; -*- Mode: lisp -*-
;;;;
;;;; Copyright (c) 2007 Raymond Toy
;;;;
;;;; Permission is hereby granted, free of charge, to any person
;;;; obtaining a copy of this software and associated documentation
;;;; files (the "Software"), to deal in the Software without
;;;; restriction, including without limitation the rights to use,
;;;; copy, modify, merge, publish, distribute, sublicense, and/or sell
;;;; copies of the Software, and to permit persons to whom the
;;;; Software is furnished to do so, subject to the following
;;;; conditions:
;;;;
;;;; The above copyright notice and this permission notice shall be
;;;; included in all copies or substantial portions of the Software.
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;;; OTHER DEALINGS IN THE SOFTWARE.


(in-package #:qd)

(defun bit-accuracy (est true)
  (let* ((diff (abs (- est true)))
	 (err (float (if (zerop true)
			 diff
			 (/ diff (abs true)))
		     1d0)))
    (if (zerop diff)
	t
	(- (log err 2)))))

(defun print-result (est true)
  (format t "est: ~A~%" est)
  (format t "tru: ~A~%" true)
  (format t "err: ~A~%" (float (- est true) 1d0))
  (format t "bits: ~,1f~%" (bit-accuracy est true)))

(defconstant +e+
  (make-instance 'qd-real :value qdi::+qd-e+))

(defconstant +log2+
  (make-instance 'qd-real :value qdi::+qd-log2+))
  
(defun test2 ()
  ;; pi/4 = 4 * arctan(1/5) - arctan(1/239)
  ;;
  ;; Arctan is computed using the Taylor series
  ;;
  ;;   arctan(x) = x - x^3/3 + x^5/5 - x^7/7
  (flet ((atan-series (x)
	   (let* ((d 1d0)
		  (eps (float (scale-float 1d0 -212) #q1))
		  (tmp x)
		  (r (* tmp tmp))
		  (s1 #q0)
		  (k 0)
		  (sign 1))
	     (loop while (> tmp eps) do
		   (incf k)
		   (setf s1
			 (if (minusp sign)
			     (- s1 (/ tmp d))
			     (+ s1 (/ tmp d))))
		   (incf d 2d0)
		   (setf tmp (* tmp r))
		   (setf sign (- sign)))
	     s1)))
    (let* ((x1 (/ #q1 5))
	   (s1 (atan-series x1))
	   (x2 (/ #q1 239))
	   (s2 (atan-series x2))
	   (p (* (- (* s1 4)
		    s2)
		 4)))
      (format t "~2&pi via Machin's atan formula~%")
      (print-result p +pi+)
      p)))

(defun test3 ()
  (declare (optimize (speed 3)))
  ;; Salamin-Brent Quadratic formula for pi
  (let* ((a #q1)
	 (b (sqrt #q.5))
	 (s #q.5)
	 (m 1d0)
	 (p (/ (* (* a a)
		  2d0)
	       s)))
    (declare (double-float m))
    (dotimes (k 9)
      (setf m (* 2 m))
      (let* ((a-new (* (+ a b) .5d0))
	     (b-new (sqrt (* a b)))
	     (s-new (- s
		       (* (- (* a-new a-new)
			     (* b-new b-new))
			  m))))
	(setf a a-new)
	(setf b b-new)
	(setf s s-new)
	(setf p (/ (* (* a a) 2d0)
			s))))
    (format t "~2&Salamin-Brent Quadratic formula for pi~%")
    (print-result p +pi+)
    p))

(defun test4 ()
  (declare (optimize (speed 3)))
  ;; Borwein Quartic formula for pi
  (let* ((a (- 6
	       (* (sqrt #q2)
		  4)))
	 (y (- (sqrt #q2)
	       1))
	 (m 2d0)
	 (p (/ a)))
    (declare (double-float m))
    (dotimes (k 9)
      (setf m (* 4 m))
      (let ((r (expt (- 1 (expt y 4))
		     1/4)))
	(setf y (/ (- 1d0 r)
		   (+ 1d0 r)))
	(setf a (- (* a
		      (expt (+ y 1d0) 4))
		   (* (* y
			 (+ (+ y (expt y 2))
			    1d0))
		      m)))
	(setf p (/ a))))
    (format t "~2&Borwein's Quartic formula for pi~%")
    (print-result p +pi+)
    p))

(defun test5 ()
  ;; Taylor series for e
  (let ((s #q2)
	(tmp #q1)
	(n 1d0)
	(delta 0d0)
	(i 0))
    (loop while (> tmp 1d-100) do
	  (incf i)
	  (incf n)
	  (setf tmp (/ tmp n))
	  (setf s (+ s tmp)))
    (format t "~2&e via Taylor series~%")
    (print-result s +e+)
    s))

(defun test6 ()
  ;; Taylor series for log 2
  ;;
  ;; -log(1-x) = x + x^2/2 + x^3/3 + x^4/4 + ...
  ;;
  ;; with x = 1/2 to get log(1/2) = -log(2)
  (let ((s #q.5)
	(tt #q.5)
	(n 1d0)
	(i 0))
    (loop while (> tt 1d-100) do
	  (incf i)
	  (incf n)
	  (setf tt (* tt .5d0))
	  (setf s (+ s
		     (/ tt n))))
    (format t "~2&log(2) via Taylor series~%")
    (print-result s +log2+)
    s))

(defun test-atan ()
  (let* ((arg (/ (sqrt #q3)))
	 (y (/ (atan arg) +pi+))
	 (true (/ #q6)))
    (format t "~2&atan for special args~%")
    (format t "atan(1/sqrt(3))/pi = 1/6~%")
    (print-result y true))
  ;; atan(sqrt(3)) = %pi/3
  (let* ((arg (sqrt #q3))
	 (y (/ (atan arg) +pi+))
	 (true (/ #q3)))
    (format t "atan(sqrt(3))/pi = 1/3~%")
    (print-result y true))
  ;; atan(1) = %pi/4
  (let* ((arg #q1)
	 (y (/ (atan arg) +pi+))
	 (true (/ #q4)))
    (format t "atan(1)/pi = 1/4~%")
    (print-result y true))
  (let* ((arg #q1q100)
	 (y (/ (atan arg) +pi+))
	 (true #q.5))
    (format t "atan(1q100)/pi = 1/2~%")
    (print-result y true))
  (let* ((arg #q-1q100)
	 (y (/ (atan arg) +pi+))
	 (true #q-.5))
    (format t "atan(-1q100)/pi = -1/2~%")
    (print-result y true)))

(defun test-sin ()
  (format t "~2&sin for special args~%")
  (let* ((arg (/ +pi+ 6))
	 (y (sin arg))
	 (true #q.5))
    (format t "sin(pi/6) = 1/2~%")
    (print-result y true))
  (let* ((arg (/ +pi+ 4))
	 (y (sin arg))
	 (true (sqrt #q.5)))
    (format t "sin(pi/4) = 1/sqrt(2)~%")
    (print-result y true))
  (let* ((arg (/ +pi+ 3))
	 (y (sin arg))
	 (true (/ (sqrt #q3) 2)))
    (format t "sin(pi/3) = sqrt(3)/2~%")
    (print-result y true)))

(defun test-tan ()
  (format t "~2&tan for special args~%")
  (let* ((arg (/ +pi+ 6))
	 (y (tan arg))
	 (true (/ (sqrt #q3))))
    (format t"tan(pi/6) = 1/sqrt(3)~%")
    (print-result y true))
  (let* ((arg (/ +pi+ 4))
	 (y (tan arg))
	 (true #q1))
    (format t "tan(pi/4) = 1~%")
    (print-result y true))
  (let* ((arg (/ +pi+ 3))
	 (y (tan arg))
	 (true (sqrt #q3)))
    (format t "tan(pi/3) = sqrt(3)~%")
    (print-result y true)))

(defun test-asin ()
  (format t "~2&asin for special args~%")
  (let* ((arg #q.5)
	 (y (asin arg))
	 (true (/ +pi+ 6)))
    (format t "asin(1/2) = pi/6~%")
    (print-result y true))
  (let* ((arg (sqrt #q.5))
	 (y (asin arg))
	 (true (/ +pi+ 4)))
    (format t "asin(1/sqrt(2) = pi/4~%")
    (print-result y true))
  (let* ((arg (/ (sqrt #q3) 2))
	 (y (asin arg))
	 (true (/ +pi+ 3)))
    (format t "asin(sqrt(3)/2) = pi/3~%")
    (print-result y true)))
    
(defun test-log ()
  (format t "~2&Log for special args~%")
  (let* ((arg #q2)
	 (y (log arg))
	 (true +log2+))
    (format t "log(2)~%")
    (print-result y true))
  (let* ((arg #q10)
	 (y (log arg))
	 (true (make-instance 'qd-real :value qdi::+qd-log10+)))
    (format t "log(10)~%")
    (print-result y true))
  (let* ((arg (+ 1 (scale-float #q1 -80)))
	 (y (log arg))
	 (true #q8.2718061255302767487140834995607996176476940491239977084112840149578911975528492q-25))
    (format t "log(1+2^-80)~%")
    (print-result y true)))

(defun test-sqrt ()
  (format t "~2&Sqrt for special args~%")
  (dolist (f '((#q1q200 #q1q100)
	       (#q1q300 #q1q150)
	       (#q1q308 #q1q154)
	       (#q1q-200 #q1q-100)
	       (#q1q-250 #q1q-125)))
    (destructuring-bind (arg true)
	f
      (let ((y (sqrt arg)))
	(format t "sqrt(~/qdi::qd-format/)~%" (qd-value arg))
	(print-result y true)))))
  
(defun all-tests ()
  (test2)
  (test3)
  (test4)
  (test5)
  (test6)
  (test-atan)
  (test-sin)
  (test-tan)
  (test-asin)
  (test-log)
  (test-sqrt))
