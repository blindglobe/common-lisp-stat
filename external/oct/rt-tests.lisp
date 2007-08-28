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

;; Compute how many bits are the same for two numbers EST and TRUE.
;; Return T if they are identical.
(defun bit-accuracy (est true)
  (let* ((diff (abs (- est true)))
	 (err (float (if (zerop true)
			 diff
			 (/ diff (abs true)))
		     1d0)))
    (if (zerop diff)
	t
	(- (log err 2)))))

(defun check-accuracy (limit est true)
  (let ((bits (bit-accuracy est true)))
    (if (numberp bits)
	(if (< bits limit)
	    (list bits limit est true)))))

(defvar *null* (make-broadcast-stream))

;;; Some simple tests from the Yozo Hida's qd package.

;; Pi via Machin's formula
(rt:deftest oct.pi.machin
    (let* ((*standard-output* *null*)
	   (val (make-instance 'qd-real :value (qdi::test2)))
	   (true qd:+pi+))
      (check-accuracy 213 val true))
  nil)

;; Pi via Salamin-Brent algorithm
(rt:deftest oct.pi.salamin-brent
    (let* ((*standard-output* *null*)
	   (val (make-instance 'qd-real :value (qdi::test3)))
	   (true qd:+pi+))
      (check-accuracy 202 val true))
  nil)

;; Pi via Borweign's Quartic formula
(rt:deftest oct.pi.borweign
    (let* ((*standard-output* *null*)
	   (val (make-instance 'qd-real :value (qdi::test4)))
	   (true qd:+pi+))
      (check-accuracy 211 val true))
  nil)

;; e via Taylor series
(rt:deftest oct.e.taylor
    (let* ((*standard-output* *null*)
	   (val (make-instance 'qd-real :value (qdi::test5)))
	   (true (make-instance 'qd-real :value qdi::+qd-e+)))
      (check-accuracy 212 val true))
  nil)

;; log(2) via Taylor series
(rt:deftest oct.log2.taylor
    (let* ((*standard-output* *null*)
	   (val (make-instance 'qd-real :value (qdi::test6)))
	   (true (make-instance 'qd-real :value qdi::+qd-log2+)))
      (check-accuracy 212 val true))
  nil)

;;; Tests of atan where we know the analytical result
(rt:deftest oct.atan.1
    (let* ((arg (/ (sqrt #q3)))
	   (y (/ (atan arg) +pi+))
	   (true (/ #q6)))
      (check-accuracy 212 y true))
  nil)

(rt:deftest oct.atan.2
    (let* ((arg (sqrt #q3))
	 (y (/ (atan arg) +pi+))
	 (true (/ #q3)))
      (check-accuracy 212 y true))
  nil)

(rt:deftest oct.atan.3
    (let* ((arg #q1)
	 (y (/ (atan arg) +pi+))
	 (true (/ #q4)))
    (check-accuracy 212 y true))
  nil)

(rt:deftest oct.atan.4
    (let* ((arg #q1q100)
	   (y (/ (atan arg) +pi+))
	   (true #q.5))
      (check-accuracy 212 y true))
  nil)

(rt:deftest oct.atan.5
    (let* ((arg #q-1q100)
	 (y (/ (atan arg) +pi+))
	 (true #q-.5))
    (check-accuracy 212 y true))
  nil)


(defun atan-qd/duplication (arg)
  (make-instance 'qd-real
		 :value (qdi::atan-qd/duplication (qd-value arg))))

;;; Tests of atan where we know the analytical result.  Same tests,
;;; but using the atan duplication formula.
(rt:deftest oct.atan/dup.1
    (let* ((arg (/ (sqrt #q3)))
	   (y (/ (atan-qd/duplication arg) +pi+))
	   (true (/ #q6)))
      (check-accuracy 212 y true))
  nil)

(rt:deftest oct.atan/dup.2
    (let* ((arg (sqrt #q3))
	 (y (/ (atan-qd/duplication arg) +pi+))
	 (true (/ #q3)))
      (check-accuracy 212 y true))
  nil)

(rt:deftest oct.atan/dup.3
    (let* ((arg #q1)
	 (y (/ (atan-qd/duplication arg) +pi+))
	 (true (/ #q4)))
    (check-accuracy 212 y true))
  nil)

(rt:deftest oct.atan/dup.4
    (let* ((arg #q1q100)
	   (y (/ (atan-qd/duplication arg) +pi+))
	   (true #q.5))
      (check-accuracy 212 y true))
  nil)

(rt:deftest oct.atan/dup.5
    (let* ((arg #q-1q100)
	 (y (/ (atan-qd/duplication arg) +pi+))
	 (true #q-.5))
    (check-accuracy 212 y true))
  nil)

;;; Tests of atan where we know the analytical result.  Same tests,
;;; but using a CORDIC implementation.
(defun atan-qd/cordic (arg)
  (make-instance 'qd-real
		 :value (qdi::atan-qd/cordic (qd-value arg))))

(rt:deftest oct.atan/cordic.1
    (let* ((arg (/ (sqrt #q3)))
	   (y (/ (atan-qd/cordic arg) +pi+))
	   (true (/ #q6)))
      (check-accuracy 212 y true))
  nil)

(rt:deftest oct.atan/cordic.2
    (let* ((arg (sqrt #q3))
	 (y (/ (atan-qd/cordic arg) +pi+))
	 (true (/ #q3)))
      (check-accuracy 212 y true))
  nil)

(rt:deftest oct.atan/cordic.3
    (let* ((arg #q1)
	 (y (/ (atan-qd/cordic arg) +pi+))
	 (true (/ #q4)))
    (check-accuracy 212 y true))
  nil)

(rt:deftest oct.atan/cordic.4
    (let* ((arg #q1q100)
	   (y (/ (atan-qd/cordic arg) +pi+))
	   (true #q.5))
      (check-accuracy 212 y true))
  nil)

(rt:deftest oct.atan/cordic.5
    (let* ((arg #q-1q100)
	 (y (/ (atan-qd/cordic arg) +pi+))
	 (true #q-.5))
    (check-accuracy 212 y true))
  nil)


;;; Tests of sin where we know the analytical result.
(rt:deftest oct.sin.1
    (let* ((arg (/ +pi+ 6))
	 (y (sin arg))
	 (true #q.5))
    (check-accuracy 212 y true))
  nil)

(rt:deftest oct.sin.2
    (let* ((arg (/ +pi+ 4))
	 (y (sin arg))
	 (true (sqrt #q.5)))
    (check-accuracy 212 y true))
  nil)

(rt:deftest oct.sin.3
    (let* ((arg (/ +pi+ 3))
	 (y (sin arg))
	 (true (/ (sqrt #q3) 2)))
    (check-accuracy 212 y true))
  nil)

;;; Tests of tan where we know the analytical result.
(rt:deftest oct.tan.1
    (let* ((arg (/ +pi+ 6))
	 (y (tan arg))
	 (true (/ (sqrt #q3))))
    (check-accuracy 212 y true))
  nil)

(rt:deftest oct.tan.2
    (let* ((arg (/ +pi+ 4))
	 (y (tan arg))
	 (true #q1))
    (check-accuracy 212 y true))
  nil)

(rt:deftest oct.tan.3
  (let* ((arg (/ +pi+ 3))
	 (y (tan arg))
	 (true (sqrt #q3)))
    (check-accuracy 212 y true))    
  nil)

;;; Tests of tan where we know the analytical result.  Uses CORDIC
;;; algorithm.

(defun tan/cordic (arg)
  (make-instance 'qd-real
		 :value (qdi::tan-qd/cordic (qd-value arg))))

(rt:deftest oct.tan/cordic.1
    (let* ((arg (/ +pi+ 6))
	 (y (tan/cordic arg))
	 (true (/ (sqrt #q3))))
    (check-accuracy 211 y true))
  nil)

(rt:deftest oct.tan/cordic.2
    (let* ((arg (/ +pi+ 4))
	 (y (tan/cordic arg))
	 (true #q1))
    (check-accuracy 211 y true))
  nil)

(rt:deftest oct.tan/cordic.3
  (let* ((arg (/ +pi+ 3))
	 (y (tan/cordic arg))
	 (true (sqrt #q3)))
    (check-accuracy 210 y true))    
  nil)

;;; Tests of asin where we know the analytical result.

(rt:deftest oct.asin.1
    (let* ((arg #q.5)
	 (y (asin arg))
	 (true (/ +pi+ 6)))
    (check-accuracy 212 y true))
  nil)

(rt:deftest oct.asin.2
    (let* ((arg (sqrt #q.5))
	   (y (asin arg))
	   (true (/ +pi+ 4)))
      (check-accuracy 212 y true))
  nil)

(rt:deftest oct.asin.3
    (let* ((arg (/ (sqrt #q3) 2))
	   (y (asin arg))
	   (true (/ +pi+ 3)))
      (check-accuracy 212 y true))
  nil)

;;; Tests of log.

(rt:deftest oct.log.1
    (let* ((arg #q2)
	   (y (log arg))
	   (true (make-instance 'qd-real :value qdi::+qd-log2+)))
      (check-accuracy 212 y true))
  nil)

(rt:deftest oct.log.2
    (let* ((arg #q10)
	 (y (log arg))
	 (true (make-instance 'qd-real :value qdi::+qd-log10+)))
    (check-accuracy 207 y true))
  nil)

(rt:deftest oct.log.3
    (let* ((arg (+ 1 (scale-float #q1 -80)))
	   (y (log arg))
	   (true #q8.2718061255302767487140834995607996176476940491239977084112840149578911975528492q-25))
      (check-accuracy 212 y true))
  nil)

;;; Tests of log using Newton iteration.

(defun log/newton (arg)
  (make-instance 'qd-real
		 :value (qdi::log-qd/newton (qd-value arg))))

(rt:deftest oct.log/newton.1
    (let* ((arg #q2)
	   (y (log/newton arg))
	   (true (make-instance 'qd-real :value qdi::+qd-log2+)))
      (check-accuracy 212 y true))
  nil)

(rt:deftest oct.log/newton.2
    (let* ((arg #q10)
	 (y (log/newton arg))
	 (true (make-instance 'qd-real :value qdi::+qd-log10+)))
    (check-accuracy 207 y true))
  nil)

(rt:deftest oct.log/newton.3
    (let* ((arg (+ 1 (scale-float #q1 -80)))
	   (y (log/newton arg))
	   (true #q8.2718061255302767487140834995607996176476940491239977084112840149578911975528492q-25))
      (check-accuracy 212 y true))
  nil)

;;; Tests of log using AGM.

(defun log/agm (arg)
  (make-instance 'qd-real
		 :value (qdi::log-qd/agm (qd-value arg))))

(rt:deftest oct.log/agm.1
    (let* ((arg #q2)
	   (y (log/agm arg))
	   (true (make-instance 'qd-real :value qdi::+qd-log2+)))
      (check-accuracy 203 y true))
  nil)

(rt:deftest oct.log/agm.2
    (let* ((arg #q10)
	 (y (log/agm arg))
	 (true (make-instance 'qd-real :value qdi::+qd-log10+)))
    (check-accuracy 205 y true))
  nil)

(rt:deftest oct.log/agm.3
    (let* ((arg (+ 1 (scale-float #q1 -80)))
	   (y (log/agm arg))
	   (true #q8.2718061255302767487140834995607996176476940491239977084112840149578911975528492q-25))
      (check-accuracy 123 y true))
  nil)

;;; Tests of log using AGM2, a faster variaton of AGM.

(defun log/agm2 (arg)
  (make-instance 'qd-real
		 :value (qdi::log-qd/agm2 (qd-value arg))))

(rt:deftest oct.log/agm2.1
    (let* ((arg #q2)
	   (y (log/agm2 arg))
	   (true (make-instance 'qd-real :value qdi::+qd-log2+)))
      (check-accuracy 203 y true))
  nil)

(rt:deftest oct.log/agm2.2
    (let* ((arg #q10)
	 (y (log/agm2 arg))
	 (true (make-instance 'qd-real :value qdi::+qd-log10+)))
    (check-accuracy 205 y true))
  nil)

(rt:deftest oct.log/agm2.3
    (let* ((arg (+ 1 (scale-float #q1 -80)))
	   (y (log/agm2 arg))
	   (true #q8.2718061255302767487140834995607996176476940491239977084112840149578911975528492q-25))
      (check-accuracy 123 y true))
  nil)

;;; Tests of log using AGM3, a faster variation of AGM2.
(defun log/agm3 (arg)
  (make-instance 'qd-real
		 :value (qdi::log-qd/agm3 (qd-value arg))))

(rt:deftest oct.log/agm3.1
    (let* ((arg #q2)
	   (y (log/agm3 arg))
	   (true (make-instance 'qd-real :value qdi::+qd-log2+)))
      (check-accuracy 203 y true))
  nil)

(rt:deftest oct.log/agm3.2
    (let* ((arg #q10)
	 (y (log/agm3 arg))
	 (true (make-instance 'qd-real :value qdi::+qd-log10+)))
    (check-accuracy 205 y true))
  nil)

(rt:deftest oct.log/agm3.3
    (let* ((arg (+ 1 (scale-float #q1 -80)))
	   (y (log/agm3 arg))
	   (true #q8.2718061255302767487140834995607996176476940491239977084112840149578911975528492q-25))
      (check-accuracy 123 y true))
  nil)

;;; Tests of sqrt to make sure we overflow or underflow where we
;;; shouldn't.

(rt:deftest oct.sqrt.1
    (let* ((arg #q1q200)
	   (y (sqrt arg))
	   (true #q1q100))
      (check-accuracy 212 y true))
  nil)

(rt:deftest oct.sqrt.2
    (let* ((arg #q1q200)
	   (y (sqrt arg))
	   (true #q1q100))
      (check-accuracy 212 y true))
  nil)

(rt:deftest oct.sqrt.3
    (let* ((arg #q1q300)
	   (y (sqrt arg))
	   (true #q1q150))
      (check-accuracy 212 y true))
  nil)

(rt:deftest oct.sqrt.4
    (let* ((arg #q1q-200)
	   (y (sqrt arg))
	   (true #q1q-100))
      (check-accuracy 212 y true))
  nil)

(rt:deftest oct.sqrt.5
    (let* ((arg #q1q-250)
	   (y (sqrt arg))
	   (true #q1q-125))
      (check-accuracy 212 y true))
  nil)

;;; Tests of log1p(x) = log(1+x), using the duplication formula.

(defun log1p/dup (arg)
  (make-instance 'qd-real
		 :value (qdi::log1p-qd/duplication (qd-value arg))))

(rt:deftest oct.log1p.1
    (let* ((arg #q9)
	   (y (log1p/dup arg))
	   (true #q2.3025850929940456840179914546843642076011014886287729760333279009675726096773525q0))
      (check-accuracy 212 y true))
  nil)

(rt:deftest oct.log1p.2
    (let* ((arg (scale-float #q1 -80))
	   (y (log1p/dup arg))
	   (true #q8.2718061255302767487140834995607996176476940491239977084112840149578911975528492q-25))
      (check-accuracy 212 y true))
  nil)

;;; Tests of expm1(x) = exp(x) - 1, using a Taylor series with
;;; argument reduction.

(defun expm1/series (arg)
  (make-instance 'qd-real
		 :value (qdi::expm1-qd/series (qd-value arg))))

(rt:deftest oct.expm1/series.1
  (let* ((arg #q0)
	 (y (expm1/series arg))
	 (true #q0))
    (check-accuracy 212 y true))
  nil)

(rt:deftest oct.expm1/series.2
  (let* ((arg #q1)
	 (y (expm1/series arg))
	 (true #q1.7182818284590452353602874713526624977572470936999595749669676277240766303535475945713821785251664274274663919320030599218174135966290435729003342952q0))
    (check-accuracy 211 y true))
  nil)

(rt:deftest oct.expm1/series.3
    (let* ((arg (scale-float #q1 -100))
	   (y (expm1/series arg))
	   (true #q7.888609052210118054117285652830973804370994921943802079729680186943164342372119432861876389514693341738324702996270767390039172777809233288470357147q-31))
      (check-accuracy 211 y true))
  nil)

;;; Tests of expm1(x) = exp(x) - 1, using duplication formula.

(defun expm1/dup (arg)
  (make-instance 'qd-real
		 :value (qdi::expm1-qd/duplication (qd-value arg))))


(rt:deftest oct.expm1/dup.1
  (let* ((arg #q0)
	 (y (expm1/dup arg))
	 (true #q0))
    (check-accuracy 212 y true))
  nil)

(rt:deftest oct.expm1/dup.2
  (let* ((arg #q1)
	 (y (expm1/dup arg))
	 (true #q1.7182818284590452353602874713526624977572470936999595749669676277240766303535475945713821785251664274274663919320030599218174135966290435729003342952q0))
    (check-accuracy 211 y true))
  nil)

(rt:deftest oct.expm1/dup.3
    (let* ((arg (scale-float #q1 -100))
	   (y (expm1/dup arg))
	   (true #q7.888609052210118054117285652830973804370994921943802079729680186943164342372119432861876389514693341738324702996270767390039172777809233288470357147q-31))
      (check-accuracy 211 y true))
  nil)
