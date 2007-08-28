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

;;; Some simple tests to see that we're computing the branch cuts
;;; correctly.
;;;
;;; NOTE: the tests assume that the functions for double-float are
;;; computing the values correctly for the branch cuts.  We need to
;;; fix this.

(in-package #:qd)

(defun check-signs (fun arg real-sign imag-sign)
  (let* ((z (funcall fun arg))
	 (x (realpart z))
	 (y (imagpart z)))
    (unless (and (= (float-sign x) real-sign)
		 (= (float-sign y) imag-sign))
      (format t "Sign of result doesn't match expected signs~%~
                 ~& fun = ~A~
                 ~& arg = ~A~
                 ~& res = ~A~
                 ~& expected = ~A ~A~%"
	      fun arg z real-sign imag-sign))))

(defun get-signs (z)
  (values (float-sign (realpart z))
	  (float-sign (imagpart z))))

;; asin branch cut is the real axis |x| > 1.  For x < -1, it is
;; continuous with quadrant II; for x > 1, continuous with quadrant
;; IV.
(defun test-asin ()
  ;; Check x < -1
  (multiple-value-bind (tr ti)
      (get-signs (asin #c(-2d0 +1d-20)))
    (check-signs #'asin -2d0 tr ti)
    (check-signs #'asin -2w0 tr ti)
    (check-signs #'asin #q-2 tr ti)
    (check-signs #'asin #c(-2d0 0d0) tr ti)
    (check-signs #'asin #c(-2w0 0w0) tr ti)
    (check-signs #'asin #q(-2 0) tr ti)
    (check-signs #'asin #c(-2d0 -0d0) tr (- ti))
    (check-signs #'asin #c(-2w0 -0w0) tr (- ti))
    (check-signs #'asin #q(-2 #q-0q0) tr (- ti))
    )

  ;; Check x > 1
  (multiple-value-bind (tr ti)
      (get-signs (asin #c(2d0 -1d-20)))
    (check-signs #'asin 2d0 tr ti)
    (check-signs #'asin 2w0 tr ti)
    (check-signs #'asin #q2 tr ti)
    (check-signs #'asin #c(2d0 -0d0) tr ti)
    (check-signs #'asin #c(2w0 -0w0) tr ti)
    (check-signs #'asin #q(2 #q-0q0) tr ti)))

;; acos branch cut is the real axis, |x| > 1.  For x < -1, it is
;; continuous with quadrant II; for x > 1, quadrant IV.
(defun test-acos ()
  ;; Check x < -1
  (multiple-value-bind (tr ti)
      (get-signs (acos #c(-2d0 +1d-20)))
    (check-signs #'acos -2d0 tr ti)
    (check-signs #'acos -2w0 tr ti)
    (check-signs #'acos #q-2 tr ti))

  ;; Check x > 1
  (multiple-value-bind (tr ti)
      (get-signs (acos #c(2d0 -1d-20)))
    (check-signs #'acos 2d0 tr ti)
    (check-signs #'acos 2w0 tr ti)
    (check-signs #'acos #q2 tr ti)))


;; atan branch cut is the imaginary axis, |y| > 1.  For y < -1, it is
;; continuous with quadrant IV; for x > 1, quadrant II.
(defun test-atan ()
  ;; Check y < -1
  (multiple-value-bind (tr ti)
      (get-signs (atan #c(1d-20 -2d0)))
    (check-signs #'atan #c(0d0 -2d0) tr ti)
    (check-signs #'atan #c(0w0 -2w0) tr ti)
    (check-signs #'atan #q(#q0 #q-2) tr ti))

  ;; Check y > 1
  (multiple-value-bind (tr ti)
      (get-signs (atan #c(-1d-20 2d0)))
    (check-signs #'atan #c(-0d0 2d0) tr ti)
    (check-signs #'atan #c(-0w0 2w0) tr ti)
    (check-signs #'atan #q(#q-0 2) tr ti)))


(defun test-atanh ()
  ;; Check x < -1
  (multiple-value-bind (tr ti)
      (get-signs (atanh #c(-2d0 -1d-20)))
    (check-signs #'atanh -2d0 tr ti)
    (check-signs #'atanh -2w0 tr ti)
    (check-signs #'atanh #q-2 tr ti))

  ;; Check x > 1
  (multiple-value-bind (tr ti)
      (get-signs (atanh #c(2d0 1d-20)))
    (check-signs #'atanh 2d0 tr ti)
    (check-signs #'atanh 2w0 tr ti)
    (check-signs #'atanh #q2 tr ti)))



