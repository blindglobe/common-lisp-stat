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

;;; This file contains various possible implementations of some of the
;;; core routines.  These were experiments on faster and/or more
;;; accurate implementations.  The routines inf qd-fun.lisp are the
;;; default, but you can select a different implementation from here
;;; if you want.
;;;
;;; The end of the file also includes some tests of the different
;;; implementations for speed.

(in-package #:qdi)

;; This works but seems rather slow, so we don't even compile it.
#+(or)
(defun exp-qd/newton (a)
  (declare (type %quad-double a))
  ;; Newton iteration
  ;;
  ;; f(x) = log(x) - a
  ;;
  ;; x' = x - (log(x) - a)/(1/x)
  ;;    = x - x*(log(x) - a)
  ;;    = x*(1 + a - log(x))
  (let ((a1 (add-qd-d a 1d0))
	(x (make-qd-d (exp (qd-0 a)))))
    (setf x (mul-qd x (sub-qd a1 (log-qd/agm x))))
    (setf x (mul-qd x (sub-qd a1 (log-qd/agm x))))
    (setf x (mul-qd x (sub-qd a1 (log-qd/agm x))))
    x))

(defun expm1-qd/series (a)
  (declare (type %quad-double a))
  ;; Compute exp(x) - 1.
  ;;
  ;; D(x) = exp(x) - 1
  ;;
  ;; First, write x = s*log(2) + r*k where s is an integer and |r*k| <
  ;; log(2)/2.
  ;;
  ;; Then D(x) = D(s*log(2)+r*k) = 2^s*exp(r*k) - 1
  ;;           = 2^s*(exp(r*k)-1) - 1 + 2^s
  ;;           = 2^s*D(r*k)+2^s-1
  ;; But
  ;; exp(r*k) = exp(r)^k
  ;;          = (D(r) + 1)^k
  ;;
  ;; So
  ;; D(r*k) = (D(r) + 1)^k - 1
  ;;
  ;; For small r, D(r) can be computed using the Taylor series around
  ;; zero.  To compute D(r*k) = (D(r) + 1)^k - 1, we use the binomial
  ;; theorem to expand out the power and to exactly cancel out the -1
  ;; term, which is the source of inaccuracy.
  ;;
  ;; We want to have small r so the Taylor series converges quickly,
  ;; but that means k is large, which means the binomial expansion is
  ;; long.  We need to compromise.  Let use choose k = 8.  Then |r| <
  ;; log(2)/16 = 0.0433.  For this range, the Taylor series converges
  ;; to 212 bits of accuracy with about 28 terms.
  ;;
  ;;
  (flet ((taylor (x)
	   (declare (type %quad-double x))
	   ;; Taylor series for exp(x)-1
	   ;; = x+x^2/2!+x^3/3!+x^4/4!+...
	   ;; = x*(1+x/2!+x^2/3!+x^3/4!+...)
	   (let ((sum +qd-one+)
		 (term +qd-one+))
	     (dotimes (k 28)
	       (setf term (div-qd-d (mul-qd term x) (float (cl:+ k 2) 1d0)))
	       (setf sum (add-qd sum term)))
	     (mul-qd x sum)))
	 (binom (x)
	   (declare (type %quad-double x))
	   ;; (1+x)^8-1
	   ;; = x*(8 + 28*x + 56*x^2 + 70*x^3 + 56*x^4 + 28*x^5 + 8*x^6 + x^7)
	   ;; = x (x (x (x (x (x (x (x + 8) + 28) + 56) + 70) + 56) + 28) + 8)
	   (mul-qd
	    x
	    (add-qd-d
	     (mul-qd x
		     (add-qd-d
		      (mul-qd x
			      (add-qd-d
			       (mul-qd x
				       (add-qd-d
					(mul-qd x
						(add-qd-d
						 (mul-qd x
							 (add-qd-d
							  (mul-qd x
								  (add-qd-d x 8d0))
							  28d0))
						 56d0))
					70d0))
			       56d0))
		      28d0))
	     8d0)))
	 (arg-reduce (x)
	   (declare (type %quad-double x))
	   ;; Write x = s*log(2) + r*k where s is an integer and |r*k|
	   ;; < log(2)/2, and k = 8.
	   (let* ((s (truncate (qd-0 (nint-qd (div-qd a +qd-log2+)))))
		  (r*k (sub-qd x (mul-qd-d +qd-log2+ (float s 1d0))))
		  (r (div-qd-d r*k 8d0)))
	     (values s r))))
    (multiple-value-bind (s r)
	(arg-reduce a)
      (let* ((d (taylor r))
	     (dr (binom d)))
	(add-qd-d (scale-float-qd dr s)
		  (cl:- (scale-float 1d0 s) 1))))))
    
(defun log-qd/newton (a)
  (declare (type %quad-double a))
  ;; The Taylor series for log converges rather slowly.  Hence, this
  ;; routine tries to determine the root of the function
  ;;
  ;; f(x) = exp(x) - a
  ;;
  ;; using Newton iteration.  The iteration is
  ;;
  ;; x' = x - f(x) / f'(x)
  ;;    = x - (1 - a * exp(-x))
  ;;    = x + a * exp(-x) - 1
  ;;
  ;; Two iterations are needed.
  (let ((x (make-qd-d (log (qd-0 a)))))
    (dotimes (k 3)
      (setf x (sub-qd-d (add-qd x (mul-qd a (exp-qd (neg-qd x))))
			1d0)))
    x))


;;(declaim (inline agm-qd))

(defun agm-qd (x y)
  (declare (type %quad-double x y)
	   (optimize (speed 3)))
  (let ((diff (qd-0 (abs-qd (sub-qd x y)))))
    (cond ((< diff +qd-eps+)
	   x)
	  (t
	   (let ((a-mean (div-qd-d (add-qd x y) 2d0))
		 (g-mean (sqrt-qd (mul-qd x y))))
	     (agm-qd a-mean g-mean))))))

#+(or)
(defun agm-qd (x y)
  (declare (type %quad-double x y)
	   (optimize (speed 3) (space 0) (safety 0)))
  (let ((diff (qd-0 (abs-qd (sub-qd x y))))
	(x x)
	(y y))
    (declare (double-float diff))
    (loop while (> diff +qd-eps+)
      do
      (let ((a-mean (scale-float-qd (add-qd x y) -1))
	    (g-mean (sqrt-qd (mul-qd x y))))
	(setf x a-mean)
	(setf y g-mean)
	(setf diff (qd-0 (abs-qd (sub-qd x y))))))
    x))

(defun log-qd/agm (x)
  (declare (type %quad-double x))
  ;; log(x) ~ pi/2/agm(1,4/x)*(1+O(1/x^2))
  ;;
  ;; Need to make x >= 2^(d/2) to get d bits of precision.  We use
  ;;
  ;; log(2^k*x) = k*log(2)+log(x)
  ;;
  ;; to compute log(x).  log(2^k*x) is computed using AGM.
  ;;
  (multiple-value-bind (frac exp)
      (decode-float (qd-0 x))
    (declare (ignore frac))
    (cond ((>= exp 106)
	   ;; Big enough to use AGM
	   (div-qd +qd-pi/2+
		   (agm-qd +qd-one+
			   (div-qd (make-qd-d 4d0)
				   x))))
	  (t
	   ;; log(x) = log(2^k*x) - k * log(2)
	   (let* ((k (cl:- 107 exp))
		  (big-x (scale-float-qd x k)))
	     ;; Compute k*log(2) using extra precision by writing
	     ;; log(2) = a + b, where a is the quad-double
	     ;; approximation and b the rest.
	     (sub-qd (log-qd/agm big-x)
		     (add-qd (mul-qd-d +qd-log2+ (float k 1d0))
			     (mul-qd-d +qd-log2-extra+ (float k 1d0)))))))))

(defun log-qd/agm2 (x)
  (declare (type %quad-double x))
  ;; log(x) ~ pi/4/agm(theta2(q^4)^2,theta3(q^4)^2)
  ;;
  ;; where q = 1/x
  ;;
  ;; Need to make x >= 2^(d/36) to get d bits of precision.  We use
  ;;
  ;; log(2^k*x) = k*log(2)+log(x)
  ;;
  ;; to compute log(x).  log(2^k*x) is computed using AGM.
  ;;
  (multiple-value-bind (frac exp)
      (decode-float (qd-0 x))
    (declare (ignore frac))
    (cond ((>= exp 7)
	   ;; Big enough to use AGM (because d = 212 so x >= 2^5.8888)
	   (let* ((q (div-qd +qd-one+
			     x))
		  (q^4 (npow q 4))
		  (q^8 (sqr-qd q^4))
		  ;; theta2(q^4) = 2*q*(1+q^8+q^24)
		  ;;             = 2*q*(1+q^8+(q^8)^3)
		  (theta2 (mul-qd-d
			   (mul-qd
			    q
			    (add-qd-d
			     (add-qd q^8
				     (npow q^8 3))
			     1d0))
			   2d0))
		  ;; theta3(q^4) = 1+2*(q^4+q^16)
		  ;;             = 1+2*(q^4+(q^4)^4)
		  (theta3 (add-qd-d
			   (mul-qd-d
			    (add-qd q^4
				    (npow q^4 4))
			    2d0)
			   1d0)))
	     (div-qd +qd-pi/4+
		     (agm-qd (sqr-qd theta2)
			     (sqr-qd theta3)))))
	  (t
	   ;; log(x) = log(2^k*x) - k * log(2)
	   (let* ((k (cl:- 7 exp))
		  (big-x (scale-float-qd x k)))
	     (sub-qd (log-qd/agm2 big-x)
		     (add-qd (mul-qd-d +qd-log2+ (float k 1d0))
			     (mul-qd-d +qd-log2-extra+ (float k 1d0)))))))))

(defun log-qd/agm3 (x)
  (declare (type %quad-double x))
  ;; log(x) ~ pi/4/agm(theta2(q^4)^2,theta3(q^4)^2)
  ;;
  ;; where q = 1/x
  ;;
  ;; Need to make x >= 2^(d/36) to get d bits of precision.  We use
  ;;
  ;; log(2^k*x) = k*log(2)+log(x)
  ;;
  ;; to compute log(x).  log(2^k*x) is computed using AGM.
  ;;
  (multiple-value-bind (frac exp)
      (decode-float (qd-0 x))
    (declare (ignore frac))
    (cond ((>= exp 7)
	   ;; Big enough to use AGM (because d = 212 so x >= 2^5.8888)
	   (let* ((q (div-qd +qd-one+
			     x))
		  (q^4 (npow q 4))
		  (q^8 (sqr-qd q^4))
		  ;; theta2(q^4) = 2*q*(1+q^8+q^24)
		  ;;             = 2*q*(1+q^8+(q^8)^3)
		  (theta2 (mul-qd-d
			   (mul-qd
			    q
			    (add-qd-d
			     (add-qd q^8
				     (npow q^8 3))
			     1d0))
			   2d0))
		  ;; theta3(q^4) = 1+2*(q^4+q^16)
		  ;;             = 1+2*(q^4+(q^4)^4)
		  (theta3 (add-qd-d
			   (mul-qd-d
			    (add-qd q^4
				    (npow q^4 4))
			    2d0)
			   1d0)))
	     ;; Note that agm(theta2^2,theta3^2) = agm(2*theta2*theta3,theta2^2+theta3^2)/2
	     (div-qd +qd-pi/4+
		     (scale-float-qd
		      (agm-qd (scale-float-qd (mul-qd theta2 theta3) 1)
			      (add-qd (sqr-qd theta2)
				      (sqr-qd theta3)))
		      -1))))
	  (t
	   ;; log(x) = log(2^k*x) - k * log(2)
	   (let* ((k (cl:- 7 exp))
		  (big-x (scale-float-qd x k)))
	     (sub-qd (log-qd/agm3 big-x)
		     (add-qd
		      (mul-qd-d +qd-log2+ (float k 1d0))
		      (mul-qd-d +qd-log2-extra+ (float k 1d0)))))))))

#+(or)
(defun atan-d (y x)
  (let* ((r (abs (complex x y)))
	 (xx (cl:/ x r))
	 (yy (cl:/ y r)))
    (let ((z (atan (float y 1f0) (float x 1f0)))
	  (sinz 0d0)
	  (cosz 0d0))
      (format t "z = ~A~%" z)
      (cond ((> xx yy)
	     (format t "xx > yy~%")
	     (dotimes (k 5)
	       (let* ((sinz (sin z))
		      (cosz (cos z))
		      (delta (cl:/ (cl:- yy sinz)
				   cosz)))
		 (format t "sz, dz = ~A ~A~%" sinz cosz)
		 (format t "delta  = ~A~%" delta)
		 (setf z (cl:+ z delta))
		 (format t "z = ~A~%" z))))
	    (t
	     (dotimes (k 20)
	       (let ((sinz (sin z))
		     (cosz (cos z)))
		 (format t "sz, dz = ~A ~A~%" sinz cosz)
		 
		 (setf z (cl:- z (cl:/ (cl:- xx cosz)
				       sinz)))
		 (format t "z = ~A~%" z)))))
      z)))

#||
(defvar *table*)
(defvar *ttable*)
(defvar *cordic-scale*)

#+nil
(defun setup-cordic ()
  (let ((table (make-array 34))
	(ttable (make-array 34)))
    (setf (aref table 0) 1d0)
    (setf (aref table 1) 1d0)
    (setf (aref table 2) 1d0)
    (setf (aref ttable 0) (cl:/ pi 4))
    (setf (aref ttable 1) (cl:/ pi 4))
    (setf (aref ttable 2) (cl:/ pi 4))
    (loop for k from 3 below 34 do
	 (setf (aref table k) (cl:* 0.5d0 (aref table (cl:1- k))))
	 (setf (aref ttable k) (atan (aref table k))))
    (setf *table* table)
    (setf *ttable* ttable)))

(defun setup-cordic ()
  (let ((table (make-array 34))
	(ttable (make-array 34)))
    (setf (aref table 0) 4d0)
    (setf (aref table 1) 2d0)
    (setf (aref table 2) 1d0)
    (setf (aref ttable 0) (atan 4d0))
    (setf (aref ttable 1) (atan 2d0))
    (setf (aref ttable 2) (cl:/ pi 4))
    (loop for k from 3 below 34 do
	 (setf (aref table k) (cl:* 0.5d0 (aref table (cl:1- k))))
	 (setf (aref ttable k) (atan (aref table k))))
    (setf *table* table)
    (setf *ttable* ttable)))

(defun setup-cordic ()
  (let ((table (make-array 34))
	(ttable (make-array 34))
	(scale 1d0))
    (loop for k from 0 below 34 do
	 (setf (aref table k) (scale-float 1d0 (cl:- 2 k)))
	 (setf (aref ttable k) (atan (aref table k)))
	 (setf scale (cl:* scale (cos (aref ttable k)))))
    (setf *table* table)
    (setf *ttable* ttable)
    (setf *cordic-scale* scale)))


(defun cordic-rot (x y)
  (let ((z 0))
    (dotimes (k (length *table*))
      (cond ((plusp y)
	     (psetq x (cl:+ x (cl:* y (aref *table* k)))
		    y (cl:- y (cl:* x (aref *table* k))))
	     (incf z (aref *ttable* k)))
	    (t
	     (psetq x (cl:- x (cl:* y (aref *table* k)))
		    y (cl:+ y (cl:* x (aref *table* k))))
	     (decf z (aref *ttable* k)))
	    ))
    (values z x y)))

(defun cordic-vec (z)
  (let ((x 1d0)
	(y 0d0)
	(scale 1d0))
    (dotimes (k 12 (length *table*))
      (setf scale (cl:* scale (cos (aref *ttable* k))))
      (cond ((minusp z)
	     (psetq x (cl:+ x (cl:* y (aref *table* k)))
		    y (cl:- y (cl:* x (aref *table* k))))
	     (incf z (aref *ttable* k)))
	    (t
	     (psetq x (cl:- x (cl:* y (aref *table* k)))
		    y (cl:+ y (cl:* x (aref *table* k))))
	     (decf z (aref *ttable* k)))
	    ))
    (values x y z scale)))

(defun atan2-d (y x)
  (multiple-value-bind (z dx dy)
      (cordic-rot x y)
    (let ((theta (cl:/ dy dx)))
      (format t "theta = ~A~%" theta)
      (let ((corr (cl:+ theta
		     (cl:- (cl:/ (expt theta 3)
			   3))
		     (cl:/ (expt theta 5)
			5))))
	(format t "corr = ~A~%" corr)
	(cl:+ z corr)))))

(defun tan-d (r)
  (multiple-value-bind (x y z)
      (cordic-vec r)
    (setf x (cl:* x *cordic-scale*))
    (setf y (cl:* y *cordic-scale*))
    (format t "x = ~A~%" x)
    (format t "y = ~A~%" y)
    (format t "z = ~A~%" z)
    ;; Need to finish of the rotation
    (let ((st (sin z))
	  (ct (cos z)))
      (format t "st, ct = ~A ~A~%" st ct)
      (psetq x (cl:- (cl:* x ct) (cl:* y st))
	     y (cl:+ (cl:* y ct) (cl:* x st)))
      (format t "x = ~A~%" x)
      (format t "y = ~A~%" y)
      (cl:/ y x)
      )))

(defun sin-d (r)
  (declare (type double-float r))
  (multiple-value-bind (x y z s)
      (cordic-vec r)
    
    ;; Need to finish the rotation
    (let ((st (sin z))
	  (ct (cos z)))
      (psetq x (cl:- (cl:* x ct) (cl:* y st))
	     y (cl:+ (cl:* y ct) (cl:* x st)))
      (cl:* s y))))
||#

;; This is the basic CORDIC rotation.  Based on code from
;; http://www.voidware.com/cordic.htm and
;; http://www.dspcsp.com/progs/cordic.c.txt.
;;
;; The only difference between this version and the typical CORDIC
;; implementation is that the first 3 rotations are all by pi/4.  This
;; makes sense.  If the angle is greater than pi/4, the rotations will
;; reduce it to at most pi/4.  If the angle is less than pi/4, the 3
;; rotations by pi/4 will cause us to end back at the same place.
;; (Should we try to be smarter?)
(defun cordic-rot-qd (x y)
  (declare (type %quad-double y x)
	   (optimize (speed 3)))
  (let* ((zero +qd-zero+)
	 (z zero))
    (declare (type %quad-double zero z))
    (dotimes (k (length +atan-table+))
      (declare (fixnum k))
      (cond ((qd-> y zero)
	     (psetq x (add-qd x (mul-qd-d y (aref +atan-power-table+ k)))
		    y (sub-qd y (mul-qd-d x (aref +atan-power-table+ k))))
	     (setf z (add-qd z (aref +atan-table+ k))))
	    (t
	     (psetq x (sub-qd x (mul-qd-d y (aref +atan-power-table+ k)))
		    y (add-qd y (mul-qd-d x (aref +atan-power-table+ k))))
	     (setf z (sub-qd z (aref +atan-table+ k))))))
    (values z x y)))

(defun atan2-qd/cordic (y x)
  (declare (type %quad-double y x))
  ;; Use the CORDIC rotation to get us to a small angle.  Then use the
  ;; Taylor series for atan to finish the computation.
  (multiple-value-bind (z dx dy)
      (cordic-rot-qd x y)
    ;; Use Taylor series to finish off the computation
    (let* ((arg (div-qd dy dx))
	   (sq (neg-qd (sqr-qd arg)))
	   (sum +qd-one+))
      ;; atan(x) = x - x^3/3 + x^5/5 - ...
      ;;         = x*(1-x^2/3+x^4/5-x^6/7+...)
      (do ((k 3d0 (cl:+ k 2d0))
	   (term sq))
	  ((< (abs (qd-0 term)) +qd-eps+))
	(setf sum (add-qd sum (div-qd-d term k)))
	(setf term (mul-qd term sq)))
      (setf sum (mul-qd arg sum))
      (add-qd z sum))))

(defun atan-qd/cordic (y)
  (declare (type %quad-double y))
  (atan2-qd/cordic y +qd-one+))

(defun atan-qd/duplication (y)
  (declare (type %quad-double y)
	   (optimize (speed 3) (space 0)))
  (cond ((< (abs (qd-0 y)) 1d-4)
	 ;; Series
	 (let* ((arg y)
		(sq (neg-qd (sqr-qd arg)))
		(sum +qd-one+))
	   ;; atan(x) = x - x^3/3 + x^5/5 - ...
	   ;;         = x*(1-x^2/3+x^4/5-x^6/7+...)
	   (do ((k 3d0 (cl:+ k 2d0))
		(term sq))
	       ((< (abs (qd-0 term)) +qd-eps+))
	     (setf sum (add-qd sum (div-qd-d term k)))
	     (setf term (mul-qd term sq)))
	   (mul-qd arg sum)))
	(t
	 ;; atan(x) = 2*atan(x/(1 + sqrt(1 + x^2)))
	 (let ((x (div-qd y
			  (add-qd-d (sqrt-qd (add-qd-d (sqr-qd y) 1d0))
				    1d0))))
	   (scale-float-qd (atan-qd/duplication x) 1)))))

(defun cordic-vec-qd (z)
  (declare (type %quad-double z)
	   (optimize (speed 3)))
  (let* ((x +qd-one+)
	 (y +qd-zero+)
	 (zero +qd-zero+))
    (declare (type %quad-double zero x y))
    (dotimes (k 30 (length +atan-table+))
      (declare (fixnum k)
	       (inline mul-qd-d sub-qd add-qd))
      (cond ((qd-> z zero)
	     (psetq x (sub-qd x (mul-qd-d y (aref +atan-power-table+ k)))
		    y (add-qd y (mul-qd-d x (aref +atan-power-table+ k))))
	     (setf z (sub-qd z (aref +atan-table+ k))))
	    (t
	     (psetq x (add-qd x (mul-qd-d y (aref +atan-power-table+ k)))
		    y (sub-qd y (mul-qd-d x (aref +atan-power-table+ k))))
	     (setf z (add-qd z (aref +atan-table+ k))))))
    (values z x y)))

(defun tan-qd/cordic (r)
  (declare (type %quad-double r))
  (multiple-value-bind (z x y)
      (cordic-vec-qd r)
    ;; Need to finish the rotation
    (multiple-value-bind (st ct)
	(sincos-taylor z)
      (psetq x (sub-qd (mul-qd x ct) (mul-qd y st))
	     y (add-qd (mul-qd y ct) (mul-qd x st)))
      (div-qd y x))))


(defun sin-qd/cordic (r)
  (declare (type %quad-double r))
  (multiple-value-bind (z x y)
      (cordic-vec-qd r)
    #+nil
    (progn
      (format t "~&x = ~/qd::qd-format/~%" x)
      (format t "~&y = ~/qd::qd-format/~%" y)
      (format t "~&z = ~/qd::qd-format/~%" z)
      (format t "~&s = ~/qd::qd-format/~%" s))
    ;; Need to finish the rotation
    (multiple-value-bind (st ct)
	(sincos-taylor z)
      #+nil
      (progn
	(format t "~&st = ~/qd::qd-format/~%" st)
	(format t "~&ct = ~/qd::qd-format/~%" ct)
	(format t "~&y  = ~/qd::qd-format/~%" (mul-qd +cordic-scale+ y)))

      (psetq x (sub-qd (mul-qd x ct) (mul-qd y st))
	     y (add-qd (mul-qd y ct) (mul-qd x st)))
      (mul-qd +cordic-scale+ y))))


;; Some timing and consing tests.
;;
;; The tests are run using the following:
;;
;; Sparc:	1.5 GHz Ultrasparc IIIi
;; Sparc2:	450 MHz Ultrasparc II
;; PPC:		1.42 GHz
;; x86:		866 MHz Pentium 3
;; PPC(fma):	1.42 GHz with cmucl with fused-multiply-add double-double.
;;

;; (time-exp #c(2w0 0) 50000)
;;
;; Time			Sparc	PPC	x86	PPC (fma)	Sparc2
;; exp-qd/reduce	2.06	 3.18	10.46	2.76		 6.12
;; expm1-qd/series	8.81	12.24	18.87	3.26		29.0
;; expm1-qd/dup		5.68	 4.34	18.47	3.64		18.78
;;
;; Consing (MB)		Sparc
;; exp-qd/reduce	 45   	 45   	 638   	44.4   		 45
;; expm1-qd/series	519   	519   	1201  	14.8   		519
;; expm1-qd/dup		 32   	 32   	1224   	32.0   		 32
;;
;; Speeds seem to vary quite a bit between architectures.
;;
;; Timing without inlining all the basic functions everywhere.  (That
;; is, :qd-inline is not a feature.)
;;
;; (time-exp #c(2w0 0) 50000)
;;
;; Time			Sparc	PPC	x86	PPC (fma)
;; exp-qd/reduce	 5.83	0.67	10.67	0.98
;; expm1-qd/series	10.65	1.45	21.06	1.35
;; expm1-qd/dup		11.17	1.36	24.01	1.25
;;
;; Consing		Sparc
;; exp-qd/reduce	 638   	 93	 638	 93
;; expm1-qd/series	1203   	120	1201	120
;; expm1-qd/dup		1224   	122	1224	122
;;
;; So inlining speeds things up by a factor of about 3 for sparc,
;; 1.5-4 for ppc.  Strangely, x86 slows down on some but speeds up on
;; others.
(defun time-exp (x n)
  (declare (type %quad-double x)
	   (fixnum n))
  (let ((y +qd-zero+))
    (declare (type %quad-double y))
    #+cmu (gc :full t)
    (format t "exp-qd/reduce~%")
    (time (dotimes (k n)
	    (declare (fixnum k))
	    (setf y (exp-qd/reduce x))))
    #+cmu (gc :full t)
    (format t "expm1-qd/series~%")
    (time (dotimes (k n)
	    (declare (fixnum k))
	    (setf y (expm1-qd/series x))))
    #+cmu (gc :full t)
    (format t "expm1-qd/duplication~%")
    (time (dotimes (k n)
	    (declare (fixnum k))
	    (setf y (expm1-qd/duplication x))))
  
    ))

;; (time-log #c(3w0 0) 50000)
;;
;; Time (s)		Sparc	PPC	x86	PPC (fma)	Sparc2
;; log-qd/newton	7.08	10.23	35.74	8.82		21.77
;; log1p-qd/dup		5.87	 8.41	27.32	6.65		20.73
;; log-qd/agm		6.58	 8.0	27.2	6.87		24.62
;; log-qd/agm2		5.8	 6.93	22.89	6.07		18.44
;; log-qd/agm3		5.45	 6.57	20.97	6.18		20.34
;; log-qd/halley	4.96	 6.8	25.11	7.01		16.13
;;
;; Consing (MB)		Sparc	PPC	x86	PPC (fma)
;; log-qd/newton	150   	150   	2194   	148   		150
;; log1p-qd/dup		 56   	 56   	1564   	 56   		 56
;; log-qd/agm		 81   	 11	1434   	 81		 81
;; log-qd/agm2		 87   	 35   	1184   	 87		 87
;; log-qd/agm3		 82   	 36   	1091   	 81   		 82
;; log-qd/halley	101   	101   	1568   	100		101
;;
;; Based on these results, it's not really clear what is the fastest.
;; But Halley's iteration is probably a good tradeoff for log.
;;
;; However, consider log(1+2^(-100)).  Use log1p as a reference:
;;  7.88860905221011805411728565282475078909313378023665801567590088088481830649115711502410110281q-31
;;
;; We have
;; log-qd
;;  7.88860905221011805411728565282475078909313378023665801567590088088481830649133878797727478488q-31
;; log-agm
;;  7.88860905221011805411728565282514580471135738786455290255431302193794546609432q-31
;; log-agm2
;;  7.88860905221011805411728565282474926980229445866885841995713611460718519856111q-31
;; log-agm3
;;  7.88860905221011805411728565282474926980229445866885841995713611460718519856111q-31
;; log-halley
;;  7.88860905221011805411728565282475078909313378023665801567590088088481830649120253326239452326q-31
;;
;; We can see that the AGM methods are grossly inaccurate, but log-qd
;; and log-halley are quite good.
;;
;; Timing results without inlining everything:
;;
;; Time			Sparc	PPC	x86	PPC (fma)
;; log-qd/newton	21.37	0.87	41.49	0.62
;; log1p-qd/dup		12.58	0.41	31.86	0.28
;; log-qd/agm		 7.17	0.23	34.86	0.16
;; log-qd/agm2		 6.35	0.22	27.53	0.15
;; log-qd/agm3		 7.49	0.17	24.92	0.14
;; log-qd/halley	14.38	0.56	30.2	0.65
;;
;; Consing
;;			Sparc	PPC	x86	PPC (fma)
;; log-qd/newton	2194   	60.7	2194	61
;; log1p-qd/dup		1114   	22.6	1564	23
;; log-qd/agm		 371   	 7.9	1434	 7.9
;; log-qd/agm2		 371   	 7.8	1185	 7.8
;; log-qd/agm3		 373   	 7.8	1091	 7.8
;; log-qd/halley	1554   	42.3	1567	42.3

(defun time-log (x n)
  (declare (type %quad-double x)
	   (fixnum n))
  (let ((y +qd-zero+))
    (declare (type %quad-double y))
    #+cmu (gc :full t)
    (format t "log-qd/newton~%")
    (time (dotimes (k n)
	    (declare (fixnum k))
	    (setf y (log-qd/newton x))))
    #+cmu (gc :full t)
    (format t "log1p-qd/duplication~%")
    (time (dotimes (k n)
	    (declare (fixnum k))
	    (setf y (log1p-qd/duplication x))))
    #+cmu (gc :full t)
    (format t "log-qd/agm~%")
    (time (dotimes (k n)
	    (declare (fixnum k))
	    (setf y (log-qd/agm x))))
  
    #+cmu (gc :full t)
    (format t "log-qd/agm2~%")
    (time (dotimes (k n)
	    (declare (fixnum k))
	    (setf y (log-qd/agm2 x))))
    #+cmu (gc :full t)
    (format t "log-qd/agm3~%")
    (time (dotimes (k n)
	    (declare (fixnum k))
	    (setf y (log-qd/agm3 x))))
    #+cmu (gc :full t)
    (format t "log-qd/halley~%")
    (time (dotimes (k n)
	    (declare (fixnum k))
	    (setf y (log-qd/halley x))))
    ))
  

;; (time-atan2 #c(10w0 0) 10000)
;;
;; Time
;;			PPC	Sparc	x86	PPC (fma)	Sparc2
;; atan2-qd/newton     	2.91	 1.91	 8.06	2.16		7.55
;; atan2-qd/cordic	1.22	 0.89	 6.68	1.43		2.47
;; atan-qd/duplication	2.51	 2.14	 5.63	1.76		5.94
;;
;; Consing
;; atan2-qd/newton     	44.4   	44.4   	481   	44.4   		44.4
;; atan2-qd/cordic	 1.6   	 1.6   	482   	 1.6   		 1.6
;; atan-qd/duplication	17.2   	 6.0   	281   	 6.0		 6.0
;;
;; Don't know why x86 is 10 times slower than sparc/ppc for
;; atan2-qd/newton.  Consing is much more too.  Not enough registers?
;;
;; atan2-qd/cordic is by far the fastest on all archs.
;;
;; Timing results without inlining everything:
;; Time
;;			PPC	Sparc	x86	PPC (fma)
;; atan2-qd/newton     	6.56	 4.48	9.75	6.15
;; atan2-qd/cordic	6.02	 4.24	7.06	5.01
;; atan-qd/duplication	3.28	 1.94	5.72	2.46
;;
;; Consing
;; atan2-qd/newton     	443	441   	482	443
;; atan2-qd/cordic	482	482   	482	482
;; atan-qd/duplication	 87	 81   	281	87
;;

(defun time-atan2 (x n)
  (declare (type %quad-double x)
	   (fixnum n))
  (let ((y +qd-zero+)
	(one +qd-one+))
    #+cmu (gc :full t)
    (format t "atan2-qd/newton~%")
    (time (dotimes (k n)
	    (declare (fixnum k))
	    (setf y (atan2-qd/newton x one))))
    #+cmu (gc :full t)
    (format t "atan2-qd/cordic~%")
    (time (dotimes (k n)
	    (declare (fixnum k))
	    (setf y (atan2-qd/cordic x one))))
    #+cmu (gc :full t)
    (format t "atan-qd/duplication~%")
    (time (dotimes (k n)
	    (declare (fixnum k))
	    (setf y (atan-qd/duplication x))))
    ))
	  
;; (time-tan #c(10w0 0) 10000)
;;
;; Time
;;			PPC	Sparc	x86	PPC (fma)	Sparc2
;; tan-qd/cordic     	2.12	 1.51	 8.26	1.77		4.61
;; tan-qd/sincos	0.68	 0.57	 2.39	0.54		2.56
;;
;; Consing
;; tan-qd/cordic     	23.0   	23.0   	473   	23.0		23.0
;; tan-qd/sincos	14.8   	14.8   	147   	14.8		14.8
;;
;; Don't know why x86 is so much slower for tan-qd/cordic.
;;
;; Without inlining everything
;;			PPC	Sparc	x86	PPC (fma)
;; tan-qd/cordic     	7.72	4.56	17.08	5.96
;; tan-qd/sincos	2.32	1.4	 4.91	1.87
;;
;; Consing
;; tan-qd/cordic     	463	463	472	463
;; tan-qd/sincos	137	136	146	137

(defun time-tan (x n)
  (declare (type %quad-double x)
	   (fixnum n))
  (let ((y +qd-zero+))
    #+cmu (gc :full t)
    (format t "tan-qd/cordic~%")
    (time (dotimes (k n)
	    (declare (fixnum k))
	    (setf y (tan-qd/cordic x))))
    #+cmu (gc :full t)
    (format t "tan-qd/sincos~%")
    (time (dotimes (k n)
	    (declare (fixnum k))
	    (setf y (tan-qd/sincos x))))))
    
