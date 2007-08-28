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

;;; Basic special functions operating on %quad-double numbers.  This
;;; includes sqrt, rounding to the nearest integer, floor, exp, log,
;;; log1p, sin, cos, tan, asin, acos, atan, atan2, sinh, cosh, tanh,
;;; asinh, acosh, atanh, and random.
;;;
;;; These special functions only work on the main domains where the
;;; argument is real and the result is real.  Behavior is undefined if
;;; this doesn't hold.

(in-package #:qdi)

#+cmu
(declaim (maybe-inline sqrt-qd))
(defun sqrt-qd (a)
  "Square root of the (non-negative) quad-float"
  (declare (type %quad-double a)
	   (optimize (speed 3) (space 0)))
  ;; Perform the following Newton iteration:
  ;;
  ;;  x' = x + (1 - a * x^2) * x / 2
  ;;
  ;; which converges to 1/sqrt(a).
  ;;
  ;; However, there appear to be round-off errors when x is either
  ;; very large or very small.  So let x = f*2^(2*k).  Then sqrt(x) =
  ;; 2^k*sqrt(f), and sqrt(f) doesn't have round-off problems.
  (when (zerop-qd a)
    (return-from sqrt-qd a))
  #+cmu
  (when (float-infinity-p (qd-0 a))
    (return-from sqrt-qd a))

  (let* ((k (logandc2 (logb-finite (qd-0 a)) 1))
	 (new-a (scale-float-qd a (- k))))
    (assert (evenp k))
    (let* ((r (make-qd-d (cl:/ (sqrt (the (double-float (0d0))
				       (qd-0 new-a))))))
	   (half 0.5d0)
	   (h (mul-qd-d new-a half)))
      (declare (type %quad-double r))
      ;; Since we start with double-float precision, three more
      ;; iterations should give us full accuracy.
      (dotimes (k 3)
	(setf r (add-qd r (mul-qd r (sub-d-qd half (mul-qd h (sqr-qd r)))))))
      (scale-float-qd (mul-qd r new-a) (ash k -1)))))

(defun logb-finite (x)
  "Same as logb but X is not infinity and non-zero and not a NaN, so
that we can always return an integer"
  (declare (type cl:float x))
  (multiple-value-bind (signif expon sign)
      (cl:decode-float x)
    (declare (ignore signif sign))
    ;; decode-float is almost right, except that the exponent
    ;; is off by one
    (1- expon)))

(defun hypot-aux-qd (x y)
  (declare (type %quad-double x y))
  (let ((k (- (logb-finite (max (cl:abs (qd-0 x))
				(cl:abs (qd-0 y)))))))
    (values (add-qd (sqr-qd (scale-float-qd x k))
		    (sqr-qd (scale-float-qd y k)))
	    (- k))))

(defun hypot-qd (x y)
  "sqrt(x^2+y^2) computed carefully without unnecessary overflow"
  (multiple-value-bind (abs^2 rho)
      (hypot-aux-qd x y)
    (scale-float-qd (sqrt-qd abs^2) rho)))

(defun nint-qd (a)
  "Round the quad-float to the nearest integer, which is returned as a
  quad-float"
  (let ((x0 (fround (qd-0 a)))
	(x1 0d0)
	(x2 0d0)
	(x3 0d0))
    (cond ((= x0 (qd-0 a))
	   ;; First double is already an integer
	   (setf x1 (fround (qd-1 a)))
	   (cond ((= x1 (qd-1 a))
		  ;; Second is an integer
		  (setf x2 (fround (qd-2 a)))
		  (cond ((= x2 (qd-2 a))
			 ;; Third is an integer
			 (setf x3 (fround (qd-3 a))))
			(t
			 (when (and (zerop (abs (cl:- x2 (qd-2 a))))
				    (minusp (qd-3 a)))
			   (decf x2)))))
		 (t
		  (when (and (zerop (abs (cl:- x1 (qd-1 a))))
			     (minusp (qd-2 a)))
		    (decf x1)))))
	  (t
	   (when (and (zerop (abs (cl:- x0 (qd-0 a))))
		      (minusp (qd-1 a)))
	     (decf x0))))
    (multiple-value-bind (s0 s1 s2 s3)
	(renorm-4 x0 x1 x2 x3)
      (make-qd-d s0 s1 s2 s3))))

(defun ffloor-qd (a)
  "The floor of A, returned as a quad-float"
  (let ((x0 (ffloor (qd-0 a)))
	(x1 0d0)
	(x2 0d0)
	(x3 0d0))
    (cond ((= x0 (qd-0 a))
	   (setf x1 (ffloor (qd-1 a)))
	   (when (= x1 (qd-1 a))
	     (setf x2 (ffloor (qd-2 a)))
	     (when (= x2 (qd-2 a))
	       (setf x3 (ffloor (qd-3 a)))))
	   (make-qd-d x0 x1 x2 x3))
	  (t
	   (%make-qd-d x0 x1 x2 x3)))))
	
			 
(defun exp-qd/reduce (a)
  ;; Strategy:  Reduce the size of x by noting that
  ;;
  ;; exp(k*r+m) = exp(m) * exp(r)^k
  ;;
  ;; Thus, by choosing m to be a multiple of log(2) closest to x, we
  ;; can make |kr| < log(2)/2 = 0.3466.  Now we can set k = 256, so
  ;; that |r| <= 0.00136.
  ;;
  ;; Then
  ;;
  ;; exp(x) = exp(k*r+s*log(2)) = 2^s*(exp(r))^256
  ;;
  ;; We can use Taylor series to evaluate exp(r).

  (let* ((k 256)
	 (z (truncate (qd-0 (nint-qd (div-qd a +qd-log2+)))))
	 (r1 (sub-qd a (mul-qd-d +qd-log2+ (float z 1d0))))
	 ;; r as above
	 (r (div-qd-d (sub-qd a (mul-qd-d +qd-log2+ (float z 1d0)))
		      (float k 1d0)))
	 ;; For Taylor series.  p = r^2/2, the first term
	 (p (div-qd-d (sqr-qd r) 2d0))
	 ;; s = 1+r+p, the sum of the first 3 terms
	 (s (add-qd-d (add-qd r p) 1d0))
	 ;; Denominator of term
	 (m 2d0))
    ;; Taylor series until the term is small enough.
    ;;
    ;; Note that exp(x) = sinh(x) + sqrt(1+sinh(x)^2).  The Taylor
    ;; series for sinh has half as many terms as for exp, so it should
    ;; be less work to compute sinh.  Then a few additional operations
    ;; and a square root gives us exp.
    (loop
       (incf m)
       (setf p (mul-qd p r))
       (setf p (div-qd-d p m))
       (setf s (add-qd s p))
       (unless (> (abs (qd-0 p)) +qd-eps+)
	 (return)))

    (setf r (npow s k))
    (setf r (scale-float-qd r z))
    r))

(defun expm1-qd/duplication (a)
  (declare (type %quad-double a))
  ;; Brent gives expm1(2*x) = expm1(x)*(2+expm1(x))
  ;;
  ;; Hence
  ;;
  ;; expm1(x) = expm1(x/2)*(2+expm1(x/2))
  ;;
  ;; Keep applying this formula until x is small enough.  Then use
  ;; Taylor series to compute expm1(x).
  (cond ((< (abs (qd-0 a)) .0001d0)
	 ;; What is the right threshold?
	 ;;
	 ;; Taylor series for exp(x)-1
	 ;; = x+x^2/2!+x^3/3!+x^4/4!+...
	 ;; = x*(1+x/2!+x^2/3!+x^3/4!+...)
	 (let ((sum +qd-one+)
	       (term +qd-one+))
	   (dotimes (k 28)
	     (setf term (div-qd-d (mul-qd term a) (float (cl:+ k 2) 1d0)))
	     (setf sum (add-qd sum term)))
	   (mul-qd a sum)))
	(t
	 (let ((d (expm1-qd/duplication (scale-float-qd a -1))))
	   (mul-qd d (add-qd-d d 2d0))))))

(defun expm1-qd (a)
  "exp(a) - 1, done accurately"
  (declare (type %quad-double a))
  #+cmu
  (when (float-infinity-p (qd-0 a))
    (return-from expm1-qd
      (if (minusp (float-sign (qd-0 a)))
	  +qd-zero+
	  a)))
  (expm1-qd/duplication a))

(defun exp-qd (a)
  "exp(a)"
  (declare (type %quad-double a))
  ;; Should we try to be more accurate than just 709?
  (when (< (qd-0 a) (log least-positive-normalized-double-float))
    (return-from exp-qd +qd-zero+))

  (when (> (qd-0 a) (log most-positive-double-float))
    #-cmu
    (error 'floating-point-overflow
	   :operation 'exp
	   :operands (list a))
    #+cmu
    (return-from exp-qd (%make-qd-d (/ 1d0 0d0) 0d0 0d0 0d0)))

  (when (zerop-qd a)
    (return-from exp-qd +qd-one+))

  ;; Default for now is exp-qd/reduce
  (exp-qd/reduce a))

(defun log-qd/halley (a)
  (declare (type %quad-double a))
  ;; Halley iteration:
  ;;
  ;; x' = x - 2*(exp(x)-a)/(exp(x)+a)
  ;;
  ;; But the above has problems if a is near
  ;; most-positive-double-float.  Rearrange the computation:
  ;;
  ;; x' = x - 2*(exp(x)/a-1)/(exp(x)/a+1)
  ;;
  ;; I think this works better, but it's also probably a little bit
  ;; more expensive because each iteration has two divisions.
  (let ((x (make-qd-d (log (qd-0 a)))))
    (flet ((iter (est)
	     (let ((exp (div-qd (exp-qd est)
				a)))
	       (sub-qd est
		       (scale-float-qd
			(div-qd (sub-qd-d exp 1d0)
				(add-qd-d exp 1d0))
			1)))))
      ;; Two iterations should be enough
      (setf x (iter x))
      (setf x (iter x))
      x)))
  

(defun log1p-qd/duplication (x)
  (declare (type %quad-double x)
	   (optimize (speed 3)))
  ;; Brent gives the following duplication formula for log1p(x) =
  ;; log(1+x):
  ;;
  ;; log1p(x) = 2*log1p(x/(1+sqrt(1+x)))
  ;;
  ;; So we apply the duplication formula until x is small enough, and
  ;; then use the series
  ;;
  ;; log(1+x) = 2*sum((x/(2+x))^(2*k+1)/(2*k+1),k,0,inf)
  ;;
  ;; Currently "small enough" means x < 0.005.  What is the right
  ;; cutoff?
  (cond ((> (abs (qd-0 x)) .005d0)
	 ;; log1p(x) = 2*log1p(x/(1+sqrt(1+x)))
	 (mul-qd-d (log1p-qd/duplication
		    (div-qd x
			    (add-d-qd 1d0
				      (sqrt-qd (add-d-qd 1d0 x)))))
		   2d0))
	(t
	 ;; Use the series
	 (let* ((term (div-qd x (add-qd-d x 2d0)))
		(mult (sqr-qd term))
		(sum term))
	   (loop for k of-type double-float from 3d0 by 2d0
	      while (> (abs (qd-0 term)) +qd-eps+)
	      do
		(setf term (mul-qd term mult))
		(setf sum (add-qd sum (div-qd-d term k))))
	   (mul-qd-d sum 2d0)))))

(defun log1p-qd (x)
  "log1p(x) = log(1+x), done more accurately than just evaluating
  log(1+x)"
  (declare (type %quad-double x))
  #+cmu
  (when (float-infinity-p (qd-0 x))
    x)
  (log1p-qd/duplication x))

(defun log-qd (a)
  "Log(a)"
  (declare (type %quad-double a))
  (cond ((onep-qd a)
	 +qd-zero+)
	((and (zerop-qd a)
	      (plusp (float-sign (qd-0 a))))
	 (%make-qd-d (/ -1d0 (qd-0 a)) 0d0 0d0 0d0))
	#+cmu
	((float-infinity-p (qd-0 a))
	 a)
	((minusp (float-sign (qd-0 a)))
	 (error "log of negative"))
	(t
	 ;; Default is Halley's method
	 (log-qd/halley a))))
  

;; sin(a) and cos(a) using Taylor series
;;
;; Assumes |a| <= pi/2048
(defun sincos-taylor (a)
  (declare (type %quad-double a))
  (let ((thresh (cl:* +qd-eps+ (abs (qd-0 a)))))
    (when (zerop-qd a)
      (return-from sincos-taylor
	(values +qd-zero+
		+qd-one+)))
    (let* ((x (neg-qd (sqr-qd a)))
	   (s a)
	   (p a)
	   (m 1d0))
      (loop
	 (setf p (mul-qd p x))
	 (incf m 2)
	 (setf p (div-qd-d p (cl:* m (cl:1- m))))
	 (setf s (add-qd s p))
	 ;;(format t "p = ~A~%" (qd-0 p))
	 (when (<= (abs (qd-0 p)) thresh)
	   (return)))
      ;; cos(c) = sqrt(1-sin(c)^2).  This seems to work ok, even
      ;; though I would have expected some round-off errors in
      ;; computing this.  sqrt(1-x^2) is normally better computed as
      ;; sqrt(1-x)*sqrt(1+x) for small x.
      (values s (sqrt-qd (add-qd-d (neg-qd (sqr-qd s)) 1d0))))))

(defun drem-qd (a b)
  (declare (type %quad-double a b))
  (let ((n (nint-qd (div-qd a b))))
    (sub-qd a (mul-qd n b))))

(defun divrem-qd (a b)
  (declare (type %quad-double a b))
  (let ((n (nint-qd (div-qd a b))))
    (values n (sub-qd a (mul-qd n b)))))
  
(defun sin-qd (a)
  "Sin(a)"
  (declare (type %quad-double a))
  ;; To compute sin(x), choose integers a, b so that
  ;;
  ;; x = s + a * (pi/2) + b*(pi/1024)
  ;;
  ;; with |x| <= pi/2048.  Using a precomputed table of sin(k*pi/1024)
  ;; and cos(k*pi/1024), we can compute sin(x) from sin(s) and cos(s).
  ;;
  ;; sin(x) = sin(s+k*(pi/1024) + j*pi/2)
  ;;        = sin(s+k*(pi/1024))*cos(j*pi/2)
  ;;            + cos(s+k*(pi/1024))*sin(j*pi/2)
  ;;
  ;; sin(s+k*pi/1024) = sin(s)*cos(k*pi/1024)
  ;;                     + cos(s)*sin(k*pi/1024)
  ;;
  ;; cos(s+k*pi/1024) = cos(s)*cos(k*pi/1024)
  ;;                     - sin(s)*sin(k*pi/1024)
  (when (zerop-qd a)
    (return-from sin-qd a))

  ;; Reduce modulo 2*pi
  (let ((r (drem-qd a +qd-2pi+)))
    ;; Now reduce by pi/2 and then by pi/1024 so that we obtain
    ;; numbers a, b, t
    (multiple-value-bind (j tmp)
	(divrem-qd r +qd-pi/2+)
      (let* ((j (truncate (qd-0 j)))
	     (abs-j (abs j)))
	(multiple-value-bind (k tmp)
	    (divrem-qd tmp +qd-pi/1024+)
	  (let* ((k (truncate (qd-0 k)))
		 (abs-k (abs k)))
	    (assert (<= abs-j 2))
	    (assert (<= abs-k 256))
	    ;; Compute sin(s) and cos(s)
	    (multiple-value-bind (sin-t cos-t)
		(sincos-taylor tmp)
	      (multiple-value-bind (s c)
		  (cond ((zerop abs-k)
			 (values sin-t cos-t))
			(t
			 ;; Compute sin(s+k*pi/1024), cos(s+k*pi/1024)
			 (let ((u (aref +qd-cos-table+ (cl:1- abs-k)))
			       (v (aref +qd-sin-table+ (cl:1- abs-k))))
			   (cond ((plusp k)
				  ;; sin(s) * cos(k*pi/1024)
				  ;; + cos(s) * sin(k*pi/1024)
				  ;;
				  ;; cos(s) * cos(k*pi/1024)
				  ;; - sin(s) * sin(k*pi/1024)
				  (values (add-qd (mul-qd u sin-t)
						  (mul-qd v cos-t))
					  (sub-qd (mul-qd u cos-t)
						  (mul-qd v sin-t))))
				 (t
				  ;; sin(s) * cos(k*pi/1024)
				  ;; - cos(s) * sin(|k|*pi/1024)
				  ;;
				  ;; cos(s) * cos(k*pi/1024)
				  ;; + sin(s) * sin(|k|*pi/1024)
				  (values (sub-qd (mul-qd u sin-t)
						  (mul-qd v cos-t))
					  (add-qd (mul-qd u cos-t)
						  (mul-qd v sin-t))))))))
		;;(format t "s = ~/qd::qd-format/~%" s)
		;;(format t "c = ~/qd::qd-format/~%" c)
		;; sin(x) =  sin(s+k*pi/1024) * cos(j*pi/2)
		;;         + cos(s+k*pi/1024) * sin(j*pi/2)
		(cond ((zerop abs-j)
		       ;; cos(j*pi/2) = 1, sin(j*pi/2) = 0
		       s)
		      ((= j 1)
		       ;; cos(j*pi/2) = 0, sin(j*pi/2) = 1
		       c)
		      ((= j -1)
		       ;; cos(j*pi/2) = 0, sin(j*pi/2) = -1
		       (neg-qd c))
		      (t
		       ;; cos(j*pi/2) = -1, sin(j*pi/2) = 0
		       (neg-qd s)))))))))))
		     
(defun cos-qd (a)
  "Cos(a)"
  ;; Just like sin-qd, but for cos.
  (declare (type %quad-double a))
  ;; To compute sin(x), choose integers a, b so that
  ;;
  ;; x = s + a * (pi/2) + b*(pi/1024)
  ;;
  ;; with |x| <= pi/2048.  Using a precomputed table of sin(k*pi/1024)
  ;; and cos(k*pi/1024), we can compute sin(x) from sin(s) and cos(s).
  ;;
  ;; sin(x) = sin(s+k*(pi/1024) + j*pi/2)
  ;;        = sin(s+k*(pi/1024))*cos(j*pi/2)
  ;;            + cos(s+k*(pi/1024))*sin(j*pi/2)
  ;;
  ;; sin(s+k*pi/1024) = sin(s)*cos(k*pi/1024)
  ;;                     + cos(s)*sin(k*pi/1024)
  ;;
  ;; cos(s+k*pi/1024) = cos(s)*cos(k*pi/1024)
  ;;                     - sin(s)*sin(k*pi/1024)
  (when (zerop-qd a)
    (return-from cos-qd +qd-one+))

  ;; Reduce modulo 2*pi
  (let ((r (drem-qd a +qd-2pi+)))
    ;; Now reduce by pi/2 and then by pi/1024 so that we obtain
    ;; numbers a, b, t
    (multiple-value-bind (j tmp)
	(divrem-qd r +qd-pi/2+)
      (let* ((j (truncate (qd-0 j)))
	     (abs-j (abs j)))
	(multiple-value-bind (k tmp)
	    (divrem-qd tmp +qd-pi/1024+)
	  (let* ((k (truncate (qd-0 k)))
		 (abs-k (abs k)))
	    (assert (<= abs-j 2))
	    (assert (<= abs-k 256))
	    ;; Compute sin(s) and cos(s)
	    (multiple-value-bind (sin-t cos-t)
		(sincos-taylor tmp)
	      (multiple-value-bind (s c)
		  (cond ((zerop abs-k)
			 (values sin-t cos-t))
			(t
			 ;; Compute sin(s+k*pi/1024), cos(s+k*pi/1024)
			 (let ((u (aref +qd-cos-table+ (cl:1- abs-k)))
			       (v (aref +qd-sin-table+ (cl:1- abs-k))))
			   (cond ((plusp k)
				  ;; sin(s) * cos(k*pi/1024)
				  ;; + cos(s) * sin(k*pi/1024)
				  ;;
				  ;; cos(s) * cos(k*pi/1024)
				  ;; - sin(s) * sin(k*pi/1024)
				  (values (add-qd (mul-qd u sin-t)
						  (mul-qd v cos-t))
					  (sub-qd (mul-qd u cos-t)
						  (mul-qd v sin-t))))
				 (t
				  ;; sin(s) * cos(k*pi/1024)
				  ;; - cos(s) * sin(|k|*pi/1024)
				  ;;
				  ;; cos(s) * cos(k*pi/1024)
				  ;; + sin(s) * sin(|k|*pi/1024)
				  (values (sub-qd (mul-qd u sin-t)
						  (mul-qd v cos-t))
					  (add-qd (mul-qd u cos-t)
						  (mul-qd v sin-t))))))))
		#+nil
		(progn
		  (format t "s = ~/qd::qd-format/~%" s)
		  (format t "c = ~/qd::qd-format/~%" c))
		;; sin(x) =  sin(s+k*pi/1024) * cos(j*pi/2)
		;;         + cos(s+k*pi/1024) * sin(j*pi/2)
		(cond ((zerop abs-j)
		       ;; cos(j*pi/2) = 1, sin(j*pi/2) = 0
		       c)
		      ((= j 1)
		       ;; cos(j*pi/2) = 0, sin(j*pi/2) = 1
		       (neg-qd s))
		      ((= j -1)
		       ;; cos(j*pi/2) = 0, sin(j*pi/2) = -1
		       s)
		      (t
		       ;; cos(j*pi/2) = -1, sin(j*pi/2) = 0
		       (neg-qd c)))))))))))

;; Compute sin and cos of a
(defun sincos-qd (a)
  (declare (type %quad-double a))
  (when (zerop-qd a)
    (return-from sincos-qd
      (values +qd-zero+
	      +qd-one+)))

  ;; Reduce modulo 2*pi
  (let ((r (drem-qd a +qd-2pi+)))
    ;; Now reduce by pi/2 and then by pi/1024 so that we obtain
    ;; numbers a, b, t
    (multiple-value-bind (j tmp)
	(divrem-qd r +qd-pi/2+)
      (let* ((j (truncate (qd-0 j)))
	     (abs-j (abs j)))
	(multiple-value-bind (k tmp)
	    (divrem-qd tmp +qd-pi/1024+)
	  (let* ((k (truncate (qd-0 k)))
		 (abs-k (abs k)))
	    (assert (<= abs-j 2))
	    (assert (<= abs-k 256))
	    ;; Compute sin(s) and cos(s)
	    (multiple-value-bind (sin-t cos-t)
		(sincos-taylor tmp)
	      (multiple-value-bind (s c)
		  (cond ((zerop abs-k)
			 (values sin-t cos-t))
			(t
			 ;; Compute sin(s+k*pi/1024), cos(s+k*pi/1024)
			 (let ((u (aref +qd-cos-table+ (cl:1- abs-k)))
			       (v (aref +qd-sin-table+ (cl:1- abs-k))))
			   (cond ((plusp k)
				  ;; sin(s) * cos(k*pi/1024)
				  ;; + cos(s) * sin(k*pi/1024)
				  ;;
				  ;; cos(s) * cos(k*pi/1024)
				  ;; - sin(s) * sin(k*pi/1024)
				  (values (add-qd (mul-qd u sin-t)
						  (mul-qd v cos-t))
					  (sub-qd (mul-qd u cos-t)
						  (mul-qd v sin-t))))
				 (t
				  ;; sin(s) * cos(k*pi/1024)
				  ;; - cos(s) * sin(|k|*pi/1024)
				  ;;
				  ;; cos(s) * cos(k*pi/1024)
				  ;; + sin(s) * sin(|k|*pi/1024)
				  (values (sub-qd (mul-qd u sin-t)
						  (mul-qd v cos-t))
					  (add-qd (mul-qd u cos-t)
						  (mul-qd v sin-t))))))))
		#+nil
		(progn
		  (format t "s = ~/qd::qd-format/~%" s)
		  (format t "c = ~/qd::qd-format/~%" c))
		;; sin(x) =  sin(s+k*pi/1024) * cos(j*pi/2)
		;;         + cos(s+k*pi/1024) * sin(j*pi/2)
		(cond ((zerop abs-j)
		       ;; cos(j*pi/2) = 1, sin(j*pi/2) = 0
		       (values s c))
		      ((= j 1)
		       ;; cos(j*pi/2) = 0, sin(j*pi/2) = 1
		       (values c (neg-qd s)))
		      ((= j -1)
		       ;; cos(j*pi/2) = 0, sin(j*pi/2) = -1
		       (values (neg-qd c) s))
		      (t
		       ;; cos(j*pi/2) = -1, sin(j*pi/2) = 0
		       (values (neg-qd s)
			       (neg-qd c))))))))))))

  
(defun atan2-qd/newton (y x)
  (declare (type %quad-double y x)
	   #+nil
	   (optimize (speed 3) (space 0)))
  ;; Instead of using Taylor series to compute atan, we instead use
  ;; Newton's iteration to solve the equation
  ;;
  ;;   sin(z) = y/r or cos(z) = x/r
  ;;
  ;; where r = sqrt(x^2+y^2)
  ;;
  ;; The iteration is
  ;;
  ;;   z' = z + (y - sin(z))/cos(z)       (for sin)
  ;;   z' = z + (x - cos(z))/sin(z)       (for cos)
  ;;
  ;; Here, x and y are normalized so that x^2 + y^2 = 1.
  ;;
  ;; If |x| > |y|, then the first iteration is used since the
  ;; denominator is larger.  Otherwise the second is used.
  (cond ((zerop-qd x)
	 ;; x = 0
	 (cond ((zerop-qd y)
		;; Both x and y are zero.  Use the signs of x and y to
		;; determine the result
		(error "atan2(0,0)"))
	       (t
		;; x = 0, but y is not.  Use the sign of y.
		(return-from atan2-qd/newton
		  (cond ((plusp (float-sign (qd-0 y)))
			 +qd-pi/2+)
			(t
			 (neg-qd +qd-pi/2+)))))))
	((zerop-qd y)
	 ;; y = 0.
	 (return-from atan2-qd/newton
	   ;; Use the sign of x and y to figure out the result.
	   (cond ((plusp (float-sign (qd-0 x)))
		  +qd-zero+)
		 ((plusp (float-sign (qd-0 y)))
		  +qd-pi+)
		 (t
		  (neg-qd +qd-pi+))))))

  (when (qd-= x y)
    (return-from atan2-qd/newton
      (if (plusp-qd y)
	  +qd-pi/4+
	  +qd-3pi/4+)))

  (when (qd-= x (neg-qd y))
    (return-from atan2-qd/newton
      (if (plusp-qd y)
	  +qd-3pi/4+
	  (neg-qd +qd-pi/4+))))

  (let* ((r (hypot-qd x y))
	 (xx (div-qd x r))
	 (yy (div-qd y r)))
    #+nil
    (progn
      (format t "r  = ~/qdi::qd-format/~%" r)
      (format t "xx = ~/qdi::qd-format/~%" xx)
      (format t "yy = ~/qdi::qd-format/~%" yy))
    
    ;; Compute double-precision approximation to atan
    (let ((z (make-qd-d (atan (qd-0 y) (qd-0 x))))
	  (sinz +qd-zero+)
	  (cosz +qd-zero+))
      (cond ((> (abs (qd-0 xx))
		(abs (qd-0 yy)))
	     ;; Newton iteration  z' = z + (y - sin(z))/cos(z)
	     (dotimes (k 3)
	       (multiple-value-setq (sinz cosz) (sincos-qd z))
	       (setf z (add-qd z (div-qd (sub-qd yy sinz)
					 cosz)))))
	    (t
	     ;; Newton iteration z' = z - (x - cos(z))/sin(z)
	     (dotimes (k 3)
	       (multiple-value-setq (sinz cosz) (sincos-qd z))
	       (setf z (sub-qd z (div-qd (sub-qd xx cosz)
					 sinz))))))
      z)))

(defun atan-qd/newton (y)
  (declare (type %quad-double y)
	   #+nil (optimize (speed 3) (space 0)))
  (atan2-qd/newton y +qd-one+))

(defun atan2-qd (y x)
  "atan2(y, x) = atan(y/x), but carefully handling the quadrant"
  (declare (type %quad-double y x))
  (atan2-qd/newton y x))

(defun atan-qd (y)
  "Atan4b*(y)"
  (declare (type %quad-double y))
  (atan-qd/newton y))

(defun asin-qd (a)
  "Asin(a)"
  (declare (type %quad-double a))
  (atan2-qd a (sqrt-qd (sub-d-qd 1d0
			       (sqr-qd a)))))

(defun acos-qd (a)
  "Acos(a)"
  (declare (type %quad-double a))
  (atan2-qd (sqrt-qd (sub-d-qd 1d0
			     (sqr-qd a)))
	    a))
  

(defun tan-qd/sincos (r)
  (declare (type %quad-double r))
  (multiple-value-bind (s c)
      (sincos-qd r)
    ;; What to do, what do?  If C is zero, we get divide by zero
    ;; error.  We could return infinity, but quad-double stuff doesn't
    ;; handle infinities very well.
    (div-qd s c)))

(defun tan-qd (r)
  "Tan(r)"
  (declare (type %quad-double r))
  (if (zerop r)
      r
      (tan-qd/sincos r)))
  

(defun sinh-qd (a)
  "Sinh(a)"
  (declare (type %quad-double a))
  ;; Hart et al. suggests sinh(x) = 1/2*(D(x) + D(x)/(D(x)+1))
  ;; where D(x) = exp(x) - 1.  This helps for x near 0.
  (cond ((zerop a)
	 a)
	#+cmu
	((float-infinity-p (qd-0 a))
	 a)
	(t
	 (let ((d (expm1-qd a)))
	   #+cmu
	   (when (float-infinity-p (qd-0 d))
	     (return-from sinh-qd d))
	   (scale-float-qd (add-qd d
				   (div-qd d (add-qd-d d 1d0)))
			   -1)))))

(defun cosh-qd (a)
  "Cosh(a)"
  (declare (type %quad-double a))
  ;; cosh(x) = 1/2*(exp(x)+exp(-x))
  (let ((e (exp-qd a)))
    #+cmu
    (when (float-infinity-p (qd-0 e))
      (return-from cosh-qd e))
    (scale-float-qd (add-qd e (div-qd +qd-one+ e))
		    -1)))

(defun tanh-qd (a)
  "Tanh(a)"
  (declare (type %quad-double a))
  ;; Hart et al. suggests tanh(x) = D(2*x)/(2+D(2*x))
  (cond ((zerop a)
	 a)
	((> (abs (qd-0 a)) (/ (+ (log most-positive-double-float)
				 (log 2d0))
			      4d0))
	 ;; For this range of A, we know the answer is +/- 1.
	 ;;
	 ;; However, we could do better if we wanted.  Assume x > 0
	 ;; and very large.
	 ;;
	 ;; tanh(x) = sinh(x)/cosh(x)
	 ;;         = (1-exp(-2*x))/(1+exp(-2*x))
	 ;;         = 1 - 2*exp(-2*x)/(1+exp(-2*x))
	 ;;
	 ;; So tanh(x) is 1 if the other term is small enough, say,
	 ;; eps.  So for x large enough we can compute tanh(x) very
	 ;; accurately, thanks to how quad-double addition works.
	 ;; (The first component is, basically 1d0, and the second is
	 ;; some very small double-float.)
	 #+nil
	 (let* ((e (exp (* -2 a)))
		(res (- 1 (/ (* 2 e) (1+ e)))))
	   (if (minusp (float-sign (qd-0 a)))
	       (neg-qd res)
	       res))
	 (make-qd-d (float-sign (qd-0 a))))
	(t
	 (let* ((a2 (mul-qd-d a 2d0))
		(d (expm1-qd a2)))
	   (div-qd d (add-qd-d d 2d0))))))

(defun asinh-qd (a)
  "Asinh(a)"
  (declare (type %quad-double a))
  ;; asinh(x) = log(x + sqrt(1+x^2))
  ;;
  ;; But this doesn't work well when x is small.
  ;;
  ;; log(x + sqrt(1+x^2)) = log(sqrt(1+x^2)*(1+x/sqrt(1+x^2)))
  ;;   = log(sqrt(1+x^2)) + log(1+x/sqrt(1+x^2))
  ;;   = 1/2*log(1+x^2) + log(1+x/sqrt(1+x^2))
  ;;
  ;; However that doesn't work well when x is large because x^2
  ;; overflows.
  ;;
  ;; log(x + sqrt(1+x^2)) = log(x + x*sqrt(1+1/x^2))
  ;;   = log(x) + log(1+sqrt(1+1/x^2))
  ;;   = log(x) + log1p(sqrt(1+1/x^2))
  #+nil
  (log-qd (add-qd a
		  (sqrt-qd (add-qd-d (sqr-qd a)
				     1d0))))
  (if (< (abs (qd-0 a)) (sqrt most-positive-double-float))
      (let ((a^2 (sqr-qd a)))
	(add-qd (scale-float-qd (log1p-qd a^2) -1)
		(log1p-qd (div-qd a
				  (sqrt-qd (add-qd-d a^2 1d0))))))
      (if (minusp-qd a)
	  (neg-qd (asinh-qd (neg-qd a)))
	  (let ((1/a (div-qd (make-qd-d 1d0) a)))
	    (+ (log-qd a)
	       (log1p-qd (sqrt-qd (add-qd-d (sqr-qd 1/a) 1d0))))))))

(defun asinh-qd (a)
  "Asinh(a)"
  (declare (type %quad-double a))
  ;; asinh(x) = log(x + sqrt(1+x^2))
  ;;
  ;; But this doesn't work well when x is small.
  ;;
  ;; log(x + sqrt(1+x^2)) = log(sqrt(1+x^2)*(1+x/sqrt(1+x^2)))
  ;;   = log(sqrt(1+x^2)) + log(1+x/sqrt(1+x^2))
  ;;   = 1/2*log(1+x^2) + log(1+x/sqrt(1+x^2))
  ;;
  ;; However that doesn't work well when x is large because x^2
  ;; overflows.
  ;;
  ;; log(x + sqrt(1+x^2)) = log(x + x*sqrt(1+1/x^2))
  ;;   = log(x) + log(1+sqrt(1+1/x^2))
  ;;   = log(x) + log1p(sqrt(1+1/x^2))
  #+nil
  (log-qd (add-qd a
		  (sqrt-qd (add-qd-d (sqr-qd a)
				     1d0))))
  (cond ((< (abs (qd-0 a)) (sqrt most-positive-double-float))
	 (let ((a^2 (sqr-qd a)))
	   (add-qd (scale-float-qd (log1p-qd a^2) -1)
		   (log1p-qd (div-qd a
				     (sqrt-qd (add-qd-d a^2 1d0)))))))
	#+cmu
	((float-infinity-p (qd-0 a))
	 a)
	(t
	 (if (minusp-qd a)
	     (neg-qd (asinh-qd (neg-qd a)))
	     (let ((1/a (div-qd (make-qd-d 1d0) a)))
	       (+ (log-qd a)
		  (log1p-qd (sqrt-qd (add-qd-d (sqr-qd 1/a) 1d0)))))))))

(defun acosh-qd (a)
  "Acosh(a)"
  (declare (type %quad-double a))
  ;; acosh(x) = log(x + sqrt(x^2-1))
  #+nil
  (log-qd (add-qd a
		  (sqrt-qd (sub-qd-d (sqr-qd a)
				     1d0))))
  ;; log(x+sqrt(x^2-1)) = log(x+sqrt((x-1)*(x+1)))
  ;;  = log(x+sqrt(x-1)*sqrt(x+1))
  #+nil
  (log-qd (add-qd a
		  (mul-qd
		   (sqrt-qd (sub-qd-d a 1d0))
		   (sqrt-qd (add-qd-d a 1d0)))))
  ;; Let x = 1 + y
  ;; log(1 + y + sqrt(y)*sqrt(y + 2))
  ;;   = log1p(y + sqrt(y)*sqrt(y + 2))
  ;;
  ;; However, that doesn't work well if x is large.
  ;;
  ;; log(x+sqrt(x^2-1)) = log(x+x*sqrt(1-1/x^2))
  ;;   = log(x) + log(1+sqrt(1-1/x^2))
  ;;   = log(x) + log1p(sqrt(1-1/x)*sqrt(1+1/x))
  ;;
  (cond ((< (abs (qd-0 a)) (sqrt most-positive-double-float))
	 (let ((y (sub-qd-d a 1d0)))
	   (log1p-qd (add-qd y (sqrt-qd (mul-qd y (add-qd-d y 2d0)))))))
	#+cmu
	((float-infinity-p (qd-0 a))
	 a)
	(t
	 (let ((1/a (div-qd (make-qd-d 1d0) a)))
	   (+ (log-qd a)
	      (log1p-qd (mul-qd (sqrt-qd (sub-d-qd 1d0 1/a))
				(sqrt-qd (add-d-qd 1d0 1/a)))))))))

(defun atanh-qd (a)
  "Atanh(a)"
  (declare (type %quad-double a))
  ;; atanh(x) = 1/2*log((1+x)/(1-x))
  ;;          = 1/2*log(1+(2*x)/(1-x))
  ;; This latter expression works better for small x
  #+nil
  (scale-float-qd (log-qd (div-qd (add-d-qd 1d0 a)
				  (sub-d-qd 1d0 a)))
		  -1)
  ;; atanh(+/-1) = +/- infinity.  Signal a division by zero or return
  ;; infinity if the division-by-zero trap is disabled.
  (if (qd-= (abs-qd a) +qd-one+)
      (div-qd (make-qd-d (float-sign (qd-0 a)))
	      +qd-zero+)
      (scale-float-qd (log1p-qd (div-qd (scale-float-qd a 1)
					(sub-d-qd 1d0 a)))
		      -1)))
  

(defun random-qd (&optional (state *random-state*))
  "Generate a quad-double random number in the range [0,1)"
  (declare (optimize (speed 3)))
  ;; Strategy:  Generate 31 bits at a time, shift the bits and repeat 7 times.
  (let* ((r +qd-zero+)
	 (m-const (scale-float 1d0 -31))
	 (m m-const))
    (declare (type %quad-double r)
	     (double-float m-const m))
    (dotimes (k 7)
      (let ((d (cl:* m (random #x7fffffff state))))
	(setf r (add-qd-d r d))
	(setf m (cl:* m m-const))))
    r))

