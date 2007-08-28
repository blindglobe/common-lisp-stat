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

;; Most of this code taken from CMUCL and slightly modified to support
;; QD-COMPLEX.

(in-package #:qd)

(defmethod two-arg-/ ((a qd-real) (b rational))
  (make-instance 'qd-real :value (div-qd (qd-value a)
					 (qd-value (float b #q0)))))

(defmethod two-arg-/ ((a rational) (b qd-real))
  (make-instance 'qd-real :value (div-qd (qd-value (float a #q0))
					 (qd-value b))))

(defmethod two-arg-* ((a qd-real) (b rational))
  (make-instance 'qd-real :value (mul-qd (qd-value a) (qd-value (float b #q0)))))

(defmethod two-arg-+ ((a qd-real) (b rational))
  (make-instance 'qd-real :value (add-qd (qd-value a) (qd-value (float b #q0)))))

(defmethod two-arg-+ ((a rational) (b qd-real))
  (make-instance 'qd-real :value (add-qd (qd-value b) (qd-value (float a #q0)))))

(defmethod two-arg-- ((a qd-real) (b rational))
  (make-instance 'qd-real :value (sub-qd (qd-value a) (qd-value (float b #q0)))))

(defmethod two-arg-- ((a rational) (b qd-real))
  (make-instance 'qd-real :value (sub-qd (qd-value (float a #q0)) (qd-value b))))

(defmethod unary-minus ((z qd-complex))
  (complex (- (realpart z))
	   (- (imagpart z))))

(defmethod qzerop ((z qd-complex))
  (and (zerop (realpart z))
       (zerop (imagpart z))))

(defmethod two-arg-+ ((a qd-complex) (b qd-complex))
  (complex (+ (realpart a) (realpart b))
	   (+ (imagpart a) (imagpart b))))

(defmethod two-arg-+ ((a qd-complex) (b real))
  (complex (+ (realpart a) b)
	   (imagpart a)))

(defmethod two-arg-+ ((a qd-complex) (b qd-real))
  (complex (+ (realpart a) b)
	   (imagpart a)))

(defmethod two-arg-+ ((a qd-real) (b qd-complex))
  (complex (+ a (realpart b))
	   (imagpart b)))

(defmethod two-arg-+ ((a qd-complex) (b cl:complex))
  (complex (+ (realpart a) (imagpart b))
	   (+ (imagpart a) (imagpart b))))

(defmethod two-arg-+ ((a number) (b qd-complex))
  (two-arg-+ b a))
  
(defmethod two-arg-- ((a qd-complex) (b qd-complex))
  (complex (- (realpart a) (realpart b))
	   (- (imagpart a) (imagpart b))))

(defmethod two-arg-- ((a qd-complex) (b real))
  (complex (- (realpart a) b)
	   (imagpart a)))

(defmethod two-arg-- ((a qd-complex) (b cl:complex))
  (complex (- (realpart a) (realpart b))
	   (- (imagpart a) (imagpart b))))

(defmethod two-arg-- ((a qd-complex) (b qd-real))
  (complex (- (realpart a) b)
	   (imagpart a)))

(defmethod two-arg-- ((a number) (b qd-complex))
  (complex (- (realpart a) (realpart b))
	   (- (imagpart a) (imagpart b))))

(defmethod two-arg-- ((a qd-real) (b qd-complex))
  (complex (- a (realpart b))
	   (- (imagpart b))))
  
(defmethod two-arg-* ((a qd-complex) (b qd-complex))
  (let* ((rx (realpart a))
	 (ix (imagpart a))
	 (ry (realpart b))
	 (iy (imagpart b)))
    (complex (- (* rx ry) (* ix iy))
	     (+ (* rx iy) (* ix ry)))))

(defmethod two-arg-* ((a qd-complex) (b real))
  (let* ((rx (realpart a))
	 (ix (imagpart a)))
    (complex (* rx b)
	     (* ix b))))

(defmethod two-arg-* ((a qd-complex) (b qd-real))
  (let* ((rx (realpart a))
	 (ix (imagpart a)))
    (complex (* rx b)
	     (* ix b))))

(defmethod two-arg-* ((a qd-real) (b qd-complex))
  (two-arg-* b a))



#+cmu
(defmethod two-arg-* ((a qd-complex) (b cl:complex))
  ;; For now, convert B into a qd-complex and use that.
  (let ((re (coerce (realpart b) 'ext:double-double-float))
	(im (coerce (imagpart b) 'ext:double-double-float)))
    (two-arg-* a (make-instance 'qd-complex
				:real (make-qd-dd re 0w0)
				:imag (make-qd-dd im 0w0)))))

#-cmu
(defmethod two-arg-* ((a qd-complex) (b cl:complex))
  ;; For now, convert B into a qd-complex and use that.
  (let ((re (coerce (realpart b) 'double-float))
	(im (coerce (imagpart b) 'double-float)))
    (two-arg-* a (make-instance 'qd-complex
				:real (make-qd-d re)
				:imag (make-qd-d im)))))

(defmethod two-arg-* ((a number) (b qd-complex))
  (two-arg-* b a))

(defmethod two-arg-/ ((x qd-complex) (y qd-complex))
  (let* ((rx (realpart x))
	 (ix (imagpart x))
	 (ry (realpart y))
	 (iy (imagpart y)))
    (if (> (abs ry) (abs iy))
	(let* ((r (/ iy ry))
	       (dn (+ ry (* r iy))))
	  (complex (/ (+ rx (* ix r)) dn)
		   (/ (- ix (* rx r)) dn)))
	(let* ((r (/ ry iy))
	       (dn (+ iy (* r ry))))
	  (complex (/ (+ (* rx r) ix) dn)
		   (/ (- (* ix r) rx) dn))))))

(defmethod two-arg-/ ((x qd-complex) (y qd-real))
  (complex (/ (realpart x) y)
	   (/ (imagpart x) y)))

(defmethod two-arg-/ ((x qd-complex) (y number))
  (complex (/ (realpart x) y)
	   (/ (imagpart x) y)))

(defmethod two-arg-/ ((x number) (y qd-complex))
  (let* ((rx (realpart x))
	 (ix (imagpart x))
	 (ry (realpart y))
	 (iy (imagpart y)))
    (if (> (abs ry) (abs iy))
	(let* ((r (/ iy ry))
	       (dn (+ ry (* r iy))))
	  (complex (/ (+ rx (* ix r)) dn)
		   (/ (- ix (* rx r)) dn)))
	(let* ((r (/ ry iy))
	       (dn (+ iy (* r ry))))
	  (complex (/ (+ (* rx r) ix) dn)
		   (/ (- (* ix r) rx) dn))))))

(defmethod two-arg-/ ((x qd-real) (y qd-complex))
  ;; This can be simplified since X is real.
  (let* ((rx (realpart x))
	 (ix (imagpart x))
	 (ry (realpart y))
	 (iy (imagpart y)))
    (if (> (abs ry) (abs iy))
	(let* ((r (/ iy ry))
	       (dn (+ ry (* r iy))))
	  (complex (/ (+ rx (* ix r)) dn)
		   (/ (- ix (* rx r)) dn)))
	(let* ((r (/ ry iy))
	       (dn (+ iy (* r ry))))
	  (complex (/ (+ (* rx r) ix) dn)
		   (/ (- (* ix r) rx) dn))))))

(defmethod unary-divide ((a qd-complex))
  (two-arg-/ #q1 a))

(defmethod coerce ((obj t) (type t))
  (cl:coerce obj type))

(defmethod coerce ((number cl:real) (type (eql 'qd-real)))
  (float number #q0))

(defmethod coerce ((number qd-real) (type (eql 'qd-real)))
  number)

(defmethod coerce ((number cl:number) (type (eql 'qd-complex)))
  (complex (float (realpart number) #q0)
	   (float (imagpart number) #q0)))

(defmethod coerce ((number qd-complex) (type (eql 'qd-complex)))
  number)

(declaim (inline square))
(defun square (x)
  (declare (type qd-real x))
  (make-instance 'qd-real :value (sqr-qd (qd-value x))))

(defun qd-complex-sqrt (z)
  "Principle square root of Z

Z may be any number, but the result is always a complex."
  (declare (type qd-complex z))
  (multiple-value-bind (rho k)
      (qd-cssqs z)
    (declare (type qd-real rho)
	     (type fixnum k))
    (let ((x (realpart z))
	  (y (imagpart z))
	  (eta #q0.0)
	  (nu #q0.0))
      (declare (type qd-real x y eta nu))

      (locally
	  ;; space 0 to get maybe-inline functions inlined.
	  (declare (optimize (speed 3) (space 0)))

	(setf rho (+ (scalb (abs x) (- k)) (sqrt rho)))

	(cond ((oddp k)
	       (setf k (ash k -1)))
	      (t
	       (setf k (1- (ash k -1)))
	       (setf rho (+ rho rho))))

	(setf rho (scalb (sqrt rho) k))

	(setf eta rho)
	(setf nu y)

	(when (not (zerop rho))
	  (setf nu (/ (/ nu rho) 2d0)))
	(when (minusp x)
	  (setf eta (abs nu))
	  (setf nu (float-sign y rho))))
      (complex eta nu))))

(defun qd-complex-log-scaled (z j)
  "Compute log(2^j*z).

This is for use with J /= 0 only when |z| is huge."
  (declare (type qd-complex z)
	   (fixnum j))
  ;; The constants t0, t1, t2 should be evaluated to machine
  ;; precision.  In addition, Kahan says the accuracy of log1p
  ;; influences the choices of these constants but doesn't say how to
  ;; choose them.  We'll just assume his choices matches our
  ;; implementation of log1p.
  (let ((t0 (/ 1 (sqrt #q2.0q0)))
	(t1 #q1.2q0)
	(t2 #q3q0)
	(ln2 #.(log #q2.0))
	(x (realpart z))
	(y (imagpart z)))
    (multiple-value-bind (rho k)
	(qd-cssqs z)
      (declare (optimize (speed 3)))
      (let ((beta (max (abs x) (abs y)))
	    (theta (min (abs x) (abs y))))
	(complex (if (and (zerop k)
			  (< t0 beta)
			  (or (<= beta t1)
			      (< rho t2)))
		     (/ (log1p (+ (* (- beta 1.0d0)
				     (+ beta 1.0d0))
				  (* theta theta)))
			2d0)
		     (+ (/ (log rho) 2d0)
			(* (+ k j) ln2)))
		 (atan y x))))))

(defun qd-complex-log (z)
  "Log of Z = log |Z| + i * arg Z

Z may be any number, but the result is always a complex."
  (declare (type qd-complex z))
  (qd-complex-log-scaled z 0))
	       

;; Let us note the following "strange" behavior.  atanh 1.0d0 is
;; +infinity, but the following code returns approx 176 + i*pi/4. The
;; reason for the imaginary part is caused by the fact that arg i*y is
;; never 0 since we have positive and negative zeroes.

;;
;; atanh(z) = (log(1+z) - log(1-z))/2
;;
;; The branch cut is on the real axis for |x| >= 1.  For x =< -1,
;; atanh is continuous with quadrant III; for x >= 1, continuous with
;; quadrant I.
(defun qd-complex-atanh (z)
  "Compute atanh z = (log(1+z) - log(1-z))/2"
  (declare (type (or qd-real qd-complex) z))
  (cond ((and (typep z 'qd-real) (< z -1))
	 (qd-complex-atanh (complex z -0d0)))
	(t
	 (flet ((careful-mul (a b)
		  ;; Carefully multiply a and b, taking care to handle
		  ;; signed zeroes.  Only need to handle the case of b
		  ;; being zero.
		  (if (zerop b)
		      (if (minusp (* (float-sign a) (float-sign b)))
			  #q-0
			  #q0)
		      (* a b))))
	   (let* ( ;; Constants
		  (theta (/ (sqrt most-positive-double-float) 4.0d0))
		  (rho (/ 4.0d0 (sqrt most-positive-double-float)))
		  (half-pi #.(/ +pi+ 2d0))
		  (rp (realpart z))
		  (beta (float-sign rp))
		  (x (* beta rp))
		  (y (careful-mul beta (- (imagpart z))))
		  (eta #q0.0q0)
		  (nu #q0.0q0))
	     ;; Shouldn't need this declare.
	     (declare (type qd-real x y))
	     (locally
		 (declare (optimize (speed 3)))
	       (cond ((or (> x theta)
			  (> (abs y) theta))
		      ;; To avoid overflow...
		      (setf nu (float-sign y half-pi))
		      ;; eta is real part of 1/(x + iy).  This is x/(x^2+y^2),
		      ;; which can cause overflow.  Arrange this computation so
		      ;; that it won't overflow.
		      (setf eta (let* ((x-bigger (> x (abs y)))
				       (r (if x-bigger (/ y x) (/ x y)))
				       (d (+ 1.0d0 (* r r))))
				  (if x-bigger
				      (/ (/ x) d)
				      (/ (/ r y) d)))))
		     ((= x #q1.0q0)
		      ;; Should this be changed so that if y is zero, eta is set
		      ;; to +infinity instead of approx 176?  In any case
		      ;; tanh(176) is 1.0d0 within working precision.
		      (let ((t1 (+ 4d0 (square y)))
			    (t2 (+ (abs y) rho)))
			(setf eta (log (/ (sqrt (sqrt t1))
					  (sqrt t2))))
			(setf nu (* 0.5d0
				    (float-sign y
						(+ half-pi (atan (* 0.5d0 t2))))))
			))
		     (t
		      (let ((t1 (+ (abs y) rho)))
			;; Normal case using log1p(x) = log(1 + x)
			(setf eta (* 0.25d0
				     (log1p (/ (* 4.0d0 x)
					       (+ (square (- 1.0d0 x))
						  (square t1))))))
			(setf nu (* 0.5d0
				    (atan (careful-mul 2.0d0 y)
					  (- (* (- 1.0d0 x)
						(+ 1.0d0 x))
					     (square t1))))))))
	       (complex (* beta eta)
			(- (* beta nu)))))))))


;; tanh(z) = sinh(z)/cosh(z)
;;
(defun qd-complex-tanh (z)
  "Compute tanh z = sinh z / cosh z"
  (declare (type (or qd-real qd-complex) z))
  (let ((x (realpart z))
	(y (imagpart z)))
    (locally
	;; space 0 to get maybe-inline functions inlined
	(declare (optimize (speed 3) (space 0)))
      (cond ((> (abs x) #.(/ (+ (log most-positive-double-float)
				(log 2d0))
			     4d0))
	     ;; The threshold above is
	     ;; asinh(most-positive-double-float)/4, but many Lisps
	     ;; cannot actually compute that.  Hence use the
	     ;; (accurate) approximation
	     ;; asinh(most-positive-double-float) =
	     ;; log(most-positive-double-float) + log(2)
	     (complex (float-sign x)
		      (float-sign y)))
	    (t
	     ;; With quad-double's it happens that tan(pi/2) will
	     ;; actually produce a division by zero error.  We need to
	     ;; handle that case carefully.
	     (let* ((tv (ignore-errors (tan y)))
		    (s (sinh x))
		    (rho (sqrt (+ 1.0d0 (* s s)))))
	       (cond (tv
		      (let* ((beta (+ 1.0d0 (* tv tv)))
			     (den (+ 1.0d0 (* beta s s))))
			(complex (/ (* beta rho s)
				    den)
				 (/ tv den))))
		     (t
		      ;; This means tan(y) produced some error.  We'll
		      ;; assume it's an overflow error because y is
		      ;; pi/2 + 2*k*pi.  But we need a value for tv to
		      ;; compute (/ tv).  This would be a signed-zero.
		      ;; For now, just return +0.
		      (complex (/ rho s)
			       #q0)))))))))

;; Kahan says we should only compute the parts needed.  Thus, the
;; realpart's below should only compute the real part, not the whole
;; complex expression.  Doing this can be important because we may get
;; spurious signals that occur in the part that we are not using.
;;
;; However, we take a pragmatic approach and just use the whole
;; expression.

;; NOTE: The formula given by Kahan is somewhat ambiguous in whether
;; it's the conjugate of the square root or the square root of the
;; conjugate.  This needs to be checked.

;; I checked.  It doesn't matter because (conjugate (sqrt z)) is the
;; same as (sqrt (conjugate z)) for all z.  This follows because
;;
;; (conjugate (sqrt z)) = exp(0.5*log |z|)*exp(-0.5*j*arg z).
;;
;; (sqrt (conjugate z)) = exp(0.5*log|z|)*exp(0.5*j*arg conj z)
;;
;; and these two expressions are equal if and only if arg conj z =
;; -arg z, which is clearly true for all z.

;; NOTE: The rules of Common Lisp says that if you mix a real with a
;; complex, the real is converted to a complex before performing the
;; operation.  However, Kahan says in this paper (pg 176):
;;
;; (iii) Careless handling can turn infinity or the sign of zero into
;;       misinformation that subsequently disappears leaving behind
;;       only a plausible but incorrect result.  That is why compilers
;;       must not transform z-1 into z-(1+i*0), as we have seen above,
;;       nor -(-x-x^2) into (x+x^2), as we shall see below, lest a
;;       subsequent logarithm or square root produce a non-zero
;;       imaginary part whose sign is opposite to what was intended.
;;
;; The interesting examples are too long and complicated to reproduce
;; here.  We refer the reader to his paper.
;;
;; The functions below are intended to handle the cases where a real
;; is mixed with a complex and we don't want CL complex contagion to
;; occur..

(declaim (inline 1+z 1-z z-1 z+1))
(defun 1+z (z)
  (complex (+ 1 (realpart z)) (imagpart z)))
(defun 1-z (z)
  (complex (- 1 (realpart z)) (- (imagpart z))))
(defun z-1 (z)
  (complex (- (realpart z) 1) (imagpart z)))
(defun z+1 (z)
  (complex (+ (realpart z) 1) (imagpart z)))

(defun qd-complex-acos (z)
  "Compute acos z = pi/2 - asin z

Z may be any number, but the result is always a complex."
  (declare (type (or qd-real qd-complex) z))
  (if (and (typep z 'qd-real) (> z 1))
      ;; acos is continuous in quadrant IV in this case.
      (qd-complex-acos (complex z -0f0))
      (let ((sqrt-1+z (qd-complex-sqrt (1+z z)))
	    (sqrt-1-z (qd-complex-sqrt (1-z z))))
	(cond ((zerop (realpart sqrt-1+z))
	       ;; Same as below, but we compute atan ourselves (because we
	       ;; have atan +/- infinity).
	       (complex 
			(if (minusp (float-sign (* (realpart sqrt-1-z)
						   (realpart sqrt-1+z))))
			    (- +pi+)
			    +pi+)
			(asinh (imagpart (* (conjugate sqrt-1+z)
					    sqrt-1-z)))))
	      (t
	       (complex (* 2 (atan (/ (realpart sqrt-1-z)
				      (realpart sqrt-1+z))))
			(asinh (imagpart (* (conjugate sqrt-1+z)
					    sqrt-1-z)))))))))

(defun qd-complex-acosh (z)
  "Compute acosh z = 2 * log(sqrt((z+1)/2) + sqrt((z-1)/2))

Z may be any number, but the result is always a complex."
  (declare (type (or qd-real qd-complex) z))
  (let* ((sqrt-z-1 (qd-complex-sqrt (z-1 z)))
	 (sqrt-z+1 (qd-complex-sqrt (z+1 z))))
    ;; We need to handle the case where real part of sqrt-z+1 is zero,
    ;; because division by zero with double-double-floats doesn't
    ;; produce infinity.
    (cond ((zerop (realpart sqrt-z+1))
	   ;; Same as below, but we compute atan ourselves (because we
	   ;; have atan +/- infinity).
	   (complex (asinh (realpart (* (conjugate sqrt-z-1)
					sqrt-z+1)))
		    (if (minusp (float-sign (* (imagpart sqrt-z-1)
					       (realpart sqrt-z+1))))
			(- +pi+)
			+pi+)))
	  (t
	   (complex (asinh (realpart (* (conjugate sqrt-z-1)
					sqrt-z+1)))
		    (* 2 (atan (/ (imagpart sqrt-z-1)
				  (realpart sqrt-z+1)))))))))

;; asin(z) = asinh(i*z)/i
;;         = -i log(i*z + sqrt(1-z^2))
(defun qd-complex-asin (z)
  "Compute asin z = asinh(i*z)/i

Z may be any number, but the result is always a complex."
  (declare (type (or qd-real qd-complex) z))
  (cond ((and (typep z 'qd-real) (> z 1))
	 (qd-complex-asin (complex z -0d0)))
	(t
	 (let* ((sqrt-1-z (qd-complex-sqrt (1-z z)))
		(sqrt-1+z (qd-complex-sqrt (1+z z)))
		(den (realpart (* sqrt-1-z sqrt-1+z))))
	   (cond ((zerop den)
		  ;; Like below but we handle atan part ourselves.
		  ;; Must be sure to take into account the sign of
		  ;; (realpart z) and den!
		  (complex (if (minusp (* (float-sign (realpart z))
					  (float-sign den)))
			       (- (/ +pi+ 2))
			       (/ +pi+ 2))
			   (asinh (imagpart (* (conjugate sqrt-1-z)
					       sqrt-1+z)))))
		 (t
		  ;; We get a invalid operation here when z is real and |z| > 1.
		  (complex (atan (/ (realpart z)
				    (realpart (* sqrt-1-z sqrt-1+z))))
			   (asinh (imagpart (* (conjugate sqrt-1-z)
					       sqrt-1+z))))))))))

(defun qd-complex-asinh (z)
  "Compute asinh z = log(z + sqrt(1 + z*z))

Z may be any number, but the result is always a complex."
  (declare (type (or qd-real qd-complex) z))
  ;; asinh z = -i * asin (i*z)
  (let* ((iz (complex (- (imagpart z)) (realpart z)))
	 (result (qd-complex-asin iz)))
    (complex (imagpart result)
	     (- (realpart result)))))
	 
(defun qd-complex-atan (z)
  "Compute atan z = atanh (i*z) / i

Z may be any number, but the result is always a complex."
  (declare (type (or qd-real qd-complex) z))
  ;; atan z = -i * atanh (i*z)
  (let* ((iz (complex (- (imagpart z)) (realpart z)))
	 (result (qd-complex-atanh iz)))
    (complex (imagpart result)
	     (- (realpart result)))))

(defun qd-complex-tan (z)
  "Compute tan z = -i * tanh(i * z)

Z may be any number, but the result is always a complex."
  (declare (type (or qd-real qd-complex) z))
  ;; tan z = -i * tanh(i*z)
  (let* ((iz (complex (- (imagpart z)) (realpart z)))
	 (result (qd-complex-tanh iz)))
    (complex (imagpart result)
	     (- (realpart result)))))

(defmethod qasin ((x qd-complex))
  (qd-complex-asin x))

(defmethod qacos ((x qd-complex))
  (qd-complex-acos x))

(defmethod qacosh ((x qd-complex))
  (qd-complex-acosh x))

(defmethod qatanh ((x qd-complex))
  (qd-complex-atanh x))

(defmethod qsin ((z qd-complex))
  (let ((x (realpart z))
	(y (imagpart z)))
    (complex (* (sin x) (cosh y))
	     (* (cos x) (sinh y)))))

(defmethod qcos ((z qd-complex))
  (let ((x (realpart z))
	(y (imagpart z)))
    (complex (* (cos x) (cosh y))
	     (- (* (sin x) (sinh y))))))

(defmethod qtan ((z qd-complex))
  (qd-complex-tan z))

(defmethod qsinh ((z qd-complex))
  (let ((x (realpart z))
	(y (imagpart z)))
    (complex (* (sinh x) (cos y))
	     (* (cosh x) (sin y)))))

(defmethod qcosh ((z qd-complex))
  (let ((x (realpart z))
	(y (imagpart z)))
    (complex (* (cosh x) (cos y))
	     (* (sinh x) (sin y)))))

(defmethod qtanh ((z qd-complex))
  (qd-complex-tanh z))

(defmethod qsqrt ((z qd-complex))
  (qd-complex-sqrt z))

(defmethod qatan ((y qd-complex) &optional x)
  (if x
      (error "First arg of 2-arg ATAN must be real")
      (qd-complex-atan y)))

(defmethod qatan ((y cl:complex) &optional x)
  (if x
      (error "First arg of 2-arg ATAN must be real")
      (cl:atan y)))

(defmethod qexp ((z qd-complex))
  (let* ((x (realpart z))
	 (y (imagpart z))
	 (ex (exp x)))
    (complex (* ex (cos y))
	     (* ex (sin y)))))

(defmethod qlog ((a qd-complex) &optional b)
  (if b
      (/ (qlog a) (qlog b))
      (complex (log (abs a))
	       (atan (imagpart a) (realpart a)))))

(defmethod qexpt ((x qd-complex) (y number))
  (exp (* (float y #q0) (log x))))

(defmethod qexpt ((x number) (y qd-complex))
  (exp (* y (log (float x #q0)))))

(defmethod qexpt ((x qd-complex) (y qd-complex))
  (exp (* y (log x))))

(defmethod qphase ((z qd-complex))
  (atan (imagpart z) (realpart z)))

(defun realp (x)
  (or (typep x 'qd-real)
      (cl:realp x)))

(defun complexp (x)
  (or (typep x 'qd-complex)
      (cl:complexp x)))

(defun numberp (x)
  (or (realp x)
      (complexp x)))
