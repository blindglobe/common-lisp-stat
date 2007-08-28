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

(defconstant +pi+
  (make-instance 'qd-real :value qdi:+qd-pi+))

#+cmu
(defconstant +quad-double-float-positive-infinity+
  (make-instance 'qd-real :value (make-qd-d ext:double-float-positive-infinity))
  "Positive infinity for qd-real")

#+cmu
(defconstant +quad-double-float-negative-infinity+
  (make-instance 'qd-real :value (make-qd-d ext:double-float-negative-infinity))
  "Negative infinity for qd-real")

(defconstant +most-positive-quad-double-float+
  (make-instance 'qd-real
		 :value (qdi::%make-qd-d most-positive-double-float
					 (cl:scale-float most-positive-double-float (cl:* 1 -53))
					 (cl:scale-float most-positive-double-float (cl:* 2 -53))
					 (cl:scale-float most-positive-double-float (cl:* 3 -53)))))

(defconstant +least-positive-quad-double-float+
  (make-instance 'qd-real
		 :value (make-qd-d least-positive-double-float)))

;; Not sure this is 100% correct, but I think if the first component
;; is any smaller than this, the last component would no longer be a
;; normalized double-float.
(defconstant +least-positive-normalized-quad-double-float+
  (make-instance 'qd-real
		 :value (make-qd-d (cl:scale-float least-positive-normalized-double-float (cl:* 3 53)))))

(defconstant +qd-real-one+
  (make-instance 'qd-real :value (make-qd-d 1d0)))

(defmethod add1 ((a number))
  (cl::1+ a))

(defmethod add1 ((a qd-real))
  (make-instance 'qd-real :value (add-qd-d (qd-value a) 1d0)))

(defmethod sub1 ((a number))
  (cl::1- a))

(defmethod sub1 ((a qd-real))
  (make-instance 'qd-real :value (sub-qd-d (qd-value a) 1d0)))

(declaim (inline 1+ 1-))

(defun 1+ (x)
  (add1 x))

(defun 1- (x)
  (sub1 x))

(defmethod two-arg-+ ((a qd-real) (b qd-real))
  (make-instance 'qd-real :value (add-qd (qd-value a) (qd-value b))))

(defmethod two-arg-+ ((a qd-real) (b cl:float))
  (make-instance 'qd-real :value (add-qd-d (qd-value a) (cl:float b 1d0))))

#+cmu
(defmethod two-arg-+ ((a qd-real) (b ext:double-double-float))
  (make-instance 'qd-real :value (add-qd-dd (qd-value a) b)))

(defmethod two-arg-+ ((a real) (b qd-real))
  (+ b a))

(defmethod two-arg-+ ((a number) (b number))
  (cl:+ a b))

(defun + (&rest args)
  (if (null args)
      0
      (do ((args (cdr args) (cdr args))
	   (res (car args)
		(two-arg-+ res (car args))))
	  ((null args) res))))

(defmethod two-arg-- ((a qd-real) (b qd-real))
  (make-instance 'qd-real :value (sub-qd (qd-value a) (qd-value b))))

(defmethod two-arg-- ((a qd-real) (b cl:float))
  (make-instance 'qd-real :value (sub-qd-d (qd-value a) (cl:float b 1d0))))

#+cmu
(defmethod two-arg-- ((a qd-real) (b ext:double-double-float))
  (make-instance 'qd-real :value (sub-qd-dd (qd-value a) b)))

(defmethod two-arg-- ((a cl:float) (b qd-real))
  (make-instance 'qd-real :value (sub-d-qd (cl:float a 1d0) (qd-value b))))

(defmethod two-arg-- ((a number) (b number))
  (cl:- a b))

(defmethod unary-minus ((a number))
  (cl:- a))

(defmethod unary-minus ((a qd-real))
  (make-instance 'qd-real :value (neg-qd (qd-value a))))

(defun - (number &rest more-numbers)
  (if more-numbers
      (do ((nlist more-numbers (cdr nlist))
	   (result number))
	  ((atom nlist) result)
         (declare (list nlist))
	 (setq result (two-arg-- result (car nlist))))
      (unary-minus number)))


(defmethod two-arg-* ((a qd-real) (b qd-real))
  (make-instance 'qd-real :value (mul-qd (qd-value a) (qd-value b))))

(defmethod two-arg-* ((a qd-real) (b cl:float))
  (make-instance 'qd-real :value (mul-qd-d (qd-value a) (cl:float b 1d0))))

#+cmu
(defmethod two-arg-* ((a qd-real) (b ext:double-double-float))
  ;; We'd normally want to use mul-qd-dd, but mul-qd-dd is broken.
  (make-instance 'qd-real :value (mul-qd (qd-value a)
					 (make-qd-dd b 0w0))))

(defmethod two-arg-* ((a real) (b qd-real))
  (* b a))

(defmethod two-arg-* ((a number) (b number))
  (cl:* a b))

(defun * (&rest args)
  (if (null args)
      1
      (do ((args (cdr args) (cdr args))
	   (res (car args)
		(two-arg-* res (car args))))
	  ((null args) res))))

(defmethod two-arg-/ ((a qd-real) (b qd-real))
  (make-instance 'qd-real :value (div-qd (qd-value a) (qd-value b))))

(defmethod two-arg-/ ((a qd-real) (b cl:float))
  (make-instance 'qd-real :value (div-qd-d (qd-value a) (cl:float b 1d0))))

#+cmu
(defmethod two-arg-/ ((a qd-real) (b ext:double-double-float))
  (make-instance 'qd-real :value (div-qd-dd (qd-value a)
					    b)))

(defmethod two-arg-/ ((a cl:float) (b qd-real))
  (make-instance 'qd-real :value (div-qd (make-qd-d (cl:float a 1d0))
					 (qd-value b))))

#+cmu
(defmethod two-arg-/ ((a ext:double-double-float) (b qd-real))
  (make-instance 'qd-real :value (div-qd (make-qd-dd a 0w0)
					 (qd-value b))))

(defmethod two-arg-/ ((a number) (b number))
  (cl:/ a b))

(defmethod unary-divide ((a number))
  (cl:/ a))

(defmethod unary-divide ((a qd-real))
  (make-instance 'qd-real :value (div-qd +qd-one+ (qd-value a))))

(defun / (number &rest more-numbers)
  (if more-numbers
      (do ((nlist more-numbers (cdr nlist))
	   (result number))
	  ((atom nlist) result)
         (declare (list nlist))
	 (setq result (two-arg-/ result (car nlist))))
      (unary-divide number)))

(macrolet ((frob (name &optional (type 'real))
	     (let ((method-name (intern (concatenate 'string "Q" (symbol-name name))))
		   (cl-name (intern (symbol-name name) :cl))
		   (qd-name (intern (concatenate 'string (symbol-name name) "-QD"))))
	       `(progn
		  (defmethod ,method-name ((x ,type))
		    (,cl-name x))
		  (defmethod ,method-name ((x qd-real))
		    (,qd-name (qd-value x)))
		  (declaim (inline ,name))
		  (defun ,name (x)
		    (,method-name x))))))
  (frob zerop number)
  (frob plusp)
  (frob minusp))

(defun bignum-to-qd (bignum)
  (make-instance 'qd-real
		 :value (qdi::make-float (if (minusp bignum) -1 1)
					 (abs bignum)
					 0
					 0
					 0)))

(defmethod qfloat ((x real) (num-type cl:float))
  (cl:float x num-type))

(defmethod qfloat ((x cl:float) (num-type qd-real))
  (make-instance 'qd-real :value (make-qd-d (cl:float x 1d0))))

(defmethod qfloat ((x integer) (num-type qd-real))
  (cond ((typep x 'fixnum)
	 (make-instance 'qd-real :value (make-qd-d (cl:float x 1d0))))
	(t
	 ;; A bignum
	 (bignum-to-qd x))))

#+nil
(defmethod qfloat ((x ratio) (num-type qd-real))
  ;; This probably has some issues with roundoff
  (two-arg-/ (qfloat (numerator x) num-type)
	     (qfloat (denominator x) num-type)))

(defmethod qfloat ((x ratio) (num-type qd-real))
  ;; This probably has some issues with roundoff
  (let ((top (qd-value (qfloat (numerator x) num-type)))
	(bot (qd-value (qfloat (denominator x) num-type))))
    (make-instance 'qd-real :value (div-qd top bot))))
  
#+cmu
(defmethod qfloat ((x ext:double-double-float) (num-type qd-real))
    (make-instance 'qd-real :value (make-qd-dd x 0w0)))

(defmethod qfloat ((x qd-real) (num-type cl:float))
  (multiple-value-bind (q0 q1 q2 q3)
      (qd-parts (qd-value x))
    (cl:float (cl:+ q0 q1 q2 q3) num-type)))

#+cmu
(defmethod qfloat ((x qd-real) (num-type ext:double-double-float))
  (multiple-value-bind (q0 q1 q2 q3)
      (qd-parts (qd-value x))
    (cl:+ (cl:float q0 1w0)
	  (cl:float q1 1w0)
	  (cl:float q2 1w0)
	  (cl:float q3 1w0))))

(defmethod qfloat ((x qd-real) (num-type qd-real))
  x)

(declaim (inline float))
(defun float (x num-type)
  (qfloat x num-type))

(defmethod qrealpart ((x number))
  (cl:realpart x))
(defmethod qrealpart ((x qd-real))
  x)
(defmethod qrealpart ((x qd-complex))
  (make-instance 'qd-real :value (qd-real x)))
(defun realpart (x)
  (qrealpart x))

(defmethod qimagpart ((x number))
  (cl:imagpart x))
(defmethod qimagpart ((x qd-real))
  (make-qd 0d0))
(defmethod qimagpart ((x qd-complex))
  (make-instance 'qd-real :value (qd-imag x)))

(defun imagpart (x)
  (qimagpart x))

(defmethod qconjugate ((a number))
  (cl:conjugate a))

(defmethod qconjugate ((a qd-real))
  (make-instance 'qd-real :value (qd-value a)))

(defmethod qconjugate ((a qd-complex))
  (make-instance 'qd-complex
		 :real (qd-real a)
		 :imag (neg-qd (qd-imag a))))

(defun conjugate (z)
  (qconjugate z))

(defmethod qscale-float ((f cl:float) (n integer))
  (cl:scale-float f n))

(defmethod qscale-float ((f qd-real) (n integer))
  (make-instance 'qd-real :value (scale-float-qd (qd-value f) n)))

(declaim (inline scale-float))
(defun scale-float (f n)
  (qscale-float f n))

(macrolet
    ((frob (op)
       (let ((method (intern (concatenate 'string "TWO-ARG-" (symbol-name op))))
	     (cl-fun (find-symbol (symbol-name op) :cl))
	     (qd-fun (intern (concatenate 'string "QD-" (symbol-name op))
			     (find-package :qdi))))
	 `(progn
	    (defmethod ,method ((a real) (b real))
	      (,cl-fun a b))
	    (defmethod ,method ((a qd-real) (b real))
	      (,qd-fun (qd-value a) (make-qd-d (cl:float b 1d0))))
	    (defmethod ,method ((a real) (b qd-real))
	      (,qd-fun (make-qd-d (cl:float a 1d0)) (qd-value b)))
	    (defmethod ,method ((a qd-real) (b qd-real))
	      (,qd-fun (qd-value a) (qd-value b)))
	    (defun ,op (number &rest more-numbers)
	      "Returns T if its arguments are in strictly increasing order, NIL otherwise."
	      (declare (optimize (safety 2))
		       (dynamic-extent more-numbers))
	      (do* ((n number (car nlist))
		    (nlist more-numbers (cdr nlist)))
		   ((atom nlist) t)
		(declare (list nlist))
		(if (not (,method n (car nlist))) (return nil))))))))
  (frob <)
  (frob >)
  (frob <=)
  (frob >=))

(macrolet ((frob (name)
	     (let ((method-name (intern (concatenate 'string "Q" (symbol-name name))))
		   (cl-name (intern (symbol-name name) :cl))
		   (qd-name (intern (concatenate 'string (symbol-name name) "-QD"))))
	       `(progn
		 (defmethod ,method-name ((x number))
		   (,cl-name x))
		 (defmethod ,method-name ((x qd-real))
		   (make-instance 'qd-real :value (,qd-name (qd-value x))))
		 (declaim (inline ,name))
		 (defun ,name (x)
		   (,method-name x))))))
  (frob abs)
  (frob exp)
  (frob sin)
  (frob cos)
  (frob tan)
  ;;(frob asin)
  ;;(frob acos)
  (frob sinh)
  (frob cosh)
  (frob tanh)
  (frob asinh)
  ;;(frob acosh)
  ;;(frob atanh)
  )

(defmethod qsqrt ((x number))
  (cl:sqrt x))

(defmethod qsqrt ((x qd-real))
  (if (minusp x)
      (make-instance 'qd-complex
		     :real +qd-zero+
		     :imag (sqrt-qd (neg-qd (qd-value x))))
      (make-instance 'qd-real :value (sqrt-qd (qd-value x)))))

(defun sqrt (x)
  (qsqrt x))

(defun scalb (x n)
  "Compute 2^N * X without compute 2^N first (use properties of the
underlying floating-point format"
  (declare (type qd-real x))
  (scale-float x n))

(declaim (inline qd-cssqs))
(defun qd-cssqs (z)
  (multiple-value-bind (rho k)
      (qdi::hypot-aux-qd (qd-value (realpart z))
			 (qd-value (imagpart z)))
    (values (make-instance 'qd-real :value rho)
	    k)))

#+nil
(defmethod qabs ((z qd-complex))
  ;; sqrt(x^2+y^2)
  ;; If |x| > |y| then sqrt(x^2+y^2) = |x|*sqrt(1+(y/x)^2)
  (multiple-value-bind (abs^2 rho)
      (hypot-qd (qd-value (realpart z))
		(qd-value (imagpart z)))
    (scale-float (make-instance 'qd-real :value (sqrt abs^2))
		 rho)))

(defmethod qabs ((z qd-complex))
  ;; sqrt(x^2+y^2)
  ;; If |x| > |y| then sqrt(x^2+y^2) = |x|*sqrt(1+(y/x)^2)
  (make-instance 'qd-real
		 :value (hypot-qd (qd-value (realpart z))
				  (qd-value (imagpart z)))))

(defmethod qlog ((a number) &optional b)
  (if b
      (cl:log a b)
      (cl:log a)))

(defmethod qlog ((a qd-real) &optional b)
  (if b
      (/ (qlog a) (qlog b))
      (if (minusp (float-sign a))
	  (make-instance 'qd-complex
			 :real (log-qd (abs-qd (qd-value a)))
			 :imag +qd-pi+)
	  (make-instance 'qd-real :value (log-qd (qd-value a))))))

(declaim (inline log))
(defun log (a &optional b)
  (qlog a b))


(defmethod log1p ((a qd-real))
  (make-instance 'qd-real :value (log1p-qd (qd-value a))))

(defmethod qatan ((y real) &optional x)
  (cond (x
	 (cond ((typep x 'qd-real)
		(make-instance 'qd-real
			       :value (atan2-qd (qd-value y) (qd-value x))))
	       (t
		(cl:atan y x))))
	(t
	 (cl:atan y))))

(defmethod qatan ((y qd-real) &optional x)
  (make-instance 'qd-real
		 :value
		 (if x
		     (atan2-qd (qd-value y) (qd-value x))
		     (atan-qd (qd-value y)))))

(defun atan (y &optional x)
  (qatan y x))


(defmethod qexpt ((x number) (y number))
  (cl:expt x y))

(defmethod qexpt ((x qd-real) (y real))
  (exp (* y (log x))))

(defmethod qexpt ((x real) (y qd-real))
  (exp (* y (log x))))

(defmethod qexpt ((x qd-real) (y cl:complex))
  (exp (* (make-instance 'qd-complex
			 :real (qd-value (realpart y))
			 :imag (qd-value (imagpart y)))
	  (log x))))

(defmethod qexpt ((x cl:complex) (y qd-real))
  (exp (* y
	  (log (make-instance 'qd-complex
			      :real (qd-value (realpart x))
			      :imag (qd-value (imagpart x)))))))

(defmethod qexpt ((x qd-real) (y qd-real))
  ;; x^y = exp(y*log(x))
  (exp (* y (log x))))

(defmethod qexpt ((x qd-real) (y integer))
  (make-instance 'qd-real
		 :value (npow (qd-value x) y)))

(declaim (inline expt))
(defun expt (x y)
  (qexpt x y))



(defmethod two-arg-= ((a number) (b number))
  (cl:= a b))
(defmethod two-arg-= ((a qd-real) (b number))
  (if (realp b)
      (qd-= (qd-value a) (make-qd-d (cl:float b 1d0)))
      nil))
(defmethod two-arg-= ((a number) (b qd-real))
  (if (realp a)
      (qd-= (make-qd-d (cl:float a 1d0)) (qd-value b))
      nil))

(defmethod two-arg-= ((a qd-real) (b qd-real))
  (qd-= (qd-value a) (qd-value b)))

(defun = (number &rest more-numbers)
  "Returns T if all of its arguments are numerically equal, NIL otherwise."
  (declare (optimize (safety 2))
	   (dynamic-extent more-numbers))
  (do ((nlist more-numbers (cdr nlist)))
      ((atom nlist) T)
    (declare (list nlist))
    (if (not (two-arg-= (car nlist) number))
	(return nil))))

(defun /= (number &rest more-numbers)
  "Returns T if no two of its arguments are numerically equal, NIL otherwise."
  (declare (optimize (safety 2))
	   (dynamic-extent more-numbers))
  (do* ((head number (car nlist))
	(nlist more-numbers (cdr nlist)))
       ((atom nlist) t)
    (declare (list nlist))
    (unless (do* ((nl nlist (cdr nl)))
		 ((atom nl) T)
	      (declare (list nl))
	      (if (two-arg-= head (car nl))
		  (return nil)))
      (return nil))))

(defmethod qcomplex ((x real) &optional y)
  (cl:complex x (if y y 0)))

(defmethod qcomplex ((x qd-real) &optional y)
  (make-instance 'qd-complex
		 :real (qd-value x)
		 :imag (if y (qd-value y) +qd-zero+)))

(defun complex (x &optional (y 0))
  (qcomplex x y))

(defmethod qinteger-decode-float ((f cl:float))
  (cl:integer-decode-float f))

(defmethod qinteger-decode-float ((f qd-real))
  (integer-decode-qd (qd-value f)))

(declaim (inline integer-decode-float))
(defun integer-decode-float (f)
  (qinteger-decode-float f))

(defmethod qdecode-float ((f cl:float))
  (cl:decode-float f))

(defmethod qdecode-float ((f qd-real))
  (multiple-value-bind (frac exp s)
      (decode-float-qd (qd-value f))
    (values (make-instance 'qd-real :value frac)
	    exp
	    (make-instance 'qd-real :value  s))))

(declaim (inline decode-float))
(defun decode-float (f)
  (qdecode-float f))

(defmethod qfloor ((x real) &optional y)
  (if y
      (cl:floor x y)
      (cl:floor x)))

(defmethod qfloor ((x qd-real) &optional y)
  (if (and y (/= y 1))
      (let ((f (qfloor (/ x y))))
	(values f
		(- x (* f y))))
      (let ((f (ffloor-qd (qd-value x))))
	(multiple-value-bind (int exp sign)
	    (integer-decode-qd f)
	  (values (ash (* sign int) exp)
		  (make-instance 'qd-real
				 :value (qd-value
					 (- x (make-instance 'qd-real
							     :value f)))))))))

(defun floor (x &optional y)
  (qfloor x y))

(defmethod qffloor ((x real) &optional y)
  (if y
      (cl:ffloor x y)
      (cl:ffloor x)))

(defmethod qffloor ((x qd-real) &optional y)
  (if (and y (/= y 1))
      (let ((f (qffloor (/ x y))))
	(values f
		(- x (* f y))))
      (let ((f (make-instance 'qd-real :value (ffloor-qd (qd-value x)))))
	(values f
		(- x f)))))

(defun ffloor (x &optional y)
  (qffloor x y))

(defun ceiling (x &optional y)
  (multiple-value-bind (f rem)
      (floor x y)
    (if (zerop rem)
	(values (+ f 1)
		rem)
	(values (+ f 1)
		(- rem 1)))))

(defun fceiling (x &optional y)
  (multiple-value-bind (f rem)
      (ffloor x y)
    (if (zerop rem)
	(values (+ f 1)
		rem)
	(values (+ f 1)
		(- rem 1)))))

(defun truncate (x &optional (y 1))
  (if (minusp x)
      (ceiling x y)
      (floor x y)))

(defun ftruncate (x &optional (y 1))
  (if (minusp x)
      (fceiling x y)
      (ffloor x y)))

(defmethod %unary-round ((x real))
  (cl::round x))

(defmethod %unary-round ((number qd-real))
  (multiple-value-bind (bits exp)
      (integer-decode-float number)
    (let* ((shifted (ash bits exp))
	   (rounded (if (and (minusp exp)
			     (oddp shifted)
			     (not (zerop (logand bits
						 (ash 1 (- -1 exp))))))
			(1+ shifted)
			shifted)))
      (if (minusp number)
	  (- rounded)
	  rounded))))

(defun round (number &optional (divisor 1))
  (if (eql divisor 1)
      (let ((r (%unary-round number)))
	(values r
		(- number r)))
      (multiple-value-bind (tru rem)
	  (truncate number divisor)
	(if (zerop rem)
	    (values tru rem)
	    (let ((thresh (/ (abs divisor) 2)))
	      (cond ((or (> rem thresh)
			 (and (= rem thresh) (oddp tru)))
		     (if (minusp divisor)
			 (values (- tru 1) (+ rem divisor))
			 (values (+ tru 1) (- rem divisor))))
		    ((let ((-thresh (- thresh)))
		       (or (< rem -thresh)
			   (and (= rem -thresh) (oddp tru))))
		     (if (minusp divisor)
			 (values (+ tru 1) (- rem divisor))
			 (values (- tru 1) (+ rem divisor))))
		    (t (values tru rem))))))))

(defun fround (number &optional (divisor 1))
  "Same as ROUND, but returns first value as a float."
  (multiple-value-bind (res rem)
      (round number divisor)
    (values (float res (if (floatp rem) rem 1.0)) rem)))

(defmethod qfloat-sign ((a real) &optional (f (float 1 a)))
  (cl:float-sign a f))

(defmethod qfloat-sign ((a qd-real) &optional f)
  (if f
      (make-instance 'qd-real
		     :value (mul-qd-d (abs-qd (qd-value f))
				      (cl:float-sign (qd-0 (qd-value a)))))
      (make-instance 'qd-real :value (make-qd-d (cl:float-sign (qd-0 (qd-value a)))))))

(declaim (inline float-sign))
(defun float-sign (n &optional float2)
  (qfloat-sign n float2))

(defun max (number &rest more-numbers)
  "Returns the greatest of its arguments."
  (declare (optimize (safety 2)) (type (or real qd-real) number)
	   (dynamic-extent more-numbers))
  (dolist (real more-numbers)
    (when (> real number)
      (setq number real)))
  number)

(defun min (number &rest more-numbers)
  "Returns the least of its arguments."
  (declare (optimize (safety 2)) (type (or real qd-real) number)
	   (dynamic-extent more-numbers))
  (do ((nlist more-numbers (cdr nlist))
       (result (the (or real qd-real) number)))
      ((null nlist) (return result))
    (declare (list nlist))
    (if (< (car nlist) result)
	(setq result (car nlist)))))

(defmethod qasin ((x number))
  (cl:asin x))

(defmethod qasin ((x qd-real))
  (if (<= -1 x 1)
      (make-instance 'qd-real :value (asin-qd (qd-value x)))
      (qd-complex-asin x)))

(declaim (inline asin))
(defun asin (x)
  (qasin x))

(defmethod qacos ((x number))
  (cl:acos x))

(defmethod qacos ((x qd-real))
  (cond ((> (abs x) 1)
	 (qd-complex-acos x))
	(t
	 (make-instance 'qd-real :value (acos-qd (qd-value x))))))

(declaim (inline acos))
(defun acos (x)
  (qacos x))

(defmethod qacosh ((x number))
  (cl:acosh x))

(defmethod qacosh ((x qd-real))
  (if (< x 1)
      (qd-complex-acosh x)
      (make-instance 'qd-real :value (acosh-qd (qd-value x)))))


(declaim (inline acosh))
(defun acosh (x)
  (qacosh x))

(defmethod qatanh ((x number))
  (cl:atanh x))

(defmethod qatanh ((x qd-real))
  (if (> (abs x) 1)
      (qd-complex-atanh x)
      (make-instance 'qd-real :value (atanh-qd (qd-value x)))))


(declaim (inline atanh))
(defun atanh (x)
  (qatanh x))

(defmethod qcis ((x real))
  (cl:cis x))

(defmethod qcis ((x qd-real))
  (multiple-value-bind (s c)
      (sincos-qd (qd-value x))
    (make-instance 'qd-complex
		   :real c
		   :imag s)))

(declaim (inline cis))
(defun cis (x)
  (qcis x))

(defmethod qphase ((x number))
  (cl:phase x))

(defmethod qphase ((x qd-real))
  (if (minusp x)
      (- +pi+)
      (make-instance 'qd-real :value (make-qd-d 0d0))))

(declaim (inline phase))
(defun phase (x)
  (qphase x))

(defun signum (number)
  "If NUMBER is zero, return NUMBER, else return (/ NUMBER (ABS NUMBER))."
  (if (zerop number)
      number
      (if (rationalp number)
	  (if (plusp number) 1 -1)
	  (/ number (abs number)))))

(defmethod random ((x cl:real) &optional (state *random-state*))
  (cl:random x state))

(defmethod random ((x qd-real) &optional (state *random-state*))
  (* x (make-instance 'qd-real
		      :value (qdi:random-qd state))))

(define-compiler-macro + (&whole form &rest args)
  (if (null args)
      0
      (do ((args (cdr args) (cdr args))
	   (res (car args)
		`(two-arg-+ ,res ,(car args))))
	  ((null args) res))))

(define-compiler-macro - (&whole form number &rest more-numbers)
  (if more-numbers
      (do ((nlist more-numbers (cdr nlist))
	   (result number))
	  ((atom nlist) result)
         (declare (list nlist))
	 (setq result `(two-arg-- ,result ,(car nlist))))
      `(unary-minus ,number)))

(define-compiler-macro * (&whole form &rest args)
  (if (null args)
      1
      (do ((args (cdr args) (cdr args))
	   (res (car args)
		`(two-arg-* ,res ,(car args))))
	  ((null args) res))))

(define-compiler-macro / (number &rest more-numbers)
  (if more-numbers
      (do ((nlist more-numbers (cdr nlist))
	   (result number))
	  ((atom nlist) result)
         (declare (list nlist))
	 (setq result `(two-arg-/ ,result ,(car nlist))))
      `(unary-divide ,number)))

;; Compiler macros to convert <, >, <=, and >= into multiple calls of
;; the corresponding two-arg-<foo> function.
(macrolet
    ((frob (op)
       (let ((method (intern (concatenate 'string "TWO-ARG-" (symbol-name op)))))
	 `(define-compiler-macro ,op (number &rest more-numbers)
	    (do* ((n number (car nlist))
		  (nlist more-numbers (cdr nlist))
		  (res nil))
		 ((atom nlist) 
		  `(and ,@(nreverse res)))
	      (push `(,',method ,n ,(car nlist)) res))))))
  (frob <)
  (frob >)
  (frob <=)
  (frob >=))

(define-compiler-macro /= (&whole form number &rest more-numbers)
  ;; Convert (/= x y) to (not (two-arg-= x y)).  Should we try to
  ;; handle a few more cases?
  (if (cdr more-numbers)
      form
      `(not (two-arg-= ,number ,(car more-numbers)))))
  

(defun read-qd-real-or-complex (stream)
  (let ((c (peek-char t stream)))
    (cond ((char= c #\()
	   ;; Read a QD complex
	   (read-char stream)		; Skip the paren
	   (let ((real (read stream t nil t))
		 (imag (read stream t nil t)))
	     (unless (char= (peek-char t stream) #\))
	       (error "Illegal QD-COMPLEX number format"))
	     ;; Read closing paren
	     (read-char stream)
	     (make-instance 'qd-complex
			    :real (qd-value (float real +qd-real-one+))
			    :imag (qd-value (float imag +qd-real-one+)))))
	  (t
	   (make-instance 'qd-real :value (read-qd stream))))))
	
(defun qd-class-reader (stream subchar arg)
  (declare (ignore subchar))
  (when arg
    (warn "Numeric argument ignored in #~DQ" arg))
  (read-qd-real-or-complex stream))

;; Yow!  We redefine the #q reader that is in qd-io.lisp to read in
;; and make a real qd-real float, instead of the hackish
;; %qd-real.
(set-dispatch-macro-character #\# #\Q #'qd-class-reader)

