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


(in-package #:qdi)

;; Compute to how many bits EST and TRUE are equal.  If they are
;; identical, return T.
(defun bit-accuracy (est true)
  (let* ((diff (abs-qd (sub-qd est true)))
	 (err (if (zerop-qd true)
		  (qd-0 diff)
		  (cl:/ (qd-0 diff) (abs (qd-0 true))))))
    (if (zerop (qd-0 diff))
	t
	(cl:- (log err 2d0)))))

(defun print-result (est true)
  (format t "est: ~/qdi::qd-format/~%" est)
  (format t "tru: ~/qdi::qd-format/~%" true)
  (format t "err: ~A~%" (qd-0 (sub-qd est true)))
  (format t "bits: ~,1f~%" (bit-accuracy est true)))
  
;; Machin's formula for pi
#+nil
(defun atan-series (x)
  (let* ((d 1d0)
	 (eps (make-qd-d (scale-float 1d0 -212)
			0d0 0d0 0d0))
	 (tmp x)
	 (r (sqr-qd tmp))
	 (s1 (make-qd-dd 0w0 0w0))
	 (k 0)
	 (sign 1))
    (loop while (qd-> tmp eps) do
	  (incf k)
	  (setf s1
		(if (minusp sign)
		    (sub-qd s1 (div-qd tmp (make-qd-d d 0d0 0d0 0d0)))
		    (add-qd s1 (div-qd tmp (make-qd-d d 0d0 0d0 0d0)))))
	  (incf d 2d0)
	  (setf tmp (mul-qd tmp r))
	  (setf sign (cl:- sign)))
    s1))

;; pi =
;; 3.1415926535897932384626433832795028841971693993751058209749445923078L0
(defun test2 ()
  ;; pi/4 = 4 * arctan(1/5) - arctan(1/239)
  ;;
  ;; Arctan is computed using the Taylor series
  ;;
  ;;   arctan(x) = x - x^3/3 + x^5/5 - x^7/7
  (flet ((atan-series (x)
	   (let* ((d 1d0)
		  (eps (make-qd-d (scale-float 1d0 -212)))
		  (tmp x)
		  (r (sqr-qd tmp))
		  (s1 (make-qd-d 0d0))
		  (k 0)
		  (sign 1))
	     (loop while (qd-> tmp eps) do
		   (incf k)
		   (setf s1
			 (if (minusp sign)
			     (sub-qd s1 (div-qd tmp (make-qd-d d)))
			     (add-qd s1 (div-qd tmp (make-qd-d d)))))
		   (incf d 2d0)
		   (setf tmp (mul-qd tmp r))
		   (setf sign (cl:- sign)))
	     s1)))
    (let* ((x1 (div-qd +qd-one+
		       (make-qd-d 5d0)))
	   (s1 (atan-series x1))
	   (x2 (div-qd +qd-one+
		       (make-qd-d 239d0)))
	   (s2 (atan-series x2))
	   (p (mul-qd-d (sub-qd (mul-qd-d s1 4d0)
				s2)
			4d0)))
      (format t "~2&pi via Machin's atan formula~%")
      (print-result p +qd-pi+)
      p)))

(defun test3 ()
  (declare (optimize (speed 3)))
  ;; Salamin-Brent Quadratic formula for pi
  (let* ((a +qd-one+)
	 (b (sqrt-qd (make-qd-d 0.5d0)))
	 (s (make-qd-d 0.5d0))
	 (m 1d0)
	 (p (div-qd (mul-qd-d (sqr-qd a) 2d0)
		    s)))
    (declare (double-float m))
    (dotimes (k 9)
      (setf m (cl:* 2 m))
      (let* ((a-new (mul-qd-d (add-qd a b) .5d0))
	     (b-new (sqrt-qd (mul-qd a b)))
	     (s-new (sub-qd s
			    (mul-qd-d (sub-qd (sqr-qd a-new)
					      (sqr-qd b-new))
				      m))))
	(setf a a-new)
	(setf b b-new)
	(setf s s-new)
	(setf p (div-qd (mul-qd-d (sqr-qd a) 2d0)
			s))))
    (format t "~2&Salamin-Brent Quadratic formula for pi~%")
    (print-result p +qd-pi+)
    p))

(defun test4 ()
  (declare (optimize (speed 3)))
  ;; Borwein Quartic formula for pi
  (let* ((a (sub-qd (make-qd-d 6d0)
		    (mul-qd-d (sqrt-qd (make-qd-d 2d0))
			      4d0)))
	 (y (sub-qd-d (sqrt-qd (make-qd-d 2d0))
		      1d0))
	 (m 2d0)
	 (p (div-qd +qd-one+
		    a)))
    (declare (double-float m))
    (dotimes (k 9)
      (setf m (cl:* 4 m))
      (let ((r (nroot-qd (sub-qd +qd-one+
				 (sqr-qd (sqr-qd y)))
			 4)))
	(setf y (div-qd (sub-d-qd 1d0
				  r)
			(add-d-qd 1d0
				r)))
	(setf a (sub-qd (mul-qd a
				(sqr-qd (sqr-qd (add-qd-d y 1d0))))
			(mul-qd-d (mul-qd y
					  (add-qd-d (add-qd y (sqr-qd y))
						    1d0))
				  m)))
	(setf p (div-qd +qd-one+
			a))))
    (format t "~2&Borwein's Quartic formula for pi~%")
    (print-result p +qd-pi+)
    p))

;; e =
;; 2.718281828459045235360287471352662497757247093699959574966967627724L0
(defun test5 ()
  ;; Taylor series for e
  (let ((s (make-qd-d 2d0))
	(tmp +qd-one+)
	(n 1d0)
	(delta 0d0)
	(i 0))
    (loop while (qd-> tmp (make-qd-d 1d-100)) do
	  (incf i)
	  (incf n)
	  (setf tmp (div-qd tmp
			    (make-qd-d (float n 1d0))))
	  (setf s (add-qd s tmp)))
    (format t "~2&e via Taylor series~%")
    (print-result s +qd-e+)
    s))

;; log(2) =
;; 0.6931471805599453094172321214581765680755001343602552541206800094934L0
(defun test6 ()
  ;; Taylor series for log 2
  ;;
  ;; -log(1-x) = x + x^2/2 + x^3/3 + x^4/4 + ...
  ;;
  ;; with x = 1/2 to get log(1/2) = -log(2)
  (let ((s (make-qd-d .5d0))
	(tt (make-qd-d .5d0))
	(n 1d0)
	(i 0))
    (loop while (qd-> tt (make-qd-d 1d-100)) do
	  (incf i)
	  (incf n)
	  (setf tt (mul-qd-d tt .5d0))
	  (setf s (add-qd s
			  (div-qd tt (make-qd-d (float n 1d0))))))
    (format t "~2&log(2) via Taylor series~%")
    (print-result s +qd-log2+)
    s))

(defun test-atan (&optional (fun #'atan-qd))
  ;; Compute atan for known values

  (format t "~2&atan via ~S~%" fun)
  ;; atan(1/sqrt(3)) = pi/6
  (let* ((arg (div-qd +qd-one+ (sqrt-qd (make-qd-d 3d0))))
	 (y (div-qd (funcall fun arg) +qd-pi+))
	 (true (div-qd +qd-one+ (make-qd-d 6d0))))
    (format t "atan(1/sqrt(3))/pi = ~/qdi::qd-format/~%" y)
    (format t "1/6                = ~/qdi::qd-format/~%" true)
    (format t "bits               = ~,1f~%"
	    (bit-accuracy y true)))
  ;; atan(sqrt(3)) = %pi/3
  (let* ((arg (sqrt-qd (make-qd-d 3d0)))
	 (y (div-qd (funcall fun arg) +qd-pi+))
	 (true (div-qd +qd-one+ (make-qd-d 3d0))))
    (format t "atan(sqrt(3))/pi   = ~/qdi::qd-format/~%" y)
    (format t "1/3                = ~/qdi::qd-format/~%" true)
    (format t "bits               = ~,1f~%"
	    (bit-accuracy y true)))
  ;; atan(1) = %pi/4
  (let* ((arg +qd-one+)
	 (y (div-qd (funcall fun arg) +qd-pi+))
	 (true (div-qd +qd-one+ (make-qd-d 4d0))))
    (format t "atan(1)/pi         = ~/qdi::qd-format/~%" y)
    (format t "1/4                = ~/qdi::qd-format/~%" true)
    (format t "bits               = ~,1f~%"
	    (bit-accuracy y true))))

(defun test-sin ()
  (format t "~2&sin~%")
  (let* ((arg (div-qd +qd-pi+ (make-qd-d 6d0)))
	 (y (sin-qd arg))
	 (true (make-qd-d 0.5d0)))
    (format t "sin(pi/6)      = ~/qdi::qd-format/~%" y)
    (format t "1/2            = ~/qdi::qd-format/~%" true)
    (format t "bits           = ~,1f~%"
	    (bit-accuracy y true)))
  (let* ((arg (div-qd +qd-pi+ (make-qd-d 4d0)))
	 (y (sin-qd arg))
	 (true (sqrt-qd (make-qd-d 0.5d0))))
    (format t "sin(pi/4)      = ~/qdi::qd-format/~%" y)
    (format t "1/sqrt(2)      = ~/qdi::qd-format/~%" true)
    (format t "bits           = ~,1f~%"
	    (bit-accuracy y true)))
  (let* ((arg (div-qd +qd-pi+ (make-qd-d 3d0)))
	 (y (sin-qd arg))
	 (true (div-qd (sqrt-qd (make-qd-d 3d0)) (make-qd-d 2d0))))
    (format t "sin(pi/3)      = ~/qdi::qd-format/~%" y)
    (format t "sqrt(3)/2      = ~/qdi::qd-format/~%" true)
    (format t "bits           = ~,1f~%"
	    (bit-accuracy y true)))
  )

(defun test-tan (&optional (f #'tan-qd))
  (format t "~2&tan via ~S~%" f)
  (let* ((arg (div-qd +qd-pi+ (make-qd-d 6d0)))
	 (y (funcall f arg))
	 (true (div-qd +qd-one+ (sqrt-qd (make-qd-d 3d0)))))
    (format t "tan(pi/6)      = ~/qdi::qd-format/~%" y)
    (format t "1/sqrt(3)      = ~/qdi::qd-format/~%" true)
    (format t "bits           = ~,1f~%"
	    (bit-accuracy y true)))
  (let* ((arg (div-qd +qd-pi+ (make-qd-d 4d0)))
	 (y (funcall f arg))
	 (true +qd-one+))
    (format t "tan(pi/4)      = ~/qdi::qd-format/~%" y)
    (format t "1              = ~/qdi::qd-format/~%" true)
    (format t "bits           = ~,1f~%"
	    (bit-accuracy y true)))
  (let* ((arg (div-qd +qd-pi+ (make-qd-d 3d0)))
	 (y (funcall f arg))
	 (true (sqrt-qd (make-qd-d 3d0))))
    (format t "tan(pi/3)      = ~/qdi::qd-format/~%" y)
    (format t "sqrt(3)        = ~/qdi::qd-format/~%" true)
    (format t "bits           = ~,1f~%"
	    (bit-accuracy y true)))
  )
    
(defun test-asin ()
  (format t "~2&asin~%")
  (let* ((arg (make-qd-d 0.5d0))
	 (y (asin-qd arg))
	 (true (div-qd +qd-pi+ (make-qd-d 6d0))))
    (format t "asin(1/2)      = ~/qdi::qd-format/~%" y)
    (format t "pi/6           = ~/qdi::qd-format/~%" true)
    (format t "bits           = ~,1f~%"
	    (bit-accuracy y true)))
  (let* ((arg (sqrt-qd (make-qd-d 0.5d0)))
	 (y (asin-qd arg))
	 (true (div-qd +qd-pi+ (make-qd-d 4d0))))
    (format t "asin(1/sqrt(2))= ~/qdi::qd-format/~%" y)
    (format t "pi/4           = ~/qdi::qd-format/~%" true)
    (format t "bits           = ~,1f~%"
	    (bit-accuracy y true)))
  (let* ((arg (div-qd (sqrt-qd (make-qd-d 3d0)) (make-qd-d 2d0)))
	 (y (asin-qd arg))
	 (true (div-qd +qd-pi+ (make-qd-d 3d0))))
    (format t "asin(sqrt(3)/2)= ~/qdi::qd-format/~%" y)
    (format t "pi/3           = ~/qdi::qd-format/~%" true)
    (format t "bits           = ~,1f~%"
	    (bit-accuracy y true)))
  )
    
(defun test-log (f)
  (format t "~2&Log via ~A~%" f)
  (let* ((arg (make-qd-d 2d0))
	 (y (funcall f arg))
	 (true +qd-log2+))
    (format t "log(2)      = ~/qdi::qd-format/~%" y)
    (format t "log(2)      = ~/qdi::qd-format/~%" true)
    (format t "bits        = ~,1f~%"
	    (bit-accuracy y true)))
  (let* ((arg (make-qd-d 10d0))
	 (y (funcall f arg))
	 (true +qd-log10+))
    (format t "log(10)     = ~/qdi::qd-format/~%" y)
    (format t "log(10)     = ~/qdi::qd-format/~%" true)
    (format t "bits        = ~,1f~%"
	    (bit-accuracy y true)))
  (let* ((arg (add-d-qd 1d0 (scale-float-qd (make-qd-d 1d0) -80)))
	 (y (funcall f arg))
	 (true (qd-from-string "8.2718061255302767487140834995607996176476940491239977084112840149578911975528492q-25")))
    (format t "log(1+2^-80) = ~/qdi::qd-format/~%" y)
    (format t "log(1+2^-80) = ~/qdi::qd-format/~%" true)
    (format t "bits         = ~,1f~%"
	    (bit-accuracy y true)))
  )

(defun test-log1p (f)
  (format t "~2&Log1p via ~A~%" f)
  (let* ((arg (make-qd-d 1d0))
	 (y (funcall f arg))
	 (true +qd-log2+))
    (format t "log1p(1)    = ~/qdi::qd-format/~%" y)
    (format t "log(2)      = ~/qdi::qd-format/~%" true)
    (format t "bits        = ~,1f~%"
	    (bit-accuracy y true)))
  (let* ((arg (make-qd-d 9d0))
	 (y (funcall f arg))
	 (true (qd-from-string "2.3025850929940456840179914546843642076011014886287729760333279009675726096773525q0")))
    (format t "log1p(9)    = ~/qdi::qd-format/~%" y)
    (format t "log(10)     = ~/qdi::qd-format/~%" true)
    (format t "bits        = ~,1f~%"
	    (bit-accuracy y true)))
  (let* ((arg (scale-float-qd (make-qd-d 1d0) -80))
	 (y (funcall f arg))
	 (true (qd-from-string "8.2718061255302767487140834995607996176476940491239977084112840149578911975528492q-25")))
    (format t "log1p(2^-80) = ~/qdi::qd-format/~%" y)
    (format t "log(1+2^-80) = ~/qdi::qd-format/~%" true)
    (format t "bits         = ~,1f~%"
	    (bit-accuracy y true)))
  )

(defun test-exp (f)
  (format t "~2&Exp via ~A~%" f)
  (let* ((arg +qd-zero+)
	 (y (funcall f arg))
	 (true +qd-zero+))
    (format t "exp(0)-1    = ~/qdi::qd-format/~%" y)
    (format t "0           = ~/qdi::qd-format/~%" true)
    (format t "bits        = ~,1f~%"
	    (bit-accuracy y true)))
  (let* ((arg +qd-one+)
	 (y (funcall f arg))
	 (true (qd-from-string "1.7182818284590452353602874713526624977572470936999595749669676277240766303535475945713821785251664274274663919320030599218174135966290435729003342952q0")))
    (format t "exp(1)-1    = ~/qdi::qd-format/~%" y)
    (format t "e-1         = ~/qdi::qd-format/~%" true)
    (format t "bits        = ~,1f~%"
	    (bit-accuracy y true)))

  (let* ((arg (scale-float-qd +qd-one+ -100))
	 (y (funcall f arg))
	 (true (qd-from-string "7.888609052210118054117285652830973804370994921943802079729680186943164342372119432861876389514693341738324702996270767390039172777809233288470357147q-31")))
    (format t "exp(2^-80)-1 = ~/qdi::qd-format/~%" y)
    (format t "exp(2^-80)-1 = ~/qdi::qd-format/~%" true)
    (format t "bits         = ~,1f~%"
	    (bit-accuracy y true)))

  )
(defun all-tests ()
  (test2)
  (test3)
  (test4)
  (test5)
  (test6)
  (test-sin)
  (test-asin)
  (dolist (f '(atan-qd/newton atan-qd/cordic atan-qd/duplication))
    (test-atan f))
  (dolist (f '(tan-qd/sincos tan-qd/cordic))
    (test-tan f))
  (dolist (f '(log-qd/newton log-qd/agm log-qd/agm2 log-qd/agm3 log-qd/halley))
    (test-log f))
  (dolist (f '(log1p-qd/duplication))
    (test-log1p f))
  (dolist (f (list #'(lambda (x)
		       (sub-qd-d (exp-qd/reduce x) 1d0))
		   #'expm1-qd/series
		   #'expm1-qd/duplication))
    (test-exp f))
  )
