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

(define-symbol-macro * cl:*)
(define-symbol-macro - cl:-)
(define-symbol-macro / cl:/)

(defclass qd-real ()
  ((qd :initform +qd-zero+
       :reader qd-value
       :initarg :value
       :type %quad-double)))

(defclass qd-complex ()
  ((real :initform +qd-zero+
	 :reader qd-real
	 :initarg :real
	 :type %quad-double)
   (imag :initform +qd-zero+
	 :reader qd-imag
	 :initarg :imag
	 :type %quad-double)))

#-cmu
(defmethod print-object ((qd qd-real) stream)
  (format stream "~/qdi::qd-format/" (qd-value qd)))

#+cmu
(defun print-qd (q stream)
  (declare (type %quad-double q))
  (if (or (ext:float-infinity-p (qd-0 q))
	  (ext:float-nan-p (qd-0 q)))
      (format stream "~/qdi::qd-format/" q)
      (format stream "#q~/qdi::qd-format/" q)))
#+cmu
(defmethod print-object ((qd qd-real) stream)
  (print-qd (qd-value qd) stream))

(defmethod make-qd ((x real))
  (make-instance 'qd-real :value (make-qd-d (float x 1d0))))

(defmethod make-qd ((x qd-real))
  (make-instance 'qd-real :value (qd-value x)))

(defmethod print-object ((qd qd-complex) stream)
  (format stream "#q(~/qdi::qd-format/ ~/qdi::qd-format/)"
	  (qd-real qd)
	  (qd-imag qd)))

(defmethod print-object ((qd qd-complex) stream)
  (write-string "#q(" stream)
  (print-qd (qd-real qd) stream)
  (write-char #\space stream)
  (print-qd (qd-imag qd) stream)
  (write-string ")" stream))

(defmethod qd-value ((x real))
  (make-qd-d (float x 1d0)))

(defmethod make-load-form ((qd qd-real) &optional environment)
  (declare (ignore environment))
  `(make-instance ',(class-of qd)
		  :value ',(qd-value qd)))

(defmethod make-load-form ((qd qd-complex) &optional environment)
  (declare (ignore environment))
  `(make-instance ',(class-of qd)
		  :real ',(qd-value (realpart qd))
		  :imag ',(qd-value (imagpart qd))))

(defmethod describe-object ((q qd-real) stream)
  (multiple-value-bind (q0 q1 q2 q3)
      (qd-parts (qd-value q))
    (format stream "~&~S is a QD-REAL with components ~
                    ~%  ~A, ~A, ~A, ~A~%"
	    q q0 q1 q2 q3)))

(defmethod describe-object ((q qd-complex) stream)
  (format stream "~&~S is a QD-COMPLEX" q)
  (format stream "~&It has components~&REAL: ")
  (describe (realpart q))
  (format stream "~&IMAG: ")
  (describe (imagpart q)))


(defgeneric add1 (a)
  (:documentation "Add 1"))

(defgeneric sub1 (a)
  (:documentation "Subtract 1"))


(defgeneric two-arg-+ (a b)
  (:documentation "A + B"))

(defgeneric two-arg-- (a b)
  (:documentation "A - B"))

(defgeneric two-arg-* (a b)
  (:documentation "A * B"))

(defgeneric two-arg-/ (a b)
  (:documentation "A / B"))

(defgeneric two-arg-< (a b)
  (:documentation "A < B"))

(defgeneric two-arg-> (a b)
  (:documentation "A > B"))

(defgeneric two-arg-<= (a b)
  (:documentation "A <= B"))

(defgeneric two-arg->= (a b)
  (:documentation "A >= B"))

(defgeneric two-arg-= (a b)
  (:documentation "A = B?"))


(defgeneric unary-minus (a)
  (:documentation "-A"))

(defgeneric unary-divide (a)
  (:documentation "1 / A"))

(defgeneric qzerop (a)
  (:documentation "A = 0?"))

(defgeneric qplusp (a)
  (:documentation "A > 0"))

(defgeneric qminusp (a)
  (:documentation "A < 0"))

(defgeneric qfloat (x ftype)
  (:documentation "Convert X to a float of the same type a FLOAT"))

(defgeneric qrealpart (x)
  (:documentation "The real part of X"))

(defgeneric qimagpart (x)
  (:documentation "The imaginary part of X"))

(defgeneric qconjugate (z)
  (:documentation "The complex conjugate of Z"))

(defgeneric qscale-float (x n)
  (:documentation "Multiply the float X by 2^N"))

(defgeneric qabs (x)
  (:documentation "Absolute value of X"))

(defgeneric qexp (x)
  (:documentation "Exponential of X"))

(defgeneric qsin (x)
  (:documentation "Sine of X"))

(defgeneric qcos (x)
  (:documentation "Cosine of X"))

(defgeneric qtan (x)
  (:documentation "Tangent of X"))

(defgeneric qsinh (x)
  (:documentation "Hyperbolic sine of X"))

(defgeneric qcosh (x)
  (:documentation "Hyperbolic cosine of X"))

(defgeneric qtanh (x)
  (:documentation "Hyperbolic tangent of X"))

(defgeneric qsqrt (x)
  (:documentation "Square root of X"))

(defgeneric qlog (a &optional b)
  (:documentation "Log of A base B.  If B not given, then natural log"))

(defgeneric log1p (x)
  (:documentation "log(1+x)"))

(defgeneric qatan (y &optional x)
  (:documentation "If X not given, atan(y).  If X is given, atan(y/x), taking
 the quadrant into account"))

(defgeneric qexpt (x y)
  (:documentation "X^Y"))

(defgeneric qcomplex (x &optional y)
  (:documentation "Create a complex number with components X and Y.  If Y not given, assume 0"))

(defgeneric qinteger-decode-float (f)
  (:documentation "integer-decode-float"))

(defgeneric qdecode-float (f)
  (:documentation "decode-float"))

(defgeneric qfloor (x &optional y))

(defgeneric qffloor (x &optional y))

(defgeneric %unary-round (x))

(defgeneric qfloat-sign (a &optional b)
  (:documentation "Transfer sign of A to B.  If B not given, assume 1"))

(defgeneric qasin (x)
  (:documentation "Inverse sine of X"))

(defgeneric qacos (x)
  (:documentation "Inverse cosine of X"))

(defgeneric qacosh (x)
  (:documentation "Inverse hyperbolic cosine of X"))

(defgeneric qatanh (x)
  (:documentation "Inverse hyperbolic tangent of X"))

(defgeneric qcis (x)
  (:documentation "(complex (cos x) (sin x))"))

(defgeneric qphase (x)
  (:documentation "Phase of X"))

(defgeneric coerce (x type)
  (:documentation "COERCE"))

(defgeneric random (x &optional state)
  (:documentation "RANDOM"))
  
