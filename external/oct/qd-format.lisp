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

(defun qd-scale-exponent (original-x)
  (let* ((x original-x))
    (multiple-value-bind (sig exponent)
	(decode-float x)
      (declare (ignore sig))
      (if (= x #q0)
	  (values #q0 1)
	  (let* ((ex (round (* exponent (log #q2 10))))
		 (x (if (minusp ex)
			
			(* x #q10.0q0 (expt #q10.0q0 (- (- ex) 1)))
			(/ x #q10.0q0 (expt #q10.0q0 (1- ex))))))
	    (do ((d #q10.0q0 (* d #q10.0q0))
		 (y x (/ x d))
		 (ex ex (1+ ex)))
		((< y #q1.0q0)
		 (do ((m #q10.0q0 (* m #q10.0q0))
		      (z y (* y m))
		      (ex ex (1- ex)))
		     ((>= z #q0.1q0)
		      (values z ex))))))))))

(defun decimal-string (n)
  (cl:write-to-string n :base 10 :radix nil :escape nil))

(defun qd-format-exp-aux (stream number w d e k ovf pad marker atsign)
  (multiple-value-bind (num expt)
      (qd-scale-exponent (abs number))
    (let* ((expt (- expt k))
	   (estr (decimal-string (abs expt)))
	   (elen (if e (max (length estr) e) (length estr)))
	   (add-zero-p nil))
      (if (and w ovf e (> elen e))	;exponent overflow
	  (dotimes (i w)
	    (write-char ovf stream))
	  (let* ((fdig (if d (if (plusp k) (1+ (- d k)) d) nil))
		 (fmin (if (minusp k)
			   1
			   fdig))
		 (spaceleft (if w
				(- w 2 elen
				   (if (or atsign (minusp (float-sign number)))
				       1 0))
				nil)))
	    (multiple-value-bind (fstr flen lpoint tpoint)
		(qdi::qd-to-string (qd-value num) spaceleft fdig k fmin)
	      (when (and d (zerop d)) (setq tpoint nil))
	      (when w 
		(decf spaceleft flen)
		;; See CLHS 22.3.3.2.  "If the parameter d is
		;; omitted, ... [and] if the fraction to be
		;; printed is zero then a single zero digit should
		;; appear after the decimal point."  So we need to
		;; subtract one from here because we're going to
		;; add an extra 0 digit later.
		(when (and (null d) (char= (aref fstr (1- flen)) #\.))
		  (setf add-zero-p t)
		  (decf spaceleft))
		(when lpoint
		  (if (or (> spaceleft 0) tpoint)
		      (decf spaceleft)
		      (setq lpoint nil)))
		(when (and tpoint (<= spaceleft 0))
		  (setq tpoint nil)))
	      (cond ((and w (< spaceleft 0) ovf)
		     ;;significand overflow
		     (dotimes (i w) (write-char ovf stream)))
		    (t (when w
			 (dotimes (i spaceleft)
			   (write-char pad stream)))
		       (if (minusp (float-sign number))
			   (write-char #\- stream)
			   (if atsign (write-char #\+ stream)))
		       (when lpoint (write-char #\0 stream))
		       (write-string fstr stream)
		       ;; Add a zero if we need it.  Which means
		       ;; we figured out we need one above, or
		       ;; another condition.  Basically, append a
		       ;; zero if there are no width constraints
		       ;; and if the last char to print was a
		       ;; decimal (so the trailing fraction is
		       ;; zero.)
		       (when (or add-zero-p
				 (and (null w)
				      (char= (aref fstr (1- flen)) #\.)))
			 ;; It's later and we're adding the zero
			 ;; digit.
			 (write-char #\0 stream))
		       (write-char (if marker
				       marker
				       #\q)
				   stream)
		       (write-char (if (minusp expt) #\- #\+) stream)
		       (when e 
			 ;;zero-fill before exponent if necessary
			 (dotimes (i (- e (length estr)))
			   (write-char #\0 stream)))
		       (write-string estr stream)))))))))

(defun qd-format-exp (stream arg colon-p at-sign-p
		      &optional w d e (k 1) ovf (pad #\space) exp-marker)
  (declare (ignore colon-p))
  (qd-format-exp-aux stream arg w d e k ovf pad exp-marker at-sign-p))
