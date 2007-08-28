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

;; Smallest exponent for a double-float.
(eval-when (:compile-toplevel :load-toplevel :execute)
(defconstant +double-float-min-e+
  -1073)

(defconstant +digits+
  "0123456789")
) ; eval-when

(defun qd-to-digits (v &optional position relativep)
  ;; V is the number to be printed.  If RELATIVEP is NIL, POSITION is
  ;; the number of digits to the left of the decimal point where we
  ;; want to stop printing.  If RELATIVEP is non-NIL, POSITION is the
  ;; total number of digits we want printed.
  ;;
  ;; Two values are returned: k, and the digit string, without a
  ;; decimal point.  k is the index into the string, before which the
  ;; decimal point would go.
  (let ((print-base 10)			; B
	(float-radix 2)			; b
	(float-digits (cl:* 4 53)) ; p
	(min-e +double-float-min-e+))
    (multiple-value-bind (f e)
	(integer-decode-qd v)
      (let ( ;; FIXME: these even tests assume normal IEEE rounding
	    ;; mode.  I wonder if we should cater for non-normal?
	    (high-ok (evenp f))
	    (low-ok (evenp f))
	    (result (make-array 50 :element-type 'base-char
				:fill-pointer 0 :adjustable t)))
	(labels ((scale (r s m+ m-)
		   ;; Keep increasing k until it's big enough
		   (do ((k 0 (1+ k))
			(s s (cl:* s print-base)))
		       ((not (let ((test (cl:+ r m+)))
			       (or (> test s)
				   (and high-ok (= test s)))))
			;; k is too big.  Decrease until
			(do ((k k (1- k))
			     (r r (cl:* r print-base))
			     (m+ m+ (cl:* m+ print-base))
			     (m- m- (cl:* m- print-base)))
			    ((not (let ((test (cl:* (cl:+ r m+) print-base)))
				    (or (< test s)
					(and (not high-ok) (= test s)))))
			     ;; k is correct.  Generate the digits.
			     (values k (generate r s m+ m-)))))))
		 (generate (r s m+ m-)
		   (multiple-value-bind (d r)
		       (truncate (cl:* r print-base) s)
		     (let ((m+ (cl:* m+ print-base))
			   (m- (cl:* m- print-base)))
		       (let ((tc1 (or (< r m-) (and low-ok (= r m-))))
			     (tc2 (let ((test (cl:+ r m+)))
				    (or (> test s)
					(and high-ok (= test s))))))
			 (cond
			   ((and (not tc1) (not tc2))
			    (vector-push-extend (char +digits+ d) result)
			    ;; FIXME sucky tail recursion.  This whole
			    ;; kaboodle should be DO*/LOOPified.
			    (generate r s m+ m-))
			   ;; pedantically keeping all the conditions
			   ;; in so that we can move them around.
			   ((and (not tc1) tc2)
			    (vector-push-extend (char +digits+ (1+ d)) result)
			    result)
			   ((and tc1 (not tc2))
			    (vector-push-extend (char +digits+ d) result)
			    result)
			   ((and tc1 tc2)
			    (vector-push-extend (char +digits+
						      (if (< (cl:* r 2) s) d (1+ d)))
						result)
			    result)))))))
	  (let (r s m+ m-)
	    (if (>= e 0)
		(let* ((be (expt float-radix e))
		       (be1 (cl:* be float-radix)))
		  (if (/= f (expt float-radix (1-
					       float-digits)))
		      (setf r (cl:* f be 2)
			    s 2
			    m+ be
			    m- be)
		      (setf r (cl:* f be1 2)
			    s (cl:* float-radix 2)
			    m+ be1
			    m- be)))
		(if (or (= e min-e) 
			(/= f (expt float-radix (1-
						 float-digits))))
		    (setf r (cl:* f 2)
			  s (cl:* (expt float-radix (cl:- e)) 2)
			  m+ 1
			  m- 1)
		    (setf r (cl:* f float-radix 2)
			  s (cl:* (expt float-radix (cl:- 1 e)) 2)
			  m+ float-radix
			  m- 1)))
	    (when position
	      (when relativep
		;;(aver (> position 0))
		(do ((k 0 (1+ k))
		     ;; running out of letters here
		     (l 1 (cl:* l print-base)))
		    ((>= (cl:* s l) (cl:+ r m+))
		     ;; k is now \hat{k}
		     (if (< (cl:+ r (cl:* s (cl:/ (expt print-base (cl:- k
							     position)) 2)))
			    (cl:* s (expt print-base k)))
			 (setf position (cl:- k position))
			 (setf position (cl:- k position 1))))))
	      (let ((low (max m- (cl:/ (cl:* s (expt print-base
					       position)) 2)))
		    (high (max m+ (cl:/ (cl:* s (expt print-base
						position)) 2))))
		(when (<= m- low)
		  (setf m- low)
		  (setf low-ok t))
		(when (<= m+ high)
		  (setf m+ high)
		  (setf high-ok t))))
	    (scale r s m+ m-)))))))

(defun qd-print-exponent (x exp stream)
  (let ((*print-radix* nil))
    (format stream "q~D" exp)))

(defun qd-to-string (x &optional width fdigits scale fmin)
  (setf x (abs-qd x))
  (cond ((zerop-qd x)
	 ;;zero is a special case which float-string cannot handle
	 (if fdigits
	     (let ((s (make-string (1+ fdigits) :initial-element #\0)))
	       (setf (schar s 0) #\.)
	       (values s (length s) t (zerop fdigits) 0))
	     (values "." 1 t t 0)))
	(t
	 (multiple-value-bind (e string)
	     (if fdigits
		 (qd-to-digits x (min (- (+ fdigits (or scale 0)))
					  (- (or fmin 0))))
		 (if (and width (> width 1))
		     (let ((w (multiple-value-list
			       (qd-to-digits x
					     (max 0
						  (+ (1- width)
						     (if (and scale (minusp scale))
							 scale 0)))
					     t)))
			   (f (multiple-value-list
			       (qd-to-digits x (- (+ (or fmin 0)
						     (if scale scale 0)))))))
		       (cond
			 ((>= (length (cadr w)) (length (cadr f)))
			  (values-list w))
			 (t (values-list f))))
		     (qd-to-digits x)))
	   (let ((e (+ e (or scale 0)))
		 (stream (make-string-output-stream)))
	     (if (plusp e)
		 (progn
		   (write-string string stream :end (min (length string)
							 e))
		   (dotimes (i (- e (length string)))
		     (write-char #\0 stream))
		   (write-char #\. stream)
		   (write-string string stream :start (min (length string)
							   e))
		   (when fdigits
		     (dotimes (i (- fdigits
				    (- (length string) 
				       (min (length string) e))))
		       (write-char #\0 stream))))
		 (progn
		   (write-string "." stream)
		   (dotimes (i (- e))
		     (write-char #\0 stream))
		   ;; If we're out of room (because fdigits is too
		   ;; small), don't print out our string.  This fixes
		   ;; things like (format nil "~,2f" 0.001).  We should
		   ;; print ".00", not ".001".
		   (when (or (null fdigits)
			     (plusp (+ e fdigits)))
		     (write-string string stream))
		   (when fdigits
		     (dotimes (i (+ fdigits e (- (length string))))
		       (write-char #\0 stream)))))
	     (let ((string (get-output-stream-string stream)))
	       (values string (length string)
		       (char= (char string 0) #\.)
		       (char= (char string (1- (length string))) #\.)
		       (position #\. string))))))))


(defun qd-output-aux (x &optional (stream *standard-output*))
  (if (zerop-qd x)
      (write-string (if (minusp (float-sign (qd-0 x)))
			"-0.0q0"
			"0.0q0")
		    stream)
      (multiple-value-bind (e string)
	  (qd-to-digits x)
	(when (minusp (float-sign (qd-0 x)))
	  (write-char #\- stream))
	(cond ((< -3 e 8)
	       ;; Free format
	       (cond ((plusp e)
		      (write-string string stream :end (min (length string) e))
		      (dotimes (i (cl:- e (length string)))
			(write-char #\0 stream))
		      (write-char #\. stream)
		      (write-string string stream :start (min (length string) e))
		      (when (<= (length string) e)
			(write-char #\0 stream))
		      (qd-print-exponent x 0 stream))
		     (t
		      (write-string "0." stream)
		      (dotimes (i (cl:- e))
			(write-char #\0 stream))
		      (write-string string stream)
		      (qd-print-exponent x 0 stream))))
	      (t
	       ;; Exponential format
	       (write-string string stream :end 1)
	       (write-char #\. stream)
	       (write-string string stream :start 1)
	       ;; CLHS 22.1.3.1.3 says at least one digit must be printed
	       ;; after the decimal point.
	       (when (= (length string) 1)
		 (write-char #\0 stream))
	       (qd-print-exponent x (1- e) stream))))))

;; Function that can be used with FORMAT ~/
#-cmu
(defun qd-format (stream arg colon-p at-sign-p &rest par)
  ;; We should do something with colon-p and at-sign-p
  (declare (ignore colon-p at-sign-p par))
  (write-string "#q" stream)
  (qd-output-aux arg stream))

#+cmu
(defun qd-output-infinity (x stream)
  (cond (*read-eval*
	 (write-string "#." stream))
	(*print-readably*
	 (error 'print-not-readable :object x))
	(t
	 (write-string "#<" stream)))
  (write-string "QD:+QUAD-DOUBLE-FLOAT" stream)
  (write-string (if (plusp (qd-0 x))
		    "-POSITIVE-"
		    "-NEGATIVE-")
		stream)
  (write-string "INFINITY+" stream)
  (unless *read-eval*
    (write-string ">" stream)))

#+cmu
(defun qd-output-nan (x stream)
  (print-unreadable-object (x stream)
    (write-string "QUAD-DOUBLE-FLOAT" stream)
    (write-string (if (float-trapping-nan-p (qd-0 x)) " Trapping" " Quiet") stream)
    (write-string " NaN" stream)))

#+cmu
(defun qd-format (stream arg colon-p at-sign-p &rest par)
  (declare (type %quad-double arg)
	   (stream stream))
  ;; We should do something with colon-p and at-sign-p
  (declare (ignore colon-p at-sign-p par))
  (cond ((ext:float-infinity-p (qd-0 arg))
	 (qd-output-infinity arg stream))
	((ext:float-nan-p (qd-0 arg))
	 (qd-output-nan arg stream))
	(t
	 (qd-output-aux arg stream))))

(defun make-float (sign int-part frac-part scale exp)
  (declare (type (member -1 1) sign)
	   (type unsigned-byte int-part frac-part)
	   (fixnum scale exp))
  #+(or)
  (progn
    (format t "sign      = ~A~%" sign)
    (format t "int-part  = ~A~%" int-part)
    (format t "frac-part = ~A~%" frac-part)
    (format t "scale     = ~A~%" scale)
    (format t "exp       = ~A~%" exp))
  (let ((int (cl:+ (cl:* int-part (expt 10 scale))
		   frac-part))
	(power (cl:- exp scale)))
    #+(or)
    (format t "~A * ~A * 10^(~A)~%" sign int power)
    (let* ((len (integer-length int)))
      #+(or)
      (format t "len = ~A~%" len)
      (cond ((<= len 53)
	     (let ((xx (make-qd-d (float int 1d0)))
		   (yy (npow (make-qd-d 10d0)
			     power)))
	       #+(or)
	       (progn
		 (format t "int = ~A~%" int)
		 (format t "fl  = ~A~%" (float int 1w0))
		 (format t "s   = ~A~%" sign)
		 (format t "sint = ~A~%" (cl:* sign  (float int 1w0)))
		 (format t "~A~%" xx)
		 (format t "npow = ~A~%" yy))
	       (if (minusp sign)
		   (neg-qd (mul-qd xx yy))
		   (mul-qd xx yy))))
	    (t
	     (let* #+nil
	       ((hi (ldb (byte 106 (cl:- len 106)) int))
		(lo (ldb (byte 106 (cl:- len 212)) int))
		(xx (make-qd-dd (cl:* sign (scale-float (float hi 1w0)
							(cl:- len 106)))
				(cl:* sign (scale-float (float lo 1w0)
							(cl:- len 106 106)))))
		(yy (npow (make-qd-d 10d0)
			  power)))
	       ((q0 (ldb (byte 53 (cl:- len 53)) int))
		(q1 (ldb (byte 53 (cl:- len (* 2 53))) int))
		(q2 (ldb (byte 53 (cl:- len (* 3 53))) int))
		(q3 (ldb (byte 53 (cl:- len (* 4 53))) int))
		(xx (make-qd-d (scale-float (float q0 1d0)
					    (cl:- len 53))
			       (scale-float (float q1 1d0)
					    (cl:- len (* 2 53)))
			       (scale-float (float q2 1d0)
					    (cl:- len (* 3 53)))
			       (scale-float (float q3 1d0)
					    (cl:- len (* 4 53)))))
		(yy (npow (make-qd-d 10d0)
			  power)))
	       #+(or)
	       (progn
		 (format t "xx = ~A~%" xx)
		 #+(or)
		 (format t "   = ~/qd::qd-format/~%" xx)
		 (format t "yy = ~A~%" yy)
		 #+(or)
		 (format t "   = ~/qd::qd-format/~%" yy)
		 (format t "q0 = ~X (~A)~%" q0
			 (scale-float (float q0 1d0)
				      (cl:- len 53)))
		 (format t "q1 = ~X (~A)~%" q1
			 (scale-float (float q1 1d0)
				      (cl:- len (* 2 53))))
		 #+(or)
		 (format t "~/qdi::qd-format/~%" (mul-qd xx yy)))
	       (if (minusp sign)
		   (neg-qd (mul-qd xx yy))
		   (mul-qd xx yy))))))))

;; This seems to work, but really needs to be rewritten!
(defun read-qd (stream)
  (labels ((read-digits (s)
	     ;; Read a sequence of digits and return the decimal
	     ;; value, the character that terminated the sequence, and
	     ;; how many characters were read.
	     (let ((val 0)
		   (ch nil)
		   (count 0))
	       (loop
		   (setf ch (peek-char nil s nil :eof))
		   (cond ((eq ch :eof)
			  (return))
			 ((digit-char-p ch)
			  (read-char s)
			  (incf count)
			  (setf val (cl:+ (digit-char-p ch)
				       (cl:* 10 val))))
			 (t
			  (return))))
	       (values ch val count)))
	   (read-sign (s)
	     (let ((maybe-sign (peek-char t s nil :eof)))
	       (cond ((eql maybe-sign #\-)
		      (read-char s)
		      -1
		      )
		     ((eql maybe-sign #\+)
		      (read-char s)
		      +1)
		     ((and (characterp maybe-sign)
			   (digit-char-p maybe-sign))
		      +1)
		     ((eql maybe-sign #\.)
		      +1)
		     (t
		      maybe-sign))))
	   (read-exp (s)
	     (let ((exp-sign (read-sign s)))
	       (when (eq :eof exp-sign)
		 (return-from read-exp 0))
	       (when (eq :eof (peek-char t s nil :eof))
		 (error "Sign of exponent, but no value"))
	       (multiple-value-bind (char expo)
		   (read-digits s)
		 (declare (ignore char))
		 (cl:* exp-sign expo)))))
    (let ((sign (read-sign stream))
	  (int-part 0)
	  (frac-part 0)
	  (exp 0)
	  (scale 0))
      (when (eq :eof (peek-char t stream nil :eof))
	(error "Sign, but no value"))
      (multiple-value-bind (char int)
	  (read-digits stream)
	(setf int-part int)
	(cond ((eql :eof char)
	       )
	      ((eql char #\.)
	       (read-char stream)
	       (multiple-value-bind (char val scale-val)
		   (read-digits stream)
		 (declare (ignore char))
		 (setf frac-part val)
		 (setf scale scale-val)
		 (let ((next (peek-char nil stream nil :eof)))
		   (when (equalp next #\q)
		     (read-char stream)
		     (let ((exp-sign (read-sign stream)))
		       (setf exp (read-exp stream))
		       (setf exp (cl:* exp exp-sign)))))))
	      ((equalp char #\q)
	       (read-char stream)
	       (setf exp (read-exp stream))
	       ))
	(make-float sign int-part frac-part scale exp)))))

(defun qd-reader (stream subchar arg)
  (read-qd stream))

(defun qd-from-string (string)
  (cl::with-input-from-string (s string)
    (read-qd s)))

#+nil
(set-dispatch-macro-character #\# #\Q #'qd-reader)
			      
