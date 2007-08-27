;;; statistics.lisp
;;; macros, functions and methods for matrix element access
;;;
;;; Copyright (c) 2004-2006 Cyrus Harmon (ch-lisp@bobobeach.com)
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

(in-package :clem)

;;; slow fallback methods

(defmethod sum ((m matrix))
  (destructuring-bind (mr mc) (dim m)
    (sum-range m 0 (- mr 1) 0 (- mc 1))))

(defmethod sum-cols ((m matrix) &key (matrix-class (class-of m)))
  (let ((mr (rows m)) (mc (cols m)))
    (let ((n (make-instance matrix-class :rows 1 :cols mc)))
      (dotimes (i mr)
        (dotimes (j mc)
          (incf (mref n 0 j) (mref m i j))))
      n)))

(defmethod sum-rows ((m matrix) &key (matrix-class (class-of m)))
  (let ((mr (rows m)) (mc (cols m)))
    (let ((n (make-instance matrix-class :rows mr :cols 1)))
      (dotimes (i mr)
        (dotimes (j mc)
          (incf (mref n i 0) (mref m i j))))
      n)))

(defmethod sum-square-range ((m matrix) (startr fixnum) (endr fixnum) (startc fixnum) (endc fixnum))
  (declare (dynamic-extent startr endr startc endc)
	   (fixnum startr endr startc endc))
  (let ((acc 0))
    (map-range m startr endr startc endc
	       #'(lambda (v i j)
		   (declare (ignore i j))
		   (incf acc (* v v))))
    acc))

(defmethod sum-square ((m matrix))
  (destructuring-bind (mr mc) (dim m)
    (sum-square-range m 0 (- mr 1) 0 (- mc 1))))

(defmethod sum-range ((m matrix) (startr fixnum) (endr fixnum) (startc fixnum) (endc fixnum))
  (declare (dynamic-extent startr endr startc endc)
	   (fixnum startr endr startc endc))
  (let ((acc 0))
    (map-range m startr endr startc endc
	       #'(lambda (v i j)
		   (declare (ignore i j))
		   (incf acc v)))
    acc))

(defmethod mean-range ((m matrix) startr endr startc endc)
  (double-float-divide (sum-range m startr endr startc endc)
		(count-range startr endr startc endc)))

(defmethod mean ((m matrix))
  (destructuring-bind (mr mc) (dim m)
    (mean-range m 0 (- mr 1) 0 (- mc 1))))

(defmethod variance-range ((m matrix) startr endr startc endc)
  (declare (dynamic-extent startr endr startc endc)
	   (fixnum startr endr startc endc))
  (let ((mu (mean-range m startr endr startc endc)))
    (let ((musq (* mu mu)))
      (let ((ssr (sum-square-range m startr endr startc endc)))
	(let ((cr (count-range startr endr startc endc)))
	  (declare (fixnum cr))
	  (- (double-float-divide ssr cr)
	     musq))))))

(defmethod variance ((m matrix))
  (destructuring-bind (mr mc) (dim m)
    (variance-range m 0 (- mr 1) 0 (- mc 1))))

(defmethod sample-variance-range ((m matrix) startr endr startc endc)
  (let* ((acc 0)
	 (mu (mean-range m startr endr startc endc))
	 (musq (* mu mu)))
    (map-range m startr endr startc endc
	       #'(lambda (v i j)
		   (declare (ignore i j))
		   (incf acc (- (* v v) musq))))
    (double-float-divide acc (1- (count-range startr endr startc endc)))))

(defmethod sample-variance ((m matrix))
  (destructuring-bind (mr mc) (dim m)
    (sample-variance-range m 0 (- mr 1) 0 (- mc 1))))

;;; faster type-specific methods

(defmacro %%sum-range (m startr endr startc endc element-type accumulator-type)
  `(let ((acc (coerce 0 ',accumulator-type))
         (a (matrix-vals ,m)))
     (declare (type ,accumulator-type acc)
              (type (simple-array ,element-type *) a))
     (do ((i ,startr (1+ i)))
         ((> i ,endr))
       (declare (dynamic-extent i) (type fixnum i))
       (do ((j ,startc (1+ j)))
           ((> j ,endc))
         (declare (dynamic-extent j) (type fixnum j))
         (setf acc (+ acc (aref a i j)))))
     acc))

(macrolet
    ((frob-sum-range (matrix-type accumulator-type)
       (let ((element-type (element-type (find-class matrix-type))))
	 `(defmethod sum-range ((m ,matrix-type)
                                (startr fixnum) (endr fixnum)
                                (startc fixnum) (endc fixnum))
            (%%sum-range m startr endr startc endc
                         ,element-type ,accumulator-type)))))
  (frob-sum-range double-float-matrix double-float)
  (frob-sum-range single-float-matrix single-float)

  (frob-sum-range ub8-matrix (unsigned-byte 32))
  (frob-sum-range ub16-matrix (unsigned-byte 32))
  (frob-sum-range ub32-matrix (unsigned-byte 32))

  (frob-sum-range sb8-matrix (signed-byte 32))
  (frob-sum-range sb16-matrix (signed-byte 32))
  (frob-sum-range sb32-matrix (signed-byte 32))

  (frob-sum-range fixnum-matrix (signed-byte 32))
  (frob-sum-range bit-matrix (signed-byte 32)))


(macrolet
    ((frob-sum-square-range (matrix-type accumulator-type)
       (let ((element-type (element-type (find-class matrix-type))))
	 `(defmethod sum-square-range ((m ,matrix-type) (startr fixnum) (endr fixnum) (startc fixnum) (endc fixnum))
	    (let ((acc (coerce 0 ',accumulator-type))
		  (a (matrix-vals m)))
	      (declare (type ,accumulator-type acc)
		       (type (simple-array ,element-type *) a))
	      (do ((i startr (1+ i)))
		  ((> i endr))
		(declare (dynamic-extent i) (type fixnum i))
		(do ((j startc (1+ j)))
		    ((> j endc))
		  (declare (dynamic-extent j) (type fixnum j))
		  (incf acc (* (aref a i j) (aref a i j)))))
	      acc)))))

  (frob-sum-square-range double-float-matrix double-float)
  (frob-sum-square-range single-float-matrix single-float)

  (frob-sum-square-range ub8-matrix (unsigned-byte 32))
  (frob-sum-square-range ub16-matrix (unsigned-byte 32))
  (frob-sum-square-range ub32-matrix (unsigned-byte 32))

  (frob-sum-square-range sb8-matrix (signed-byte 32))
  (frob-sum-square-range sb16-matrix (signed-byte 32))
  (frob-sum-square-range sb32-matrix (signed-byte 32))

  (frob-sum-square-range fixnum-matrix (signed-byte 32))
  (frob-sum-square-range bit-matrix (signed-byte 32)))

