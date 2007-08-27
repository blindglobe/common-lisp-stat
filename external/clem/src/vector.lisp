;;; vector.lisp
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

;;; this might all go away soon, but for the moment put some stuff
;;; that is common between row-vector and column-vector in here.

(in-package :clem)

(defclass base-vector (matrix) ())

(defgeneric vec-dim (vec))
(defgeneric vec-val (vec i))
(defgeneric vec-set-val (vec i val))

(defclass row-vector (base-vector) ())
(defclass col-vector (base-vector) ())

(defmethod allocate-matrix-vals ((object row-vector) &key rows cols adjustable initial-element)
  (declare (ignore rows))
  (setf (slot-value object 'm)
	(make-array (list cols)
		    :adjustable adjustable
		    :initial-element initial-element
		    :element-type (element-type (class-of object)))))

(defgeneric array->row-vector (a))
(defmethod array->row-vector ((a array))
  (let ((d (array-dimensions a)))
    (cond ((= (length d) 2)
	   (let* ((cols (second d))
		  (m (make-instance 'row-vector :cols cols)))
	     (dotimes (i cols)
	       (vec-set-val m i (aref a 0 i)))
	     m))
	  ((= (length d) 1)
	   (let ((m (make-instance 'row-vector :cols (first d))))
	     (dotimes (i (first d)) (vec-set-val m i (aref a i)))
	     m)))))

(defmethod vec-dim ((rv row-vector)) (array-dimensions (matrix-vals rv)))
(defmethod vec-val ((rv row-vector) i) (aref (matrix-vals rv) i))
(defmethod vec-set-val ((rv row-vector) i v) (setf (aref (matrix-vals rv) i) v))

(defmethod dim ((rv row-vector)) (append '(1) (vec-dim rv)))
(defmethod rows ((rv row-vector)) 1)
(defmethod cols ((rv row-vector)) (first (array-dimensions (matrix-vals rv))))
(defmethod val ((rv row-vector) i j) (declare (ignore i)) (vec-val rv j))
(defmethod set-val ((rv row-vector) i j v &key (coerce t))
  (declare (ignore i))
  (vec-set-val rv j (if coerce (coerce v (element-type (class-of rv))) v)))

(defmethod transpose ((rv row-vector))
  (let ((c (cols rv)))
    (let ((cv (make-instance 'col-vector :rows c)))
      (dotimes (j c cv)
	(vec-set-val cv j (vec-val rv j))))))
      
(defmethod print-matrix ((rv row-vector))
  (let ((d (vec-dim rv)))
    (format t "~&((")
    (dotimes (i (first d))
      (format t (if (= i 0) "~a" " ~a") (vec-val rv i)))
    (format t "))")))

(defmethod set-row ((m matrix) r (v row-vector))
  (do
      ((i 0 (+ i 1)))
      ((= i (cols v)))
    (set-val m r i (vec-val v i))))

(defmethod set-col ((m matrix) r (v row-vector))
  (do
      ((i 0 (+ i 1)))
      ((= i (cols v)))
    (set-val m i r (vec-val v i))))

(defgeneric get-row-vector (m r))
(defmethod get-row-vector ((m matrix) r)
  (let ((rv (make-instance 'row-vector :cols (second (dim m)))))
    (dotimes (i (second (dim m)))
      (vec-set-val rv i (val m r i)))))

(defgeneric zero-row-vector (j))
(defmethod zero-row-vector((j fixnum))
  (zero-matrix 1 j))

(defmethod allocate-matrix-vals ((object col-vector) &key rows cols adjustable initial-element)
  (declare (ignore cols))
  (setf (slot-value object 'm)
	(make-array (list rows)
		    :adjustable adjustable
		    :initial-element initial-element
		    :element-type (element-type (class-of object)))))

(defmethod vec-dim ((cv col-vector)) (array-dimensions (matrix-vals cv)))
(defmethod vec-val ((cv col-vector) i) (aref (matrix-vals cv) i))
(defmethod vec-set-val ((cv col-vector) i v) (setf (aref (matrix-vals cv) i) v))

(defmethod dim ((cv col-vector)) (append (vec-dim cv) '(1)))
(defmethod rows ((cv col-vector)) (first (array-dimensions (matrix-vals cv))))
(defmethod cols ((cv col-vector)) 1)
(defmethod val ((cv col-vector) i j) (declare (ignore j)) (vec-val cv i))
(defmethod set-val ((cv col-vector) i j v &key (coerce t))
  (declare (ignore j))
  (vec-set-val cv i (if coerce (coerce v (element-type (class-of cv))) v)))

(defgeneric array->col-vector (a))

(defmethod array->col-vector ((a array))
  (let ((d (array-dimensions a)))
    (cond ((= (length d) 2)
	   (let* ((rows (first d))
		  (m (make-instance 'col-vector :rows rows)))
	     (dotimes (i rows) (vec-set-val m i (aref a i 0)))
	     m))
	  ((= (length d) 1)
	   (let ((m (make-instance 'col-vector :rows (first d))))
	     (dotimes (i (first d)) (vec-set-val m i (aref a i)))
	     m)))))

(defmethod transpose ((cv col-vector))
  (let ((r (rows cv)))
    (let ((rv (make-instance 'row-vector :cols r)))
      (dotimes (j r rv)
	(vec-set-val rv j (vec-val cv j))))))

(defmethod print-matrix ((cv col-vector))
  (let ((d (vec-dim cv)))
    (format t "~&(")
    (dotimes (i (first d))
      (format t (if (= i 0) "(~a)" " (~a)") (vec-val cv i)))
    (format t ")")))

(defmethod set-row ((m matrix) r (v col-vector))
  (do
      ((i 0 (+ i 1)))
      ((= i (rows v)))
    (set-val m r i (vec-val v i))))

(defmethod set-col ((m matrix) c (v col-vector))
  (do
      ((i 0 (+ i 1)))
      ((= i (first (dim v))))
    (set-val m i c (val v i 0))))

(defgeneric get-row-as-col-vector (m r))
(defmethod get-row-as-col-vector ((m matrix) r)
  (let ((cv (make-instance 'col-vector :rows (second (dim m)))))
    (dotimes (i (second (dim m)))
      (vec-set-val cv i (val m r i)))))
		 
(defgeneric get-col-vector (m r))
(defmethod get-col-vector ((m matrix) r)
  (let ((cv (make-instance 'col-vector :rows (first (dim m)))))
    (dotimes (i (first (dim m)))
      (vec-set-val cv i (val m i r)))))

(defgeneric zero-col-vector (j))
(defmethod zero-col-vector((j fixnum))
  (zero-matrix j 1))
