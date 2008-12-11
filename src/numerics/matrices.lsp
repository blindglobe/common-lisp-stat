;;; -*- mode: lisp -*-
;;;
;;; Copyright (c) 2005--2007, by A.J. Rossini <blindglobe@gmail.com>
;;; See COPYRIGHT file for any additional restrictions (BSD license).
;;; Since 1991, ANSI was finally finished.  Modified to match ANSI
;;; Common Lisp.  

;;;; matrices -- Basic matrix operations
;;;; 
;;;; Copyright (c) 1991, by Luke Tierney. Permission is granted for
;;;; unrestricted use.

;;; Issues:
;;; #1 - Need to extend to use lisp-matrix?
;;; #2 - do as a second alternative? 

(in-package :lisp-stat-matrix)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;          Array to Row-Major Data Vector Conversion Functions
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun array-data-vector (a)
"Args: (a)
Displaces array A to a vector"
  (make-array (array-total-size a)
	      :displaced-to a
	      :element-type (array-element-type a)))

(defun vector-to-array (v dims)
"Args: (v dims)
Displaces vector V to array with dimensions DIMS"
  (make-array dims
	      :displaced-to v
	      :element-type (array-element-type v)))


(defun check-square-matrix (a)
  (if (and (check-matrix a)
	   (/= (array-dimension a 0) (array-dimension a 1))
	   (error "matrix not square - ~s" a))
      t))

(defun inner-product (x y)
  "Args: (x y)

Returns inner product of sequences X and Y."
  (check-sequence x)
  (check-sequence y)
  (let ((n (length x))
        (cx (make-next-element x))
        (cy (make-next-element y))
        (result 0))
    (declare (fixnum n))
    (if (/= n (length y)) (error "sequence lengths do not match"))
    (dotimes (i n result) 
      (declare (fixnum i))
      (setf result 
            (+ result (* (get-next-element cx i)
			 (get-next-element cy i)))))))

(defun outer-product (x y &optional (f #'*))
  "Args: (x y &optional (fcn #'*))

Returns the generalized outer product of x and y, using fcn. Tat is, the result
is a matrix of dimension ((length x) (length y)) and the (i j) element of the
result is computed as (apply fcn (aref x i) (aref y j))."

  (let* ((x (coerce x 'vector))
         (y (coerce y 'vector))
         (m (length x))
         (n (length y))
         (a (make-array (list m n))))
    (declare (fixnum m n))
    (dotimes (i m a)
      (declare (fixnum i))
      (dotimes (j n)
        (declare (fixnum j))
        (setf (aref a i j) (funcall f (aref x i) (aref y j)))))))

(defun cross-product (x)
  "Args: (x)

If X is a matrix returns (matmult (transpose X) X). If X is a vector returns
(inner-product X X)."

  (check-matrix x)
  (let* ((n (num-rows x))
         (p (num-cols x))
         (c (make-array (list p p))))
    (declare (fixnum n p))
    (dotimes (i p c)
      (declare (fixnum i))
      (dotimes (j (+ i 1))
        (declare (fixnum j))
        (let ((val 0))
          (dotimes (k n)
            (declare (fixnum k))
            (incf val (* (aref x k i) (aref x k j))))
          (setf (aref c i j) val)
          (setf (aref c j i) val))))))

(defun transpose-list (x)
  (let ((m (length (first x))))
    (dolist (next x)
      (if (not (consp next)) (error "not a list - ~a" x))
      (if (/= m (length next)) (error "sublists not the same length")))
    (do* ((cx (copy-list x))
          (result (make-list m))
          (next result (cdr next)))
         ((null next) result)
      (setf (first next) (mapcar #'first cx))
      (do ((next cx (cdr next)))
          ((null next))
        (setf (first next) (rest (first next)))))))
    
(defun transpose (x)
"Args: (m)
Returns the transpose of the matrix M."
  (cond
   ((consp x) (transpose-list x))
   (t
    (check-matrix x)
    (let* ((m (num-rows x))
           (n (num-cols x))
           (tx (make-array (list n m))))
      (declare (fixnum m n))
      (dotimes (i m tx)
        (declare (fixnum i))
        (dotimes (j n)
          (declare (fixnum j))
          (setf (aref tx j i) (aref x i j))))))))


