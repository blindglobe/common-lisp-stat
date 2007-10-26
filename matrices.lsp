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


;;; Need to extend to use CLEM 



;;;;
;;;; Package Setup
;;;;

(defpackage :lisp-stat-matrix
  (:use :common-lisp
	:lisp-stat-compound-data
	:lisp-stat-sequence)
  (:export matrixp num-rows num-cols matmult identity-matrix diagonal
	   row-list column-list inner-product outer-product
	   cross-product transpose bind-columns bind-rows
	   array-data-vector vector-to-array

	   check-matrix check-square-matrix

	   copy-array copy-vector
	   ))

(in-package :lisp-stat-matrix)

(deftype matrix () 'array)  ;; temp fix

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

;;;;

(defun check-matrix (a)
  (if (not (and (arrayp a) (= (array-rank a) 2)))
      (error "not a matrix - ~s" a)
    t))

(defun check-square-matrix (a)
  (if (and (check-matrix a)
	   (/= (array-dimension a 0) (array-dimension a 1))
	   (error "matrix not square - ~s" a))
      t))

(defun matrixp (x)
"Args: (x)
Returns T if X is a matrix, NIL otherwise."
  (and (arrayp x) (= (array-rank x) 2)))

(defun num-rows (x)
"Args: (x)
Returns number of rows in X."
  (array-dimension x 0))

(defun num-cols (x)
"Args: (x)
Returns number of columns in X."
  (array-dimension x 1))

(defun matmult (a b &rest args)
  "Args: (a b &rest args)
Returns the matrix product of matrices a, b, etc. If a is a vector it is
treated as a row vector; if b is a vector it is treated as a column vector."
  ;; fixme: why does SBCL claim this is unreachable?
  (let ((rtype (cond ((and (matrixp a) (matrixp b)) 'matrix)
                     ((and (sequencep a) (sequencep b)) 'number)
                     ((sequencep a) (if (consp a) 'list 'vector))
                     ((sequencep b) (if (consp b) 'list 'vector)))))
             
    (if (sequencep a) 
      (setf a (vector-to-array (coerce a 'vector) (list 1 (length a)))))
    (if (sequencep b)
      (setf b (vector-to-array (coerce b 'vector) (list (length b) 1))))
    (if (not (= (array-dimension a 1) (array-dimension b 0)))
      (error "dimensions do not match"))
    (if args 
      (reduce #'matmult args :initial-value (matmult a b))
      (let* ((n (array-dimension a 0))
             (m (array-dimension b 1))
             (p (array-dimension a 1))
             (c (make-array (list n m)))
	     x)
        (declare (fixnum n m p))
	(dotimes (i n)
		 (declare (fixnum i))
		 (dotimes (j m)
			  (declare (fixnum j))
			  (setq x 0)
			  (dotimes (k p)
				   (declare (fixnum k))
				   (setq x (+ x 
					      (* (aref a i k) (aref b k j)))))
			  (setf (aref c i j) x)))
        (case rtype
          (matrix c)
          (number (aref c 0 0))
          (t (coerce (compound-data-seq c) rtype)))))))

(defun identity-matrix (n)
"Args: (n)
Returns the identity matrix of rank N."
  (let ((result (make-array (list n n) :initial-element 0)))
    (dotimes (i n result) 
	     (declare (fixnum i))
	     (setf (aref result i i) 1))))

;; this thing is not very efficient at this point - too much coercing
(defun diagonal (x)
"Args: (x)
If X is a matrix, returns the diagonal of X. If X is a sequence, returns a
diagonal matrix of rank (length X) with diagonal elements eq to the elements
of X."
  (cond ((matrixp x)
	 (let* ((n (min (num-rows x) (num-cols x)))
		(result (make-array n)))
	   (dotimes (i n (coerce result 'list)) 
		    (setf (aref result i) (aref x i i)))))
	((sequencep x)
	 (let* ((x (coerce x 'vector))
		(n (length x))
		(result (make-array (list n n) :initial-element 0)))
	   (dotimes (i n result)
		    (setf (aref result i i) (aref x i)))))
	(t (error "argument must be a matrix or a sequence"))))
	   		  
(defun row-list (x)
"Args: (m)
Returns a list of the rows of M as vectors"
  (check-matrix x)
  (let ((m (num-rows x))
        (n (num-cols x))
        (result nil))
    (declare (fixnum m n))
    (flet ((get-row (k)
             (declare (fixnum k))
             (let ((row (make-array n)))
               (dotimes (i n row)
                 (declare (fixnum i))
                 (setf (aref row i) (aref x k i))))))
      (dotimes (i m result)
        (declare (fixnum i))
        (setf result (cons (get-row (- m i 1)) result))))))
        
(defun column-list (x)
"Args: (m)
Returns a list of the columns of M as vectors"
  (check-matrix x)
  (let ((m (num-rows x))
        (n (num-cols x))
        (result nil))
    (declare (fixnum m n))
    (flet ((get-col (k)
             (declare (fixnum k))
             (let ((col (make-array m)))
               (dotimes (i m col)
                 (declare (fixnum i))
                 (setf (aref col i) (aref x i k))))))
      (dotimes (i n result)
        (declare (fixnum i))
        (setf result (cons (get-col (- n i 1)) result))))))

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
            (+ result (* (get-next-element cx i) (get-next-element cy i)))))))

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

(defun bind-columns (&rest args)
"Args (&rest args)
The ARGS can be matrices, vectors, or lists. Arguments are bound into a matrix
along their columns.
Example: (bind-columns #2a((1 2)(3 4)) #(5 6)) returns #2a((1 2 5)(3 4 6))"
  (flet ((check-arg (x)
           (if (not (or (sequencep x) (matrixp x)))
               (error "bad argument type")))
         (arg-cols (x) (if (sequencep x) 1 (num-cols x)))
         (arg-rows (x) (if (sequencep x) (length x) (num-rows x))))
    (dolist (x args) (check-arg x))
    (let ((m (arg-rows (first args)))
          (n (arg-cols (first args))))
      (declare (fixnum m n))
      (dolist (x (rest args))
        (if (/= m (arg-rows x)) (error "column lengths do not match"))
        (incf n (arg-cols x)))
      (do* ((result (make-array (list m n)))
            (args args (rest args))
            (firstcol 0)
            (x (first args) (first args)))
           ((null args) result)
        (cond
         ((sequencep x)
          (let ((cx (make-next-element x)))
            (dotimes (i m)
              (setf (aref result i firstcol) (get-next-element cx i)))))
         (t
          (let ((k (arg-cols x)))
            (dotimes (i m)
              (dotimes (j k)
                (setf (aref result i (+ firstcol j)) (aref x i j)))))))
        (incf firstcol (arg-cols x))))))

(defun bind-rows (&rest args)
"Args (&rest args)
The ARGS can be matrices, vectors, or lists. Arguments are bound into a matrix
along their rows.
Example: (bind-rows #2a((1 2)(3 4)) #(5 6)) returns #2a((1 2)(3 4)(5 6))"
  (flet ((check-arg (x)
           (if (not (or (sequencep x) (matrixp x)))
               (error "bad argument type")))
         (arg-cols (x) (if (sequencep x) (length x) (num-cols x)))
         (arg-rows (x) (if (sequencep x) 1 (num-rows x))))
    (dolist (x args) (check-arg x))
    (let ((m (arg-rows (first args)))
          (n (arg-cols (first args))))
      (declare (fixnum m n))
      (dolist (x (rest args))
        (if (/= n (arg-cols x)) (error "row lengths do not match"))
        (incf m (arg-rows x)))
      (do* ((result (make-array (list m n)))
            (args args (rest args))
            (firstrow 0)
            (x (first args) (first args)))
           ((null args) result)
        (cond
         ((sequencep x)
          (let ((cx (make-next-element x)))
            (dotimes (i n)
              (setf (aref result firstrow i) (get-next-element cx i)))))
         (t
          (let ((k (arg-rows x)))
            (dotimes (i n)
              (dotimes (j k)
                (setf (aref result (+ firstrow j) i) (aref x j i)))))))
        (incf firstrow (arg-rows x))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;                           Copying Functions
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; COPY-VECTOR function
;;;

(defun copy-vector (x)
"Args: (x)
Returns a copy of the vector X"
  (copy-seq x))

;;;
;;; COPY-ARRAY function
;;;

(defun copy-array (a)
"Args: (a)
Returns a copy of the array A"
  (vector-to-array (copy-seq (array-data-vector a))
		   (array-dimensions a)))

