i;;; -*- mode: lisp -*-
;;;
;;; Copyright (c) 2008--, by A.J. Rossini <blindglobe@gmail.com>
;;; See COPYRIGHT file for any additional restrictions (BSD license).
;;; Since 1991, ANSI was finally finished.  Modified to match ANSI
;;; Common Lisp.  

;; matrix-matlisp -- extended functions for matrix and linear algebra
;; using MATLISP.

;; Package Setup

(in-package :cl-user)

(defpackage :lisp-stat-matrix-matlisp
  (:use :common-lisp
	:matlisp)
  (:export sweep
	   ))

(in-package :lisp-stat-matrix-matlisp)

;; Might check with features that we've (pushnew :matlisp *features*)
;; and then we can do  #+matlisp    as needed.

;;;
;;; SWEEP Operator, MATLISP implementation.
;;;

(defun make-sweep-front (x y w n p mode has_w x_mean result)
  "Compute the sweep matrix, a generalized computation based on the regression components Y and X. 
X is: numerical version of design/regt matrix
y is: response
w is: weights (might be simply 1
n is: ?
p is: ?
mode is: real, complex, though complex is not yet supported.
has_w just indicates if weights (w) are meaningful.
x_mean: 
result: ? place to store?
"
  (declare (fixnum n p mode has_w))
  (let ((x_data nil)
	(result_data nil)
	(val 0.0)
	(dxi 0.0)
	(dyi 0.0)
	(dv 0.0)
	(dw 0.0)
	(sum_w 0.0)
	(dxik 0.0)
	(dxjk 0.0)
	(dyj 0.0)
	(dx_meani 0.0)
	(dx_meanj 0.0)
	(dy_mean 0.0)
	(has-w (if (/= 0 has_w) t nil))
	(RE 1))
    (declare (long-float val dxi dyi dv dw sum_w dxik dxjk dyj
		    dx_meani dx_meanj dy_mean))
  
    (setf x_data (compound-data-seq x))
    (setf result_data (compound-data-seq result))
  
    ;; find the mean of y
    (dotimes (i n)
      (declare (fixnum i))
      (setf dyi (makedouble (aref y i)))
      (when has-w
	(setf dw (makedouble (aref w i)))
	(incf sum_w dw)
	(setf dyi (* dyi dw)))
      (incf val dyi))
    (if (not has-w) (setf sum_w (float n 0.0)))
    (if (<= sum_w 0.0) (error "non positive sum of weights"))
    (setf dy_mean (/ val sum_w))
  
    ;; find the column means of X
    (dotimes (j p)
      (declare (fixnum j))
      (setf val 0.0)
      (dotimes (i n)
        (declare (fixnum i))
	(setf dxi (makedouble (aref x_data (+ (* p i) j))))
	(when has-w
	  (setf dw (makedouble (aref w i)))
	  (setf dxi (* dxi dw)))
	(incf val dxi))
      (setf (aref x_mean j) (/ val sum_w)))
  
    ;; put 1/sum_w in topleft, means on left, minus means on top
    (setf (aref result_data 0) (/ 1.0 sum_w))
    (dotimes (i p)
      (declare (fixnum i))
      (setf dxi (makedouble (aref x_mean i)))
      (setf (aref result_data (+ i 1)) (- dxi))
      (setf (aref result_data (* (+ i 1) (+ p 2))) dxi))
    (setf (aref result_data (+ p 1)) (- dy_mean))
    (setf (aref result_data (* (+ p 1) (+ p 2))) dy_mean)
  
    ;; put sums of adjusted cross products in body
    (dotimes (i p)
      (declare (fixnum i))
      (dotimes (j p)
        (declare (fixnum j))
	(setf val 0.0)
	(dotimes (k n)
	  (declare (fixnum k))
	  (setf dxik (makedouble (aref x_data (+ (* p k) i))))
	  (setf dxjk (makedouble (aref x_data (+ (* p k) j))))
	  (setf dx_meani (makedouble (aref x_mean i)))
	  (setf dx_meanj (makedouble (aref x_mean j)))
	  (setf dv (* (- dxik dx_meani) (- dxjk dx_meanj)))
	  (when has-w
	    (setf dw (makedouble (aref w k)))
	    (setf dv (* dv dw)))
	  (incf val dv))
	(setf (aref result_data (+ (* (+ i 1) (+ p 2)) (+ j 1))) val)
	(setf (aref result_data (+ (* (+ j 1) (+ p 2)) (+ i 1))) val))
      (setf val 0.0)
      (dotimes (j n)
        (declare (fixnum j))
        (setf dxik (makedouble (aref x_data (+ (* p j) i))))
	(setf dyj (makedouble (aref y j)))
	(setf dx_meani (makedouble (aref x_mean i)))
	(setf dv (* (- dxik dx_meani) (- dyj dy_mean)))
	(when has-w
	  (setf dw (makedouble (aref w j)))
	  (setf dv (* dv dw)))
	(incf val dv))
      (setf (aref result_data (+ (* (+ i 1) (+ p 2)) (+ p 1))) val)
      (setf (aref result_data (+ (* (+ p 1) (+ p 2)) (+ i 1))) val))
    (setf val 0.0)
    (dotimes (j n)
      (declare (fixnum j))
      (setf dyj (makedouble (aref y j)))
      (setf dv (* (- dyj dy_mean) (- dyj dy_mean)))
      (when has-w
        (setf dw (makedouble (aref w j)))
        (setf dv (* dv dw)))
      (incf val dv))
    (setf (aref result_data (+ (* (+ p 1) (+ p 2)) (+ p 1))) val)))

;;; FIXME: use matlisp
(defun sweep-in-place-front (a rows cols mode k tol)
  "Sweep algorithm for linear regression."
  (declare (long-float tol))
  (declare (fixnum rows cols mode k))
  (let ((data nil)
	(pivot 0.0)
	(aij 0.0)
	(aik 0.0)
	(akj 0.0)
	(akk 0.0)
	(RE 1))
    (declare (long-float pivot aij aik akj akk))
  
    (if (> mode RE) (error "not supported for complex data yet"))
    (if (or (< k 0) (>= k rows) (>= k cols)) (error "index out of range"))
  
    (setf tol (max tol machine-epsilon))
    (setf data (compound-data-seq a))

    (setf pivot (makedouble (aref data (+ (* cols k) k))))
  
    (cond
     ((or (> pivot tol) (< pivot (- tol)))
      (dotimes (i rows)
        (declare (fixnum i))
	(dotimes (j cols)
	  (declare (fixnum j))
	  (when (and (/= i k) (/= j k))
	    (setf aij (makedouble (aref data (+ (* cols i) j))))
	    (setf aik (makedouble (aref data (+ (* cols i) k))))
	    (setf akj (makedouble (aref data (+ (* cols k) j))))
	    (setf aij (- aij (/ (* aik akj) pivot)))
	    (setf (aref data (+ (* cols i) j)) aij))))

      (dotimes (i rows)
        (declare (fixnum i))
	(setf aik (makedouble (aref data (+ (* cols i) k))))
	(when (/= i k)
	  (setf aik (/ aik pivot))
	  (setf (aref data (+ (* cols i) k)) aik)))

      (dotimes (j cols)
        (declare (fixnum j))
	(setf akj (makedouble (aref data (+ (* cols k) j))))
	(when (/= j k)
          (setf akj (- (/ akj pivot)))
	  (setf (aref data (+ (* cols k) j)) akj)))

      (setf akk (/ 1.0 pivot))
      (setf (aref data (+ (* cols k) k)) akk)
      1)
     (t 0))))

;; FIXME: use matlisp
(defun make-sweep-matrix (x y &optional w)
  "Args: (x y &optional weights)
 X is matrix, Y and WEIGHTS are sequences. Returns the sweep matrix of the
 (weighted) regression of Y on X"
   (if (not (typep x 'real-matrix)) (error "Make Sweep Matrix: X not real-matrix"))
   (if (not (typep y 'real-matrix)) (error "Make Sweep Matrix: Y not real-matrix"))
   (if w (if (not (typep w 'real-matrix)) (error "Make Sweep Matrix: W not real-matrix")))
   (let ((n (n x))
	 (p (m x)))
     (if (/= n (length y)) (error "dimensions do not match"))
     (if (and w (/= n (length w))) (error "dimensions do not match"))
     (let ((mode (max (la-data-mode x) 
		      (la-data-mode x) 
		      (if w (la-data-mode w) 0)))
	   (result (make-array (list (+ p 2) (+ p 2))))
	   (x-mean (make-array p))
	   (y (coerce y 'vector))
	   (w (if w (coerce w 'vector)))
	   (has-w (if w 1 0)))
       (make-sweep-front x y w n p mode has-w x-mean result)
       result)))

 (defun sweep-in-place (a k tol)
  (check-matrix a)
  (check-one-fixnum k)
  (check-one-real tol)
  (let ((rows (num-rows a))
	(cols (num-cols a))
	(mode (la-data-mode a)))
    (let ((swept (sweep-in-place-front a rows cols mode k tol)))
      (if (/= 0 swept) t nil))))

(defun sweep-operator (a columns &optional tolerances)
  "Args: (a indices &optional tolerances)

A is a matrix, INDICES a sequence of the column indices to be
swept. Returns a list of the swept result and the list of the columns
actually swept. (See MULTREG documentation.) If supplied, TOLERANCES
should be a list of real numbers the same length as INDICES. An index
will only be swept if its pivot element is larger than the
corresponding element of TOLERANCES."

  ;; Why we should use generics/methods....
  (if (not (typep a 'real-matrix))
      (error "a is not a matrix"))
  ;; following: we need integer?
  (if (and (not (typep columns 'sequence))
	   (not (typep (setf columns (list columns)) 'sequence)))
      (error "columns not coerceable to sequence"))
  (if tolerances
      (if (and (not (typep tolerances 'sequence))
	       (not (setf tolerances (list tolerances))))
	  (error "tolerances not coercable to sequence.")))

  ;; (check-real a) done by matlisp.
  (check-fixnum columns)
  (if tolerances (check-real tolerances))

  (do ((tol .0000001)
       (result (copy-array a))
       (swept-columns nil)
       (columns (coerce columns 'list) (cdr columns))
       (tolerances (if (consp tolerances) (coerce tolerances 'list))
		   (if (consp tolerances) (cdr tolerances))))
      ((null columns) (list result swept-columns))
    ;; this should be done in the context of matlisp, I think...?
    (let ((col (first columns))
	    (tol (if (consp tolerances) (first tolerances) tol)))
	(if (sweep-in-place result col tol)
	    (setf swept-columns (cons col swept-columns))))))

