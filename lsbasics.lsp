;;; -*- mode: lisp -*-
;;; Copyright (c) 2005--2007, by A.J. Rossini <blindglobe@gmail.com>
;;; See COPYRIGHT file for any additional restrictions (BSD license).
;;; Since 1991, ANSI was finally finished.  Edited for ANSI Common Lisp. 

;;;; lsbasics -- Low level Lisp-Stat functions
;;;; 
;;;; Copyright (c) 1991, by Luke Tierney. Permission is granted for
;;;; unrestricted use.

;;; Package Setup

(in-package :cl-user)

(defpackage :lisp-stat-basics
    (:use :common-lisp
	  :lisp-stat-object-system
	  :lisp-stat-types
	  :lisp-stat-float
	  :lisp-stat-macros
	  :lisp-stat-compound-data)
  (:shadowing-import-from :lisp-stat-object-system
			  slot-value call-method call-next-method)
  (:export permute-array sum prod count-elements mean
	   if-else sample))

(in-package :lisp-stat-basics)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;                    Array Permutation Functions
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun permute-indices (x y perm check) 
  "Args: (x y perm check).
permute x into y using perm; all should be vectors; If check is TRUE
the routine will check to make sure no indices are reused, but x
will be destroyed."
  (let ((rank (length x)))
    (declare (fixnum rank))
    (dotimes (i rank)
      (declare (fixnum i))
      (let ((k (aref perm i)))
        (if (not (fixnump k)) (error "bad permutation sequence - ~a" perm))
        (if (or (< k 0) (>= k rank))
	    (error "bad permutation sequence - ~a" perm))
        (setf (aref y i) (aref x k))
	;; to insure dimensions are not re-used
        (if check (setf (aref x k) NIL))))))

(defun indices-from-rowmajor (a k result)
  "Args: (a k result).
Compute indices in a from rowmajor index k, put in vector result."
  (declare (fixnum k))

  (if (not (arrayp a)) (error "not an array - ~a" a))
  (if (or (> 0 k) (>= k (array-total-size a))) (error "index out of range"))

  (let ((face 1)
        (rank (array-rank a))
        (dim (array-dimensions a)))
    (declare (fixnum face rank))
    (let ((cdim (make-next-element dim)))
      (dotimes (i rank)
        (declare (fixnum i))
        (setf face (* face (get-next-element cdim i)))))
    (let ((cdim (make-next-element dim)))
      (dotimes (i rank)
        (setf face (/ face (get-next-element cdim i)))
        (setf (aref result i) (floor (/ k face)))
        (setf k (rem k face))))))

(defun translate-index (i result x perm indices oldindices ilist)
  "Args: (i result x perm indices oldindices ilist).
Translate row major index in original array to row major index in new
array. Use indices vectors and ilist for temporary storage."
  (declare (fixnum i))
  (let ((rank (array-rank x)))
    (declare (fixnum rank))
    (indices-from-rowmajor x i oldindices)
    (permute-indices oldindices indices perm nil)
    (do ((next ilist (rest next))
         (k 0 (+ k 1)))
        ((not (and (< k rank) (consp next))))
      (setf (first next) (aref indices k)))
    (apply #'array-row-major-index result ilist)))

(defun permute-array (x perm)
  "Args: (a p)
Returns a copy of the array A permuted according to the permutation P."
  (if (not (arrayp x)) (error "not an array - ~a" x))
  (check-sequence perm)
  (if (/= (length perm) (array-rank x)) 
    (error "bad permutation sequence - ~a" perm))
  (let* ((perm (coerce perm 'vector))
         (rank (array-rank x))
         (dim (make-array rank))
         (olddim (coerce (array-dimensions x) 'vector)))
    (declare (fixnum rank))
    ;; construct new dimension vector
    (permute-indices olddim dim perm t)
    ;; make result array and the index vectors and lists */
    (let* ((result (make-array (coerce dim 'list)))
          (indices (make-array rank))
          (oldindices (make-array rank))
          (ilist (make-list rank))
          (data (compound-data-seq x))
          (result_data (compound-data-seq result))
          (n (length data)))
      (declare (fixnum n))
      (dotimes (i rank)
        (declare (fixnum i))
        (setf (aref oldindices i) (list nil)))
      ;; fill in the result
      (if (/= n (length result_data)) (error "bad data"))
      (dotimes (i n result)
        (declare (fixnum i))
        (let ((k (translate-index i result x perm indices oldindices ilist)))
          (declare (fixnum k))
          (setf (aref result_data k) (aref data i)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;              SUM, PROD, COUNT-ELEMENTS, and MEAN Functions
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sum-1 (x)
  (if (numberp x)
    x
    (let ((seq (compound-data-seq x))
          (sum 0))
      (if (consp seq)
        (dolist (x seq sum)
          (setf sum (+ sum (if (numberp x) x (sum-1 x)))))
        (let ((n (length seq)))
          (declare (fixnum n))
          (dotimes (i n sum)
            (declare (fixnum i))
            (let ((x (aref seq i)))
              (setf sum (+ sum (if (numberp x) x (sum-1 x)))))))))))

(defun sum (&rest args)
"Args: (&rest number-data)
Returns the sum of all the elements of its arguments. Returns 0 if there
are no arguments. Vector reducing."
  (if args
    (sum-1 (if (rest args) args (first args)))
    0))



(defun prod-1 (x)
  (if (numberp x)
    x
    (let ((seq (compound-data-seq x))
          (prod 1))
      (if (consp seq)
        (dolist (x seq prod)
          (setf prod (* prod (if (numberp x) x (prod-1 x)))))
        (let ((n (length seq)))
          (declare (fixnum n))
          (dotimes (i n prod)
            (declare (fixnum i))
            (let ((x (aref seq i)))
              (setf prod (* prod (if (numberp x) x (prod-1 x)))))))))))

(defun prod (&rest args)
"Args: (&rest number-data)
Returns the product of all the elements of its arguments. Returns 1 if there
are no arguments. Vector reducing."
  (if args
    (prod-1 (if (rest args) args (first args)))
    1))

(defun count-elements (x)
"Args: (number &rest more-numbers)
Returns the number of its arguments. Vector reducing"
  (if (compound-data-p x)
    (let ((seq (compound-data-seq x))
          (count 0))
      (if (consp seq)
        (dolist (x seq count)
          (incf count (if (compound-data-p x) (count-elements x) 1)))
        (let ((n (length seq)))
          (declare (fixnum n))
          (dotimes (i n count)
            (declare (fixnum i))
            (let ((x (aref seq i)))
              (incf count (if (compound-data-p x) (count-elements x) 1)))))))
    1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;                    IF-ELSE Functions
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun if-else (a x y)
"Args: (first x y)
Takes simple or compound data items FIRST, X and Y and returns result
of elementswise selecting from X if FIRST is not NIL and from Y
otherwise."
  (flet ((base-if-else (a x y) (if a x y)))
    (recursive-map-elements #'base-if-else #'if-else a x y)))
