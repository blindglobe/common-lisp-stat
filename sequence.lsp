;;; -*- mode: lisp -*-
;;; Copyright (c) 2005--2007, by A.J. Rossini <blindglobe@gmail.com>
;;; See COPYRIGHT file for any additional restrictions (BSD license).
;;; Since 1991, ANSI was finally finished.  Edited for ANSI Common Lisp. 

;;;; Copyright (c) 1991, by Luke Tierney. Permission is granted for
;;;; unrestricted use.  (though Luke never had this file).

;;;;
;;;; Package Setup
;;;;

(in-package :cl-user)

(defpackage :lisp-stat-sequence
  (:use :common-lisp
	:lisp-stat-types)
  (:export check-sequence
	   get-next-element make-next-element set-next-element
	   sequencep iseq

	   ;; maybe?
	   ordered-nneg-seq
	   select

	   which
	   ;; vector differences
	   difference rseq))

(in-package :lisp-stat-sequence)

;;; Sequences are part of ANSI CL, being a supertype of vector and
;;; list (ordered set of things).
;;; 
;;; Need to use the interenal structure when possible -- silly to be
;;; redundant!  However, this means we need to understand what
;;; sequences were intending to do, which I'm not clear on yet.

;;; The original ordering, object-wise, was to have compound
;;; functionality passed into sequences, into other data sources.
;;; However, at this point, we will see about inverting this and
;;; having basic data types pushed through compound, to simplify
;;; packaging. 

;;;                      Type Checking Functions

(defun check-sequence (a)
  ;; FIXME:AJR: does this handle consp as well?  (Luke had an "or"
  ;; with consp).
  (if (not (typep a 'sequence))
      (error "not a sequence - ~s" a)))

;;;                       Sequence Element Access


;;; (elt x i) -- NOT.  This is more like "pop".
(defun get-next-element (x i)
  "Get element i from seq x.  FIXME: not really??"
  (let ((myseq (first x)))
    (if (consp myseq)
        (let ((elem (first myseq)))
          (setf (first x) (rest myseq))
          elem)
      (aref myseq i))))

;;; (setf (elt x i) v)
(defun set-next-element (x i v)
  (let ((seq (first x)))
    (cond ((consp seq)
           (setf (first seq) v)
           (setf (first x) (rest seq)))
          (t (setf (aref seq i) v)))))

(defun make-next-element (x) (list x))


;;;                         Sequence Functions


;; to prevent breakage.
(defmacro sequencep (x) 
  (typep x 'sequence))

(defun iseq (a &optional b)
"Args: (n &optional m)
Generate a sequence of consecutive integers from a to b.
With one argumant returns a list of consecutive integers from 0 to N - 1.
With two returns a list of consecutive integers from N to M.
Examples: (iseq 4) returns (0 1 2 3)
          (iseq 3 7)  returns (3 4 5 6 7)
          (iseq 3 -3) returns (3 2 1 0 -1 -2 -3)"
  (if b
      (let ((n (+ 1 (abs (- b a))))
	    (x nil))
	(dotimes (i n x)
		 (setq x (cons (if (< a b) (- b i) (+ b i)) x))))
      (cond 
       ((= 0 a) nil)
       ((< a 0) (iseq (+ a 1) 0))
       ((< 0 a) (iseq 0 (- a 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;               Subset Selection and Mutation Functions
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; is x an ordered sequence of nonnegative positive integers?
(defun ordered-nneg-seq(x)
  ;; FIXME -- sbcl warning about unreachable code, might be a logic error here. 
  (if (sequencep x)
      (let ((n (length x))
            (cx (make-next-element x))
            (m 0))
        (dotimes (i n t)
          (let ((elem (check-nonneg-fixnum (get-next-element cx i))))
            (if (> m elem) (return nil) (setf m elem)))))))

;;;; select or set the subsequence corresponding to the specified indices
(defun sequence-select(x indices &optional (values nil set-values))
  ;; FIXME -- sbcl warning about unreachable code, might be a logic error here. 
  (let ((rlen 0)
        (dlen 0)
        (vlen 0)
        (data nil)
        (result nil))
    (declare (fixnum rlen dlen vlen))

    ;; Check the input data
    (check-sequence x)
    (check-sequence indices)
    (if set-values (check-sequence values))
    
    ;; Find the data sizes
    (setf data (if (ordered-nneg-seq indices) x (coerce x 'vector)))
    (setf dlen (length data))
    (setf rlen (length indices))
    (when set-values
      (setf vlen (length values))
      (if (/= vlen rlen) (error "value and index sequences do not match")))

    ;; set up the result/value sequence
    (setf result
          (if set-values
              values
              (make-sequence (if (listp x) 'list 'vector) rlen)))

    ;; get or set the sequence elements 
    (if set-values
      (do ((nextx x)
           (cr (make-next-element result))
           (ci (make-next-element indices))
           (i 0 (+ i 1))
           (j 0)
           (index 0))
          ((>= i rlen))
        (declare (fixnum i j index))
        (setf index (get-next-element ci i))
	(if (<= dlen index) (error "index out of range - ~a" index))
        (let ((elem (get-next-element cr i)))
          (cond
           ((listp x)
            (when (> j index)
              (setf j 0)
              (setf nextx x))
            (do ()
                ((not (and (< j index) (consp nextx))))
              (incf j 1)
              (setf nextx (rest nextx)))
            (setf (first nextx) elem))
           (t (setf (aref x index) elem)))))
      (do ((nextx data)
           (cr (make-next-element result))
           (ci (make-next-element indices))
           (i 0 (+ i 1))
           (j 0)
           (index 0)
           (elem nil))
          ((>= i rlen))
        (declare (fixnum i j index))
        (setf index (get-next-element ci i))
	(if (<= dlen index) (error "index out of range - ~a" index))
	(cond
         ((listp data) ;; indices must be ordered
          (do ()
              ((not (and (< j index) (consp nextx))))
            (incf j 1)
            (setf nextx (rest nextx)))
          (setf elem (first nextx)))
         (t (setf elem (aref data index))))
	(set-next-element cr i elem)))
  
    result))

;;;
;;; SELECT function
;;;

(defun select (x &rest args)
"Args: (a &rest indices)
A can be a list or an array. If A is a list and INDICES is a single number
then the appropriate element of A is returned. If  is a list and INDICES is
a list of numbers then the sublist of the corresponding elements is returned.
If A in an array then the number of INDICES must match the ARRAY-RANK of A.
If each index is a number then the appropriate array element is returned.
Otherwise the INDICES must all be lists of numbers and the corresponding
submatrix of A is returned. SELECT can be used in setf."
  (cond
   ((every #'fixnump args)
    (if (listp x) (nth (first args) x) (apply #'aref x args)))
   ((sequencep x) (sequence-select x (first args)))
   (t (subarray-select x args))))


;; Built in SET-SELECT (SETF method for SELECT)
(defun set-select (x &rest args)
  (let ((indices (butlast args))
        (values (first (last args))))
    (cond
     ((sequencep x)
      (if (not (consp indices)) (error "bad indices - ~a" indices))
      (let* ((indices (first indices))
             (i-list (if (fixnump indices) (list indices) indices))
             (v-list (if (fixnump indices) (list values) values)))
        (sequence-select x i-list v-list)))
     ((arrayp x)
      (subarray-select x indices values))
     (t (error "bad argument type - ~a" x)))
    values))

(defsetf select set-select)

;;;;
;;;; Basic Sequence Operations
;;;;

(defun difference (x)
"Args: (x)
Returns differences for a sequence X."
  (let ((n (length x)))
    (- (select x (iseq 1 (1- n))) (select x (iseq 0 (- n 2))))))

(defun rseq (a b num)
"Args: (a b num)
Returns a list of NUM equally spaced points starting at A and ending at B."
  (+ a (* (values-list (iseq 0 (1- num))) (/ (float (- b a)) (1- num)))))


;;;;
;;;; WHICH function
;;;;

(defun which (x)
"Args: (x)
Returns a list of the indices where elements of sequence X are not NIL."
  (let ((x (list (compound-data-seq x)))
	(result nil)
	(tail nil))
    (flet ((add-result (x)
             (if result (setf (rest tail) (list x)) (setf result (list x)))
	     (setf tail (if tail (rest tail) result)))
	   (get-next-element (seq-list i)
	     (cond ((consp (first seq-list))
		    (let ((elem (first (first seq-list))))
		      (setf (first seq-list) (rest (first seq-list)))
		      elem))
		   (t (aref (first seq-list) i)))))
	  (let ((n (length (first x))))
	    (dotimes (i n result)
		     (if (get-next-element x i) (add-result i)))))))
