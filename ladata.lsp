;;; -*- mode: lisp -*-
;;; Copyright (c) 2005--2007, by A.J. Rossini <blindglobe@gmail.com>
;;; See COPYRIGHT file for any additional restrictions (BSD license).
;;; Since 1991, ANSI was finally finished.  Edited for ANSI Common Lisp. 

;;;; ladata -- Data handling functions for linear algebra interface
;;;; 
;;;; Copyright (c) 1991, by Luke Tierney. Permission is granted for
;;;; unrestricted use.

;;;;
;;;; Package Setup
;;;;

(in-package #:lisp-stat-basics)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;                        Data Mode Functions
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;
;;;; These constants need to be redefined if IN, RE or CX in linalg.h
;;;; are changed.
;;;;
(defparameter +mode-in+ 0)
(defparameter +mode-re+ 1)
(defparameter +mode-cx+ 2)

(defun mode-of (x) 
  (etypecase x
	     (fixnum +mode-in+)
	     (rational +mode-re+)
	     (float +mode-re+)
	     (complex +mode-cx+)))

(defun la-data-mode (data)
  (let ((data (compound-data-seq data))
	(mode 0))
    (cond 
     ((vectorp data)
      (let ((n (length data)))
	(declare (fixnum n))
	(dotimes (i n mode)
		 (declare (fixnum i))
		 (setf mode (max mode (mode-of (aref data i)))))))
     ((consp data) (dolist (x data mode) (setf mode (max mode (mode-of x)))))
     (t (error "bad sequence - ~s" data)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;                        Internal Allocation Funcitons
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *la-allocations* nil)

(defun la-allocate (n m)
  (let ((p (la-base-allocate n m)))
    (if (null-ptr-p p) (error "allocation failed"))
    (if (member p *la-allocations* :test #'ptr-eq)
      (error "pointer is already on the list"))
    (push p *la-allocations*)
    p))

(defun la-free (p)
  (when (and (not (null-ptr-p p)) (member p *la-allocations* :test #'ptr-eq))
	(setf *la-allocations* (delete p *la-allocations* :test #'ptr-eq))
	(la-base-free p)))

(defun la-cleanup-allocations ()
  (let ((allocs (copy-list *la-allocations*)))
    (dolist (p allocs) (la-free p))))
	  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;                       C Vector and Array Allocation
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun la-vector(n mode) (la-allocate n (la-mode-size mode)))
(defun la-free-vector (v) (la-free v))

(defun la-matrix (n m mode)
  (let ((matrix (la-allocate n (la-mode-size +mode-in+))))
    (dotimes (i n)
      (la-put-pointer matrix i (la-allocate m (la-mode-size mode))))
    matrix))

(defun la-free-matrix (matrix n)
  (dotimes (i n)
    (la-free (la-get-pointer matrix i)))
  (la-free matrix))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;                   C to/from Lisp Data Conversion
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun la-data-to-vector (data mode)
  (check-sequence data)
  (let* ((n (length data))
	 (vec (la-vector n mode))
	 (d (make-next-element data)))
    (declare (fixnum n))
    (cond
     ((= mode +mode-in+)
      (dotimes (i n) 
        (declare (fixnum i))
	(la-put-integer vec i (get-next-element d i))))
     ((= mode +mode-re+)
      (dotimes (i n) 
        (declare (fixnum i))
	(la-put-double vec i (get-next-element d i))))
      ((= mode +mode-cx+)
       (dotimes (i n) 
         (declare (fixnum i))
	 (let ((x (get-next-element d i)))
	   (la-put-complex vec i (realpart x) (imagpart x))))))
    vec))

(defun la-data-to-matrix (data mode)
  (check-matrix data)
  (let* ((n (num-rows data))
	 (m (num-cols data))
	 (mat (la-matrix n m mode)))
    (declare (fixnum n m))
    (cond
     ((= mode +mode-in+)
      (dotimes (i n)
        (declare (fixnum i))
	(let ((vec (la-get-pointer mat i)))
	  (dotimes (j m)
	    (la-put-integer vec j (aref data i j))))))
      ((= mode +mode-re+)
       (dotimes (i n) 
         (declare (fixnum i))
	 (let ((vec (la-get-pointer mat i)))
	   (dotimes (j m)
	     (la-put-double vec j (aref data i j))))))
      ((= mode +mode-cx+)
       (dotimes (i n) 
         (declare (fixnum i))
	 (let ((vec (la-get-pointer mat i)))
	   (dotimes (j m)
	     (let ((x (aref data i j)))
	       (la-put-complex vec i (realpart x) (imagpart x))))))))
    mat))

(defun la-vector-to-data (vec n mode data)
  (declare (fixnum n))
  (check-sequence data)
  (let ((d (make-next-element data))
	(gf (cond
	      ((= mode +mode-in+) #'la-get-integer)
	      ((= mode +mode-re+) #'la-get-double)
	      ((= mode +mode-cx+) #'la-get-complex))))
    (dotimes (i n)
      (declare (fixnum i))
      (set-next-element d i (funcall gf vec i))))
  data)

(defun la-matrix-to-data (mat n m mode result)
  (declare (fixnum n m))
  (check-matrix result)
  (let ((gf (cond
	      ((= mode +mode-in+) #'la-get-integer)
	      ((= mode +mode-re+) #'la-get-double)
	      ((= mode +mode-cx+) #'la-get-complex))))
    (dotimes (i n)
      (declare (fixnum i))
      (let ((vec (la-get-pointer mat i)))
	(dotimes (j m)
	  (declare (fixnum j))
	  (setf (aref result i j) (funcall gf vec j))))))
  result)
