;;; -*- mode: lisp -*-
;;; Copyright (c) 2005--2007, by A.J. Rossini <blindglobe@gmail.com>
;;; See COPYRIGHT file for any additional restrictions (BSD license).
;;; Since 1991, ANSI was finally finished.  Edited for ANSI Common Lisp. 

;;;; fastmap -- Fast version of MAP
;;;; 
;;;; Copyright (c) 1991, by Luke Tierney. Permission is granted for
;;;; unrestricted use.
;;;;
;;;; The FASTMAP function is a version of MAP that is assumed to
;;;;
;;;;   a) be reasonable fast on any combination of lists and vectors
;;;;      as its arguments
;;;;
;;;;   b) not hang if at least one of its arguments is not a circular
;;;;      list.
;;;;
;;;; This function is the core of the vectorized arithmetic system, so it
;;;; may be worth optimizing for each CL implementation. I tried to improve
;;;; it for (A)KCL but have not yet been able to obtain a significant
;;;; increase in speed.

;;;;
;;;; Package Setup
;;;;

(defpackage :lisp-stat-fastmap
  (:use :common-lisp)
  (:export fastmap))

(in-package #:lisp-stat-fastmap)

;;;;
;;;; Functions
;;;;

(defun sequencep (x) 
"Args: (x)
Returns NIL unless X is a list or vector."
  (or (listp x) (vectorp x)))

(defun cdr-lists (args)
  (do ((list args (cdr list)))
      ((null list))
    (if (consp (car list)) (rplaca list (cdar list)))))

(defun get-result-size (args)
  (macrolet ((any-nulls (ls) `(dolist (x ,ls) (if (null x) (return t)))))
    (let ((lists nil)
	  (n nil))
      (dolist (x args)
        (if (consp x)
          (push x lists)
	  (setf n (if n (min n (length x)) (length x)))))
      (cond
       ((and n lists)
	(let ((m 0))
	  (loop
	   (if (or (<= n m) (any-nulls lists)) (return))
	   (cdr-lists lists)
	   (incf m))
	  (min n m)))
       (lists
	(let ((m 0))
	  (loop
	   (if (any-nulls lists) (return))
	   (cdr-lists lists)
	   (incf m))
	  m))
       (t n)))))

(defun fastmap (type fcn &rest args)
  (cond ((and (eq type 'list) (every #'listp args))
	 (apply #'mapcar fcn args))
	((and (eq type 'vector) (every #'vectorp args))
	 (apply #'map 'vector fcn args))
	((every #'sequencep args)
	 (let* ((n (get-result-size args))
		(result (make-sequence type n))
		(farg (make-list (length args))))
	   (declare (fixnum n))
	   (macrolet ((fill-arglist ()
		        `(do ((f farg (cdr f))
			      (a args (cdr a)))
			     ((null f))
			   (rplaca f 
				   (if (consp (car a))
				     (caar a)
				     (aref (car a) i))))))
	     (if (consp result)
	       (let ((r result))
		 (dotimes (i n result)
	           (declare (fixnum i))
		   (fill-arglist)
		   (rplaca r (apply fcn farg))
		   (setf r (cdr r))
		   (cdr-lists args)))
	       (dotimes (i n result)
	         (declare (fixnum i))
		 (fill-arglist)
		 (setf (aref result i) (apply fcn farg))
		 (cdr-lists args))))))
	(t (error "not all sequences"))))
