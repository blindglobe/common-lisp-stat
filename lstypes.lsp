;;; -*- mode: lisp -*-

;;; Copyright (c) 2005--2007, by A.J. Rossini <blindglobe@gmail.com>
;;; See COPYRIGHT file for any additional restrictions (BSD license).
;;; Since 1991, ANSI was finally finished.  Edited for ANSI Common Lisp. 

(in-package :cl-user)


(defpackage :lisp-stat-types
  (:documentation "Provides some typeing for LispStat, but is clearly
                   a bit incomplete.")
  (:use :common-lisp)
  (:export fixnump check-nonneg-fixnum check-one-fixnum
	   check-one-real check-one-number))

(in-package :lisp-stat-types)

;;; Type Checking Functions
;;; (Moved out of lsbasics.lsp, placed there by initial design).

(defun fixnump (x)
  "Args: (x)

Returns T if X is a fixnum; NIL otherwise."
  (declare (inline typep))
  (typep x 'fixnum))

(defun check-nonneg-fixnum (x)
  "Ensure that x or all elts of x are non-negative fixnums."
  (cond ((typep x 'sequence) ;; seq rather than list, allows for vector?
	 (map 'list #'check-one-nonneg-fixnum x))
	(t (check-one-nonneg-fixnum x))))

(defun check-one-nonneg-fixnum (x)
  "return value if true, throw error otherwise."
  (if (and (fixnump x) (<= 0 x))
      x
      (error "Expected non-negative fixnum, but got ~A" x)))

(defun check-one-fixnum (x)
  (if (not (fixnump x))
      (error "not a fixnum - ~a" x)))

(defun check-one-real (a)
  (if (not (or (rationalp a) (floatp a)))
      (error "not a real number ~s" a)
    t))

(defun check-one-number (a)
  (if (not (numberp a))
      (error "not a number ~s" a)
    t))
