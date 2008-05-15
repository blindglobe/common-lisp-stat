i;;; -*- mode: lisp -*-
;;;
;;; Copyright (c) 2008--, by A.J. Rossini <blindglobe@gmail.com>
;;; See COPYRIGHT file for any additional restrictions (BSD license).
;;; Since 1991, ANSI was finally finished.  Modified to match ANSI
;;; Common Lisp.  

;;;; matrix-matlisp  -- matrix and linear algebra using MATLISP.

;;;
;;; Package Setup
;;;

(in-package :cl-user)

(defpackage :lisp-stat-matrix-matlisp
  (:use :common-lisp
	:cffi
	:matlisp
	:lisp-stat-compound-data)
  (:export matrixp ;;  matrix -- conflicts!
	   num-rows num-cols matmult identity-matrix diagonal
	   row-list column-list inner-product outer-product
	   cross-product transpose bind-columns bind-rows
	   array-data-vector vector-to-array

	   check-matrix check-square-matrix

	   copy-array copy-vector
	   ))

(in-package :lisp-stat-matrix-matlisp)



