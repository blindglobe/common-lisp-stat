;;; -*- mode: lisp -*-

;;; Time-stamp: <2009-01-17 18:00:55 tony>
;;; Creation:   <2008-03-11 19:18:34 user> 
;;; File:       packages.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2009--, AJ Rossini.  BSD, LLGPL, or GPLv2, depending
;;;             on how it arrives.  
;;; Purpose:    sweep operator implementation.

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

;;; This organization and structure is new to the 21st Century
;;; version.


(in-package :lisp-stat-numerics-sweep)

;; macros

;; internal:  divide up the operator strategy to handle the variety of
;; possibilities that we consider for analysis of the matrix, in
;; prticular we needf to think about what it is that we want to do to
;; identify the matrix

(defun op-swap-rows (m r1n r2n))

(defun op-scalar-mult-row (m s r))

(defun op-subtract-rows (m r1 r2))



;; external 
(defun sweep-operator (m type)
  "Return the swept matrix n. 

   The basic idea behind the sweep operator is that we can get a
 matrix A converted to I in a few steps.  It is critical that we can
 figure out the general approach for the algorithm, as doing it in
 line, i.e. [A|I] => [I|A^*] means we can probably do something a bit
 more like [A] => [A^*], since the original operations work on a
 column-by-column basis."
  (let ((n (copy! m)))
    (dotimes (i (nrows m))
      (set-ith-ith-entry-to-1 n)
      (zero-other-entries)) 
    n))
