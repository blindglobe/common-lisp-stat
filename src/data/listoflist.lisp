;;; -*- mode: lisp -*-

;;; Time-stamp: <2009-08-27 08:36:35 tony>
;;; Creation:   <2008-09-08 08:06:30 tony>
;;; File:       listoflist.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c) 2007-2008, AJ Rossini <blindglobe@gmail.com>.  BSD.
;;; Purpose:    Manipulating structures which are lists of lists
;;;             with matrix-likes and dataframe-likes.

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

(in-package :cls-data)

;; We currently assume that the list-of-list representation is in
;; row-major form, i.e. that lists represent rows and not columns.
;; The original lisp-stat had the other way around.  We could augment
;; the top-level list with a property to check orientation
;; (row-major/column-major), but this hasn't been done yet.

;; See XARRAY's listoflist example for more details and support
;; functions (migrated from here to there).

(defun listoflist->matrix-like (lol &key
				(orientation :row-major)
				(coerce-to 'double-float))
  "From a listoflists structure of numbers, return a matrix-like.

FIXME: need to verify that the listoflists is a valid structure (same
size rows, typing if required, etc.

FIXME: need to grep special variables to make the right kind of
matrix-like.

<example>
  (defparameter *lol-ml-test*
      (list (list 1d0 1d0 2.1d0)
            (list 2d0 2d0 1.1d0)))
  (length *lol-ml-test*)
  (length (elt *lol-ml-test* 0))

  (defparameter *lol-ml-result* (listoflist->matrix-like *lol-ml-test*))
  (matrix-dimensions *lol-ml-result*)
</example>"
  (declare (ignorable coerce-to))
  (let ((n (length lol))
	(p (length (elt lol 0))))
    (let ((result (make-matrix  n p :initial-element 0d0)))
      (dotimes (i n)
	(dotimes (j p)
	  (if (equal orientation :row-major)
	      (setf (mref result i j) (coerce (elt (elt lol i) j) coerce-to)) 
	      (setf (mref result i j) (coerce (elt (elt lol j) i) coerce-to)))))
      result)))


;; the following will be handy to help out folks adjust.  It should
;; provide a means to write code faster and better.
(defun listoflist->dataframe (lol) ; &key (type :row-major))
  "Create a cases-by-variables data frame consisting of numeric data,
from a ROW-MAJOR list-of-lists representation.  A COLUMN-MAJOR
representation should be handled using the transpose-listoflists
function."
  (if (lists-of-same-size lol)
      (make-dataframe  (listoflist->array lol))
      (error "make-data-set-from-lists: no combining different length lists"))
  (error "make-data-set-from-lists: proposed name exists"))


