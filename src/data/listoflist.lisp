;;; -*- mode: lisp -*-

;;; Time-stamp: <2009-07-14 19:18:19 tony>
;;; Creation:   <2008-09-08 08:06:30 tony>
;;; File:       listoflist.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c) 2007-2008, AJ Rossini <blindglobe@gmail.com>.  BSD.
;;; Purpose:    Manipulating structures which are lists of lists
;;;             rather than arrays or matrix-likes.

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

;;; Thoughts for organization: there are 2 general flavors of
;;; activities.  The first is that we do list-of-list to list-of-list
;;; transforms, and these do not rely on external packages being in
;;; existence.  The second is that we do transformations from them
;;; into other similar rectangular or ragged data structures.
;;; Within-structure should include item-selection and subsetting,
;;; while between-structure should include copying and ideally,
;;; "pass-through" referential structures.  The latter is probably
;;; going to take a performance hit, but should allow for maximal
;;; memory use.

;; Where should this go?
(in-package :cls-data-listoflist) ;; probably better in cls-data...

;; Currently, we assume that the list-of-list representation is in
;; row-major form, i.e. that lists represent rows and not columns.
;; The original lisp-stat had the other way around.  We could augment
;; the top-level list with a property to check orientation
;; (row-major/column-major), but this hasn't been done yet.

#|
;; Test cases:
 (and T T nil T)
 (and T T T)
 (defparameter *x1* (list 1 2 3))
 (defparameter *x2* (list 1 2 3))
 (defparameter *x3* (list 1 2 3 4))
 (defparameter *x4* (list 1 2 3))
 (reduce #'(lambda (x y)
	      (if (= x y) y -1))
	  (mapcar #'length (list *x1* *x2* *x3*)))
 (reduce #'(lambda (x y)
	      (if (= x y) y -1))  (list 2 3 2))
 (lists-of-same-size *x1* *x2* *x4*) ; => T
 (lists-of-same-size *x1* *x3* *x4*) ; => F
 (lists-of-same-size *x1* *x2* *x3*) ; => F
 (lists-of-same-size *x3* *x1* *x3*) ; => F
|#


(defun lists-of-same-size (&rest list-of-list-names)
  "Check if the lengths of the lists are equal (T, otherwise NIL), to
justify further processing and initial conditions."
  (if (< 0  (reduce #'(lambda (x y) (if  (= x y) y -1))
		    (mapcar #'length list-of-list-names)))
      T nil))


(defun listoflist->array (lol &key (type 'row-major))
  "From a listoflists structure, make an array.

FIXME: need to verify that the listoflists is a valid structure (same
size rows, typing if required, etc.

<example>
  (defparameter *mdfl-test*
      (list (list 'a 1 2.1)
            (list 'b 2 1.1)
            (list 'c 1 2.0)
            (list 'd 2 3.0)))
  (length *mdfl-test*)
  (length (elt *mdfl-test* 0))

  (defparameter *mdfl-test-dt* (make-datatable-from-listoflists *mdfl-test*))
  (array-dimensions *mdfl-test-dt*)
</example>"
  (let ((n (length lol))
	(p (length (elt lol 0))))
    (let ((result (make-array (list n p))))
      (dotimes (i n)
	(dotimes (j p)
	  (if (equal  type 'row-major)
	      (setf (aref result i j) (elt (elt lol i) j))
	      (setf (aref result i j) (elt (elt lol j) i)))))
      result)))


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


(defun transpose-listoflist (listoflist)
  "This function does the moral-equivalent of a matrix transpose on a
list-of-lists data structure"
  (apply #'mapcar #'list listoflist))

;; (defparameter LOL-2by3 (list (list 1 2) (list 3 4) (list 5 6)))
;; (defparameter LOL-3by2 (list (list 1 3 5) (list 2 4 6)))
;; (transpose-listoflists (transpose-listoflists LOL-2by3))
;; (transpose-listoflists (transpose-listoflists LOL-3by2))

(defun equal-listoflist (x y)
  "FIXME: This function, when written, should walk through 2 listoflists and
return T/nil based on equality."
  (and (= (list-length x) ;; top-level length same
	  (list-length y))
       ;; FIXME: within-level lengths same
       ;; FIXME: flattened values same, walking through
       ;; (loop over x and verify same tree as y)
       ))



#|
  (defparameter *mdfl-test*
      (list (list 'a 1 2.1)
            (list 'b 2 1.1)
            (list 'c 1 2.0)
            (list 'd 2 3.0)))
  (length *mdfl-test*)
  (length (elt *mdfl-test* 0))

  (defparameter *mdfl-test-dt* (make-datatable-from-listoflists *mdfl-test*))
  (array-dimensions *mdfl-test-dt*)

|#
