;;; -*- mode: lisp -*-

;;; Time-stamp: <2009-01-12 17:38:14 tony>
;;; Creation:   <2008-09-08 08:06:30 tony>
;;; File:       listoflist.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c) 2007-2008, AJ Rossini <blindglobe@gmail.com>.  BSD.
;;; Purpose:    Manipulating structures which are lists of lists
;;;             rather than arrays or matrix-likes,

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.


;; Where should this go?
(in-package :cls-data)


;; Glib commentary:
;; Serious flaw -- need to consider that we are not really well
;; working with the data structures, in that Luke created compound as
;; a base class, which turns out to be slightly backward if we are to
;; maintain the numerical structures as well as computational
;; efficiency.
;;
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


;; the following will be handy to help out folks adjust.  It should
;; provide a means to write code faster and better.
(defmacro make-data-set-from-lists (datasetname
				    &optional (force-overwrite nil)
				    &rest lists-of-data-lists)
  "Create a cases-by-variables data frame consisting of numeric data,
from a ROW-MAJOR list-of-lists representation.  A COLUMN-MAJOR
representation should be handled using the transpose-listoflists
function."
  (if (or (not (boundp datasetname))
	  force-overwrite)
      (if (lists-of-same-size lists-of-data-lists)
	  `(defparameter ,datasetname
	     (make-matrix (length iron) 2
			  :initial-contents
			  (mapcar #'(lambda (x y) 
				      (list (coerce x 'double-float)
					    (coerce y 'double-float)))
				  ,@lists-of-data-lists)))
	  (error "make-data-set-from-lists: no combining different length lists"))
      (error "make-data-set-from-lists: proposed name exists")))

#|
(macroexpand '(make-data-set-from-lists
	       this-data
	       :force-overwrite nil
	       aluminum iron))
(princ this-data)
|#

(defun transpose-listoflists (listoflists)
  "This function does the moral-equivalent of a matrix transpose on a
list-of-lists data structure"
  (apply #'mapcar #'list listoflists))

;; (defparameter LOL-2by3 (list (list 1 2) (list 3 4) (list 5 6)))
;; (defparameter LOL-3by2 (list (list 1 3 5) (list 2 4 6)))
;; (transpose-listoflists (transpose-listoflists LOL-2by3))
;; (transpose-listoflists (transpose-listoflists LOL-2by3))

(defun equal-listoflists (x y)
  "FIXME: This function, when written, should walk through 2 listoflists and
return T/nil based on equality."
  (and
   ;; top-level length same
   (= (list-length x)
      (list-length y))
   ;; FIXME: within-level lengths same
   ()
   ;; FIXME: flattened values same, walking through
   (loop over x and verify same tree as y)))
