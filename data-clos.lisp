;;; -*- mode: lisp -*-

;;; File:       data-clos.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2008, AJ Rossini.  BSD, LLGPL, or GPLv2, depending
;;;             on how it arrives.  
;;; Purpose:    data package for lispstat
;;; Time-stamp: <2008-03-12 17:18:42 user>
;;; Creation:   <2008-03-12 17:18:42 user>

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

;;; This organization and structure is new to the 21st Century
;;; version.

;;; data-clos.lisp
;;; 
;;; redoing regression in a CLOS based framework.
;;; See regression.lsp for basis of work.

(in-package :cl-user)

(defpackage :lisp-stat-data-clos
  (:use :common-lisp
	:clem ;;?? or :matlisp , or :...?
	)
  (:export dataset extract set))

(in-package :lisp-stat-data-clos)


;; Need to figure out typed vectors.   We then map a series of typed
;; vectors over to tables where columns are equal typed.  In a sense,
;; this is a relation (1-1) of equal-typed arrays.  For the most part,
;; this ends up making the R data.frame into a relational building
;; block (considering 1-1 mappings using row ID as a relation).  


(defclass table ())

(defclass relation ())

(defclass dataset ()
  ((store :initform nil
	  :initarg :storage
	  :accessor dataset)
   (documentation-string :initform nil
			 :initarg :doc
			 :accessor doc-string)
   (case-labels :initform nil
		:initarg :case-labels 
		:accessor case-labels)
   (var-labels :initform nil
	       :initarg :var-labels
	       :accessor var-labels)))
;;  (:documentation "Standard Cases by Variables Dataset."))

;; Need to set up dataset as a table or a relation.  One way or
;; another it should all work out.  TODO:  How do we do multiple
;; inheritance or composite structures?

(defvar my-ds-1 nil 
  "test ds for experiment.")
(setf my-ds-1 (make-instance 'dataset))
my-ds-1

(defvar my-ds-2 nil 
  "test ds for experiment.")
(setf my-ds-2 (make-instance 'dataset
			     :storage #2((1 2 3 4 5) (10 20 30 40 50))
			     :doc "This is an interesting dataset"
			     :case-labels (list "a" "b" "c" "d" "e")
			     :var-labels (list "x" "y")))
my-ds-2
(dataset my-ds-2)
(doc-string my-ds-2)
(case-labels my-ds-2)
(var-labels my-ds-2)

(defun print-structure-table (ds)
  "example of what we want the methods to look like.  Should be sort
of like a spreadsheet if the storage is a table."
  (print-as-row (var-labels ds))
  (let ((j -1))
    (dolist (i (case-labels ds))
      (princ "%i %v" i (row-extract (dataset ds) (incr j))))))

(defun print-structure-relational (ds)
  "example of what we want the methods to look like.  Should be sort
of like a graph of spreadsheets if the storage is a relational
structure."
  (dolist (k (relations ds))
    (print-as-row (var-labels ds))
    (let ((j -1))
      (dolist (i (case-labels ds))
	(princ "%i %v" i (row-extract (dataset ds) (incr j))))))
  



(defgeneric extract  (dataform what into-form))

(defmethod extract ((ds dataset) what into-form)
  (reshape (get ds what) into-form))



(defclass data-format())


(defun transpose (x)
  "map NxM to MxN.")

(defun reorder-by-rank (x order &key (by-row t))
  " .")

(defun reorder-by-permutation (x perm &key (by-row t))
  " .")

;;; need a setter for case and var names.  I think we can do that in
;;; the class definitinon?





