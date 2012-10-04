;;; -*- mode: lisp -*-

;;; Time-stamp: <2012-07-01 12:02:55 tony>
;;; Creation:   <2009-03-12 17:14:56 tony>
;;; File:       dataframe-listoflist.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2009--, AJ Rossini.  BSD, LLGPL, or GPLv2, depending
;;;             on how it arrives.  
;;; Purpose:    Instance of dataframe with the storage done using
;;;             LISTOFLIST data storage.

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

;;; This organization and structure is new to the 21st Century
;;; version..   Think, "21st Century Schizoid Man".

(in-package :cls-dataframe)

;;; DATAFRAME-LISTOFLIST
;;; 
;;; example/implementatin of using lisp-matrix datastructures for
;;; dataframe storage.

(defclass dataframe-listoflist (dataframe-like)
  ((store :initform nil
	  :initarg :storage
	  :type list
	  :accessor dataset
	  :documentation "Data storage: typed as matrix-like
  (numerical only)."))
  (:documentation "example implementation of dataframe-like using
  storage based on lisp-matrix structures."))

(defmethod nrows ((df dataframe-listoflist))
  "specializes on inheritance from listoflist in lisp-matrix."
  (length (dataset df)))

(defmethod ncols ((df dataframe-listoflist))
  "specializes on inheritance from matrix-like in lisp-matrix."
  (length (elt (dataset df) 0)))

(defmethod xref ((df dataframe-listoflist) &rest subscripts)
  "Returns a scalar in array, in the same vein as aref, mref, vref,
etc. idx1/2 is row/col or case/var."
  (elt (elt (dataset df) (elt subscripts 0)) (elt subscripts 1))) ;; ??

(defmethod (setf xref) (value (df dataframe-listoflist) &rest subscripts)
  "Sets a value for df-ml."
  ;; NEED TO CHECK TYPE!
  ;; (check-type val (elt (vartype df) index2))
  (setf (elt (elt (dataset df) (elt subscripts 1)) (elt subscripts 0)) value))

;;;;;; IMPLEMENTATION INDEPENDENT FUNCTIONS AND METHODS
;;;;;; (use only xref, nrows, ncols and similar dataframe-like
;;;;;; components as core).

(defun xref-var (df index return-type)
  "Returns the data in a single variable as type.
type = sequence, vector, vector-like (if valid numeric type) or dataframe."
  (ecase return-type
    (('list)
     (map 'list
	  #'(lambda (x) (xref df index x))
	  (gen-seq (nth 2 (array-dimensions (dataset df))))))
    (('vector) t)
    (:vector-like t)
    (:matrix-like t)
    (:dataframe t)))

(defun xref-case (df index return-type)
  "Returns row as sequence."
  (ecase return-type
    (:list 
     (map 'list
	  #'(lambda (x) (xref df x index))
	  (gen-seq (nth 1 (array-dimensions (dataset df))))))
    (:vector t)
    (:vector-like t)
    (:matrix-like t)
    (:dataframe t)))

;; FIXME
(defun xref-2indexlist (df indexlist1 indexlist2 &key (return-type :array))
  "return an array, row X col dims.  FIXME TESTME"
  (case return-type
    (:array 
     (let ((my-pre-array (list)))
       (dolist (x indexlist1)
	 (dolist (y indexlist2)
	   (append my-pre-array (xref df x y))))
       (make-array (list (length indexlist1)
			 (length indexlist2))
		   :initial-contents my-pre-array)))
    (:dataframe
     (make-instance 'dataframe-array
		    :storage (make-array
			      (list (length indexlist1)
				    (length indexlist2))
			      :initial-contents (dataset df))
		    ;; ensure copy for this and following
		    :doc (doc-string df)
		    ;; the following 2 need to be subseted based on
		    ;; the values of indexlist1 and indexlist2
		    :case-labels (case-labels df)
		    :var-labels (var-labels df)))))

