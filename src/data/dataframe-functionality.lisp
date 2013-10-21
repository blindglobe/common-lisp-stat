;;; -*- mode: lisp -*-

;;; Time-stamp: <2013-10-21 09:31:47 tony>
;;; Creation:   <2009 tony>
;;; File:       dataframe-functionality
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2012--, AJ Rossini.  Currently licensed under MIT
;;;             license.  See file LICENSE.mit in top-level directory
;;;             for information.
;;; Purpose:    Functions (not methods) for dataframes.

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

;; This file contains functions.  Most functionality is properly done
;; via generics, because Tony doesn't care about speed (and he is
;; waiting for hostile remarks from people reading this code), but
;; when we need to have functions, then we need to ensure that all
;; components are preloaded.

(in-package :cls-dataframe)

(defgeneric nvars (df)
  (:documentation  "number of variables represented in storage type.")
  (:method ((df matrix-like))
    (ncols df))
  (:method ((df dataframe-array))
    (ncols df))
  (:method ((df list))
    (ncols df))
  (:method ((df array))
    (array-dimension  df 1)))

(defgeneric ncases (df)
  (:documentation "number of cases (indep, or indep within context,
  observantions) within DF storage form.")
  (:method ((df simple-array))
    (array-dimension df 0))
  (:method ((df matrix-like))
    (nrows df))
  (:method ((df list))
    (nrows df)) ;; probably should do a valid LISTOFLIST structure
		;; test but this would be inefficient
  (:method ((df dataframe-array))
    (nrows df))
  (:method ((df array))
    (array-dimension df 0)))

(defun make-dataframe (newdata
		       &key  (vartypes nil)
		       (caselabels nil) (varlabels nil)
		       (doc "no docs"))
  "Helper function to use instead of make-instance to assure
construction of proper DF-array. Needs some thought so we don't have
to use listoflist->array when creating a dataframe array so much"

  (check-type newdata (or matrix-like array list))
  (check-type caselabels sequence)
  (check-type varlabels sequence)
  (check-type vartypes sequence)
  (check-type doc string)
  (let ((ncases (ncases newdata))
	(nvars (nvars newdata)))
    
    (if caselabels (assert (= ncases (length caselabels))))
    (if varlabels (assert (= nvars (length varlabels))))
    (let ((newcaselabels (if caselabels
			     caselabels
			     (make-labels "C" ncases)))
	  (newvarlabels (if varlabels
			    varlabels
			    (make-labels "V" nvars))))
    
      (etypecase newdata 
	(list
	 (make-instance 'dataframe-listoflist
			:storage newdata
			:nrows (length newcaselabels)
			:ncols (length newvarlabels)
			:case-labels newcaselabels
			:var-labels newvarlabels
			:var-types vartypes))
	(array
	 (make-instance 'dataframe-array
			:storage newdata
			:nrows (length newcaselabels)
			:ncols (length newvarlabels)
			:case-labels newcaselabels
			:var-labels newvarlabels
			:var-types vartypes))
	
	(matrix-like
	 (make-instance 'dataframe-matrixlike
			:storage newdata
			:nrows (length newcaselabels)
			:ncols (length newvarlabels)
			:case-labels newcaselabels
			:var-labels newvarlabels
			:var-types vartypes))))))

#| 
 (make-dataframe #2A((1.2d0 1.3d0) (2.0d0 4.0d0)))
 (make-dataframe #2A(('a 1) ('b 2)))
 (xref (make-dataframe #2A(('a 1) ('b 2))) 0 1)
 (xref (make-dataframe #2A(('a 1) ('b 2))) 1 0)
 (make-dataframe 4) ; ERROR, should we allow?
 (make-dataframe #2A((4)))
 (make-dataframe (rand 10 5)) ;; ERROR, but should work!
|#




(defun  df->grid (df &rest cols)
   "A helper function that creates a foreign grid of (ncase df)
and (length cols) specifically for passing to gsll. If a column is a
date then it will be converted into the equivalent fixnum
representation - thats not implemented as yet. if colls is the single
keyword :all then just do all the cols."

  (flet ((create-subset (df  cols)
	   "return selected columns as a list"
	   (loop for the-row below (ncases df)
		 collect (loop for col in cols
			       collect (xref df the-row))))
	 (translate-columns (&rest cols)
	   "convert list of column keywords to indexes"
	   (if (equal cols :all)
	       (alexandria:iota (nvars df))
	       (loop for c in cols
		     collect (translate-column df c)))))
  (grid:make-foreign-array 'double-float
			   :initial-contents (create-subset df (translate-columns cols)))))

