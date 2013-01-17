1
;;; -*- mode: lisp -*-

;;; Time-stamp: <2013-01-09 08:22:40 tony>
;;; Creation:   <2009-03-12 17:14:56 tony>
;;; File:       dataframe-array.lisp
;;; Authors:    AJ Rossini <blindglobe@gmail.com>
;;;             David Hodge <davidbhodge@gmail.com>

;;; Copyright:  (c)2009--, AJ Rossini. MIT
;;; Copyright:  (c)2012--, David Hodge. MIT

;;; Purpose:    real dataframe class using lisp-arrays as storage.

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

;;; This organization and structure is new to the 21st Century
;;; version..   Think, "21st Century Schizoid Man".

(in-package :cls-dataframe)

;;;;; DATAFRAME-ARRAY

(defclass dataframe-array (dataframe-like)
  ((store :initform nil
	  :initarg :storage
	  :type (array * *)
	  :accessor dataset
	  :documentation "Data storage: typed as array.")
   (store-class :initform 'array
		:accessor store
		:documentation "Storage class is ARRAY."))
  (:documentation "example implementation of dataframe-like using storage
  based on lisp arrays.  An obvious alternative could be a
  dataframe-matrix-like which uses the lisp-matrix classes."))

(defun translate-column (df column)
  "for production use, we would wrap this in a handler to enable entry of the correct column id"
  (cond
    ((typep column 'keyword)
     (let ((col (position column (varlabels df))))
       (if col
	   col
	   (error "Column name misspelt: try again ~a~%" column))))
    ((typep column 'number) column)
    (t (error "Invalid argument passed to translate-column ~a~%" column))))


(defun translate-column2 (df column)
  (cond
    ((typep column 'keyword) (position column (varlabels df)))
    ((typep column 'number) column)
    (t (error "Invalid argument passed to translate-column"))))


(defmethod make-dataframe2 ((data dataframe-array)
			    &key vartypes varlabels caselabels doc)
  (check-dataframe-params data vartypes varlabels caselabels doc)
  (let ((newcaselabels (if caselabels
			   caselabels
			     (make-labels "C" (ncases data))))
	  (newvarlabels (if varlabels
			    varlabels
			    (make-labels "V" (nvars data))))
	  ;; also should determine most restrictive possible (compsci
	  ;; and/or statistical) variable typing (integer, double,
	  ;; string, symbol, *).  FIXME: until we get the mixed typing system in place, we will just leave null
	  (newvartypes (if vartypes
			  vartypes
			  (make-labels "*" (nvars data)))))
      (make-instance 'dataframe-array
		     :storage data
		     :nrows (length newcaselabels)
		     :ncols (length newvarlabels)
		     :case-labels newcaselabels
		     :var-labels newvarlabels
		     :var-types newvartypes)))


(defmethod nrows ((df dataframe-array))
  "specializes on inheritance from matrix-like in lisp-matrix."
  (array-dimension (dataset df) 0))

(defmethod ncols ((df dataframe-array))
  "specializes on inheritance from matrix-like in lisp-matrix."
  (array-dimension (dataset df) 1))



(defmethod xref ((df dataframe-array) &rest subscripts)
  "Returns a scalar in array, in the same vein as aref, mref, vref, etc.
idx1/2 is row/col or case/var."
  (assert (>= 2 (length subscripts)))
#| ;; needed?
  (assert (typep (elt subscripts 0) integer))
  (assert (typep (elt subscripts 1) integer))
|#
  (aref (dataset df) (elt subscripts 0) (translate-column df (elt subscripts 1))))

(defmethod (setf xref) (value (df dataframe-array) &rest subscripts)
  "set value for df-ar."
  ;; (check-type val (elt (var-type df) index2))
  (setf (aref (dataset df) (elt subscripts 0) (elt subscripts 1)) value))

(defparameter *default-dataframe-class* 'dataframe-array)

(defmethod dfselect ((df dataframe-array) 
		     &optional cases vars indices)
  "Extract the OR of cases, vars, or have a list of indices to extract"
  (if indices (error "Indicies not used yet"))
  (let ((newdf (make-instance *default-dataframe-class*
		:storage (make-array (list  (length cases) (length vars)))
		:nrows (length cases)
		:ncols (length vars)
#|
		:case-labels (select-list caselist (case-labels df))
		:var-labels (select-list varlist (var-labels df))
		:var-types (select-list varlist (vartypes df))
|#
		)))
    (dotimes (i (length cases))
      (dotimes (j (length vars))
	(setf (xref newdf i j)
	      (xref df
		    (position (elt cases i) (case-labels df))
		    (position (elt vars j) (var-labels df))))))))


(defmethod dfhead ((df dataframe-array)
		   &optional (rows 10))
  (dotimes ( i rows)
    (format t "~A: " i)
    (dotimes ( j (ncols df))
      (format t "~A~t" (xref df i j)) )
    (format t "~%")))

(defmethod dfgroupby ((df dataframe-array) variable)
  "a quick hack for summarise."
  (let ( (h (make-hash-table :test #'equal)))
    (dotimes (i (nrows df))
      (incf (gethash (xref df i variable) h 0)))
    (maphash #'(lambda (k v) (format t "~a => ~a~%" k v)) h)))

(defmethod dfsummarisebycategory ((df dataframe-array) category observation function)
  "apply function to the observation in rows identifed by the category variable"
  (let ((category (translate-column df category))
	(observation (translate-column df observation))
	(h (make-hash-table :test #'equal)))
    (dotimes (i (nrows df))
      (push i (gethash (xref df i category) h (list))))
   (loop for k being the hash-keys in h using (hash-value rows)
	 collect (cons k
		       (loop for row in rows
			     collect (xref df row observation) into g
			     finally (return (funcall function g)))) into groups
	 finally (return groups))) )



(defun reduce-column (df function column )
  "reduce a column of a df with function yielding a scalar"
  (assert (and (>= column 0) (< column (ncols df))) )
  (loop with result = (xref df 0 column)
	for i from 1 below (nrows df) do
	  (setf result (funcall function result (xref df i column)))
	finally (return result)))

(defun map-column (df function column)
  (assert (and (>= column 0) (< column (ncols df))) )
  (loop with result = (make-sequence 'vector (nrows df) )
	for i from 1 below (nrows df) do
	  (setf (xref result i ) (funcall function (xref df i column)))
	finally (return result)))

(defun column-type-classifier (df column)
  "column type classifier, finds the smallest subtype that can
  accomodate the elements of list, in the ordering fixnum < integer <
  float < complex < t.  Rational, float (any kind) are classified as
  double-float, and complex numbers as (complex double-float).  Meant
  to be used by dataframe constructors so we can guess at column data types. The presence of a non numeric in a column implies the column is represented as a non numeric, as reduces and numeric maps will fail."

  (case (reduce #'max (map-column df #' 
				  (lambda (x)
				    (typecase x
				      (fixnum 0)
				      (integer 1)
				      ((or rational double-float) 2)
				      (complex 3)
				      (simple-array 4)
				      (keyword 5)
				      (t 6))) column))
    (0 'fixnum)
    (1 'integer)
    (2 'double-float)
    (3 '(complex double-float))
    (4 'string) ;; for the moment a categorical variable
    (5 'keyword) ;; and likewise, regarded as a categorical varial
    (6 t))) ;; nil will end up here.

(defun infer-dataframe-types (df)
  "infer the numeric types for each column in the dataframe. note that all non numerc types are lumped into T, so further discrimination will be required."
  (let ((column-types (loop for col  below (ncols df)
			    collect (column-type-classifier df col))))
    column-types))


(defmethod dfsummary (df dataframe-array))
