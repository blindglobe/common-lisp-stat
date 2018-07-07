;;; -*- mode: lisp -*-

;;; Time-stamp: <2018-06-10 13:37:02 tony>
;;; Creation:   <2009-03-12 17:14:56 tony>
;;; File:       dataframe-array.lisp
;;; Authors:    AJ Rossini <blindglobe@gmail.com>
;;;             David Hodge <davidbhodge@gmail.com>
;;; Copyright:  (c)2009--, AJ Rossini. MIT license.  See README.mit in
;;;             the top-level directory
;;; Copyright:  (c)2012--, David Hodge. MIT license.
;;; Purpose:    realized dataframe class using lisp-arrays as storage.

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

(in-package :cls-dataframe)

;;; DATAFRAME-ARRAY

(defclass dataframe-array (dataframe-like)
  ((store :initform nil
	  :initarg :storage
	  :type (array * *)
	  :accessor dataset
	  :documentation "Data storage: typed as array.")
   (store-class :initform 'array
		:accessor store
		:documentation "Storage class is ARRAY."))
  (:documentation
   "example implementation of dataframe-like using storage based on
    lisp arrays.  An obvious alternative could be a
    dataframe-matrix-like which uses the lisp-matrix classes."))

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

(defmethod nvars ((df dataframe-array))
  "specializes on inheritance from matrix-like in lisp-matrix."
  (array-dimension (dataset df) 1))
;;    (ncols df))

(defmethod ncases ((df dataframe-array))
  "specializes on inheritance from matrix-like in lisp-matrix."
  (array-dimension (dataset df) 0))
;;    (nrows df))


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

