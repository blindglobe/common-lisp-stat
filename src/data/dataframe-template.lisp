
;;;;; TEMPLATE for building a dataframe from other structures


;;; -*- mode: lisp -*-

;;; Time-stamp: 
;;; Creation:   
;;; File:       dataframe-#####.lisp
;;; Authors:    
;;; Copyright:  (c)20##--, . MIT license.  See README.mit in
;;;             the top-level directory
;;; Purpose:    temporary template.

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

(in-package :cls-dataframe)

;;; DATAFRAME-#####
;;;

(defclass dataframe-#### (dataframe-like)
  ((store :initform nil
	  :initarg :storage
	  :type (array * *)
	  :accessor dataset
	  :documentation "Data storage: typed as array.")
   (store-class :initform 'array
		:accessor store
		:documentation "Storage class is ARRAY."))
  (:documentation "example implementation of dataframe-like using
  storage based on lisp arrays.  An obvious alternative could be a
  dataframe-matrix-like which uses the lisp-matrix classes."))


;;; Add a method to build
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


;;; Add methods to describe the storage
(defmethod nrows ((df dataframe-array))
  "specializes on inheritance from matrix-like in lisp-matrix."
  (array-dimension (dataset df) 0))

(defmethod ncols ((df dataframe-array))
  "specializes on inheritance from matrix-like in lisp-matrix."
  (array-dimension (dataset df) 1))


;;;; Add a way to access and set a value

(defmethod xref ((df dataframe-array) &rest subscripts)
  "Returns a scalar from dataframe, in the same vein as aref, mref, vref, etc.
idx1/2 is row/col or case/var."
  (assert (>= 2 (length subscripts)))

  )

(defmethod (setf xref) (value (df dataframe-array) &rest subscripts)
  "set value for df-ar."
  ;; (check-type val (elt (var-type df) index2))
  (setf (aref (dataset df) (elt subscripts 0) (elt subscripts 1)) value))

;;; 



;;; FIXME: THIS COMPLETELY OBSOLETE?
;;; from dataframe-xarray experiment

#| 
 (defmethod xref ((obj dataframe-like)  &rest subscripts)
  "For data-frame-like, dispatch on storage object."
  (xref (dataset obj) subscripts))

 (defmethod (setf xref) (value (obj dataframe-like) &rest subscripts)
  (setf (xref (dataset obj) subscripts) value))
  
 (defmethod xref ((obj matrix-like) &rest indices))
  
 (defmethod xtype ((obj dataframe-like))
  "Unlike the standard xtype, here we need to return a vector of the
  types.  Vectors can have single types, but arrays have single type.
  Dataframe-like have multiple types, variable-like single type,
  case-like has multiple types, and matrix-like has single type.")

 (defmethod xdims ((obj dataframe-like))
  (dataframe-dimensions obj))

 ;; use default methods at this point, except for potentially weird DFs
 (defmethod xdims* ())

 (defmethod xdim ((obj dataframe-like) index)
  (dataframe-dimension index))


 (defmethod xrank ())

 (defmethod slice ())
  
 (defmethod take ())

 (defmethod carray ())

 (defmacro with-dataframe (env &rest progn) 
  "Compute using variable names with with.data.frame type semantics.")

 (defmacro with-data (body)
  "Stream-handling, maintaining I/O through object typing.")

|#

