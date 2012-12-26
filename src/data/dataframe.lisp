;;; -*- mode: lisp -*-

;;; Time-stamp: <2010-03-03 16:23:01 tony>
;;; Creation:   <2008-03-12 17:18:42 blindglobe@gmail.com>
;;; File:       dataframe.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2008--2010, AJ Rossini.  See LICENSE.mit in
;;;             toplevel directory for conditions.  

;;; Purpose:    Data packaging and access for Common Lisp Statistics,
;;;             using a DATAFRAME-LIKE virtual structure.
;;;             This redoes dataframe structures in adfcolumn CLOS based
;;;             framework.   Currently contains the virtual class
;;;             DATAFRAME-LIKE as well as the actual classes
;;;             DATAFRAME-ARRAY and DATAFRAME-MATRIXLIKE.

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

(in-package :cls-dataframe)

;;; No real basis for work, there is a bit of new-ness and R-ness to
;;; this work. In particular, the notion of relation is key and
;;; integral to the analysis.  Tables are related and matched vectors,
;;; for example.  "column" vectors are related observations (by
;;; measure/recording) while "row" vectors are related readings (by
;;; case, independence).  This does mean that we are placing
;;; statistical semantics into the computational data object -- and
;;; that it is a violation of use to consider rows which are not at
;;; the least conditionally independent (though the conditioning
;;; should be outside the data set, not internally specified).

;;; So we want a verb-driven API for data collection construction.  We
;;; should encode independence or lack of, as a computable status.

;;; Need to figure out statistically-typed vectors.  We then map a
;;; series of typed vectors over to tables where columns are equal
;;; typed.  In a sense, this is a relation (1-1) of equal-typed
;;; arrays.  For the most part, this ends up making the R data.frame
;;; into a relational building block (considering 1-1 mappings using
;;; row ID as a relation).  Is this a worthwhile generalization or
;;; communicable analogy?

;;; verbs vs semantics for DF construction -- consider the possibily
;;; of how adverbs and verbs relate, where to put which semantically
;;; to allow for general approach.

;;; Need to consider modification APIs
;;; actions are:
;;; - import 
;;; - get/set row names (case names)
;;; -         column names (variable names)
;;; -         dataset values
;;; -         annotation/metadata
;;; -    make sure that we do coherency checking in the exported
;;; -    functions.
;;; -    ... 
;;; - reshapeData/reformat/reshapr a reformed version of the dataset (no
;;;           additional input). 
;;; -         either overwriting or not, i.e. with or without copy.
;;; - check consistency of resulting data with metadata and related
;;;           data information.

;;; Is there any need for an N-way dataframe (N>2) ?   Am currently
;;; assuming not, that this is specializing only the
;;; "independent cases"-by-variables format and that there would be
;;; other tools for other structures.

;;; Misc Functions (to move into a lisp data manipulation support package)

;; the next two should be merged into a general replicator or iterator
;; pattern.


(defun repeat-seq (n item)
  "dbh: append/recursion changed to loop.   
  (repeat-seq 3 \"d\") ; => (\"d\" \"d\" \"d\")
  (repeat-seq 3 'd) ; => ('d 'd 'd)
  (repeat-seq 3 (list 1 2))"
  (if (>= n 1)
      (loop for i below n collect item  )))


(defun string->number (str)
  "Convert a string <str> representing a number to a number. A second
value is returned indicating the success of the conversion.  Examples:
   (string->number \"123\") ; =>  123 t
   (string->number \"1.23\") ; =>  1.23 t"
   (let ((*read-eval* nil))
     (let ((num (read-from-string str)))
       (values num (numberp num)))))

;;; abstract dataframe class

(defclass dataframe-like (matrix-like)
  ((case-labels :initform nil
		:initarg :case-labels
		:type list
		:accessor case-labels
		:documentation "labels used for describing cases (doc
		  metadata), possibly used for merging.")
   (var-labels :initform nil
	       :initarg :var-labels
	       :type list
	       :accessor var-labels
	       :documentation "Variable names. List order matches
	         order in STORE.")
   (var-types :initform nil
	      :initarg :var-types
	      :type list
	      :accessor var-types
	      :documentation "List of symbols representing classes
	        which describe the range of contents for a particular
	        variable. Symbols must be valid types for check-type.
	        List order matches order in STORE.")
   (doc-string :initform nil
	       :initarg :doc
	       :accessor doc-string
	       :documentation "additional information, potentially
  	         uncomputable, possibly metadata, about dataframe-like
	         instance.")
   (variables :initarg :variables
	      :initform (list)
	      :accessor variables
	      :documentation " a plist of the meta data for each variable. "))
  (:documentation "Abstract class for standard statistical analysis
     dataset for (possible conditionally, externally) independent
     data.  Rows are considered to be independent, matching
     observations.  Columns are considered to be type-consistent,
     match a variable with distribution.  inherits from lisp-matrix
     base MATrRixsr-LIKE class.  MATRIX-LIKE (from lisp-matrix) is
     basically a rectangular table without storage.  We emulate that,
     and add storage, row/column labels, and within-column-typing.

     DATAFRAME-LIKE is the basic cases by variables framework.  Need
     to embed this within other structures which allow for generalized
     relations.  Goal is to ensure that relations imply and drive the
     potential for statistical relativeness such as correlation,
     interference, and similar concepts.

     STORE is the storage component.  We ignore this in the
     DATAFRAME-LIKE class, as it is the primary differentiator,
     spec'ing the structure used for storing the actual data.  We
     create methods which depend on STORE for access.  The onlyr
     critical component is that STORE be a class which is
     xarray-compliant.  Examples of such mixins are DATAFRAME-ARRAY
     and DATAFRAME-MATRIXLIKE.  The rest of this structure is
     metadata."))

;;; Specializing on superclasses...
;;;
;;; Access and Extraction: implementations needed for any storage
;;; type.  But here, just to point out that we've got a specializing
;;; virtual subclass (DATAFRAME-LIKE specializing MATRIX-LIKE).

;;generics


(defun translate-column (df column &optional ( nil-when-error nil))
  "for production use, we would wrap this in a handler to enable entry of the correct column id.
nil on error is for non interactive use"
  (typecase column
    (keyword
     (let ((col (position column (varlabels df))))
       (if col
	   col
	   (if nil-when-error nil  (error "Column name misspelt: try again ~a~%" column)))))
    (number column)
    (t (error "Invalid argument passed to translate-column ~a~%" column))))

(defmethod reduce-column (df function column )
  "reduce a column of a df with function yielding a scalar"
  (assert (and (>= column 0) (< column (nvars df))) )
  (loop with result = (xref df 0 column)
	for i from 1 below (ncases df) do
	  (setf result (funcall function result (xref df i column)))
	finally (return result)))

(defmethod map-column (df function column)
  (assert (and (>= column 0) (< column (ncols df))) )
  (loop with result = (make-sequence 'vector (ncases df) )
	for i from 1 below (ncases df) do
	  (setf (xref result i ) (funcall function (xref df i column)))
	finally (return result)))

;;; FUNCTIONS WHICH DISPATCH ON INTERNAL METHODS OR ARGS
;;;
;;; Q: change the following to generic functions and dispatch on 
;;; array, matrix, and dataframe?  Others?
(defun make-labels (initstr num)
  "generate a list of strings which can be used as labels, i.e. something like 
  (make-labels \"a\" 3) => '(\"a1\" \"a2\" \"a3\")."
  (check-type initstr string)
  (mapcar #'(lambda (x y)  (concatenate 'string x y))
	  (repeat-seq num initstr)
	  (mapcar #'(lambda (x) (format nil "~A" x)) (alexandria:iota num))))

(defun vartypes (df)
  (var-types df))

(defun set-vartypes (df vt)
  (assert (= (length vt) (ncols df)))
  (setf (var-types df) vt))
(defsetf vartypes set-vartypes)

(defun make-comparison-function (df function field value)
  `#'(lambda (row) (funcall ,function (xref df row ,field) ,value)))

(defun row-order-as-list (ary)
  "Pull out data in row order into a list. naughty, should use xref"
  (let ((result (list))
	(nrows (nth 0 (array-dimensions ary)))
	(ncols (nth 1 (array-dimensions ary))))
    (dotimes (i ncols)
      (dotimes (j nrows)
	(append result (aref ary i j))))))

(defun col-order-as-list (ary)
  "Pull out data in row order into a list. naughty should use xref"
  (let ((result (list))
	(nrows (nth 0 (array-dimensions ary)))
	(ncols (nth 1 (array-dimensions ary))))
    (dotimes (i nrows)
      (dotimes (j ncols)
	(append result (aref ary i j))))))

(defun transpose-array (ary)
  "map NxM to MxN."
  (make-array (reverse (array-dimensions ary))
      :initial-contents (col-order-as-list ary)))

;;; THE FOLLOWING 2 dual-sets done to provide error checking
;;; possibilities on top of the generic function structure.  Not
;;; intended as make-work!

(defun varlabels (df)
  "Variable-name handling for DATAFRAME-LIKE.  Needs error checking."
  (var-labels df))

(defun set-varlabels (df vl)
  "Variable-name handling for DATAFRAME-LIKE.  Needs error checking."
  (if (= (length (var-labels df))
	 (length vl))
      (setf (var-labels df) vl)
      (error "set varlables: wrong size.")))

(defsetf varlabels set-varlabels)

;;; Case-name handling for Tables.  Needs error checking.
(defun caselabels (df)
  "Case-name handling for DATAFRAME-LIKE.  Needs error checking."
  (case-labels df))

(defun set-caselabels (df cl)
  "Case-name handling for DATAFRAME-LIKE.  Needs error checking."
  (if (= (length (case-labels df))
	 (length cl))
      (setf (case-labels df) cl)
      (error "set caselabels: wrong size.")))

(defsetf caselabels set-caselabels)


;;;;;;;;;;;; IMPLEMENTATIONS, with appropriate methods.
;; See also:
;; (documentation 'dataframe-like  'type)



;;; Do we establish methods for dataframe-like, which specialize to
;;; particular instances of storage?



;;; Vector-like generalizations: we consider observation-like and
;;; variable-like to be abstract classes which provide row and column
;;; access to dataframe structures.  These will be specialized, in
;;; that rows correspond to an observation (or case?) which are
;;; multitype, while columns correspond to a variable, which must be
;;; singularly typed.

(defclass observation-like (dataframe-like)
  ()
  (:documentation "dataframe-like with only 1 row, is an observation-like."))

(defclass variable-like (dataframe-like)
  ()
  (:documentation "dataframe-like with only 1 column is a variable-like."))

;;; Need to implement views, i.e. dataframe-view-like,
;;; observation-view-like, variable-view-like.  



;;; Need to consider read-only variants, leveraging the xref
;;; strategy.



;;; Dataframe <-> Listoflist support
;; the following will be handy to help out folks adjust.  It should
;; provide a means to write code faster and better.
;; 
;; leverages listoflist support in our version of xarray
(defun listoflist->dataframe (lol) ; &key (type :row-major))
  "Create a cases-by-variables data frame consisting of numeric data,
from a ROW-MAJOR list-of-lists representation.  A COLUMN-MAJOR
representation should be handled using the transpose-listoflists
function."
  (check-type lol list) ; imperfect -- must verify all list elements are also lists.
  (if (listoflist:sublists-of-same-size-p lol)
      (make-dataframe  (listoflist:listoflist->array lol))
      (error "make-data-set-from-lists: no combining different length lists"))
  (error "make-data-set-from-lists: proposed name exists"))


;;;;;;;; from dataframe-xarray experiment

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




