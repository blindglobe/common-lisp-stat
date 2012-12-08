;;; -*- mode: lisp -*-

;;; Time-stamp: <2012-10-12 16:40:46 tony>
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
(defun gen-seq (n &optional (start 1))
  "Generates an integer sequence of length N starting at START. Used
 for indexing."
  (if (>= n start)
      (append (gen-seq (- n 1) start) (list n))))

(defun repeat-seq (n item)
  "dbh: append/recursion changed to loop.   
  (repeat-seq 3 \"d\") ; => (\"d\" \"d\" \"d\")
  (repeat-seq 3 'd) ; => ('d 'd 'd)
  (repeat-seq 3 (list 1 2))"
  (if (>= n 1)
      (loop for i upto n collect item  )))

(defun strsym->indexnum (df strsym)
  "Returns a number indicating the DF column labelled by STRSYM.
Probably should be generic/methods dispatching on DATAFRAME-LIKE type."
  (position strsym (varlabels df)))

(defun string->number (str)
  "Convert a string <str> representing a number to a number. A second
value is returned indicating the success of the conversion.  Examples:
   (string->number \"123\") ; =>  123 t
   (string->number \"1.23\") ; =>  1.23 t"
   (let ((*read-eval* nil))
     (let ((num (read-from-string str)))
       (values num (numberp num)))))

#|
  (equal 'testme 'testme)
  (defparameter *test-pos* 'testme)
  (position *test-pos* (list 'a 'b 'testme 'c))
  (position #'(lambda (x) (equal x "testme")) (list "a" "b" "testme" "c"))
  (position #'(lambda (x) (equal x 1)) (list 2 1 3 4))
|#

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
				      ((or symbol  keyword) 5)
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


;;; abstract dataframe class

(defclass dataframe-like (matrix-like)
  (
#|
   (store :initform nil
	  :accessor dataset
	  :documentation "not useful in the -like virtual class case,
 	    contains actual data")
   (store-class :initform nil
		:accessor store-class
		:documentation "Lisp class used for the dataframe storage.")
|#
   (case-labels :initform nil
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
   (print-widths :initform nil
		 :initarg :print-widths
		 :accessor print-widths
		 :documentation " the print widths of each of the variables. "))
  (:documentation "Abstract class for standard statistical analysis
     dataset for (possible conditionally, externally) independent
     data.  Rows are considered to be independent, matching
     observations.  Columns are considered to be type-consistent,
     match a variable with distribution.  inherits from lisp-matrix
     base MATRIX-LIKE class.  MATRIX-LIKE (from lisp-matrix) is
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
     create methods which depend on STORE for access.  The only
     critical component is that STORE be a class which is
     xarray-compliant.  Examples of such mixins are DATAFRAME-ARRAY
     and DATAFRAME-MATRIXLIKE.  The rest of this structure is
     metadata."))

;;; Specializing on superclasses...
;;;
;;; Access and Extraction: implementations needed for any storage
;;; type.  But here, just to point out that we've got a specializing
;;; virtual subclass (DATAFRAME-LIKE specializing MATRIX-LIKE).

(defmethod dfcolumn (( df dataframe-array) variable)
  "return a column as a list. a quick hack until we decide what the array manipulations should be"
  (loop for row below (nrows df) collect (xref df row variable)))

(defgeneric nvars (df)
  (:documentation "number of variables represented in storage type.")
  (:method ((df simple-array))
    (array-dimension df 1))
  (:method ((df dataframe-like))
    (xdim (store df) 1))
  (:method ((df array))
    (xdim df 1)))


#|
 (defun nvars-store (store)
  "Return number of variables (columns) in dataframe storage.  Doesn't
test that that list is a valid listoflist dataframe structure."
  (etypecase store
    (array (array-dimension store 1))
    (matrix-like (ncols store))
    (list (length (elt store 0)))))
|#

(defgeneric ncases (df)
  (:documentation "number of cases (indep, or indep within context,
  observantions) within DF storage form.")
  (:method ((df simple-array))
    (array-dimension df 0))
  (:method ((df matrix-like))
    (nrows df))
  (:method ((df list))
    (nrows df)) ;; probably should do a valid LISTOFLIST structure test but this would be inefficient
  (:method ((df array))
    (nrows df)))

(defun translate-column (df column &optional ( nil-on-error nil))
  "for production use, we would wrap this in a handler to enable entry of the correct column id.
nil on error is for non interactive use"
  (typecase column
    (keyword
     (let ((col (position column (varlabels df))))
       (if col
	   col
	   (if nil-on-error nil  (error "Column name misspelt: try again ~a~%" column)))))
    (number column)
    (t (error "Invalid argument passed to translate-column ~a~%" column))))



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
				      ((or symbol  keyword) 5)
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
  (let ((column-types (loop for col  below (nvars df)
			    collect (column-type-classifier df col))))
    column-types))

;; Testing consistency/coherency.

(defgeneric consistent-dataframe-p (df)
  (:documentation "methods to check for consistency.  Mostly of
  internal interest, since ideally we'd have to use standard
  constructs to ensure that we do not get the dataframe structure
  misaligned.")
  (:method (object) "General objects are not consistent dataframes!" nil)
  (:method ((df dataframe-like)) 
    "At minimum, must dispatch on virtual-class."
    (and
     ;; ensure dimensionality
     (= (length (var-labels df)) (ncols df)) ; array-dimensions (dataset df))
     (= (length (case-labels df)) (nrows df))
     (= (length (var-types df) (ncols df)))
     ;; ensure claimed STORE-CLASS
     ;; when dims are sane, ensure variable-typing is consistent
     (progn
       (dotimes (i (nrows df))
	 (dotimes (j (ncols df))
	   ;; xref bombs if not a df-like subclass so we don't worry
	   ;; about specialization.  Need to ensure xref throws a
	   ;; condition we can recover from.
	   ;; (check-type  (aref dt i j) (elt lot j)))))) ???
	   (typep (xref df i j) (nth j (var-types df))))) 
       t))))

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

(defun check-dataframe-params (data vartypes varlabels caselabels doc)
  "This will throw an exception (FIXME: Need to put together a CLS exception system, this could be part of it)"
  ;; type checking
  (check-type data (or matrix-like array list))
  (check-type caselabels sequence)
  (check-type varlabels sequence)
  (check-type vartypes sequence)
  (check-type doc string)
  ;; dimension checking
  (if vartypes   (assert (= (nvars data) (length vartypes))))
  (if varlabels  (assert (= (nvars data) (length varlabels))))
  (if caselabels (assert (= (ncases data) (length varlabels)))))

(defun vartypes (df)
  (var-types df))

(defun set-vartypes (df vt)
  (assert (= (length vt) (ncols df)))
  (setf (var-types df) vt))
(defsetf vartypes set-vartypes)


(defmacro build-dataframe (type) 
  `(progn
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
      (make-instance ,type
		     :storage data
		     :nrows (length newcaselabels)
		     :ncols (length newvarlabels)
		     :case-labels newcaselabels
		     :var-labels newvarlabels
		     :var-types newvartypes))))

;;  (macroexpand '(build-dataframe 'test)))

(defgeneric make-dataframe2 (data &key vartypes varlabels caselabels doc)
  (:documentation "testing generic dispatch.  Data should be in table format desired for use."))

(defun make-dataframe (newdata
		       &key  (vartypes nil)
		       (caselabels nil) (varlabels nil)
		       (doc "no docs"))
  "Helper function to use instead of make-instance to assure
construction of proper DF-array."
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

(defun make-comparison-function (df function field value)
  `#'(lambda (row) (funcall ,function (xref df row ,field) ,value)))

(defun dfquery (df ))
(defmethod dfextract (df  &key ( head 5) (tail 5) )
  "just for the moment "
  (let* ((rows (ncases df))
	 (head-rows (loop for row below  (min head rows) collect (dfrow df row)))
	 (tail-rows (loop for row from (max 0 (- rows tail)) below rows collect (dfrow df row))))
    ; this is only temprorary. need to get dataframe-list sorted out
    (make-dataframe (listoflist:listoflist->array  (append head-rows tail-rows))
		    :vartypes (vartypes df)
		    :varlabels (varlabels df))))

#| 
 (make-dataframe #2A((1.2d0 1.3d0) (2.0d0 4.0d0)))
 (make-dataframe #2A(('a 1) ('b 2)))
 (xref (make-dataframe #2A(('a 1) ('b 2))) 0 1)
 (xref (make-dataframe #2A(('a 1) ('b 2))) 1 0)
 (make-dataframe 4) ; ERROR, should we allow?
 (make-dataframe #2A((4)))
 (make-dataframe (rand 10 5)) ;; ERROR, but should work!
|#

(defparameter *CLS-DATE-FORMAT* :UK
  "should be one of :UK (d/m/y) :US (m/d/y) or maybe others as required. Giving a hint to the parsing routine.SUffix with a -TIME (is :US-TIME for MDY hhmmss. Or supply the ANTIK specification as a list '(2 1 0 3 4 5)  ")

(defparameter *CLS-DATE-TEST-LIMIT* 5
  "the number of rows to check when deciding if the column is a date column or not.")
(defun antik-date-format-helper (date)
  "provide decoding for shorthand notation in *CLS-DATE-FORMAT*  or allow the full spec to be supplied "
  (cond
    ((equal date :UK) '(2 1 0))
    ((equal date :UK-TIME) '(2 1 0 3 4 5))
    ((equal date :US) '(2 0 1))
    ((equal date :US-TIME) '(2 0 1 3 4 5))
    (t date)))

(defun date-conversion-fu (df)
  "for any string column in the dataframe, try to parse the first n entries as a date according to the global format. If we can do that successfully for at least one entry, the convert the column, converting failures to nil"
  (labels ((read-timepoint (row column)
	   "read a timepoint. if there is an error return nil"
	   (handler-case
			 (antik:read-timepoint (xref df row column)
					       (antik-date-format-helper *CLS-DATE-FORMAT*))
	     (error () nil)))
	 
	   (date-column-detected (index)
	     "guess if the column has dates or not"
	   (loop
	     for i below *CLS-DATE-TEST-LIMIT*
	     collect  (read-timepoint i index) into result
	     finally (return (some #'identity result))))
	 
	 (convert-date-column (column )
	   (loop for i below (nrows df) do
	     (setf (xref df i column) (read-timepoint i column)))))
    
    (let ((maybe-dates
	    (loop for i upto (length (vartypes df))
			     and item in (var-types df)
			     when (equal 'string item)
			       collect i)))
      
      (when maybe-dates
	(dolist (index maybe-dates)
	  (when (date-column-detected index)
	 	    (convert-date-column  index)
	    (setf (nth index (vartypes df) ) 'date)))))))

(defun classify-print-type (df column)
  (labels ((integer-variable (variable)
	     (member variable '(FIXNUM INTEGER RATIONAL)))
	   (float-variable (variable)
	     (member variable '(NUMBER FLOAT LONG-FLOAT SHORT-FLOAT SINGLE-FLOAT DOUBLE-FLOAT)))
	   (string-variable (variable)
	     (equal variable 'STRING))
	   (keyword-variable (variable)
	     (equal variable 'KEYWORD))
	   (date-variable (variable)
	     (member variable '(DATE ANTIK:TIMEPOINT))))

    (let ((variable-type (elt (var-types df) column)))
      
      (cond
	( (integer-variable variable-type) :INTEGER)
	((float-variable variable-type) :FLOAT)
	((keyword-variable variable-type) :KEYWORD)
	((string-variable variable-type) :STRING)
	((date-variable variable-type) :DATE)
	(t (error "classify-print-type, unrecognized type ~a~%" variable-type))))))
  
(defun determine-print-width (df  column)
  "build the format string by checking widths of each column. to be rewritten as a table "
  (labels ((numeric-width (the-col)
	     (reduce #'max (mapcar #'(lambda (x) (ceiling (log (abs x) 10))) (dfcolumn df the-col)) ))
	   (string-width (the-col)
	     (reduce  #'max (mapcar #'length (dfcolumn df the-col))))
	   (keyword-width (the-col)
	     (reduce #'max (mapcar #'(lambda (x) (length (symbol-name x))) (dfcolumn df the-col))))
	   ;; FIXME - what is the print width of a timepoint?
	   (date-width (the-col) 12))
    
    (case (classify-print-type df column)
      ((:INTEGER :FLOAT) (numeric-width column))
      (:KEYWORD          (keyword-width column))
      (:STRING           (string-width column))
      (:DATE             (date-width column))
      (t (error "determine-print-width, unrecognized type ~%" )))))


(defun make-variable-metadata (df)
  " this is a first attempt at consolidating the metadata for a variable. ultimately i expect that the other lists will disappear when I figureo ut a convenient initiaslization method"
  (format t "vars = ~A~%" (nvars df))
  (loop for index below (nvars df) 
	collect
	(list
	 :name (elt (var-labels df) index) 
	 :type (elt (var-types df) index)
	 :print-type (classify-print-type df index)
	 :print-width (determine-print-width df index)) into variable-plist
	finally (setf (slot-value df 'variables) variable-plist)))

(defmethod initialize-instance :after ((df dataframe-like) &key)
  "Do post processing for variables  after we initialize the object"
					; obviously I want to nuke var types & var labels at some point
 
  (unless (var-types df) 
    (setf (vartypes df) (infer-dataframe-types df)))
  (when (var-labels df)
    (setf (var-labels df) (mapcar #'(lambda (keyword)
				      (alexandria:make-keyword (string-upcase keyword))) (var-labels df))))
 
  (date-conversion-fu df)
  (make-variable-metadata df)
  (format t "Dataframe created:~% Variables ~{ ~a ~} ~% types  ~{~a,~}~%" (var-labels df) (var-types df)))

;;; FIXME: the following two functions hurt the eyes.  I think that
;;;
;;;      array->listoflist :order '(:column :row)
;;;
;;; would be a better approach.  But don't we already have this in the
;;; listoflist package?  and more critically, we should have this as a
;;; generic, so that it would be more like
;;;
;;;      dataframe->listoflist :order '(:column :row)
;;;

(defun row-order-as-list (ary)
  "Pull out data in row order into a list."
  (let ((result (list))
	(nrows (nth 0 (array-dimensions ary)))
	(ncols (nth 1 (array-dimensions ary))))
    (dotimes (i ncols)
      (dotimes (j nrows)
	(append result (aref ary i j))))))

(defun col-order-as-list (ary)
  "Pull out data in column order into a list."
  (let ((result (list))
	(nrows (nth 0 (array-dimensions ary)))
	(ncols (nth 1 (array-dimensions ary))))
    (dotimes (i nrows)
      (dotimes (j ncols)
	(append result (aref ary i j))))))


;;; FIXME: need to have a by-reference and by-copy variant.
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
      (error "wrong size.")))

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
      (error "wrong size.")))

(defsetf caselabels set-caselabels)


;;;;;;;;;;;; IMPLEMENTATIONS, with appropriate methods.
;; See also:
;; (documentation 'dataframe-like  'type)



;;; Do we establish methods for dataframe-like, which specialize to
;;; particular instances of storage?

(defparameter dataframe-print-formats '((FIXNUM . "~7D")
					(INTEGER . "~7D")
					(STRING . "~7A")
					(SIMPLE-STRING . "~A")
					(CONS . "~a")
					(SYMBOL . "~7a")
					(KEYWORD . "~7a")
					(RATIONAL . "~7a")
					(NUMBER . "~7a")
					(FLOAT . "~7a")
					(DATE . "~9a")
					(LONG-FLOAT . "~7,3G")
					(SHORT-FLOAT . "~7,3G")
					(SINGLE-FLOAT . "~7,3G")
					(DOUBLE-FLOAT . "~7,3G")))
(defparameter new-dataframe-print-formats '((FIXNUM . "D")
					(INTEGER . "D")
					(STRING . "A")
					(SIMPLE-STRING . "A")
					(CONS . "a")
					(SYMBOL . "7a")
					(KEYWORD . "a")
					(RATIONAL . "a")
					(NUMBER . "a")
					(FLOAT . "a")
					(DATE . "a")
					(LONG-FLOAT . "G")
					(SHORT-FLOAT . "~G")
					(SINGLE-FLOAT . "~G")
					    (DOUBLE-FLOAT . "~G")))
(defun build-format-string (df)
  "build the format string by checking widths of each column. to be rewritten as a table "
  
  (loop for  variable in  (variables df) 
    collect (case (getf variable :print-type)
	      ((:INTEGER :KEYWORD :STRING :DATE) (format nil "~~~AA " (getf variable :print-width)))
	      (:FLOAT (format nil "~~~A,3G " (getf variable :print-width)))) into format-control
	
	finally  (return (format nil "~~{~{~a~}~~}~~%" format-control))))

(defun print-directive (df col)
  (cdr  (assoc (elt (vartypes df) col) DATAFRAME-PRINT-FORMATS)))

(defun print-headings (df stream)
  (loop for variable in (variables df)
	nconc (list
	       (1+ (max  (getf variable :print-width)
			 (length (symbol-name  (getf variable :name)))))
	       (getf variable :name) ) into control-string
	  finally  (format stream "~{~VA~}~%" control-string) ))

(defun row (df row)
  (loop for col  below (ncols df)
	collect (xref df row col)))

(defmethod print-object ((object dataframe-like) stream)
  
  (print-unreadable-object (object stream :type t)
    (declare (optimize (debug 3)))
    (format stream " ~d x ~d" (nrows object) (ncols object))
    (terpri stream)
    ;; (format stream "~T ~{~S ~T~}" (var-labels object))
    (let ((format-control (build-format-string object))
	  (case-format (format nil "~~~AA: " (reduce #'max (mapcar #'length (case-labels object))))))
      (dotimes (j (ncols object))	; print labels
	(write-char #\tab stream)
	(write-char #\tab stream)
	(format stream "~T~A~T" (nth j (var-labels object))))
      (dotimes (i (nrows object))	; print obs row
	(terpri stream)
	(format stream case-format (nth i (case-labels object)))
	(format stream format-control (row object i))))))

#|
 (defun print-structure-relational (ds)
  "example of what we want the methods to look like.  Should be sort
of like a graph of spreadsheets if the storage is a relational
structure."
  (dolist (k (relations ds))
    (let ((currentRelationSet (getRelation ds k)))
      (print-as-row (var-labels currentRelationSet))
      (let ((j -1))
	(dolist (i (case-labels currentRelationSet))
	  (print-as-row
	   (append (list i)
		   (xref-obsn (dataset currentRelationSet)
                               (incf j)))))))))

 (defun testecase (s)
   (ecase s
     ((scalar) 1)
     ((asd asdf) 2)))

 (testecase 'scalar)
 (testecase 'asd)
 (testecase 'asdf)
 (testecase 'as)
|#


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




