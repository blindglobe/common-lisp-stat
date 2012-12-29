;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-

;; this contains the dataframe functions used in determining the metadata for columns and types.
;; there are also some print related functions - equivalents of head and tail

(in-package #:cls-dataframe)

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
    (5 'keyword) ;; and likewise, regarded as a categorical variable
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
     (= (length (var-labels df)) (nvars df)) ; array-dimensions (dataset df))
     (= (length (case-labels df)) (ncases df))
     (= (length (var-types df)) (nvars df))
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


(defun make-dataframe (newdata
		       &key  (vartypes nil)
		       (caselabels nil) (varlabels nil)
		       (doc "no docs"))
  "Helper function to use instead of make-instance to assure
construction of proper DF-array."
  (check-type newdata (or matrix-like array list ))
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
	    ;; FIXME: A nice accessor required!
	    (setf (getf  (nth index (variables df)) :type ) 'date)))))))

(defun classify-print-type (df column)
  " look at the type of each column, assuming that types are homogenous of course, and assign a descriptive type - number, date, string etc, to enable  for nice tabular printing"
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
  "build the format string by checking widths of each column. "
  (labels ((numeric-width (the-col)
	     (1+  (reduce #'max (mapcar #'(lambda (x) (ceiling (log (abs x) 10))) (dfcolumn df the-col)) )))
	   (string-width (the-col)
	     (1+ (reduce  #'max (mapcar #'length (dfcolumn df the-col)))))
	   (keyword-width (the-col)
	     (1+ (reduce #'max (mapcar #'(lambda (x) (length (symbol-name x))) (dfcolumn df the-col)))))
	   ;; FIXME - what is the print width of a timepoint?
	   (date-width (the-col) 12))
    
    (case (classify-print-type df column)
      ((:INTEGER :FLOAT) (numeric-width column))
      (:KEYWORD          (keyword-width column))
      (:STRING           (string-width column))
      (:DATE             (date-width column))
      (t (error "determine-print-width, unrecognized type ~%" )))))


  

(defun make-variable-metadata (df)
  " this is a first attempt at consolidating the metadata for a variable. ultimately i expect that the other lists (varlabels etc) will disappear when I figure out a convenient initialization method"
  
  (loop for index below (nvars df) 
	collect
	(list
	 :name (elt (var-labels df) index) 
	 :type (column-type-classifier df index)
	 :print-type (classify-print-type df index)
	 :print-width (determine-print-width df index)) into variable-plist
	finally (setf (slot-value df 'variables) variable-plist)))

(defmethod initialize-instance :after ((df dataframe-like) &key)
  "Do post processing for variables  after we initialize the object"
					
 
  ;; i have yet to figure out a way of getting rid of var-labels.....
  
  (when (var-labels df)
    (setf (var-labels df)
	  (mapcar #'(lambda (keyword)
		      (unless (keywordp x) (alexandria:make-keyword (string-upcase keyword))))
		  (var-labels df))))
 
 ;; only do the metadata stuff when all the information has been supplied 
  (when (and (vartypes df) (varlabels df))
    (date-conversion-fu df)
    (make-variable-metadata df))
  ;; actually I am finding this quite useful, so will leave it here for the moment
  (format t "Dataframe created:~% Variables ~{ ~a ~} ~% types  ~{~a,~}~%" (varlabels df) (vartypes df)))
