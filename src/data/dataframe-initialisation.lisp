;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-

;; this contains the dataframe functions used in determining the metadata for columns and types.
;; there are also some print related functions - equivalents of head and tail

(in-package #:cls-dataframe)

;;;;;;;;;; DATE MANAGEMENT (FIXME: move elsewhere)

(defparameter *CLS-DATE-FORMAT* :UK
  "should be one of :UK (d/m/y) :US (m/d/y) or maybe others as
  required. Giving a hint to the parsing routine.SUffix with a
  -TIME (is :US-TIME for MDY hhmmss. Or supply the ANTIK specification
  as a list '(2 1 0 3 4 5) ")

(defparameter *CLS-DATE-TEST-LIMIT* 5
  "the number of rows to check when deciding if the column is a date column or not.")

;;; Why is this "ANTIK"?  We naturally will be including the antik
;;; package within this framework. At this point we are heavy, heavy,
;;; heavy, and done on purpose.  Need to be heavy before we are
;;; light-weight.
(defun antik-date-format-helper (date)
  "provide decoding for shorthand notation in *CLS-DATE-FORMAT* or
allow the full spec to be supplied."
  (cond
    ((equal date :UK) '(2 1 0))
    ((equal date :UK-TIME) '(2 1 0 3 4 5))
    ((equal date :US) '(2 0 1))
    ((equal date :US-TIME) '(2 0 1 3 4 5))
    (t date)))

(defun date-conversion-fu (df)
  "for any string column in the dataframe, try to parse the first n
entries as a date according to the global format. If we can do that
successfully for at least one entry, the convert the column,
converting failures to nil"
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
			     and item in (vartypes df)
			     when (equal 'string item)
			       collect i)))
      
      (when maybe-dates
	(dolist (index maybe-dates)
	  (when (date-column-detected index)
	    (convert-date-column  index)
	    ;; FIXME: A nice accessor required!
	    (setf (getf  (nth index (variables df)) :type ) 'date)))))))


;;;;;;;; PRINTING (FIXME: move elsewhere)

;; FIXME: in order to pretty print, we seem to be trying to get the
;; different column types aligned.  I suspect it would be better just
;; to ignore pretty-printing right now.
;; 
;; Current thinking, according to the code: LISP types map N:1 to a
;; PRINT type.  However, this mapping could be built by having a
;; default print-width for standard LISP types, CLS types, and then
;; enforcing that data variable types would be required to also
;; suggest a means to print.  i.e. having a DEF-STAT-VAR macro, which
;; built the generic dispatch function.  
;;
;; Note that for future plans of having statistical theory encoded
;; into the computation, we could easily have a DEF-STAT-VAR which is
;; a kinetic series or a network or similar structure.
;; 
;; What is currently killing us is the confusion related to typing,
;; and the different domains (LISP, STAT, PRINT) that may have N:M
;; mappings.  I think this is solvable.




;;; Do we establish methods for dataframe-like, which specialize to
;;; particular instances of storage?

(defparameter dataframe-print-formats
  '((FIXNUM . "~7D")
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

(defparameter new-dataframe-print-formats
  '((FIXNUM . "D")
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

(defun extract-row (df row)
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
	(format stream format-control (extract-row object i))))))

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



(defun classify-print-type (variable-type)
  "Look at the type of each column, assuming that types are homogenous
of course, and assign a descriptive type - number, date, string etc,
to enable for nice tabular printing.

VARIABLE-TYPE should be a lisp type.  Which means that when we add a
new lisp type for holding statistical/experimental data, we need to
add dispatch here.  Perhaps this would be better as a generic
function, so that types do the right thing.

FIXME: These types are generally supposed to be LISP types, not
STATISTICAL types.  Though we could imagine having a print-type
category.

FIXME: should we have a PRINT type classification? i.e. numbers,
strings, factors, timeseries/longitudinal/kinetics category?"
  (labels ((integer-variable (variable)
	     (member variable '(FIXNUM INTEGER RATIONAL)))
	   (float-variable (variable)
	     (member variable '(NUMBER
				FLOAT
				LONG-FLOAT SHORT-FLOAT
				SINGLE-FLOAT DOUBLE-FLOAT)))
	   (string-variable (variable)
	     (equal variable 'STRING))
	   (keyword-variable (variable)
	     (equal variable 'KEYWORD))
	   (date-variable (variable)
	     (member variable '(DATE ANTIK:TIMEPOINT))))
    
    (cond
      ((integer-variable variable-type) :INTEGER)
      ((float-variable variable-type) :FLOAT)
      ((keyword-variable variable-type) :KEYWORD)
      ((string-variable variable-type) :STRING)
      ((date-variable variable-type) :DATE)
      (t (error "classify-print-type, unrecognized type ~a~%" variable-type)))))
  
(defun determine-print-width (df column type) 
  "build the format string by checking widths of each column in the
dataframe DF based on COLUMN TYPE.

FIXME: MAKE-VARIABLE-METADATA is busted with respect to this
computation, so simply using 10 at this point.  See commentary above
for the challenge."
  (labels ((numeric-width (the-col)
	     (1+ (reduce #'max
			 (mapcar #'(lambda (x)
				     (ceiling (log (abs x) 10)))
				 (dfcolumn df the-col)))))
	   (string-width (the-col)
	     (1+ (reduce #'max
			 (mapcar #'length (dfcolumn df the-col)))))
	   (keyword-width (the-col)
	     (1+ (reduce #'max
			 (mapcar #'(lambda (x)
				     (length (symbol-name x)))
				 (dfcolumn df the-col)))))
	   ;; FIXME: what is the print width of a timepoint?
	   (date-width (the-col) 
	     (declare (ignore the-col))
	     12))
    
    (case (classify-print-type type)
      ((:INTEGER :FLOAT) (numeric-width column))
      (:KEYWORD          (keyword-width column))
      (:STRING           (string-width column))
      (:DATE             (date-width column))
      (t (error "determine-print-width, unrecognized type ~%" )))))
