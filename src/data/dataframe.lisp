;;; -*- mode: lisp -*-

;;; Time-stamp: <2009-08-18 08:07:01 tony>
;;; Creation:   <2008-03-12 17:18:42 blindglobe@gmail.com>
;;; File:       dataframe.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2008, AJ Rossini.  BSD, LLGPL, or GPLv2, depending
;;;             on how it arrives.  

;;; Purpose:    Data packaging and access for Common Lisp Statistics.
;;;             This redoes dataframe structures in a CLOS based
;;;             framework.   Currently contains the virtual class
;;;             DATAFRAME-LIKE as well as the actual classes
;;;             DATAFRAME-ARRAY and DATAFRAME-MATRIXLIKE

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

(in-package :cls-dataframe)

;;; No real basis for work, there is a bit of new-ness and R-ness to
;;; this work. In particular, the notion of relation is key and
;;; integral to the analysis.  Tables are related and matched vectors,
;;; for example.  "column" vectors are related observations (by
;;; measure/recording) while "row" vectors are related readings (by
;;; case, independence).   This does mean that we are placing
;;; statistical semantics into the computational data object -- and
;;; that it is a violation of use to consider rows which are not at
;;; the least conditionally independent (though the conditioning
;;; should be outside the data set, not internally specified).

;;; So we want a verb-driven API for data collection construction.  We
;;; should encode independence or lack of, as possible.

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


;;; Misc Functions (to move into a lisp data manipulation support package)

;; the next two should be merged into a general replicator pattern.
(defun gen-seq (n &optional (start 1))
  "Generates an integer sequence of length N starting at START. Used
for indexing."
  (if (>= n start)
      (append (gen-seq (- n 1) start) (list n))))

(defun repeat-seq (n item)
  "FIXME: There has to be a better way -- I'm sure of it!   
 (repeat-seq 3 \"d\") ; => (\"d\" \"d\" \"d\")
 (repeat-seq 3 'd) ; => ('d 'd 'd)
 (repeat-seq 3 (list 1 2))"
  (if (>= n 1)
      (append (repeat-seq (1- n)  item) (list item))))


(defun strsym->indexnum (df strsym)
  "Returns a number indicating the DF column labelled by STRSYM.  
Probably should be a method dispatching on DATAFRAME-LIKE type."
  (position strsym (varlabels df)))

(defun string->number (str)
  "Convert a string <str> representing a number to a number. A second value is
returned indicating the success of the conversion.
Examples:
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
	       :documentation "Variable names.")
   (var-types :initform nil
	      :initarg :var-types
	      :type list
	      :accessor var-types
	      :documentation "variable types to ensure fit")
   (doc-string :initform nil
	       :initarg :doc
	       :accessor doc-string
	       :documentation "additional information, potentially
                               uncomputable, possibly metadata, about
                               dataframe-like instance."))
  (:documentation "Abstract class for standard statistical analysis
                   dataset for independent data.  Rows are considered
                   to be independent, matching observations.  Columns
                   are considered to be type-consistent, match a
                   variable with distribution.  inherits from
                   lisp-matrix base MATRIX-LIKE class.
                   MATRIX-LIKE (from lisp-matrix) is basically a
                   rectangular table without storage.  We emulate
                   that, and add storage, row/column labels, and
                   within-column-typing.

                   DATAFRAME-LIKE is the basic cases by variables
                   framework.  Need to embed this within other
                   structures which allow for generalized relations.
                   Goal is to ensure that relations imply and drive
                   the potential for statistical relativeness such as
                   correlation, interference, and similar concepts.

                   STORE is the storage component.  We ignore this in
                   the DATAFRAME-LIKE class, as it is the primary
                   differentiator, spec'ing the structure used for
                   storing the actual data.  We create methods which
                   depend on STORE for access.  See DATAFRAME-ARRAY
                   and DATAFRAME-MATRIXLIKE for examples.  The rest of
                   this is metadata."))

;;; Generics specialized above matrix-like, particularly for
;;; dataframe-like objects.  Need implementation of methods which
;;; depend on storage form.

(defgeneric dataframe-dimensions (df)
  (:documentation "")
  (:method ((df dataframe-like))
    (error "Dispatch on virtual class, Method needed for
  DATAFRAME-DIMENSIONS with class ~A." (find-class df))))

(defgeneric dataframe-dimension (df index)
  (:documentation "")
  (:method ((df dataframe-like) index)
    (elt (dataframe-dimensions df) index)))

(defgeneric dfselect (df &optional cases vars indices)
  (:documentation "access to sub-dataframes. Always returns a dataframe.")
  (:method ((df dataframe-like) &optional cases vars indices)
    (declare (ignorable cases vars))
    (if indices (error "Indicies not used yet"))
    (error "Dispatch on virtual class, Method needed for DFSELECT with
  class ~A." (find-class df))))

;;; Specializing on superclasses...
;;;
;;; Access and Extraction: implementations needed for any storage
;;; type.  But here, just to point out that we've got a specializing
;;; virtual subclass (DATAFRAME-LIKE specializing MATRIX-LIKE).

(defmethod nrows ((df dataframe-like))
  "specializes on inheritance from matrix-like in lisp-matrix."
  (error "Need implementation; can't dispatch on virtual class DATAFRAME-LIKE."))

(defmethod ncols ((df dataframe-like))
  "specializes on inheritance from matrix-like in lisp-matrix."
  (error "Need implementation; can't dispatch on virtual class DATAFRAME-LIKE."))

;; Testing consistency/coherency.

(defgeneric consistent-dataframe-p (df)
  (:documentation "methods to check for consistency.")
  (:method ((df dataframe-like))
    (and
     ;; ensure dimensionality
     (= (length (var-labels df)) (ncols df)) ; array-dimensions (dataset df))
     (= (length (case-labels df)) (nrows df))
     ;; when dims sane, check-type for each variable
     (progn
       (dotimes (i (nrows df))
	 (dotimes (j (ncols df))
	   ;; xref bombs if not a df-like subclass so we don't worry
	   ;; about specialization.
	   ;; (check-type  (aref dt i j) (elt lot j)))))) ???
	   (typep (xref df i j) (nth j (var-types df))))) 
       t))))


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
	  (mapcar #'(lambda (x) (format nil "~A" x)) (gen-seq num))))

(defun ncase-store (store)
  "Return number of cases (rows) in dataframe storage.  Doesn't test
that that list is a valid listoflist dataframe structure."
  (etypecase store
    (array (array-dimension store 0))
    (matrix-like (nrows store))
    (list (length store))))

(defun nvars-store (store)
  "Return number of variables (columns) in dataframe storage.  Doesn't
test that that list is a valid listoflist dataframe structure."
  (etypecase store
    (array (array-dimension store 1))
    (matrix-like (ncols store))
    (list (length (elt store 0)))))


(defun make-dataframe (newdata
		       &key  (vartypes nil)
		       (caselabels nil) (varlabels nil)
		       (doc "no docs"))
  "Helper function to use instead of make-instance to assure
construction of proper DF-array."
  (check-type newdata (or matrix-like array list))
  (check-type caselabels sequence)
  (check-type varlabels sequence)
  (check-type doc string)
  (let ((ncases (ncase-store newdata))
	(nvars (nvars-store newdata)))
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
			:var-types vartypes))

	))))

#| 
 (make-dataframe #2A((1.2d0 1.3d0) (2.0d0 4.0d0)))
 (make-dataframe #2A(('a 1) ('b 2)))
 (xref (make-dataframe #2A(('a 1) ('b 2))) 0 1)
 (xref (make-dataframe #2A(('a 1) ('b 2))) 1 0)
 (make-dataframe 4) ; ERROR, should we allow?
 (make-dataframe #2A((4)))
 (make-dataframe (rand 10 5)) ;; ERROR, but should work!
|#


(defun row-order-as-list (ary)
  "Pull out data in row order into a list."
  (let ((result (list))
	(nrows (nth 0 (array-dimensions ary)))
	(ncols (nth 1 (array-dimensions ary))))
    (dotimes (i ncols)
      (dotimes (j nrows)
	(append result (aref ary i j))))))

(defun col-order-as-list (ary)
  "Pull out data in row order into a list."
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

;;;;; DATAFRAME-ARRAY

(defclass dataframe-array (dataframe-like)
  ((store :initform nil
	  :initarg :storage
	  :type (array * *)
	  :accessor dataset
	  :documentation "Data storage: typed as array."))
  (:documentation "example implementation of dataframe-like using storage
  based on lisp arrays.  An obvious alternative could be a
  dataframe-matrix-like which uses the lisp-matrix classes."))

(defmethod nrows ((df dataframe-array))
  "specializes on inheritance from matrix-like in lisp-matrix."
  (array-dimension (dataset df) 0))

(defmethod ncols ((df dataframe-array))
  "specializes on inheritance from matrix-like in lisp-matrix."
  (array-dimension (dataset df) 1))

(defmethod xref ((df dataframe-array)
		  (index1 number) (index2 number))
  "Returns a scalar in array, in the same vein as aref, mref, vref, etc.
idx1/2 is row/col or case/var."
  (aref (dataset df) index1 index2))

(defmethod (setf xref) ((df dataframe-array) (index1 number) (index2 number) val)
  "set value for df-ar."
  ;; (check-type val (elt (var-type df) index2))
  (setf (aref (dataset df) index1 index2) val))

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

;;; DATAFRAME-MATRIXLIKE
;;; 
;;; example/implementatin of using lisp-matrix datastructures for
;;; dataframe storage.

(defclass dataframe-matrixlike (dataframe-like)
  ((store :initform nil
	  :initarg :storage
	  :type matrix-like
	  :accessor dataset
	  :documentation "Data storage: typed as matrix-like
  (numerical only)."))
  (:documentation "example implementation of dataframe-like using storage
  based on lisp-matrix structures."))

(defmethod nrows ((df dataframe-matrixlike))
  "specializes on inheritance from matrix-like in lisp-matrix."
  (matrix-dimension (dataset df) 0))

(defmethod ncols ((df dataframe-matrixlike))
  "specializes on inheritance from matrix-like in lisp-matrix."
  (matrix-dimension (dataset df) 1))

(defmethod xref ((df dataframe-matrixlike)
		 (index1 number) (index2 number))
  "Returns a scalar in array, in the same vein as aref, mref, vref, etc.
idx1/2 is row/col or case/var."
  (mref (dataset df) index1 index2))

(defmethod (setf xref) ((df dataframe-matrixlike)
			(index1 number) (index2 number) val)
  "Sets a value for df-ml."
  ;; NEED TO CHECK TYPE!
  ;; (check-type val (elt (vartype df) index2))
  (setf (mref (dataset df) index1 index2) val))




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
  (:documentation "example implementation of dataframe-like using storage
  based on lisp-matrix structures."))

(defmethod nrows ((df dataframe-listoflist))
  "specializes on inheritance from listoflist in lisp-matrix."
  (length (dataset df)))

(defmethod ncols ((df dataframe-listoflist))
  "specializes on inheritance from matrix-like in lisp-matrix."
  (length (elt (dataset df) 0)))

(defmethod xref ((df dataframe-listoflist)
		  (index1 number) (index2 number))
  "Returns a scalar in array, in the same vein as aref, mref, vref, etc.
idx1/2 is row/col or case/var."
  (elt (elt (dataset df) index1) index2)) ;; ??

(defmethod (setf xref) ((df dataframe-listoflist)
		      (index1 number) (index2 number) val)
  "Sets a value for df-ml."
  ;; NEED TO CHECK TYPE!
  ;; (check-type val (elt (vartype df) index2))
  (setf (elt (elt (dataset df) index2) index1) val))



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

;;; Do we establish methods for dataframe-like, which specialize to
;;; particular instances of storage?

(defmethod print-object ((object dataframe-like) stream)
  (print-unreadable-object (object stream :type t)
    (format stream " ~d x ~d" (nrows object) (ncols object))
    (terpri stream)
    ;; (format stream "~T ~{~S ~T~}" (var-labels object))
    (dotimes (j (ncols object)) ; print labels
      (write-char #\tab stream)
      (write-char #\tab stream)
      (format stream "~T~A~T" (nth j (var-labels object))))
    (dotimes (i (nrows object)) ; print obs row
      (terpri stream)
      (format stream "~A:~T" (nth i (case-labels object)))
      (dotimes (j (ncols object))
        (write-char #\tab stream) ; (write-char #\space stream)
        ;; (write (xref object i j) :stream stream)
        (format stream "~7,3E" (xref object i j)) ; if works, need to include a general output mechanism control
	))))

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