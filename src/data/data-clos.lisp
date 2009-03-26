;;; -*- mode: lisp -*-

;;; Time-stamp: <2009-03-26 18:32:57 tony>
;;; Creation:   <2008-03-12 17:18:42 blindglobe@gmail.com>
;;; File:       data-clos.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2008, AJ Rossini.  BSD, LLGPL, or GPLv2, depending
;;;             on how it arrives.  

;;; Purpose:    Data packaging and access for Common Lisp Statistics.
;;;             This redoes data storage structures in a CLOS based
;;;             framework.
;;;

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

(in-package :lisp-stat-data-clos)

;;; No real basis for work, there is a bit of new-ness and R-ness to
;;; this work. In particular, the notion of relation is key and
;;; integral to the analysis.  Tables are related and matched vectors,
;;; for example.  "column" vectors are related observations (by
;;; measure/recording) while "row" vectors are related readings (by
;;; case)

;;; Relational structure -- can we capture a completely unnormalized
;;; data strucutre to propose possible modeling approaches, and
;;; propose appropriate models and inferential strategies?

;;; So we want a verb-driven API for data collection construction.  We
;;; should encode independence or lack of, as possible.

;;; Need to figure out typed vectors.  We then map a series of typed
;;; vectors over to tables where columns are equal typed.  In a sense,
;;; this is a relation (1-1) of equal-typed arrays.  For the most
;;; part, this ends up making the R data.frame into a relational
;;; building block (considering 1-1 mappings using row ID as a
;;; relation).  Is this a worthwhile generalization?

;;; verbs vs semantics for DS conversion -- consider the possibily of
;;; how adverbs and verbs relate, where to put which semantically to
;;; allow for general approach.

;;; eg. Kasper's talk on the FUSION collection of parsers.

;;; 
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
;;; - 


;;; Misc Functions (to move into a lisp data manipulation support package)

(defun gen-seq (n &optional (start 1))
  "There has to be a better way -- I'm sure of it!  default count from 1.
 (gen-seq 4) ; =>  (1 2 3 4)
 (gen-seq 0) ; => nil
 (gen-seq 5 3) ; => 3 4 5
"
  (if (>= n start)
      (append (gen-seq (- n 1) start) (list n))))

(defun repeat-seq (n item)
  "FIXME: There has to be a better way -- I'm sure of it!
 (repeat-seq 3 \"d\") ; => (\"d\" \"d\" \"d\")
 (repeat-seq 3 'd) ; => ('d 'd 'd)
"
  (if (>= n 1)
      (append (repeat-seq (1- n)  item) (list item))))



(defun strsym->indexnum (df strsym)
  "Returns a number indicating which column in DF has STRSYM labeling
it.  Probably should be a method dispatching on the type of
DATAFRAME-LIKE."
  (position strsym (varlabels df)))

(defun string->number (str)
  "Convert a string <str> representing a number to a number. A second value is
returned indicating the success of the conversion.
Example: (rsm.string:string->number \"123\")
          123
          t"
  (let ((*read-eval* nil))
    (let ((num (read-from-string str)))
      (values num (numberp num)))))

(string->number "1.22")


#|

  (equal 'testme 'testme)
  (defparameter *test-pos* 'testme)
  (position *test-pos* (list 'a 'b 'testme 'c))
  (position #'(lambda (x) (equal x "testme")) (list "a" "b" "testme" "c"))
  (position #'(lambda (x) (equal x 1)) (list 2 1 3 4))


|#
;;; abstract dataframe class

(defclass dataframe-like (matrix-like)
  (
   ;; Matrix-like (from lisp-matrix) is basically a rectangular table
   ;; without storage.  We emulate that, and add storage, row/column
   ;; labels, and within-column-typing.

   ;; STORE is the storage component.  We ignore this in the DATAFRAME-LIKE
   ;; class, as it is the primary differentiator, driving how access
   ;; (getting/setting) is done.   We create methods depending on the
   ;; storage component, which access data as appropriate.  See
   ;; DATAFRAME-ARRAY for an example implementation.
   ;; the rest of this is metadata.  In particular, we should find a
   ;; more flexible, compact way to store this.
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
	       :documentation "Variable names.")
   (var-types :initform nil
	      :initarg :var-types
	      :type list
	      :accessor var-types
	      :documentation "variable types to ensure fit")
   (documentation-string :initform nil
			 :initarg :doc
			 :accessor doc-string
			 :documentation "additional information,
  potentially uncomputable, possibly metadata, about dataframe-like
  instance."))
  (:documentation "Abstract class for standard statistical analysis
                   dataset for independent data.  Rows are considered
                   to be independent, matching observations.  Columns
                   are considered to be type-consistent, match a
                   variable with distribution.  inherits from
                   lisp-matrix base MATRIX-LIKE class.

                   DATAFRAME-LIKE is the basic cases by variables
                   framework.  Need to embed this within other
                   structures which allow for generalized relations.
                   Goal is to ensure that relations imply and drive
                   the potential for statistical relativeness such as
                   correlation, interference, and similar concepts."))


;;; Generics specialized above matrix-like, particularly for
;;; dataframe-like objects.  Need methods for any storage
;;; implementation.

(defgeneric dataframe-dimensions (df)
  (:documentation "")
  (:method ((df dataframe-like))
    (error "dispatch on virtual class.")))

(defgeneric dataframe-dimension (df index)
  (:documentation "")
  (:method ((df dataframe-like) index)
    (elt (dataframe-dimensions df) index)))

(defgeneric dfref (df index1 index2) ; &key return-type
  (:documentation "scalar access with selection of possible return
  object types.")
  (:method ((df dataframe-like) index1 index2) ;  &key return-type
    (error "need a real class with real storage to reference elements.")))

;;; Specializing on superclasses...
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
    (error "Need implementation; can't dispatch on virtual class DATAFRAME-LIKE.")))

#|

 (defun ensure-consistent-datatable-type (dt lot)
  "given a datatable and a listoftypes, ensure that the datatble
  variables are consistent."
  (destructuring-bind (n p) ;; why use let when we can be cool?  Sigh.
      (array-dimensions dt)
    (dotimes (i n)
      (dotimes (j p)
	(check-type  (aref dt i j) (elt lot j))))))
|#


;;; GENERAL FUNCTIONS WHICH DISPATCH ON INTERNAL METHODS OR ARGS  
;;;
;;; Q: change the following to generic functions and dispatch on 
;;; array, matrix, and dataframe?  Others?
(defun make-labels (initstr num &key (return-type 'string))
  "generate a list of strings (or symbols?) which can be used as
labels, i.e. something like 
   '(a1 a2 a3) 
or 
   '(\"a1\" \"a2\" \"a3\")."
  (check-type initstr string)
  (mapcar #'(lambda (x y)  (concatenate 'string x y))
	  (repeat-seq num initstr)
	  (mapcar #'(lambda (x) (format nil "~A" x)) (gen-seq num))))

#|
 (make-labels 'c 2)
 (make-labels "c" 4)
|#

(defun make-dataframe (newdata
		       &key  (vartypes nil)
		       (caselabels nil) (varlabels nil)
		       (doc "no docs"))
  "Helper function to use instead of make-instance to assure
construction of proper DF-array."
  (check-type newdata array)
  (check-type caselabels sequence)
  (check-type varlabels sequence)
  (check-type doc string)
  (if caselabels (assert (= (array-dimension newdata 0) (length caselabels))))
  (if varlabels (assert (= (array-dimension newdata 1) (length varlabels))))
  (let ((newcaselabels (if caselabels
			   caselabels
			   (make-labels "C" (array-dimension newdata 0))))
	(newvarlabels (if varlabels
			  varlabels
			  (make-labels "V" (array-dimension newdata 1)))))
    (make-instance 'dataframe-array
		   :storage newdata
		   :nrows (length newcaselabels)
		   :ncols (length newvarlabels)
		   :case-labels newcaselabels
		   :var-labels newvarlabels
		   :var-types vartypes)))

#| 
 (make-dataframe #2A((1.2d0 1.3d0) (2.0d0 4.0d0)))
 (make-dataframe #2A(('a 1) ('b 2)))
 (dfref (make-dataframe #2A(('a 1) ('b 2))) 0 1)
 (dfref (make-dataframe #2A(('a 1) ('b 2))) 1 0)
 (make-dataframe 4) ; ERROR
 (make-dataframe #2A((4)))
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

(defmethod consistent-dataframe-p ((ds dataframe-array))
  "Test that dataframe-like is internally consistent with metadata.
Ensure that dims of stored data are same as case and var labels.

Currently checks length of things, but needs to check type of things
as well."
  (and
   ;; ensure dimensionality
   (equal (list (ncols ds) (nrows ds)) ; array-dimensions (dataset ds))
	  (list (length (var-labels ds))
		(length (case-labels ds))))
   ;; when dims sane, check-type for each variable
   (progn
     (dolist (i (ncols ds))
       (dotimes (j (nrows ds))
	 (typep (aref (dataset ds) i j) (nth i (var-types ds)))))
     t)))



#|

 (defun testecase (s)
   (ecase s
     ((scalar) 1)
     ((asd asdf) 2)))

 (testecase 'scalar)
 (testecase 'asd)
 (testecase 'asdf)
 (testecase 'as)
|#

(defmethod dfref ((df dataframe-array)
		  (index1 number) (index2 number))
  "Returns a scalar in array, in the same vein as aref, mref, vref, etc.
idx1/2 is row/col or case/var.  Return-type could be 'scalar,
'dataframe, ..."
  (aref (dataset df) index1 index2))

#|

 (defmethod dfref ((df dataframe-array)
		  (index1 number) (index2 number)
		  &key ((return-type scalar) symbol))
  "Returns a scalar in array, in the same vein as aref, mref, vref, etc.
idx1/2 is row/col or case/var.  Return-type could be 'scalar,
'dataframe, ..."
  (ecase return-type
    ((scalar)
     (aref (dataset df) index1 index2))
    ((dataframe)
     (make-instance 'dataframe-array
		    :storage (make-array
			      (list 1 1)
			      :initial-contents (dfref df index1 index2))
		    ;; ensure copy for this and following
		    :doc (doc-string df)
		    :case-labels (nth index1 (caselabels df))
		    :var-labels (nth index2  (varlabels df))
		    ;; shound the type spec assume, as
		    ;; below, or should it inherit from the
		    ;; dataframe we are selecting from?
		    :var-types (nth index2 (var-types df))))))

 (defmethod dfref ((df dataframe-array)
		  (index1 string) (index2 string)
		  &key ((return-type scalar) symbol))
  "Returns a scalar in array, in the same vein as aref, mref, vref, etc.
idx1/2 is row/col or case/var.  This method dispatches when using
strings or symbols.  Merge with the index-as-number variant?"
  (let ((idx1 (strsym->indexnum df index1))
	(idx2 (strsym->indexnum df index2)))
    (ecase return-type
      ((scalar) (aref (dataset df) idx1 idx2))
      ((dataframe) (make-instance 'dataframe-array
				  :storage (make-array
					    (list 1 1)
					    :initial-contents (dfref df idx1 idx2))
				  ;; ensure copy for this and following
				  :doc (doc-string df)
				  :case-labels (elt (caselabels df) idx1)
				  :var-labels  (elt (varlabels df) idx2)
				  ;; shound the type spec assume, as
				  ;; below, or should it inherit from the
				  ;; dataframe we are selecting from?
				  :var-types (nth idx2 (var-types df)))))))
|#

(defun dfref-var (df index return-type)
  "Returns the data in a single variable as type.
type = sequence, vector, vector-like (if valid numeric type) or dataframe."
  (ecase return-type
    (('list)
     (map 'list
	  #'(lambda (x) (dfref df index x))
	  (gen-seq (nth 2 (array-dimensions (dataset df))))))
    (('vector) t)
    (:vector-like t)
    (:matrix-like t)
    (:dataframe t)))

(defun dfref-case (df index return-type)
  "Returns row as sequence."
  (ecase return-type
    (:list 
     (map 'list
	  #'(lambda (x) (dfref df x index))
	  (gen-seq (nth 1 (array-dimensions (dataset df))))))
    (:vector t)
    (:vector-like t)
    (:matrix-like t)
    (:dataframe t)))

;; FIXME
(defun dfref-2indexlist (df indexlist1 indexlist2 &key (return-type :array))
  "return an array, row X col dims.  FIXME TESTME"
  (case return-type
    (:array 
     (let ((my-pre-array (list)))
       (dolist (x indexlist1)
	 (dolist (y indexlist2)
	   (append my-pre-array (dfref df x y))))
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

(defmethod print-object ((object dataframe-array) stream)
  (print-unreadable-object (object stream :type t)
    (format stream " ~d x ~d" (nrows object) (ncols object))
    (terpri stream)
    ;; (format stream "~T ~{~S ~T~}" (var-labels object))
    (dotimes (j (ncols object))
      (write-char #\tab stream)
      (format stream "~A~T" (nth j (var-labels object))))
    (dotimes (i (nrows object))
      (terpri stream)
      (format stream "~A:~T" (nth i (case-labels object)))
      (dotimes (j (ncols object))
        ;; (write-char #\space stream)
        (write-char #\tab stream)
        (write (dfref object i j) :stream stream)))))

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
		   (dfref-obsn (dataset currentRelationSet)
                               (incf j)))))))))
|#
