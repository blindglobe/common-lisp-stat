;;; -*- mode: lisp -*-

;;; Time-stamp: <2009-03-21 09:24:56 tony>
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
;;; 

;;; Relational structure -- can we capture a completely unnormalized
;;; data strucutre to propose possible modeling approaches, and
;;; propose appropriate models and inferential strategies?
;;; 
;;; So we want a verb-driven API for data collection construction.  We
;;; should encode independence or lack of, as possible.

;; Need to figure out typed vectors.   We then map a series of typed
;; vectors over to tables where columns are equal typed.  In a sense,
;; this is a relation (1-1) of equal-typed arrays.  For the most part,
;; this ends up making the R data.frame into a relational building
;; block (considering 1-1 mappings using row ID as a relation).  
;; Is this a worthwhile generalization?

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

(defclass dataframe-like (matrix-like)
  (
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
  potentially uncomputable, about dataframe-like instance."))
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


;;; Access and Extraction

(defun dfref (df idx1 idx2 &key (type :scalar))
  "Returns a scalar in array, in the same vein as aref, mref, vref, etc.
idx1/2 is row/col or case/var."
  (case type
    (:scalar (aref (dataset df) idx1 idx2))
    (:dataframe (make-instance 'dataframe-array
			       :storage (make-array
					 (list 1 1)
					 :initial-contents (dfref df idx1 idx2))
			       ;; ensure copy for this and following
			       :doc (doc-string df)
			       :case-labels (nth idx1 (caseNames df))
			       :var-labels (nth idx2  (varNames df))
			       ;; shound the type spec assume, as
			       ;; below, or should it inherit from the
			       ;; dataframe we are selecting from?
			       :var-types (nth idx2 (var-types df))))))


(defun gen-seq (n &optional (start 1))
  "There has to be a better way -- I'm sure of it!  default count from 1.
 (gen-seq 4) ; =>  (1 2 3 4)
 (gen-seq 0) ; => nil
 (gen-seq 5 3) ; => 3 4 5
"
  (if (>= n start)
      (append (gen-seq (- n 1) start) (list n))))

(defun dfref-var (sds index &key (type :list))
  "Returns data as type.
type = sequence, vector, vector-like (if valid numeric type) or dataframe."
  (ecase type
    (:list 
     (map 'list
	  #'(lambda (x) (dfref sds index x))
	  (gen-seq (nth 2 (array-dimensions (dataset sds))))))
    (:vector t)
    (:vector-like t)
    (:dataframe t)))

(defun dfref-obsn (sds index)
  "Returns row as sequence."
  (map 'sequence
       #'(lambda (x) (extract-1 sds x index))
       (gen-seq (nth 1 (array-dimensions (dataset sds))))))

;; FIXME
(defun extract-idx (sds idx1Lst idx2Lst)
  "return an array, row X col dims.  FIXME TESTME"
  (let ((my-pre-array (list)))
    (dolist (x idx1Lst)
      (dolist (y idx2Lst)
	(append my-pre-array (extract-1 sds x y))))
    (make-array (list (length idx1Lst) (length idx2Lst))
		:initial-contents my-pre-array)))


(defun extract-idx-sds (sds idx1Lst idx2Lst)
  "return a dataset encapsulated version of extract-idx."
  (make-instance 'dataframe-array
		 :storage (make-array
			   (list (length idx1Lst) (length idx2Lst))
				 :initial-contents (dataset sds))
		 ;; ensure copy for this and following
		 :doc (doc-string sds)
		 :case-labels (caseNames sds)
		 :var-labels (varNames sds)))

(defgeneric extract (sds whatAndRange)
  (:documentation "data extraction approach"))

;; Testing consistency/coherency.

(defgeneric consistent-dataframe-like-p (ds)
  (:documentation "methods to check for consistency."))

(defmethod consistent-dataframe-like-p ((ds dataframe-like))
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

 (defun ensure-consistent-datatable-type (dt lot)
  "given a datatable and a listoftypes, ensure that the datatble
  variables are consistent."
  (destructuring-bind (n p)
      (array-dimensions dt)
    (dotimes (i n)
      (dotimes (j p)
	(check-type  (aref dt i j) (elt lot j))))))
|#

;;; Printing methods and support.

(defun print-as-row (seq)
  "Print a sequence formated as a row in a table."
  (format t "~{~D~T~}" seq))

;; (print-as-row (list 1 2 3))

(defun print-structure-table (ds)
  "example of what we want the methods to look like.  Should be sort
of like a spreadsheet if the storage is a table."
  (print-as-row (var-labels ds))
  (let ((j -1))
    (dolist (i (case-labels ds))
      (print-as-row (append (list i)
			    (extract-row (dataset ds) (incf j)))))))

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
		   (extract-row (dataset currentRelationSet)
				(incf j)))))))))
|#
  

;;; Shaping for computation

(defgeneric reshapeData  (dataform into-form as-copy)
  (:documentation "pulling data into a new form"))

(defmethod reshapeData ((sds dataframe-like) what into-form))

(defmethod reshapeData ((ds array) (sp list) copy-p)
  "Array via specList specialization: similar to the common R
approaches to redistribution.")

(defclass data-format () ())

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

(defun transpose (ary)
  "map NxM to MxN."
  (make-array (reverse (array-dimensions ary))
      :initial-contents (col-order-as-list ary)))


;;; Variable-name handling for Tables.  Needs error checking.
(defun varNames (ds)
  (var-labels ds))

(defun set-varNames (ds vN)
  (if (= (length (var-labels ds))
	 (length vN))
      (setf (var-labels ds) vN)
      (error "wrong size.")))

(defsetf varNames set-varNames)

;;; Case-name handling for Tables.  Needs error checking.
(defun caseNames (ds)
  (case-labels ds))

(defun set-caseNames (ds vN)
  (if (= (length (case-labels ds))
	 (length vN))
      (setf (case-labels ds) vN)
      (error "wrong size.")))

(defsetf caseNames set-caseNames)

;;;;;;;;;;;; IMPLEMENTATIONS, with appropriate methods.

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
  (array-dimension (dataset df) 0))

(defmethod ncols ((df dataframe-array))
  (array-dimension (dataset df) 1))

;;; NEED TO FIGURE OUT HOW TO EXTEND THE MATRIX-LIKE CLASS PRINT
;;; METHOD!


(defmethod print-object ((object dataframe-array) stream)
  (print-unreadable-object (object stream :type t)
    (format stream " ~d x ~d" (nrows object) (ncols object))
    (terpri stream)
    (format stream "~{~A~}" (var-labels object))
    (dotimes (i (nrows object))
      (terpri stream)
      (dotimes (j (ncols object))
	(format stream " obs ~A" (nth i (case-labels object)))
        (write-char #\space stream)
        (write (dfref object i j) :stream stream)))))


