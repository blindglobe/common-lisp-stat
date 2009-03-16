;;; -*- mode: lisp -*-

;;; Time-stamp: <2009-03-16 21:03:58 tony>
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
#|
   ;; STORE is the storage component.  We ignore this in the -like ;
   ;; class, as it is the primary differentiator, driving how access
   ;; (getting/setting) is done.   We create methods depending on the
   ;; storage component, which access data as appropriate.

   ;; so: subclass this based on storage type, and ensure that generic
   ;; accessors have the right methods to do the right thing.

   (store :initform nil
	  :initarg :storage
	  :accessor dataset
	  :documentation "Data storage: typed as table, array,
                          relation, or pointer/reference to such.")
|#
   (documentation-string :initform nil
			 :initarg :doc
			 :accessor doc-string
			 :documentation "uncomputable information
                                         about dataframe-like
                                         instance.")

   ;; the rest of this is metadata.  In particular, we should find a
   ;; more flexible, compact way to store this.
   (case-labels :initform nil
		:initarg :case-labels 
		:accessor case-labels
		:documentation "labels used for describing cases (doc
                                metadata), possibly used for merging.")
   (var-labels :initform nil
	       :initarg :var-labels
	       :accessor var-labels
	       :documentation "Variable names.")
   (var-types :initform nil
	      :initarg :var-types
	      :accessor var-types
	      :documentation "variable types to ensure fit"
	      ))
  (:documentation "Abstract class for standard statistical analysis
                   dataset for independent data.  Rows are considered
                   to be independent, matching observations.  Columns
                   are considered to be type-consistent, match a
                   varioable with distribution.  inherits from
                   lisp-matrix base matrix-like class. 

                   dataframe-like
                   is the basic cases by variables framework.  Need to
                   embed this within other structures which allow for
                   generalized relations.  Goal is to ensure that
                   relations imply and drive the potential for
                   statistical relativeness such as correlation,
                   interference, and similar concepts.
"))

;; (documentation 'dataframe-like  'type)

(defclass dataframe-array (dataframe-like)
  ((store :initform nil
	  :initarg :storage
	  :type (array * *)
	  :accessor dataset
	  :documentation "Data storage: typed as array."))
  (:documentation "example implementation of dataframe-like using storage
  based on lisp arrays."))

#|
  (let ((df (make-new 'dataframe-array))))

|#

;; Actions on a statistical data structure.


(defgeneric consistent-dataframe-like-p (ds)
  (:documentation "methods to check for consistency."))

(defmethod consistent-dataframe-like-p ((ds dataframe-like))
  "Test that dataframe-like is internally consistent with metadata.
Ensure that dims of stored data are same as case and var labels."
  (equal (array-dimensions (dataset ds))
       (list (length (var-labels ds))
	     (length (case-labels ds)))))
;; FIXME: NEED TO CHECK TYPING AS WELL!


;;; Extraction

(defgeneric access (dataframe-like spec-list)
  (:documentation "access to array presevingtype."))

(defgeneric get-variable-matrix (dataframe-like-object list-of-variable-names)
  (:documentation "retrieves a matrix whose columns are the variable
  names in same order specified.")) 

(defgeneric get-variable-vector (dataframe-like-object variable-name))

(defun extract-1 (sds idx1 idx2)
  "Returns a scalar."
  (aref (dataset sds) idx1 idx2))

(defun extract-1-as-sds (sds idx1 idx2)
  "Need a version which returns a dataset."
  (make-instance 'dataframe-array
		 :storage (make-array
			   (list 1 1)
			   :initial-contents (extract-1 sds idx1 idx2))
		 ;; ensure copy for this and following
		 :doc (doc-string sds)
		 :case-labels (caseNames sds)
		 :var-labels (varNames sds)))

(defun gen-seq (n &optional (start 1))
  "There has to be a better way -- I'm sure of it!  Always count from 1."
  (if (>= n start)
      (append (gen-seq (- n 1) start) (list n))))
;; (gen-seq 4)
;; =>  (1 2 3 4)
;; (gen-seq 0)
;; => nil
;; (gen-seq 5 3)
;; => 3 4 5
;; 

(defun extract-col (sds index)
  "Returns data as sequence."
  (map 'sequence
       #'(lambda (x) (extract-1 sds index x))
       (gen-seq (nth 2 (array-dimensions (dataset sds))))))

(defun extract-col-as-sds (sds index)
  "Returns data as SDS, copied."
  (map 'sequence
       #'(lambda (x) (extract-1 sds index x))
       (gen-seq (nth 2 (array-dimensions (dataset sds))))))

(defun extract-row (sds index)
  "Returns row as sequence."
  (map 'sequence
       #'(lambda (x) (extract-1 sds x index))
       (gen-seq (nth 1 (array-dimensions (dataset sds))))))

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

