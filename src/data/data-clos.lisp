;;; -*- mode: lisp -*-

;;; Time-stamp: <2008-10-03 02:15:49 tony>
;;; Creation:   <2008-03-12 17:18:42 blindglobe@gmail.com>
;;; File:       data-clos.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2008, AJ Rossini.  BSD, LLGPL, or GPLv2, depending
;;;             on how it arrives.  
;;; Purpose:    data package for lispstat.   redoing data structures
;;;             in a CLOS based framework. 
;;;
;;; No real basis for work, there is a bit of new-ness and R-ness to
;;; this work. In particular, the notion of relation is key and
;;; integral to the analysis.  Tables are related and matched
;;; vectors,for example.  "column" vectors are related observations
;;; (by measure/recording) while "row" vectors are related readings
;;; (by case) 


;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

;;; This organization and structure is new to the 21st Century
;;; version.

(in-package :lisp-stat-data-clos)

;;; Relational structure -- can we capture a completely unnormalized
;;; data strucutre to propose possible modeling approaches, and
;;; propose appropriate models and inferential strategies?

;; verb-driven schema for data collection.  Should encode independence
;; or lack of when possible.

#+nil(progn
       (def-statschema MyDB
	   :tables (list (list t1 )
			 (list t2 )
			 (list t4 ))
	   :unique-key key
	   :stat-relation '(t1 (:nest-within t2) (:nest-within t3))
	   :))

;; Need to figure out typed vectors.   We then map a series of typed
;; vectors over to tables where columns are equal typed.  In a sense,
;; this is a relation (1-1) of equal-typed arrays.  For the most part,
;; this ends up making the R data.frame into a relational building
;; block (considering 1-1 mappings using row ID as a relation).  
;; Is this a worthwhile generalization?

;;; verbs vs semantics for dt conversion -- consider the possibily of
;;; how adverbs and verbs relate, where to put which semantically to
;;; allow for general approach.

;;; eg. Kasper's talk on the FUSION collection of parsers.


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

(defclass data-pointer ()
  ((store :initform nil
	  :initarg :storage
	  :accessor dataset
	  :documentation "Data storage: typed as table, array,
                          relation, or pointer/reference to such.")
   (documentation-string :initform nil
			 :initarg :doc
			 :accessor doc-string
			 :documentation "uncomputable information
                                         about statistical-dataset
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
	       :documentation "Variable names."))
  (:documentation "Standard Cases by Variables Statistical-Dataset,
                   i.e. an S data.frame."))


(defgeneric get-variable-matrix (dataset-pointer list-of-variable-names)
  (:documentation "retrieves a matrix whose columns are the variable
  names in same order specified.")) 

(defgeneric get-variable-vector (dataset-pointer variable-name))

;; statistical-dataset is the basic cases by variables framework.
;; Need to embed this within other structures which allow for
;; generalized relations.  Goal is to ensure that relations imply and
;; drive the potential for statistical relativeness such as
;; correlation, interference, and similar concepts.
;;
;; Actions on a statistical data structure.
