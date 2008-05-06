;;; -*- mode: lisp -*-

;;; File:       data-clos.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2008, AJ Rossini.  BSD, LLGPL, or GPLv2, depending
;;;             on how it arrives.  
;;; Purpose:    data package for lispstat
;;; Time-stamp: <2008-03-12 17:18:42 user>
;;; Creation:   <2008-03-12 17:18:42 user>

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

;;; This organization and structure is new to the 21st Century
;;; version.

;;; data-clos.lisp
;;; 
;;; redoing data structures in a CLOS based framework.
;;;
;;; No real basis for work, there is a bit of new-ness and R-ness to
;;; this work. In particular, the notion of relation is key and
;;; integral to the analysis.  Tables are related and matched
;;; vectors,for example.  "column" vectors are related observations
;;; (by measure/recording) while "row" vectors are related readings
;;; (by case)
;;;

;;; Relational structure -- can we capture a completely unnormalized
;;; data strucutre to propose possible modeling approaches, and
;;; propose appropriate models and inferential strategies?
;;;

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

	   

(in-package :cl-user)

(defpackage :lisp-stat-data-clos
  (:use :common-lisp
	;;:clem
	)
  (:export statistical-dataset ;; primary class for working.

	   modifyData ;; metadata mods
	   importData ;; get it in
	   reshapeData  ;; data mods

	   consistent-statistical-dataset-p
	   varNames caseNames ;; metadata explicit modifiers

	   extract
	   ;; and later, we remove the following, exposing only
	   ;; through the above method.
	   extract-1 extract-row extract-col extract-idx
	   ))

(in-package :lisp-stat-data-clos)

;; Need to figure out typed vectors.   We then map a series of typed
;; vectors over to tables where columns are equal typed.  In a sense,
;; this is a relation (1-1) of equal-typed arrays.  For the most part,
;; this ends up making the R data.frame into a relational building
;; block (considering 1-1 mappings using row ID as a relation).  
;; Is this a worthwhile generalization?

(defclass statistical-dataset ()
  ((store :initform nil
	  :initarg :storage
	  :accessor dataset
	  :documentation "Data storage slot.  Should be an array or a
relation,")
   (documentation-string :initform nil
			 :initarg :doc
			 :accessor doc-string
			 :documentation "Information about statistical-dataset.")
   (case-labels :initform nil
		:initarg :case-labels 
		:accessor case-labels
		:documentation "labels used for describing cases (doc
metadata), possibly used for merging.")
   (var-labels :initform nil
	       :initarg :var-labels
	       :accessor var-labels
	       :documentation "Variable names."))
  (:documentation "Standard Cases by Variables Statistical-Dataset."))

;;
;; statistical-dataset is the basic cases by variables framework.
;; Need to embed this within other structures which allow for
;; generalized relations.  Goal is to ensure that relations imply and
;; drive the potential for statistical relativeness such as
;; correlation, interference, and similar concepts.
;;
;; Actions on a statistical data structure.
;;

(defgeneric consistent-statistical-dataset-p (ds)
  (:documentation "methods to check for consistency."))

(defmethod consistent-statistical-dataset-p ((ds statistical-dataset))
  "Test that statistical-dataset is internally consistent with metadata.
Ensure that dims of stored data are same as case and var labels."
  (equal (array-dimensions (dataset ds))
       (list (length (var-labels ds))
	     (length (case-labels ds)))))

;;; Extraction

(defun extract-1 (sds idx1 idx2)
  "Returns a scalar."
  (aref (dataset sds) idx1 idx2))

(defun extract-1-as-sds (sds idx1 idx2)
  "Need a version which returns a dataset."
  (make-instance 'statistical-dataset
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
  (make-instance 'statistical-dataset
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

(defmethod reshapeData ((sds statistical-dataset) what into-form))

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
	(nappend result (aref ary i j))))))

(defun col-order-as-list (ary)
  "Pull out data in row order into a list."
  (let ((result (list))
	(nrows (nth 0 (array-dimensions ary)))
	(ncols (nth 1 (array-dimensions ary))))
    (dotimes (i nrows)
      (dotimes (j ncols)
	(nappend result (aref ary i j))))))

(defun transpose (ary)
  "map NxM to MxN."
  (make-array (reverse (array-dimensions ary))
      :initial-contents (col-order-as-list ary)))


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

;;; General modification approaches.

(defgeneric importData (source featureList)
  (:documentation "command to get data into CLS.  Specific methods
  will need to handle pathnames, internal data structures, and
  external services such as DBMS's.  We would like to be able to do
  thinks like: 
     (importData MyPathName '(:formattype 'csvString))
     (importData '(sqlConnection :server host.domain.net :port 666)
                 '(:formattype 'table   
  and so on."))

#|

(defun pathname-example (name) 
  (let ((my-path (parse-namestring name)))
    (values (pathname-name my-path :case :common) 
            (pathname-name my-path :case :local))))

(defvar sourceTypes (list 'csv 'lisp 'tsv 'special)
  "list of possible symbols used to specify source formats that might
be supported for input.  CSV and TSV are standard, LISP refers to
forms, and SPECIAL refers to a FUNCTION which parses as
appropriately.") 

;;; WRONG LOGIC.
(defmethod importData ((fileHandle pathname)
		       (fmt list)) ;sourceTypes))
  "File-based input for data.
Usually used by:
 (importData (parse-namestring 'path/to/file')
	     (list :format 'csv))

 (importData myPathName (list :format 'lisp))
."
  (let* ((fmtType (getf fmt :format))
	 (newData (getDataAsLists fileHandle fmtType)))
    (case fmtType
      ('csv (  ))
      ('tsv (  ))
      ('lisp ( ))
      ('special (let ((parserFcn (getf fmt :special-parser)))))
      (:default (error "no standard default importData format")))))

(defmethod importData ((ds array) (fmt list))
  "mapping arrays into CLS data.")

(defmethod importData ((dsSpec DBMSandSQLextract)
		       (fmt mappingTypes))
  "mapping DBMS into CLS data.")
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Unit-testing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)

;; if needed, but need to set the ASDf path first...!
;; (asdf:oos 'asdf:load-op :lift)

(defpackage :lisp-stat-data-clos-example
  (:use :common-lisp
	:lift  :lisp-stat-unittests
	:lisp-stat-data-clos))

(in-package :lisp-stat-data-clos-example)

;;;
;;; Use of this package:  To see what gets exported for use in others,
;;; and how much corruption can be done to objects within a package.
;;;

(deftestsuite lisp-stat-dataclos () ()) ;;(lisp-stat) ())

(addtest (lisp-stat-dataclos) genseq
	 (ensure
	  (equal (lisp-stat-data-clos::gen-seq 4) (list 1 2 3 4))))

(addtest (lisp-stat-dataclos) genseq-null
	 (ensure
	  (equal (lisp-stat-data-clos::gen-seq 0) nil)))

(addtest (lisp-stat-dataclos) genseq-offset
	 (ensure
	  (equal (lisp-stat-data-clos::gen-seq 4 2) (list 2 3 4))))

(addtest (lisp-stat-dataclos) equaltestnameData
	 (ensure-error
	  (equal (lisp-stat-data-clos::dataset
		  (make-instance 'statistical-dataset
				 :storage #2A(('a 'b) ('c 'd))))
		 #2A(('a 'b) ('c 'd)))))

(defvar my-ds-1 nil 
  "test ds for experiment.")
(setf my-ds-1 (make-instance 'statistical-dataset))
my-ds-1


(defvar my-ds-2 nil 
  "test ds for experiment.")
(setf my-ds-2 (make-instance 'statistical-dataset
			     :storage #2A((1 2 3 4 5) (10 20 30 40 50))
			     :doc "This is an interesting statistical-dataset"
			     :case-labels (list "a" "b" "c" "d" "e")
			     :var-labels (list "x" "y")))
my-ds-2
(make-array (list 3 5))

(array-dimensions (lisp-stat-data-clos::dataset my-ds-2))


(addtest (lisp-stat-dataclos) consData
	 (ensure
	  (consistent-statistical-dataset-p my-ds-2)))

(addtest (lisp-stat-dataclos) badAccess1
	 (ensure-error
	  (slot-value my-ds-2 'store)))

(addtest (lisp-stat-dataclos) badAccess2
	 (ensure-error
	  (slot-value my-ds-2 'store)))

(addtest (lisp-stat-dataclos) badAccess3
	 (ensure-error
	  (lisp-stat-data-clos::dataset my-ds-2)))

(addtest (lisp-stat-dataclos) badAccess4
	 (ensure
	  (equal
	   (slot-value my-ds-2 'lisp-stat-data-clos::store)
	   (lisp-stat-data-clos::dataset my-ds-2))))


(addtest (lisp-stat-dataclos) badAccess5
	 (ensure
	  (eq (lisp-stat-data-clos::dataset my-ds-2)
	      (slot-value my-ds-2 'lisp-stat-data-clos::store))))


;; NEVER DO THE FOLLOWING, UNLESS YOU WANT TO MUCK UP STRUCTURES...
(addtest (lisp-stat-dataclos) badAccess6
	 (ensure
	  (lisp-stat-data-clos::doc-string my-ds-2)))

(addtest (lisp-stat-dataclos) badAccess7
	 (ensure
	  (lisp-stat-data-clos::case-labels my-ds-2)))

(addtest (lisp-stat-dataclos) badAccess8
	 (ensure
	  (lisp-stat-data-clos::var-labels my-ds-2)))

;; need to ensure that for things like the following, that we protect
;; this a bit more so that the results are not going to to be wrong.
;; That would be a bit nasty if the statistical-dataset becomes
;; inconsistent.

(addtest (lisp-stat-dataclos) badAccess9
	 (ensure
	  (setf (lisp-stat-data-clos::var-labels my-ds-2)
		(list "a" "b"))))

(addtest (lisp-stat-dataclos) badAccess10
	 (ensure
	  (progn 
	    ;; no error, but corrupts structure
	    (setf (lisp-stat-data-clos::var-labels my-ds-2)
		  (list "a" "b" "c"))
	    ;; error happens here
	    (not (consistent-statistical-dataset-p my-ds-2))))) ;; Nil

(addtest (lisp-stat-dataclos) badAccess12
	 (ensure
	  (setf (lisp-stat-data-clos::var-labels my-ds-2)
		(list "a" "b"))))

(addtest (lisp-stat-dataclos) badAccess13
	 (ensure
	  (consistent-statistical-dataset-p my-ds-2))) ;; T

;; This is now done by:
(addtest (lisp-stat-dataclos) badAccess14
	 (ensure-error
	  (let ((old-varnames (varNames my-ds-2)))
	    (setf (varNames my-ds-2) (list "a" "b")) ;; should error
	    (setf (varNames my-ds-2) old-varnames)
	    (error "don't reach this point in badaccess14"))))

;; break this up.
(defvar origCaseNames nil)

(addtest (lisp-stat-dataclos) badAccess15
	 (ensure
	  (progn
	    (setf origCaseNames (caseNames my-ds-2))
	    (setf (caseNames my-ds-2) (list "a" "b" "c" 4 5))
	    (caseNames my-ds-2)
	    (ignore-errors (setf (caseNames my-ds-2) (list "a" "b" 4 5)))
	    (setf (caseNames my-ds-2) origCaseNames))))

;; (run-tests)
;; (describe (run-tests))
