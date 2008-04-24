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


(defun print-structure-table (ds)
  "example of what we want the methods to look like.  Should be sort
of like a spreadsheet if the storage is a table."
  (print-as-row (var-labels ds))
  (let ((j -1))
    (dolist (i (case-labels ds))
      (princ "%i %v" i (row-extract (dataset ds) (incr j))))))

(defun print-structure-relational (ds)
  "example of what we want the methods to look like.  Should be sort
of like a graph of spreadsheets if the storage is a relational
structure."
  (dolist (k (relations ds))
    (print-as-row (var-labels ds))
    (let ((j -1))
      (dolist (i (case-labels ds))
	(princ "%i %v" i (row-extract (dataset ds) (incr j)))))))
  



(defgeneric reshapeData  (dataform into-form as-copy)
  (:documentation "pulling data into a new form"))

(defmethod reshapeData ((ds statistical-dataset) what into-form)
  (reshape (get ds what) into-form))

(defmethod reshapeData ((ds array) (sp list) copy-p)
  "Array via specList specialization: similar to the common R
approaches to redistribution."
  (let ((widep (getf sp :toWide))
	(primaryKey (getf sp :primaryKey)))
    ))


(defclass data-format () ())

(defun transpose (x)
  "map NxM to MxN.")

(defun reorder-by-rank (x order &key (by-row t))
  " .")

(defun reorder-by-permutation (x perm &key (by-row t))
  " .")

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
  will need to handle files, internal data structures, and DBMS's.  We
  would like to be able to do:



"))


(defun pathname-example (name) 
  (let ((my-path (parse-namestring name)))) 
    (values (pathname-name my-path :case :common) 
            (pathname-name my-path :case :local)))

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
  (let ((newData (getDataAsLists fileHandle fmtType))
	(fmtType (getf fmt :format)))
    (case fmtType
      ('csv (  ))
      ('tsv (  ))
      ('lisp ( ))
      ('special (let ((parserFcn (getf fmt :special-parser)))))
      (:default (error "no standard default importData format")))))

(defmethod importData ((ds array) (fmt list))
  "mapping arrays into CLS data.")

#|
(defmethod importData ((dsSpec DBMSandSQLextract)
		       (fmt mappingTypes))
  "mapping DBMS into CLS data.")
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EXPERIMENT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)

;; if needed, but need to set the ASDf path first...!
;; (asdf:oos 'asdf:load-op :lift)

(defpackage :lisp-stat-data-clos-example
  (:use :common-lisp
	:lift
	:lisp-stat-data-clos))

(in-package :lisp-stat-data-clos-example)

;;;
;;; Use of this package:  To see what gets exported for use in others,
;;; and how much corruption can be done to objects within a package.
;;;


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

(consistent-statistical-dataset-p my-ds-2)
my-ds-2
(make-array (list 3 5))

(ignore-errors (slot-value my-ds-2 'store))  ;; should fail, this and next.
(ignore-errors (dataset my-ds-2))

(slot-value my-ds-2 'lisp-stat-data-clos::store)
(lisp-stat-data-clos::dataset my-ds-2)


(eq (lisp-stat-data-clos::dataset my-ds-2)
    (slot-value my-ds-2 'lisp-stat-data-clos::store))

;; NEVER DO THE FOLLOWING, UNLESS YOU WANT TO MUCK UP STRUCTURES...
(lisp-stat-data-clos::doc-string my-ds-2)
(lisp-stat-data-clos::case-labels my-ds-2)
(lisp-stat-data-clos::var-labels my-ds-2)


;; need to ensure that for things like the following, that we protect
;; this a bit more so that the results are not going to to be wrong.
;; That would be a bit nasty if the statistical-dataset becomes inconsistent.
(setf (lisp-stat-data-clos::var-labels my-ds-2) (list "a" "b"))
(setf (lisp-stat-data-clos::var-labels my-ds-2) (list "a" "b" "c")) ;; Should error!
(consistent-statistical-dataset-p my-ds-2) ;; Nil
(setf (lisp-stat-data-clos::var-labels my-ds-2) (list "a" "b"))
(consistent-statistical-dataset-p my-ds-2) ;; T

;; This is now done by:
	
(varNames my-ds-2)
(setf (varNames my-ds-2) (list "a" "b"))
(varNames my-ds-2)

(defvar origCaseNames nil)
(setf origCaseNames (caseNames my-ds-2))
(setf (caseNames my-ds-2) (list "a" "b" "c" 4 5))
(caseNames my-ds-2)
(ignore-errors (setf (caseNames my-ds-2) (list "a" "b" 4 5)))
(setf (caseNames my-ds-2) origCaseNames)


;;; This is semi-external to lispstat core packages.  The dependency
;;; should be that lispstat packages are dependencies for the unit
;;; tests.  However, where they will end up is still to be
;;; determined. 

(in-package :cl-user)

(defpackage :lisp-stat-unittests-dataclos
  (:use :common-lisp :lift :lisp-stat :lisp-stat-unittests)
  (:shadowing-import-from :lisp-stat
	slot-value call-method call-next-method ;; objects
	expt + - * / ** mod rem abs 1+ 1- log exp sqrt sin cos tan ;; lsmath
	asin acos atan sinh cosh tanh asinh acosh atanh float random
	truncate floor ceiling round minusp zerop plusp evenp oddp 
	< <= = /= >= > ;; complex
	conjugate realpart imagpart phase
	min max logand logior logxor lognot ffloor fceiling
	ftruncate fround signum cis)
  (:export unittest-data))

(in-package :lisp-stat-unittests-dataclos)

;;; TESTS

(defun run-lisp-stat-tests ()
  (run-tests :suite 'lisp-stat))

(defun run-lisp-stat-test (&rest x)
  (run-test x))


(deftestsuite lisp-stat-dataclos (lisp-stat)
  ()
  (:tests
   (initdata (ensure-true ))))



(deftestsuite lisp-stat-testsupport (lisp-stat)
  ()
  (:tests
   (almost=1 (ensure (almost= 3 3.001 :tol 0.01)))



(addtest (lisp-stat-dataclos) testnameData
	 (ensure-same
	  (dataset (list a b c d) :form (list 2 2))
	  #2A((a b) (c d))
	  :test 'eql))


o