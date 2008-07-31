;;; -*- mode: lisp -*-

;;; File:       unittests-data-clos.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2008, AJ Rossini.  BSD, LLGPL, or GPLv2, depending
;;;             on how it arrives.  
;;; Purpose:    unittests for the data-clos package
;;; Time-stamp: <2008-05-09 14:18:19 tony>
;;; Creation:   <2008-05-09 14:18:19 tony>

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

;;; This organization and structure is new to the 21st Century
;;; version.

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
	    (ignore-errors
	      (setf (caseNames my-ds-2)
		    (list "a" "b" 4 5)))
	    (setf (caseNames my-ds-2) origCaseNames))))

;;;
;; (run-tests)
;; (describe (run-tests))
