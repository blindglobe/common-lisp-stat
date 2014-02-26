;;; -*- mode: lisp -*-

;;; Time-stamp: <2014-02-26 08:09:21 tony>
;;; Creation:   <2009-10-30 07:52:31 tony>
;;; File:       unittests-data.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2008--, AJ Rossini.  This instance licensed under
;;;             the MIT license.  See file LICENSE.mit in top-level
;;;             directory for information.
;;; Purpose:    unittests for abstract data

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

(in-package :lisp-stat-unittests)

(deftestsuite lisp-stat-ut-data (lisp-stat-ut)
  ((my-statvar-emp-1
    (make-instance 'empirical-statistical-variable
		   :nobs 12))
   (my-statvar-mb-2
    (make-instance 'modelbased-statistical-variable
		   :pdmf #'(lambda (x) 1.0d0)
		   :drawf #'random))))

;;; Ensure helper-functions work

(addtest (lisp-stat-ut-data) emp-stat-var-class
	 (ensure
	  (equal (nobs my-statvar-emp-1)
		 12)))

(describe (run-test :name 'emp-stat-var-class :suite 'lisp-stat-ut-data))

(addtest (lisp-stat-ut-dataframe) repeatseq
	 (ensure
	  (equal (cls-dataframe::repeat-seq 3 "d")
		 (list "d" "d" "d")))
	 	 (ensure
	  (equal (cls-dataframe::repeat-seq 3 'd)
		 (list 'd 'd 'd))))


(addtest (lisp-stat-ut-dataframe) make-labels
	 (ensure
	  (equal (cls-dataframe::make-labels "c" 3)
		 (list "c1" "c2" "c3")))
	 (ensure-error
	   (cls-dataframe::make-labels 'c 3)))


;;; Dataframe tests

(addtest (lisp-stat-ut-dataframe) df-equalp
	 (ensure
	  (equalp (dataset (make-instance 'dataframe-array
					  :storage #2A(('a 'b)
						       ('c 'd))))
		  #2A(('a 'b)
		      ('c 'd)))))

(addtest (lisp-stat-ut-dataframe) df-consdata
	 (ensure 
	  (consistent-dataframe-p my-df-1)))

(addtest (lisp-stat-ut-dataframe) df-consdata
	 (ensure-error
	   (slot-value my-df-1 'store)))

(addtest (lisp-stat-ut-dataframe) df-consdata
	 (ensure
	  (slot-value my-df-1 'cls-dataframe::store)))

(addtest (lisp-stat-ut-dataframe) df-consdata
	 (ensure
	  (dataset my-df-1)))

(addtest (lisp-stat-ut-dataframe) df-consdata
	 (ensure
	  (equalp
	   (slot-value my-df-1 'cls-dataframe::store)
	   (cls-dataframe::dataset my-df-1))))

(addtest (lisp-stat-ut-dataframe) df-consdata
	 (ensure
	  (eq (cls-dataframe::dataset my-df-1)
	      (slot-value my-df-1 'cls-dataframe::store))))

;; NEVER REACH INTO CLASS INTERIORS, NO PROMISE
;; GUARANTEE OF LATER CONSISTENCY...
	      
(addtest (lisp-stat-ut-dataframe) df-consdata
	 (ensure
	  (cls-dataframe::doc-string my-df-1))
	 (ensure-error 
	   (doc-string my-df-1)))
  
(addtest (lisp-stat-ut-dataframe) df-consdata
	 (ensure
	  (cls-dataframe::case-labels my-df-1))
	 (ensure-error
	   (case-labels my-df-1)))
  
(addtest (lisp-stat-ut-dataframe) df-consdata
	 (ensure
	  (cls-dataframe::var-labels my-df-1))
	 (ensure-error
	   (var-labels my-df-1)))

;; need to ensure that for things like the following, that we protect
;; this a bit more so that the results are not going to to be wrong.
;; That would be a bit nasty if the dataframe-array becomes
;; inconsistent.

(addtest (lisp-stat-ut-dataframe) badAccess9
	 (ensure
	  (setf (cls-dataframe::var-labels my-df-1)
		(list "a" "b"))))
  
(addtest (lisp-stat-ut-dataframe) badAccess10
	 (ensure
	  (progn 
	    ;; no error, but corrupts structure
	    (setf (cls-dataframe::var-labels my-df-1)
		  (list "a" "b" "c"))
	    ;; error happens here
	    (not (consistent-dataframe-p my-df-1))))) ;; Nil

(addtest (lisp-stat-ut-dataframe) badAccess12
	 (ensure
	  (setf (cls-dataframe::var-labels my-df-1)
		(list "a" "b"))))
  
(addtest (lisp-stat-ut-dataframe) badAccess13
	 (ensure
	  (consistent-dataframe-p my-df-1))) ;; T
  
;; This is now done by:
(addtest (lisp-stat-ut-dataframe) badAccess14
	 (ensure-error
	   (let ((old-varnames (varNames my-df-1)))
	     (setf (varNames my-df-1) (list "a" "b")) ;; should error
	     (setf (varNames my-df-1) old-varnames)
	     (error "don't reach this point in badaccess14"))))


;; NEED TO CONFIRM THERE IS AN ERROR.  
(addtest (lisp-stat-ut-dataframe) badAccess15
	 (ensure-error
	  (let ((origCaseNames (caselabels my-df-1)))
	    (setf (caselabels my-df-1) (list "a" "b" "c" 4 5))
	    (caselabels my-df-1)
	    (ignore-errors
	      (setf (caselabels my-df-1)
		    (list "a" "b" 4 5)))
	    (setf (caselabels my-df-1) origCaseNames))))

;;;
;; (run-tests)
;; (describe (run-tests))

  (equalp (dataset
	   (make-instance 'dataframe-array
			  :storage #2A(('a 'b)
				       ('c 'd))))
	  #2A(('a 'b)
	      ('c 'd)) )

  (equalp (dataset
	  (make-instance 'dataframe-array
			 :storage #2A((1 2)
				      (3 4))))
		 #2A((1 2)
		     (3 4)))

  (equalp (dataset
	  (make-instance 'dataframe-array
			 :storage #2A((1d0 2d0)
				      (3d0 4d0))))
		 #2A((1d0 2d0)
		     (3d0 4d0)))

