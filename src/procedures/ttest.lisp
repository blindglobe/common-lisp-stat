;;; -*- mode: lisp -*-

;;; Time-stamp: <2009-10-06 11:17:37 tony>
;;; Creation:   <2009-03-12 17:14:56 tony>
;;; File:       ttest.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2009--, AJ Rossini.  Currently licensed under MIT
;;;             license.  See file LICENSE.mit in top-level directory
;;;             for information.
;;; Purpose:    Template header file for Statistical Procedures

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

;;; This organization and structure is new to the 21st Century
;;; version..   Think, "21st Century Schizoid Man".

(in-package :cls-statproc)

;;; Set up the data and metadata requirements 
(define-statistical-proc-metadata t-test/metadata ()
  '((verify :number-of-variables 2)
    (verify :variable-exists (discrete :levels 2) 'independent 'group)
    (verify :variable-exists 'continuous 'dependent)))

;;; Set up the t-test class.  This should provide the code for processing.
(define-statistical-proc-class t-test/class ())

;;; Set up the class for the results.  This should store the processed
;;; results.  i.e. instantiated proc: 
;;;     data + analytic proc(s) => instantiated proc 
;;; 
(define-statistical-proc-results t-test/results ()
  ((:variables '(group response))
   (:evaluate '(defun t-test (group response)
		"Estimate t-test statistic from data."
		(let ((2listsofdata (split response :by group))
		      (mean1 (mean (elt 2listsofdata 0)))
		      (mean2 (mean (elt 2listsofdata 1)))
		      (stddev1 (standard-deviation (elt 2listsofdata 0)))
		      (stddev2 (standard-deviation (elt 2listsofdata 1))))
		  (/ (- mean1 mean2)
		     (sqrt (*  (/ stddev1 n1) (/ stddev2 n2) ))))))
   (:result-form '(test-statistic :following t-distribution))
   (:documentation "...")))

;;; Define how the generics should work with this
(defmethod print-object ((proc t-test/class)))
(defmethod print-object ((results t-test/results)))
(defmethod print-object ((metadata t-test/metadata)))


(defmethod proc-consistent-data-p ((metadata t-test/metadata)
				   (data dataframe-like))
  ;; verify only 2 variables.
  ;; verify that one variable has the attributes  response and continuous.
  ;; verify that the other variable has the attributes dependent, discrete, group (2 levels).
  )

(defmethod process-data ((obj t-test/class)
			 (data dataframe-like)))


(defmethod display-results ((results t-test/results)))

(defmethod print-results ((results t-test/results)))

(defmethod visualize-results ((results t-test/results)))

(defmethod simulate-data-from-results ((results t-test/results)
				       &key (newdata  (obj2 t-test/data))))

(defmethod simulate-data-from-proc ((obj t-test/results)))
