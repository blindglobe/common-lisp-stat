;;; -*- mode: lisp -*-

;;; Time-stamp: <2009-10-09 12:01:02 tony>
;;; Creation:   <2009-03-12 17:14:56 tony>
;;; File:       template.lisp
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

;;; Basic Idea: do a grep-and-replace to identify the procedure as a
;;; new procedure.  The name of the procedure should not use "/" as
;;; part of the identifier.  (for example,
;;;     M-x query-replace procedure t-test

;;; Set up the data and metadata requirements (input components)
(define-statistical-proc-metadata procedure/metadata ())

;;; Set up the procedure class (processing)
(define-statistical-proc-class procedure/class ())

;;; Set up the class for the results (output components)
(define-statistical-proc-results procedure/results ()
  '((:variables '(var1 var2 ...))
    (:evaluate '(defun proc (var1 var2 ...) "doc-string" t))
    (:return-form '())
    (:documentation "...")))




;;; Define how the generics should work with this
(defmethod print-object ((obj procedure/class)))
(defmethod print-object ((obj procedure/results)))
(defmethod print-object ((obj procedure/metadata)))


(defmethod proc-consistent-data-p ((metadata procedure/metadata)
				   (data dataframe-like)))

(defmethod process-data ((obj procedure/class)
			 (data dataframe-like)))


(defmethod display-results ((results procedure/results)))

(defmethod print-results ((results procedure/results)))

(defmethod visualize-results ((results procedure/results)))

(defmethod simulate-data-from-results ((results procedure/results)
				       &key (newdata  (obj2 procedure/data))))

(defmethod simulate-data-from-proc ((obj procedure/results)))
