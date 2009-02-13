;;; -*- mode: lisp -*-

;;; Time-stamp: <2009-02-06 16:06:02 tony>
;;; Creation:   <2008-03-11 19:18:34 user> 
;;; File:       bootstrap.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2009--, AJ Rossini.  BSD, LLGPL, or GPLv2, depending
;;;             on how it arrives.  
;;; Purpose:    bootstrapping algorithms for lispstat

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

(in-package :cls-algorithms-bootstrap)

;;; implememented through general macros for a lispy approach.  There
;;; could be a functional approach as well, i.e.
;;;            (bootstrap data #'function args) 
;;;

(defmacro with-data-bootstrap ((list-of-sources-and-var n) @body)
  "A proposed lispy implementation, such as:
     (with-data-bootstrap ((a dataset1)
                           (b dataset2))
        (some-form-with-inputs a b c))
where there could be multiple datasets, with a and b, etc, being
bootstrap realizations of dataset1 and dataset2."
  (Destructure list-of-sources-and-var)
  (loop repeat n
       (progn (pull-samples sources)
	      @body)
     accumulate in result-list))

(defmacro with-correlated-data-bootstrap ())

;;; functional

(defgeneric bootstrap-sample (data &optional n replace)
  (:documentation "generate a dataset of N obs from DATA either with
  or without replace(ment)")
  (:default-method (data &optional n replace)))

(defgeneric bootstrap (data function args)
  (:documentation  "used such as: (bootstrap dataset t-test :significance 0.5)")
  (:default-method (funcall #'function (bootstrap-sample data) (values args))))


#|
  2 possible paradigms:

   (with-data-bootstrap ((a dataset1))
      (t-test a :significance 0.05))

   (bootstrap #'t-test a :significance 0.05)
|#
