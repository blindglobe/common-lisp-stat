;;; -*- mode: lisp -*-

;;; Time-stamp: <2008-11-25 08:15:06 tony>
;;; Creation:   <2008-09-08 08:06:30 tony>
;;; File:       TODO.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c) 2007-2008, AJ Rossini <blindglobe@gmail.com>.  BSD.
;;; Purpose:    demonstrations of how one might use CLS.

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

;;; This file contains the current challenges to solve, including a
;;; description of the setup and the work to solve....
 
;;; SET UP

(in-package :cl-user)
;;(asdf:oos 'asdf:compile-op 'lispstat)
;;(asdf:oos 'asdf:load-op 'lispstat)

(in-package :lisp-stat-unittests)

(describe (run-tests :suite 'lisp-stat-ut))
(run-tests :suite 'lisp-stat-ut)
;; tests = 68, failures = 12, errors = 5


(in-package :ls-user)

;;; Example: currently not relevant, yet
#|
  (describe 
    (lift::run-test
      :test-case  'lisp-stat-unittests::create-proto
      :suite 'lisp-stat-unittests::lisp-stat-ut-proto))
|#


(defparameter m nil
  "holding variable.")
(def m (regression-model (list iron aluminum) absorbtion :print nil))
(send m :compute)
(send m :sweep-matrix)
(format t "~%~A~%" (send m :sweep-matrix))

;;; FIXME

;; need to get multiple-linear regression working (simple linear regr
;; works)... to do this, we need to redo the whole numeric structure,
;; I'm keeping these in as example of brokenness...

(send m :basis) ;; this should be positive?
(send m :coef-estimates)

;;; FIXME 

;; Need to clean up data examples, licenses, attributions, etc.


;; The following breaks because we should use a package to hold
;; configuration details, and this would be the only package outside
;; of packages.lisp, as it holds the overall defsystem structure.
(load-data "iris.lsp")  ;; (the above partially fixed).
(variables)
diabetes


;;; FIXME

;; Data.Frames probably deserve to be lists -- either lists of cases,
;; or lists of variables.   We probably do not want to mix them, but
;; want to be able to convert between them.

(defparameter *my-case-data*
  '((:cases
     (:case1 Y Med  3.4 5)
     (:case2 N Low  3.2 3)
     (:case3 Y High 3.1 4))
    (:var-names (list "Response" "Level" "Pressure" "Size"))))

*my-case-data*

(elt *my-case-data* 1)
(elt *my-case-data* 0)
(elt *my-case-data* 2) ;; error
(elt (elt *my-case-data* 0) 1)
(elt (elt *my-case-data* 0) 0)
(elt (elt (elt *my-case-data* 0) 1) 0)
(elt (elt (elt *my-case-data* 0) 1) 1)
(elt (elt (elt *my-case-data* 0) 1) 2)
(elt (elt *my-case-data* 0) 3)


;;; FIXME: read data from CSV file.  To do.

;; challenge is to ensure that we get mixed arrays when we want them,
;; and single-type (simple) arrays in other cases.

(defparameter *csv-num* (read-csv "Data/example-num.csv" :type 'numeric))
(defparameter *csv-mix* (read-csv "Data/example-mixed.csv" :type 'data))

;; The handling of these types should be compariable to what we do for
;; matrices, but without the numerical processing.  i.e. mref, bind2,
;; make-dataframe, and the class structure should be similar. 

;; With numerical data, there should be a straightforward mapping from
;; the data.frame to a matrix.   With categorical data (including
;; dense categories such as doc-strings, as well as sparse categories
;; such as binary data), we need to include metadata about ordering,
;; coding, and such.  So the structures should probably consider 

;; Using the CSV file:

(asdf:oos 'asdf:compile-op 'csv :force t)
(asdf:oos 'asdf:load-op 'parse-number)
(asdf:oos 'asdf:load-op 'csv)
(fare-csv:read-csv-file "Data/example-numeric.csv")
