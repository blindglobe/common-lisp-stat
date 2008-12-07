;;; -*- mode: lisp -*-

;;; Time-stamp: <2008-12-07 10:42:43 tony>
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
;; tests = 68, failures = 12, errors = 7

(in-package :ls-user)

;;; FIXME: Example: currently not relevant, yet
#|
  (describe 
    (lift::run-test
      :test-case  'lisp-stat-unittests::create-proto
      :suite 'lisp-stat-unittests::lisp-stat-ut-proto))
|#

:;; FIXME: data frames and structural inheritance
;;
;; Serious flaw -- need to consider that we are not really well
;; working with the data structures, in that Luke created compound as
;; a base class, which turns out to be slightly backward if we are to
;; maintain the numerical structures as well as computational
;; efficiency.



;;; FIXME: Regression modeling
(progn 

  (defparameter m nil
    "holding variable.")
  ;; need to make vectors and matrices from the lists...

  (def m (regression-model (list iron aluminum) absorbtion :print nil))
  
  (defparameter *indep-vars-1-matrix*
    (make-matrix 1 (length iron)
		 :initial-contents
		 (list (mapcar #'(lambda (x) (coerce x 'double-float))
			       iron))))
  ;; *indep-vars-1-matrix*
  
  (defparameter *indep-vars-2-matrix*
    (make-matrix 2 (length iron)
		 :initial-contents
		 (list
		  (mapcar #'(lambda (x) (coerce x 'double-float))
			  iron)
		  (mapcar #'(lambda (x) (coerce x 'double-float))
			  aluminum))))
  ;; *indep-vars-2-matrix*
  

  ;; FAILS due to coercion issues; it just isn't lispy, it's R'y.
  ;; (defparameter *dep-var* (make-vector (length absorbtion)
  ;; 				     :initial-contents (list absorbtion)))
  ;; BUT this should be the right type.
  (defparameter *dep-var*
    (make-vector (length absorbtion)
		 :type :row
		 :initial-contents
		 (list 
		  (mapcar #'(lambda (x) (coerce x 'double-float))
			  absorbtion))))
  ;; *dep-var*

  
  (defparameter *dep-var-int*
    (make-vector (length absorbtion)
		 :type :row
		 :element-type 'integer
		 :initial-contents (list absorbtion)))
  
  (typep *dep-var* 'matrix-like) ; => T
  (typep *dep-var* 'vector-like) ; => T
  
  (typep *indep-vars-1-matrix* 'matrix-like) ; => T
  (typep *indep-vars-1-matrix* 'vector-like) ; => T
  (typep *indep-vars-2-matrix* 'matrix-like) ; => T
  (typep *indep-vars-2-matrix* 'vector-like) ; => F

  (def m1 (regression-model-new *indep-vars-1-matrix* *dep-var* ))
  (def m2 (regression-model-new *indep-vars-2-matrix* *dep-var* ))
  
  iron
  ;; following fails, need to ensure that we work on list elts, not just
  ;; elts within a list:
  ;; (coerce iron 'real) 

  ;; the following is a general list-conversion coercion approach -- is
  ;; there a more efficient way?
  (mapcar #'(lambda (x) (coerce x 'double-float)) iron)

  (coerce 1 'real)

  (send m :compute)
  (send m :sweep-matrix)
  (format t "~%~A~%" (send m :sweep-matrix))

  ;; need to get multiple-linear regression working (simple linear regr
  ;; works)... to do this, we need to redo the whole numeric structure,
  ;; I'm keeping these in as example of brokenness...
  
  (send m :basis) ;; this should be positive?
  (send m :coef-estimates)


  )


(progn ;; FIXME: Need to clean up data examples, licenses, attributions, etc.

  ;; The following breaks because we should use a package to hold
  ;; configuration details, and this would be the only package outside
  ;; of packages.lisp, as it holds the overall defsystem structure.
  (load-data "iris.lsp")  ;; (the above partially fixed).
  (variables)
  diabetes
  )

(progn
  ;; FIXME: Data.Frames probably deserve to be related to lists --
  ;; either lists of cases, or lists of variables.  We probably do not
  ;; want to mix them, but want to be able to convert between such
  ;; structures.

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

  )


(progn ;; FIXME: read data from CSV file.  To do.

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

  ;; but I think the cl-csv package is broken, need to use the dsv-style
  ;; package.
  )


(progn
  (defparameter *x* (make-vector 5 :initial-contents '((1d0 2d0 3d0 4d0 5d0))))
  (/ (loop for i from 0 to (- (nelts *x*) 1)
	summing (vref *x* i))
     (nelts *x*)))


(progn 
  ;;; A study in array vs list access
  (defparameter *x* (list 1 2 3))
  (defparameter *y* #(1 2 3))
  (nth 1 *x*)
  (aref *y* 1)
  (setf (nth 1 *x*) 6)
  *x*
  (setf (aref *y* 1) 6)
  *y*
  )