;;; -*- mode: lisp -*-

;;; Time-stamp: <2009-08-27 07:52:57 tony>
;;; Creation:   <2008-09-08 08:06:30 tony>
;;; File:       TODO.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c) 2007-2008, AJ Rossini <blindglobe@gmail.com>.  BSD.
;;; Purpose: Stuff that needs to be made working sits inside the
;;;          progns... This file contains the current challenges to
;;;          solve, including a description of the setup and the work
;;;          to solve....
 
;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

;;; SET UP

(in-package :cl-user)

(progn 
  (defun init-CLS ()

    ;; core system
    ;;(asdf:oos 'asdf:load-op 'lisp-matrix)
    ;;(asdf:oos 'asdf:compile-op 'cls :force t)
    (asdf:oos 'asdf:load-op 'cls)
    
    ;; visualization
    (asdf:oos 'asdf:load-op 'cl-cairo2-x11)
    (asdf:oos 'asdf:load-op 'cl-2d)
    
    ;; doc reporting
    (asdf:oos 'asdf:load-op 'cl-pdf)
    (asdf:oos 'asdf:load-op 'cl-typesetting))

  (init-CLS))

(in-package :lisp-stat-unittests)

;; tests = 80, failures = 8, errors = 20
(run-tests :suite 'lisp-stat-ut)
(describe (run-tests :suite 'lisp-stat-ut))

(describe 'lisp-stat-ut)
(documentation 'lisp-stat-ut 'type)

;; FIXME: Example: currently not relevant, yet
;;   (describe (lift::run-test :test-case  'lisp-stat-unittests::create-proto
;;                             :suite 'lisp-stat-unittests::lisp-stat-ut-proto))

(describe (lift::run-tests :suite 'lisp-stat-ut-dataframe))
(lift::run-tests :suite 'lisp-stat-ut-dataframe)

(describe (lift::run-test
	   :test-case  'lisp-stat-unittests::create-proto
	   :suite 'lisp-stat-unittests::lisp-stat-ut-proto))

(in-package :ls-user)

;;; Tasks working on...

#+nil
(progn
  ;; use of extension packages supporting versioning and validation of
  ;; CLOS objects?
  (asdf:oos 'asdf:load-op 'versioned-objects)
  (asdf:oos 'asdf:load-op 'validations))

#+nil
(progn
  ;; Syntax examples using lexical scope, closures, and bindings to
  ;; ensure a clean communication of results
  (with-data dataset ((dsvarname1 [usevarname1])
                      (dsvarname2 [usevarname2]))
      @body))


(defparameter *df-test*
  (make-instance 'dataframe-array
		 :storage #2A (('a "test0" 0 0d0)
			       ('b "test1" 1 1d0)
			       ('c "test2" 2 2d0)
			       ('d "test3" 3 3d0)
			       ('e "test4" 4 4d0))
		 :doc "test reality"
		 :case-labels (list "0" "1" "2" "3" "4")
		 :var-labels (list "symbol" "string" "integer" "double-float")
		 :var-types (list 'symbol 'string 'integer 'double-float)))

*df-test* ; but with SBCL, ints become floats? 

(defun check-var (df colnum)
  (let ((nobs (xdim df 0)))
    (dotimes (i nobs)
      (check-type (xref df i colnum) (elt (var-types df) i)))))

(check-var *df-test* 0)

(xref *df-test* 1 2)

(integerp (xref *df-test* 1 2))
(floatp (xref *df-test* 1 2))
(integerp (xref *df-test* 1 3))
(type-of (xref *df-test* 1 3))
(floatp (xref *df-test* 1 3))

(type-of (vector 1 1d0))



(loop )

(xref *df-test* 2 1)
(xref *df-test* 0 0)
(xref *df-test* 1 0)

(xref *df-test* 1 '*)