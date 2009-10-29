;;; -*- mode: lisp -*-

;;; Time-stamp: <2009-09-21 22:53:53 tony>
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

  ;; (asdf:oos 'asdf:load-op 'xarray)
  ;; (asdf:oos 'asdf:load-op 'cl-opengl)
  ;; (asdf:oos 'asdf:load-op 'tinaa)

  (init-CLS))

(in-package :lisp-stat-unittests)

;; tests = 80, failures = 7, errors = 21
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

;;; Experiments with cl-variates

;; (asdf:oos 'asdf:compile-op 'cl-variates :force t)
;; (asdf:oos 'asdf:compile-op 'cl-variates-test :force t)
;; (asdf:oos 'asdf:load-op 'lift)
;; (asdf:oos 'asdf:load-op 'cl-variates)
(asdf:oos 'asdf:load-op 'cl-variates-test)

(in-package :cl-variates-test)
;; check tests
(run-tests :suite 'cl-variates-test)
(describe (run-tests :suite 'cl-variates-test))

(in-package :cl-variates-user)
;; example usage
(defparameter state (make-random-number-generator))
(setf (random-seed state) 44)
(random-seed state)
(loop for i from 1 to 10 collect
                  (random-range state 0 10))
;; => (1 5 1 0 7 1 2 2 8 10)
(setf (random-seed state) 44)
(loop for i from 1 to 10 collect
                  (random-range state 0 10))
;; => (1 5 1 0 7 1 2 2 8 10)

(setf (random-seed state) 44)
(random-seed state)
(loop for i from 1 to 10 collect
                  (normal-random state 0 1))
;; => 
;; (-1.2968656102820426 0.40746363934173213 -0.8594712469518473 0.8795681301148328
;;  1.0731526250004264 -0.8161629082481728 0.7001813608754809 0.1078045427044097
;;  0.20750134211656893 -0.14501914108452274)

(setf (random-seed state) 44)
(loop for i from 1 to 10 collect
                  (normal-random state 0 1))
;; => 
;; (-1.2968656102820426 0.40746363934173213 -0.8594712469518473 0.8795681301148328
;;  1.0731526250004264 -0.8161629082481728 0.7001813608754809 0.1078045427044097
;;  0.20750134211656893 -0.14501914108452274)


;;; experiments with LLA
(in-package :cl-user)
(asdf:oos 'asdf:load-op 'lla)
(in-package :lla-user)

