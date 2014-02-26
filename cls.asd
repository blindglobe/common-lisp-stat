;;  -*- mode: lisp -*-
;;; Time-stamp: <2014-02-25 12:41:45 tony>
;;; Created:    <2005-05-30 17:09:47 blindglobe>
;;; File:       cls.asd
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c) 2005--2013, by AJ Rossini <blindglobe@gmail.com>
;;; License:    MIT, see the file LICENSE.mit in this directory for details.
;;; Purpose:    ASDF specification for Common Lisp Statistical System (CLS)

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

(in-package :cl-user)

(cl:defpackage #:cls-system
    (:use :common-lisp :asdf))

(in-package #:cls-system)

(defsystem "cls"
  :name "Common Lisp Statistical System"
  :version 
     #.(with-open-file
	   (vers (merge-pathnames "version.lisp-expr"
				  *load-truename*))
	 (read vers))
  :author "A.J. Rossini <blindglobe@gmail.com>"
  :license "MIT"
  :description "Common Lisp Statistics (CLS): A System for Statistical
  Computing with Common Lisp; based on Common LispStat (CLS alpha1) by
  Luke Tierney <luke@stat.uiowa.edu> (apparently originally written
  when Luke was at CMU, on leave at Bell Labs?).  Last touched by him
  in 1991, then restarted by AJR in 2005.  Except for TeX/LaTeX, how
  much 14 year old code can honestly stay stable?"
  :serial t
  :depends-on (:alexandria
	       :lift ; unit testing -- use a different system?
	       :clunit ; unit testing next system
	       :xarray ; array-like access?
	       :listoflist ; on xarray
	       :lisp-matrix ; on fnv, cl-blapack, ffa, cffi
	       :fare-csv ; DSV stream processing
	       :cl-variates ;; ? :cl-random  ;; need to select pRNG stream system
	       :gsll ;; GSLL and Antik could provide numerical infrastructure.
	       :antik
	       ;; :cl-cairo2  :cl-2d ;;; one option for graphics
	       :data-format-validation) ;; for David H's dataframe
  :components ((:static-file "version" :pathname #p"version.lisp-expr")
	       (:static-file "LICENSE.mit")
	       (:static-file "README.org")

	       (:module
		"packaging"
		:pathname #p"src/"
		:serial t
		:components
		((:file "packages")
		 (:file "config")))

	       (:module
		"proto-objects"
		:pathname "src/objsys/"
		:serial t
		:depends-on ("packaging")
		:components
		((:file "lsobjects")))

	       (:module "cls-core"
			:pathname "src/basics/"
			:serial t
			:depends-on ("packaging" "proto-objects")
			:components
			((:file "lstypes")
			 (:file "lsfloat")
			 (:file "compound")
			 (:file "lsmacros" 
				:depends-on ("compound"))
			 (:file "lsmath"
				:depends-on ("compound"
					     "lsmacros"
					     "lsfloat"))))
	       (:module "stat-data"
		:pathname "src/data/"
		:depends-on ("packaging"
			     "proto-objects"
			     "cls-core")
		:components
		((:file "dataframe") ; basic -like structure
		 ;; specific implementations based on different underlying storage structures
		 (:file "dataframe-array")
		 (:file "dataframe-matrixlike")
		 (:file "dataframe-listoflist")
		 ;; functions which use the basic structure AND specific implementations
		 (:file "dataframe-functionality")
		 (:file "dataframe-query")
		 (:file "dataframe-initialisation")

		 (:file "data")
		 (:file "data-xls-compat")
		 (:file "import")))

	       (:module
		"cls-basics"
		:pathname "src/basics/"
		:depends-on ("packaging"
			     "proto-objects"
			     "cls-core"
			     ;; "numerics-internal"
			     "stat-data")
		:components
		((:file "lsbasics")))

	       (:module
		"descriptives"
		:pathname "src/describe/"
		:depends-on ("packaging"
			     "proto-objects"
			     "cls-core"
			     ;; "numerics-internal"
			     "stat-data"
			     "cls-basics")
		:components
		((:file "statistics")))
#| need to write this sometime
	       (:module "visualize"
		:pathname "src/visualize/"
		:depends-on ("cls-core")
		:components
		((:file "plot")))
|#
	       ;; Applications
	       (:module
		"example-data"
		:pathname "Data/"
		:depends-on ("packaging"
			     "proto-objects"
			     "cls-core"
			     "cls-basics"
			     "descriptives")
		:components
		((:file "examples")
		 (:file "absorbtion")
		 (:file "diabetes")
		 (:file "leukemia")
		 (:file "randu")
		 (:file "aircraft")
		 (:file "metabolism")
		 (:file "book")
		 (:file "heating")
		 (:file "oxygen")
		 (:file "stackloss") 
		 (:file "car-prices")
		 (:file "iris")
		 (:file "puromycin")
		 (:file "tutorial")))

	       (:module
		 "lisp-stat-unittest"
		:depends-on  ("packaging" "proto-objects"
			      "cls-core"
			      "stat-data"
			      "cls-basics"
			      "descriptives"
			      "example-data")
		 :pathname "src/unittests/"
		 :components ((:file "unittests")
			      (:file "unittests-lstypes" :depends-on ("unittests"))
			      (:file "unittests-specfn" :depends-on ("unittests"))
			      ;;    (:file "unittests-prob" :depends-on ("unittests"))
			      (:file "unittests-proto" :depends-on ("unittests"))
			      (:file "unittests-listoflist" :depends-on ("unittests"))
			      (:file "unittests-arrays" :depends-on ("unittests"))
			      ;;(:file "unittests-dataframe" :depends-on ("unittests"))
			      (:file "unittests-regression" :depends-on ("unittests"))))

	       (:module
		 "lisp-stat-unittest2"
		:depends-on  ("packaging" "proto-objects"
			      "cls-core"
			      "stat-data"
			      "cls-basics"
			      "descriptives"
			      "example-data")
		 :pathname "src/unittests2/"
		 :components ((:file "unittests2")))))
		              ;; (:file "unittests2-lstypes" :depends-on ("unittests2"))
			      ;; (:file "unittests2-specfn" :depends-on ("unittests2"))
			      ;; (:file "unittests2-prob" :depends-on ("unittests2"))
			      ;; (:file "unittests2-proto" :depends-on ("unittests2"))
			      ;; (:file "unittests2-listoflist" :depends-on ("unittests2"))
			      ;; (:file "unittests2-arrays" :depends-on ("unittests2"))
			      ;; (:file "unittests2-dataframe" :depends-on ("unittests2"))
			      ;; (:file "unittests2-regression" :depends-on ("unittests2"))))))


;;; NEED TO ADD A TEST OPERATION, THIS IS THE WRONG VARIANT OF IT.
#|
 (defmethod perform ((o test-op) (c (eql (find-system :cls))))
    (describe (lift:run-tests :suite 'lisp-stat-unittests::lisp-stat-ut)))
|#
