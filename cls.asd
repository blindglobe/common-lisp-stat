;;  -*- mode: lisp -*-
;;; Time-stamp: <2010-11-30 17:58:30 tony>
;;; Created:    <2005-05-30 17:09:47 blindglobe>
;;; File:       cls.asd
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c) 2005--2010, by AJ Rossini <blindglobe@gmail.com>
;;; License:    MIT, see the file LICENSE.mit in this directory for details.
;;; Purpose:    ASDF specification for Common Lisp Statistical System

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.


(in-package :cl-user)


(cl:defpackage #:cls-system
    (:use :common-lisp :asdf))

(in-package #:cls-system)


(defsystem "cls"
  :name "Common Lisp Statistical System"
  :version #.(with-open-file
                 (vers (merge-pathnames "version.lisp-expr" *load-truename*))
               (read vers))
  :author "A.J. Rossini <blindglobe@gmail.com>"
  :license "MIT"
  :description "Common Lisp Statistics (CLS): A System for Statistical
  Computing with Common Lisp; based on Common LispStat (CLS alpha1) by
  Luke Tierney <luke@stat.uiowa.edu> (apparently originally written
  when Luke was at CMU, on leave at Bell Labs?).  Last touched by him
  in 1991, then by AJR starting in 2005."
  :serial t
  :depends-on (:cldoc
	       :cffi
	       :xarray
	       :lift
	       :lisp-matrix ; on fnv, cl-blapack, ffa
	       :listoflist
	       :rsm-string
	       ;;; need to select pRNG stream system
	       ;; :cl-random ;; or cl-variates, or...?
	       :cl-variates
 	       ;;; if graphics exist, then...
	       ;; :cl-cairo2  :cl-2d

	       )
  :components ((:static-file "version" :pathname #p"version.lisp-expr")
	       (:static-file "LICENSE.mit")
	       (:static-file "README")

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

	       


	       ;; Dataframes and statistical structures.
	       (:module
		"stat-data"
		:pathname "src/data/"
		:depends-on ("packaging"
			     "proto-objects"
			     "cls-core")
		:components
		((:file "dataframe")
		 (:file "dataframe-array")
		 (:file "dataframe-matrixlike")
		 (:file "dataframe-listoflist")
		 (:file "data")
		 (:file "data-xls-compat")
		 (:file "import")))

	       (:module
		"cls-basics"
		:pathname "src/basics/"
		:depends-on ("packaging"
			     "proto-objects"
			     "cls-core"
			     "stat-data")
		:components
		((:file "lsbasics")))


	       
	       (:module
		"descriptives"
		:pathname "src/describe/"
		:depends-on ("packaging"
			     "proto-objects"
			     "cls-core"
			     "stat-data"
			     "cls-basics")
		:components
		((:file "statistics")))

	       (:module
		"visualize"
		:pathname "src/visualize/"
		:depends-on ("cls-core")
		:components
		((:file "plot")))

#|	       (:module
		"optimization"
		:pathname "src/numerics/"
		:depends-on ("packaging"
			     "proto-objects"
			     "cls-core"
			     "stat-data"
			     "cls-basics")
		:components
	       ((:file "optimize")))
		 
	       
	       ;; Applications
	       (:module
		"stat-models"
		:pathname "src/stat-models/"
		:depends-on ("packaging"
			     "proto-objects"
			     "cls-core"
			     "cls-basics"
			     "descriptives"
			     "optimization")
		:components
		((:file "regression")
		 ;; (:file "nonlin"
		 ;;	  :depends-on ("regression"))
		 ;; (:file "bayes"
		 ;;	  :depends-on ("regression"))
		 ))
	       |#
	       
	       ;; Applications
	       (:module
		"example-data"
		:pathname "Data/"
		:depends-on ("packaging"
			     "proto-objects"
			     "cls-core"
			     "cls-basics"
			     "descriptives"
			    ; "optimization"
			     )
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
;			      "optimization"
;			      "stat-models"
			      "example-data")
		 :pathname "src/unittests/"
		 :components ((:file "unittests")
			      (:file "unittests-lstypes" :depends-on ("unittests"))
			      (:file "unittests-specfn" :depends-on ("unittests"))
			      (:file "unittests-prob" :depends-on ("unittests"))
			      (:file "unittests-proto" :depends-on ("unittests"))
			      (:file "unittests-regression" :depends-on ("unittests"))
			      (:file "unittests-listoflist" :depends-on ("unittests"))
			      (:file "unittests-arrays" :depends-on ("unittests"))
			      (:file "unittests-dataframe" :depends-on ("unittests"))))))

#|
 (defmethod perform ((o test-op) (c (eql (find-system :cls))))
    (describe (lift:run-tests :suite 'lisp-stat-unittests::lisp-stat-ut)))
|#
