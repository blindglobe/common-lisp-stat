;;  -*- mode: lisp -*-
;;; Time-stamp: <2012-12-19 16:49:32 tony>
;;; Created:    <2005-05-30 17:09:47 blindglobe>
;;; File:       cls.asd
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c) 2005--2013, by AJ Rossini <blindglobe@gmail.com>
;;; License:    MIT, see the file LICENSE.mit in this directory for details.
;;; Purpose:    ASDF specification for Common Lisp Statistical System

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

;; Load ASDF if it isn't loaded -- BUT we need to ensure that we sync
;; properly with the new ASDF!  Maybe we should make the git repo a
;; submodule, and tag a particular version to use, pulling that
;; version?  However, most "modern" CLs use the "new" (>=v2) ASDF.

#-asdf(load (pathname (concatenate 'string (namestring *cls-external-dir*) "asdf")))

(in-package :cl-user)

;;(pushnew *cls-asdf-dir* asdf:*central-registry*)
;;(pushnew #p"C:/Lisp/libs/" asdf-util:*source-dirs* :test #'equal) ; eg for Microsoft

(cl:defpackage #:cls-system
    (:use :common-lisp :asdf))

(in-package #:cls-system)

;;; To avoid renaming everything from *.lsp to *.lisp...  borrowed
;;; from Cyrus Harmon's work, for example for the ch-util.  NOT secure
;;; against serving multiple architectures/hardwares from the same
;;; file system (i.e. PPC and x86 would not be differentiated).
;;; However, this might be more of a solution for quicklisp?

(defclass cls-lsp-source-file (cl-source-file) ())
(defparameter *fasl-directory*
   (make-pathname :directory '(:relative
			       #+sbcl "fasl-sbcl"
			       #+openmcl "fasl-ccl"
			       #+openmcl "fasl-ccl"
			       #+cmu "fasl-cmucl"
			       #+clisp "fasl-clisp"
			       #-(or sbcl openmcl clisp cmucl) "fasl"
			       )))

;;; Handle Luke's *.lsp suffix
(defmethod source-file-type ((c cls-lsp-source-file) (s module)) "lsp")
(defmethod asdf::output-files :around ((operation compile-op)
				       (c cls-lsp-source-file))
  (list (merge-pathnames *fasl-directory*
			 (compile-file-pathname (component-pathname c)))))
;;; again, thanks to Cyrus for saving me time...


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
  :depends-on (;;  :cldoc  ;; documentation tool? (not Lit Prog, but coding support)
	       ;;  :cffi   ;; only needed within lisp-matrix
	       :xarray
	       :lisp-matrix ;; on fnv, cl-blapack, ffa
	       :listoflist
	       :lift
	       :fare-csv
	       ;;; need to select pRNG stream system
	       ;; :cl-random ;; or cl-variates, or...?
	       :cl-variates
 	       ;;; if graphics exist, then...
	       ;; :cl-cairo2  :cl-2d

	       ;; we are experimenting with GSLL and Antik for the
	       ;; numerical infrastructure.
	       :gsll 
	       :antik
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
		((:cls-lsp-source-file "lsobjects")))

	       (:module "cls-core"
			:pathname "src/basics/"
			:serial t
			:depends-on ("packaging" "proto-objects")
			:components
			((:cls-lsp-source-file "lstypes")
			 (:cls-lsp-source-file "lsfloat")
			 
			 (:cls-lsp-source-file "compound")
			 (:cls-lsp-source-file "lsmacros" 
						    :depends-on ("compound"))
			 
			 (:cls-lsp-source-file "lsmath"
						    :depends-on ("compound"
								 "lsmacros"
								 "lsfloat"))))

;; 	       (:module
;; 		"numerics-internal"
;; 		:pathname "src/numerics/"
;; 		:depends-on ("packaging" "proto-objects" "cls-core")
;; 		:components
;; 		((:cls-lsp-source-file "cffiglue")
;; 		 (:cls-lsp-source-file "dists"
;; 					    :depends-on ("cffiglue"))
;; #|
;; 		 (:cls-lsp-source-file "matrices"
;; 					    :depends-on ("cffiglue"))
;; 		 (:cls-lsp-source-file "ladata"
;; 					    :depends-on ("cffiglue"
;; 							 "matrices"))
;; 		 (:file "linalg"
;; 			:depends-on ("cffiglue"
;; 				     "matrices"
;; 				     "ladata"))
;; |#
;; 		 ))


	       ;; Dataframes and statistical structures.
	       (:module
		"stat-data"
		:pathname "src/data/"
		:depends-on ("packaging"
			     "proto-objects"
			     "cls-core"
			     ;; "numerics-internal"
			     )
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
			     ;; "numerics-internal"
			     "stat-data")
		:components
		((:cls-lsp-source-file "lsbasics")))


	       
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
		((:cls-lsp-source-file "statistics")))
#|
	       (:module
		"visualize"
		:pathname "src/visualize/"
		:depends-on ("cls-core")
		:components
		((:file "plot")))
|#

#|

;;; FIXME:  OPTIMIZE USES ORIG CLS CFFI-based LIBCODE, NEED TO REPLACE WITH ALT MINIMIZERS
	       (:module
		"optimization"
		:pathname "src/numerics/"
		:depends-on ("packaging"
			     "proto-objects"
			     "cls-core"
			     ;; "numerics-internal"
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
			     ;;   "numerics-internal"
			     "cls-basics"
			     "descriptives"
			     "optimization")
		:components
		((:file "regression")
		 ;; (:cls-lsp-source-file "nonlin"
		 ;;	  :depends-on ("regression"))
		 ;; (:cls-lsp-source-file "bayes"
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
			     ;; "numerics-internal"
			     "cls-basics"
			     "descriptives"
			     ;;  "optimization"
			     )
		:components
		((:file "examples")
		 (:cls-lsp-source-file "absorbtion")
		 (:cls-lsp-source-file "diabetes")
		 (:cls-lsp-source-file "leukemia")
		 (:cls-lsp-source-file "randu")
		 (:cls-lsp-source-file "aircraft")
		 (:cls-lsp-source-file "metabolism")
		 (:cls-lsp-source-file "book")
		 (:cls-lsp-source-file "heating")
		 (:cls-lsp-source-file "oxygen")
		 (:cls-lsp-source-file "stackloss") 
		 (:cls-lsp-source-file "car-prices")
		 (:cls-lsp-source-file "iris")
		 (:cls-lsp-source-file "puromycin")
		 (:cls-lsp-source-file "tutorial")))

	       (:module
		 "lisp-stat-unittest"
		:depends-on  ("packaging" "proto-objects"
			      "cls-core"
			      ;; "numerics-internal" 
			      "stat-data"
			      "cls-basics"
			      "descriptives"
			      ;; "optimization"
			      ;; "stat-models"
			      "example-data")
		 :pathname "src/unittests/"
		 :components ((:file "unittests")
			      (:file "unittests-lstypes" :depends-on ("unittests"))
			      (:file "unittests-specfn" :depends-on ("unittests"))
			      ;;    (:file "unittests-prob" :depends-on ("unittests"))
			      (:file "unittests-proto" :depends-on ("unittests"))
			      (:file "unittests-regression" :depends-on ("unittests"))
			      (:file "unittests-listoflist" :depends-on ("unittests"))
			      (:file "unittests-arrays" :depends-on ("unittests"))
			      (:file "unittests-dataframe" :depends-on ("unittests"))))))

#|

;;; NEED TO ADD A TEST OPERATION
 (defmethod perform ((o test-op) (c (eql (find-system :cls))))
    (describe (lift:run-tests :suite 'lisp-stat-unittests::lisp-stat-ut)))
|#
