;;  -*- mode: lisp -*-
;;; Time-stamp: <2009-10-02 09:00:12 tony>
;;; Created:    <2005-05-30 17:09:47 blindglobe>
;;; File:       cls.asd
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c) 2005--2008, by AJ Rossini <blindglobe@gmail.com>
;;; License:    BSD, see the top level directory file LICENSE for details.
;;; Purpose:    ASDF packaging for CommonLisp Stat

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

(in-package :cl-user)

(defpackage :lisp-stat-config
  (:use :common-lisp)
  (:export *default-path*
	   *lsos-files* *basic-files* *ls-files*
	   *cls-home-dir*
	   *cls-data-dir* *cls-examples-dir*))

(in-package :lisp-stat-config)

(defparameter *cls-home-dir*
  (directory-namestring
   (truename (asdf:system-definition-pathname :cls)))
  "Value considered \"home\" for our data")

(macrolet ((ls-dir (root-str)
	     `(pathname (concatenate 'string
				     (namestring *cls-home-dir*) ,root-str)))

	   (ls-defdir (target-dir-var  root-str)
	     `(defvar ,target-dir-var (ls-dir ,root-str))))

  ;; reminder of testing
  ;;(macroexpand '(ls-defdir *cls-asdf-dir* "ASDF"))
  ;;(macroexpand-1 '(ls-defdir *cls-asdf-dir* "ASDF"))
  ;;(macroexpand-1 '(ls-dir "ASDF"))

  (ls-defdir *cls-asdf-dir* "ASDF/")
  (ls-defdir *cls-data-dir* "Data/")
  (ls-defdir *cls-external-dir* "external/")
)

;;(pushnew *cls-asdf-dir* asdf:*central-registry*)


;; (pushnew #p"C:/Lisp/libs/" asdf-util:*source-dirs* :test #'equal) ; eg for Microsoft

;;; back to our regularly scheduled work...
;;; We should not need these, I think, but?
;; (asdf:oos 'asdf:compile-op :cffi)            ;; FFI
;; (asdf:oos 'asdf:compile-op :lift)            ;; Unit Testing 
;; (asdf:oos 'asdf:load-op :cffi)            ;; FFI
;; (asdf:oos 'asdf:load-op :lift)            ;; Unit Testing 

;;; MAJOR HACK, FIXME!
;;(load "/media/disk/Desktop/sandbox/matlisp.git/start.lisp")

(in-package :cl-user)

(defpackage #:cls-system
    (:use :common-lisp :asdf))

(in-package #:cls-system)

;;; To avoid renaming everything from *.lsp to *.lisp...
;;; borrowed from Cyrus Harmon's work, for example for the ch-util.
;;; NOT secure against serving multiple architectures/hardwares from
;;; the same file system (i.e. PPC and x86 would not be
;;; differentiated). 

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
  :version #.(with-open-file
                 (vers (merge-pathnames "version.lisp-expr" *load-truename*))
               (read vers))
  :author "A.J. Rossini <blindglobe@gmail.com>"
  :license "BSD"
  :description "Common Lisp Statistics (CLS): A System for Statistical
  Computing with Common Lisp; based on CLS alpha1 by Luke Tierney
  <luke@stat.uiowa.edu> (apparently originally written when Luke was
  at CMU, on leave at Bell Labs?).  Last touched by him in 1991, then
  by AJR in 2005--2009."
  :serial t
  :depends-on (:cffi
	       :xarray
	       :lisp-matrix
	       :lift
	       :rsm-string
	       ;;    :cl-cairo2  :cl-2d
	       )
  :components ((:static-file "version" :pathname #p"version.lisp-expr")
	       (:static-file "LICENSE")
	       (:static-file "README")

	       ;; reading in DSV files for data access
#|
	       (:module 
		"csv-data"
		:pathname "src/data/"
		:components
		((:file "test-cybertiggyr")
		 (:file "dsv-cybertiggyr" :depends-on ( "test-cybertiggyr"))))
|#	       
	       (:module
		"packaging"
		:pathname #p"src/"
		;; :depends-on ("csv-data")
		:components
		((:file "packages")))

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
			(; (:cls-lsp-source-file "defsys")
			 (:cls-lsp-source-file "lstypes")
			 (:cls-lsp-source-file "lsfloat")
			 
			 (:cls-lsp-source-file "compound")
			 (:cls-lsp-source-file "lsmacros" 
						    :depends-on ("compound"))
			 
			 (:cls-lsp-source-file "lsmath"
						    :depends-on ("compound"
								 "lsmacros"
								 "lsfloat"))))

	       (:module
		"numerics-internal"
		:pathname "src/numerics/"
		:depends-on ("packaging" "proto-objects" "cls-core")
		:components
		((:cls-lsp-source-file "cffiglue")
		 (:cls-lsp-source-file "dists"
					    :depends-on ("cffiglue"))
#|
		 (:cls-lsp-source-file "matrices"
					    :depends-on ("cffiglue"))
		 (:cls-lsp-source-file "ladata"
					    :depends-on ("cffiglue"
							 "matrices"))
		 (:file "linalg"
			:depends-on ("cffiglue"
				     "matrices"
				     "ladata"))
|#
		 ))


	       ;; Dataframes and statistical structures.
	       (:module
		"stat-data"
		:pathname "src/data/"
		:depends-on ("packaging"
			     "proto-objects"
			     "cls-core"
			     "numerics-internal")
		:components
		((:file "dataframe")
		 (:file "dataframe-array")
		 (:file "dataframe-matrixlike")
		 (:file "dataframe-listoflist")
		 (:file "data")
		 (:file "import")
		 ;;(:file "listoflist")
		 ))

	       (:module
		"cls-basics"
		:pathname "src/basics/"
		:depends-on ("packaging"
			     "proto-objects"
			     "cls-core"
			     "numerics-internal"
			     "stat-data")
		:components
		((:cls-lsp-source-file "lsbasics")))


	       
	       (:module
		"descriptives"
		:pathname "src/describe/"
		:depends-on ("packaging"
			     "proto-objects"
			     "cls-core"
			     "numerics-internal"
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
	       (:module
		"optimization"
		:pathname "src/numerics/"
		:depends-on ("packaging"
			     "proto-objects"
			     "cls-core"
			     "numerics-internal"
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
			     "numerics-internal"
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

	       ;; Applications
	       (:module
		"example-data"
		:pathname "Data/"
		:depends-on ("packaging"
			     "proto-objects"
			     "cls-core"
			     "numerics-internal"
			     "cls-basics"
			     "descriptives"
			     "optimization")
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
			      "numerics-internal" 
			      "stat-data"
			      "cls-basics"
			      "descriptives"
			      "optimization"
			      "stat-models"
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
