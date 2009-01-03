;;  -*- mode: lisp -*-
;;; Time-stamp: <2008-12-19 08:47:22 tony>
;;; Created:    <2005-05-30 17:09:47 blindglobe>
;;; File:       lispstat.asd
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
	   *lispstat-home-dir*
	   *lispstat-data-dir* *lispstat-examples-dir*))

(in-package :lisp-stat-config)

(defparameter *lispstat-home-dir*
  (directory-namestring
   (truename (asdf:system-definition-pathname :lispstat)))
  "Value considered \"home\" for our data")

#|
  (setf *lispstat-home-dir*
      (directory-namestring (truename (asdf:system-definition-pathname
				       :lispstat))))
|#

(macrolet ((ls-dir (root-str)
	     `(pathname (concatenate 'string
				     (namestring *lispstat-home-dir*) ,root-str)))

	   (ls-defdir (target-dir-var  root-str)
	     `(defvar ,target-dir-var (ls-dir ,root-str))))

  ;; reminder of testing
  ;;(macroexpand '(ls-defdir *lispstat-asdf-dir* "ASDF"))
  ;;(macroexpand-1 '(ls-defdir *lispstat-asdf-dir* "ASDF"))
  ;;(macroexpand-1 '(ls-dir "ASDF"))

  (ls-defdir *lispstat-asdf-dir* "ASDF/")
  (ls-defdir *lispstat-data-dir* "Data/")
  (ls-defdir *lispstat-external-dir* "external/")
)

(pushnew *lispstat-asdf-dir* asdf:*central-registry*)
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

(defpackage #:lispstat-system
    (:use :common-lisp :asdf))

(in-package #:lispstat-system)

;;; To avoid renaming everything from *.lsp to *.lisp...
;;; borrowed from Cyrus Harmon's work, for example for the ch-util.
;;; NOT secure against serving multiple architectures/hardwares from
;;; the same file system (i.e. PPC and x86 would not be
;;; differentiated). 

(defclass lispstat-lsp-source-file (cl-source-file) ())
(defparameter *fasl-directory*
   (make-pathname :directory '(:relative
			       #+sbcl "sbcl-fasl"
			       #+openmcl "openmcl-fasl"
			       #+cmu "cmucl-fasl"
			       #+clisp "clisp-fasl"
			       #-(or sbcl openmcl clisp cmucl) "fasl"
			       )))


;;; Handle Luke's *.lsp suffix
(defmethod source-file-type ((c lispstat-lsp-source-file) (s module)) "lsp")
(defmethod asdf::output-files :around ((operation compile-op)
				       (c lispstat-lsp-source-file))
  (list (merge-pathnames *fasl-directory*
			 (compile-file-pathname (component-pathname c)))))
;;; again, thanks to Cyrus for saving me time...


(defsystem "lispstat"
  :version #.(with-open-file
                 (vers (merge-pathnames "version.lisp-expr" *load-truename*))
               (read vers))
  :author "A.J. Rossini <blindglobe@gmail.com>"
  :license "BSD"
  :description "CommonLispStat (CLS): A System for Statistical
  Computing with Common Lisp; based on CLS alpha1 by Luke Tierney
  <luke@stat.uiowa.edu> (apparently originally written when Luke was
  at CMU, on leave somewhere?).   Last touched by him in 1991, then in
  2005--2009." 
  :serial t
  :depends-on (:cffi :lisp-matrix :lift)
  :components ((:static-file "version" :pathname #p"version.lisp-expr")
	       (:static-file "LICENSE")
	       (:static-file "README")
	       
	       (:module "packaging"
			:pathname #p"src/"
			:components
			((:file "packages")))

	       (:module "proto-objects"
			:pathname "src/objsys/"
			:serial t
			:depends-on ("packaging")
			:components
			((:lispstat-lsp-source-file "lsobjects")))

	       (:module "lispstat-core"
			:pathname "src/basics/"
			:serial t
			:depends-on ("packaging" "proto-objects")
			:components
			(; (:lispstat-lsp-source-file "defsys")
			 (:lispstat-lsp-source-file "lstypes")
			 (:lispstat-lsp-source-file "lsfloat")
			 
			 (:lispstat-lsp-source-file "compound")
			 (:lispstat-lsp-source-file "lsmacros" 
						    :depends-on ("compound"))
			 
			 (:lispstat-lsp-source-file "lsmath"
						    :depends-on ("compound"
								 "lsmacros"
								 "lsfloat"))))

	       (:module
		"numerics-internal"
		:pathname "src/numerics/"
		:depends-on ("packaging" "proto-objects" "lispstat-core")
		:components
		((:lispstat-lsp-source-file "cffiglue")
		 (:lispstat-lsp-source-file "dists"
					    :depends-on ("cffiglue"))
#|
		 (:lispstat-lsp-source-file "matrices"
					    :depends-on ("cffiglue"))
		 (:lispstat-lsp-source-file "ladata"
					    :depends-on ("cffiglue"
							 "matrices"))
		 (:file "linalg"
			:depends-on ("cffiglue"
				     "matrices"
				     "ladata"))
|#
		 ))

	       ;; prototype and CLOS approaches.
	       (:module
		"stat-data"
		:pathname "src/data/"
		:depends-on ("packaging"
			     "proto-objects"
			     "lispstat-core"
			     "numerics-internal")
		:components
		((:file "data-clos")
		 (:file "data")))

	       (:module
		"lispstat-basics"
		:pathname "src/basics/"
		:depends-on ("packaging"
			     "proto-objects"
			     "lispstat-core"
			     "numerics-internal"
			     "stat-data")
		:components
		((:lispstat-lsp-source-file "lsbasics")))


	       (:module
		"descriptives"
		:pathname "src/describe/"
		:depends-on ("packaging"
			     "proto-objects"
			     "lispstat-core"
			     "numerics-internal"
			     "stat-data"
			     "lispstat-basics")
		:components
		((:lispstat-lsp-source-file "statistics")))

	       (:module
		"optimization"
		:pathname "src/numerics/"
		:depends-on ("packaging"
			     "proto-objects"
			     "lispstat-core"
			     "numerics-internal"
			     "stat-data"
			     "lispstat-basics")
		:components
		((:file "optimize")))
		 
	       
	       ;; Applications
	       (:module
		"stat-models"
		:pathname "src/stat-models/"
		:depends-on ("packaging"
			     "proto-objects"
			     "lispstat-core"
			     "numerics-internal"
			     "lispstat-basics"
			     "descriptives"
			     "optimization")
		:components
		((:file "regression")
		 ;; (:lispstat-lsp-source-file "nonlin"
		 ;;	  :depends-on ("regression"))
		 ;; (:lispstat-lsp-source-file "bayes"
		 ;;	  :depends-on ("regression"))
		 ))

	       ;; Applications
	       (:module
		"example-data"
		:pathname "Data/"
		:depends-on ("packaging"
			     "proto-objects"
			     "lispstat-core"
			     "numerics-internal"
			     "lispstat-basics"
			     "descriptives"
			     "optimization")
		:components
		((:file "examples")
		 (:lispstat-lsp-source-file "absorbtion")
		 (:lispstat-lsp-source-file "diabetes")
		 (:lispstat-lsp-source-file "leukemia")
		 (:lispstat-lsp-source-file "randu")
		 (:lispstat-lsp-source-file "aircraft")
		 (:lispstat-lsp-source-file "metabolism")
		 (:lispstat-lsp-source-file "book")
		 (:lispstat-lsp-source-file "heating")
		 (:lispstat-lsp-source-file "oxygen")
		 (:lispstat-lsp-source-file "stackloss") 
		 (:lispstat-lsp-source-file "car-prices")
		 (:lispstat-lsp-source-file "iris")
		 (:lispstat-lsp-source-file "puromycin")
		 (:lispstat-lsp-source-file "tutorial")))

	       (:module
		 "lisp-stat-unittest"
		:depends-on  ("packaging" "proto-objects"
			      "lispstat-core"
			      "numerics-internal" 
			      "stat-data"
			      "lispstat-basics"
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
			      ;; not ready yet:
			      ;; (:file "unittests-data-clos" :depends-on ("unittests"))
			      ;; changing linear algebra system
			      ;; (:file "unittests-arrays" :depends-on ("unittests"))
			      ))))
