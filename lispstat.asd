;;  -*- mode: lisp -*-

;;; Copyright (c) 2005--2006, by AJ Rossini <blindglobe@gmail.com>
;;; ASDF packaging for CommonLispStat

;;(asdf:oos 'asdf:load-op 'cffi)

(defpackage #:lispstat-system
  (:use :asdf :common-lisp))

(in-package #:lispstat-system)

;;; To avoid renaming everything from *.lsp to *.lisp...
;;; borrowed from Cyrus Harmon's work, for example for the ch-util.
(defclass lispstat-lsp-source-file (cl-source-file) ())
(defparameter *fasl-directory*
   (make-pathname :directory '(:relative #+sbcl "sbcl-fasl"
			      #+openmcl "openmcl-fasl"
			      #-(or sbcl openmcl) "fasl")))

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
  :description "CommonLispStat (CLS): A System for Statistical Computing with Common Lisp;
based on CLS by Luke Tierney <luke@stat.uiowa.edu> (originally written when Luke was at CMU, apparently).
Last touched 1991, then in 2005--2007."
  :serial t
  :depends-on (:cffi )
  :components ((:static-file "version" :pathname #p"version.lisp-expr")
	       (:lispstat-lsp-source-file "lsobjects")
	       (:lispstat-lsp-source-file "fastmap")
	       (:lispstat-lsp-source-file "lstypes")
	       (:lispstat-lsp-source-file "lsfloat")
	       (:lispstat-lsp-source-file "compound" 
					  :depends-on ("lsobjects"
						       "fastmap"))
	       (:lispstat-lsp-source-file "lsmacros" 
					  :depends-on ("compound"))
	       (:lispstat-lsp-source-file "sequence"
					  :depends-on ("compound"))
	       (:lispstat-lsp-source-file "matrices"
					  :depends-on ("sequence"))
	       (:lispstat-lsp-source-file "linalg"
					  :depends-on ("lstypes"
						       "matrices"))
	       (:lispstat-lsp-source-file "lsbasics"
					  :depends-on ("lsobjects"
						       "lstypes"
						       "lsmacros"
						       "sequence"
						       "lsfloat"
						       "matrices"
						       "linalg"))

	       (:lispstat-lsp-source-file "lsmath"
					  :depends-on ("lsbasics"
						       "lsmacros"
						       "lsfloat"))

	       (:lispstat-lsp-source-file "dists"
					  :depends-on ("lsbasics"))
	       (:lispstat-lsp-source-file "ladata"
					  :depends-on ("lsbasics")) ;; in lisp-stat-basics


	       ;; mix/match lsp vs. lisp in next 2.
	       (:file "data" :depends-on ("lsobjects"
					  "compound"))
	       
	       ;; Applications
	       (:lispstat-lsp-source-file "regression"
					  :depends-on ("lsbasics"
						       "lsobjects"))
	       (:lispstat-lsp-source-file "nonlin"
					  :depends-on ("regression"))
	       (:lispstat-lsp-source-file "statistics"
					  :depends-on ("lsobjects"
						       "data" ))
	       (:file "optimize" :depends-on ("lsobjects"))
	       (:lispstat-lsp-source-file "bayes"
					  :depends-on ("lsobjects"
						       "dists")) ; in lisp-stat

	       ;;; Top level
	       (:file "ls-user" :depends-on ("statistics"
						    "data"))))
