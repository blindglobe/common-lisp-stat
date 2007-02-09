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

  :depends-on (:cffi )
  
  ;; FFI systems:
  ;; kclglue.lsp  kclpatch.lsp exclglue.lsp mclglue.lsp 

  :components ((:static-file "version" :pathname #p"version.lisp-expr")
	       (:lispstat-lsp-source-file "lsobjects")
	       (:lispstat-lsp-source-file "fastmap")
	       (:lispstat-lsp-source-file "lspackages"
					  :depends-on ("fastmap"
						       "lsobjects"))
	       ;; ls-basisc
	       (:lispstat-lsp-source-file "compound" 
					  :depends-on ("lsobjects"
						       "fastmap"))
	       (:lispstat-lsp-source-file "lsmacros" 
					  :depends-on ("compound"))
	       (:lispstat-lsp-source-file "lsbasics"
					  :depends-on ("lsobjects"
						       "lsmacros")) 
	       (:lispstat-lsp-source-file "dists"
					  :depends-on ("lsbasics"))
	       (:lispstat-lsp-source-file "ladata"
					  :depends-on ("lsbasics")) ;; in lisp-stat-basics
	       (:lispstat-lsp-source-file "linalg"
					  :depends-on ("ladata")) ;; in lisp-stat-basics
	       (:lispstat-lsp-source-file "matrices"
					  :depends-on ("lsbasics"))
	       (:lispstat-lsp-source-file "lsfloat"
					  :depends-on ("lsbasics")) ;; in lisp-stat-basics
	       (:lispstat-lsp-source-file "lsmath"
					  :depends-on ("lsbasics"))
	       
	       ;; Applications
	       (:lispstat-lsp-source-file "regression"
					  :depends-on ("lsbasics"))
	       (:lispstat-lsp-source-file "nonlin"
					  :depends-on ("regression"))
	       (:lispstat-lsp-source-file "statistics"
					  :depends-on ("lsobjects"))
	       (:lispstat-lsp-source-file "maximize"
					  :depends-on ("lsobjects"))
	       (:lispstat-lsp-source-file "bayes"
					  :depends-on ("lsobjects"
						       "dists")) ; in lisp-stat
	       
	       ;;(:lispstat-lsp-source-file "numlib") ;; do we need this?
	       (:lispstat-lsp-source-file "defsys")
	       (:lispstat-lsp-source-file "lstoplevel")))
