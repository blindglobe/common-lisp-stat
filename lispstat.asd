;;  -*- mode: lisp -*-

;;; Copyright (c) 2005--2006, by AJ Rossini <blindglobe@gmail.com>
;;; ASDF packaging for CommonLispStat

;;(asdf:oos 'asdf:load-op 'cffi)


(defpackage #:lispstat-system
  (:use :asdf :common-lisp))

(in-package #:lispstat-system)


;;; To avoid renaming everything from *.lsp to *.lisp...
;;; borrowed from Cyrus Harmon's work, for example for the ch-util.
(defclass lispstat-lsp-source-file (lsp-source-file) ())
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

  :components (
	       (:file "lsobjects") ;; :depends-on ("lspackages"))
	       ;;(:file "lspackages") ?? what good is this??
	       (:file "fastmap" :depends-on ("lsobjects"))
	       (:file "compound" :depends-on ("lsobjects" "fastmap"))
	       (:file "lsmacros" :depends-on ("compound"))
	       (:file "lsbasics" :depends-on ("lsobjects" "lsmacros"))
	       (:file "dists" :depends-on ("lsbasics"))
	       (:file "ladata" :depends-on ("lsbasics"))
	       (:file "linalg" :depends-on ("ladata"))
	       (:file "matrices" :depends-on ("lsbasics"))
	       (:file "lsfloat" :depends-on ("lsbasics"))
	       (:file "lsmath" :depends-on ("lsbasics"))
	       
	       ;; Applications
	       (:file "regression" :depends-on ("lsobjects"))
	       (:file "nonlin" :depends-on ("lsobjects" "regression"))
	       (:file "statistics" :depends-on ("lsobjects"))
	       (:file "maximize" :depends-on ("lsobjects"))
	       (:file "bayes" :depends-on ("lsobjects"
					   "dists"))
	       
	       (:file "numlib")
	       (:file "defsys")
	       (:file "lspackages")
	       (:file "lstoplevel")

	       ;; Testing comes last!
	       ;; (:file "unittests" :depends-on ("lsobjects"))
	       ))