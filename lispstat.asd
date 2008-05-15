;;  -*- mode: lisp -*-
;;; Copyright (c) 2005--2008, by AJ Rossini <blindglobe@gmail.com>
;;; ASDF packaging for CommonLisp Stat
;;; License: BSD, see the top level directory file LICENSE for details.
;;; Time-stamp: <2008-05-15 12:51:47 tony>
;;; Created:    <2005-05-30 17:09:47 blindglobe>


;; (setf *my-base-directory*
;;       #p"/home/tony/sandbox/CLS.git/"
;;       #p"/Users/ungil/lisp/CommonLispStat/")




;; What package should we be in?  Contaminating cl-user is probably EVIL.
(in-package :cl-user)


(defvar *lispstat-home-dir*
  ;; #p"/home/tony/sandbox/CommonLispStat.git/"
  ;; #p"/cygdrive/c/local/sandbox/Lisp/CommonLispStat/"
  ;; #p"/Users/ungil/lisp/CommonLispStat/")
  ;; #p"/home/rossini/public_html/GIT.repos/CommonLispStat/"
  #p"/home/tony/Desktop/sandbox/CLS.git/"
  "Value considered \"home\" for our data")

(setf *lispstat-home-dir*
      ;; #p"/cygdrive/c/local/sandbox/Lisp/CommonLispStat/"
      ;; #p"/home/tony/sandbox/CommonLispStat.git/"
      ;; #p"/Users/ungil/lisp/CommonLispStat/")
      ;; #p"/home/rossini/public_html/GIT.repos/CommonLispStat/"
      #p"/home/tony/Desktop/sandbox/CLS.git/"
      )

(macrolet ((ls-dir (root-str)
	     `(pathname (concatenate 'string
				     (namestring *lispstat-home-dir*) ,root-str)))

	   (ls-defdir (target-dir-var  root-str)
	     `(defvar ,target-dir-var (ls-dir ,root-str))))

  ;;(macroexpand '(ls-defdir *lispstat-asdf-dir* "ASDF"))
  ;;(macroexpand-1 '(ls-defdir *lispstat-asdf-dir* "ASDF"))
  ;;(macroexpand-1 '(ls-dir "ASDF"))

  (ls-defdir *lispstat-asdf-dir* "ASDF/")
  (ls-defdir *lispstat-data-dir* "data/")
  (ls-defdir *lispstat-external-dir* "external/")
  (ls-defdir *lispstat-examples-dir* "examples/"))

(pushnew *lispstat-asdf-dir* asdf:*central-registry*)
;; (pushnew #p"C:/Lisp/libs/" asdf-util:*source-dirs* :test #'equal)

;;; back to our regularly scheduled work...

(in-package :cl-user)

(defpackage #:lispstat-system
    (:use :asdf :common-lisp))

(in-package #:lispstat-system)

;;; To avoid renaming everything from *.lsp to *.lisp...
;;; borrowed from Cyrus Harmon's work, for example for the ch-util.
(defclass lispstat-lsp-source-file (cl-source-file) ())
(defparameter *fasl-directory*
   (make-pathname :directory '(:relative
			       #+sbcl "sbcl-fasl"
			       #+openmcl "openmcl-fasl"
			       #+cmu "cmucl-fasl"
			       #+clisp "clisp-fasl"
			       #-(or sbcl openmcl clisp cmucl) "fasl"
			       )))

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
  :depends-on (:cffi :lift) ;;  :clem) not yet but soon!
  :components ((:static-file "version" :pathname #p"version.lisp-expr")
	       (:lispstat-lsp-source-file "lsobjects")
	       (:lispstat-lsp-source-file "cffiglue")
	       (:lispstat-lsp-source-file "defsys")
	       (:lispstat-lsp-source-file "lstypes")
	       (:lispstat-lsp-source-file "lsfloat")

	       (:lispstat-lsp-source-file "compound" 
					  :depends-on ("lsobjects"))
	       (:lispstat-lsp-source-file "lsmacros" 
					  :depends-on ("compound"))

	       (:lispstat-lsp-source-file "dists"
					  :depends-on ("cffiglue"
						       "lsmacros"))

	       (:lispstat-lsp-source-file "lsmath"
					  :depends-on ("lsobjects"
						       "compound"
						       "lsmacros"
						       "lsfloat"))


	       (:lispstat-lsp-source-file "matrices"
					  :depends-on ("cffiglue"
						       "compound"))

	       (:lispstat-lsp-source-file "ladata"
					  :depends-on ("cffiglue"
						       "defsys"
						       "lstypes"
						       "compound"
						       "matrices"))

	       (:lispstat-lsp-source-file "linalg"
					  :depends-on ("cffiglue"
						       "lsmath"
						       "matrices"
						       "ladata"
						       "lsfloat"
						       "lstypes"
						       "compound"))

	       (:file "data" :depends-on ("lsobjects"
					  "compound"
					  "matrices"
					  "linalg"))

	       ;; there is a circ reference which we need to solve.
	       (:lispstat-lsp-source-file "lsbasics"
					  :depends-on ("lsobjects"
						       "lstypes"
						       "lsmacros"
						       "lsfloat"
						       "matrices"
						       "linalg"
						       "dists"))

	       (:lispstat-lsp-source-file "statistics"
					  :depends-on ("lsobjects"
						       "lsbasics"
						       "compound"
						       "ladata" "matrices" "linalg"
						       "lsmath"
						       "data" ))

	       (:file "optimize" :depends-on ("lsobjects"
					      "cffiglue"
					      "lstypes"
					      "compound"
					      "lsmath"
					      "lsfloat"
					      "lsbasics"
					      "matrices"
					      "ladata"
					      "linalg"))
	       
	       ;; Applications
	       (:lispstat-lsp-source-file "regression"
					  :depends-on ("lsobjects"
						       "lsbasics"
						       "compound"
						       "lsmath"
						       "matrices"
						       "linalg"
						       "statistics"))
;	       (:lispstat-lsp-source-file "nonlin"
;					  :depends-on ("regression"))

;	       (:lispstat-lsp-source-file "bayes"
;					  :depends-on ("lsobjects"
;						       "lsmath"
;						       "dists"))


	       (:file "ls-user" :depends-on ("lsobjects"
					     "lsbasics"
					     "compound"
					     "dists"
					     "lstypes"
					     "lsfloat"
					     "data"
					     "lsmath"
					     "matrices"
					     "linalg"
					     "statistics"
					     "regression"))


	       (:file "unittests" :depends-on ("ls-user"))

	       ))

