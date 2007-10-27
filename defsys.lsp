;;; -*- mode: lisp -*-

(in-package :cl-user)

(defpackage :lisp-stat-config
  (:use :common-lisp)
  (:export *default-path*
	   *lsos-files* *basic-files* *ls-files*

	   *lispstat-data-dir* *lispstat-examples-dir*
	   ))

(in-package :lisp-stat-config)

;; KCL
;; (proclaim '(optimize (safety 2) (space 3) (speed 3)))
;; (setf *break-enable* nil)


;;;;
;;;; EXCL (Allegro)
;;;;

;; (setf *read-default-float-format* 'double-float)

(defvar *common-lisp-stat-version* "1.0 Alpha 1")

(defvar *default-path* "./")

;;;
;;; Functions for switching into and out of debug mode
;;;
;;; Note that ANSI Common Lisp post-dates these, we need to ensure
;;; that CL's signal handling is taken into consideration.


(defun ls-debug-on (signals-to-break-on) ;; FIXME:AJR
  (setf *break-on-signals* signals-to-break-on))

(defun ls-debug-off ()
  (setf *break-on-signals* nil))


;;;;
;;;; Compilation and Loading Utilities
;;;;

(defvar *lsos-files* (list "lsobjects"))

(defvar *basic-files*
  (list "lsbasics"
	"lsfloat"
	"fastmap"
        "compound"
        "matrices"
	"ladata"
	"linalg"
        "dists"))

(defvar *ls-files*
  (list "lsmath"
;	#-:kcl "help"
        "statistics"
        "regression"
	"nonlin"
	"maximize"
	"bayes"
	"lstoplevel"))

(defun use-ls-package (name)
  (shadowing-import (package-shadowing-symbols name))
  (use-package name))

(defun use-stats ()
  #+:kcl (shadowing-import '(ls::x))
  (use-ls-package 'lisp-stat-object-system)
  (use-ls-package 'lisp-stat-basics)
  (use-ls-package 'lisp-stat))

(defun lispfile (x)
  "Add suffix/type to string name."
  (concatenate 'string x
	       #+:kcl ".lsp"
	       #+(or :mcl :excl) ".lisp"))

(defun load-files (files)
  (dolist (f files) (load f :verbose t)))

(defun compile-load-files (files &optional (load t))
  (dolist (f files)
	  #+:mcl (format t "compiling ~a~%" f)
	  #+:excl (load (lispfile f))
	  (compile-file f)
	  (if load (load f))))

(defun load-lsos ()
  (load-files *lsos-files*))

(defun load-ls-basics ()
  (load-lsos)
  (load-files *basic-files*)
  #+:kcl (if (and (probe-file "kclglue.o")
		  (probe-file "lib/clib.a"))
	     (si:faslink "kclglue" *clibs*)))

(defun load-stats ()
  (load-ls-basics)
  (load-files *ls-files*))

(defun compile-lsos ()
  (compile-load-files *lsos-files*))

(defun compile-ls-basics (&optional (compile-all t))
  (if compile-all (compile-lsos) (load-lsos))
  (compile-load-files *basic-files*)
  #+:kcl (progn (compile-file "kclglue")
		(si:faslink "kclglue" *clibs*)))

(defun compile-stats (&optional (compile-all t))
  (if compile-all (compile-ls-basics) (load-ls-basics))
  (compile-load-files *ls-files*))


(defvar *lispstat-home-dir* #p"/home/tony/sandbox/CLS.git/"
	"Value considered \"home\" for our data")

(setf *lispstat-home-dir*
      ;; #p"/cygdrive/c/local/sandbox/Lisp/CommonLispStat/"w
      ;; #p"/home/tony/sandbox/CommonLispStat.git/"
      #p"/home/tony/sandbox/CLS.git/")
  
(defmacro ls-dir (root-str)
  `(pathname (concatenate 'string (namestring *lispstat-home-dir*) ,root-str)))

(defmacro ls-defdir (target-dir-var  root-str)
  `(defvar ,target-dir-var (ls-dir ,root-str)))

;;(macroexpand '(ls-defdir *lispstat-asdf-dir* "ASDF"))
;;(macroexpand-1 '(ls-defdir *lispstat-asdf-dir* "ASDF"))
;;(macroexpand-1 '(ls-dir "ASDF"))
  
(ls-defdir *lispstat-asdf-dir* "ASDF/")
(ls-defdir *lispstat-data-dir* "data/")
(ls-defdir *lispstat-external-dir* "external/")
(ls-defdir *lispstat-examples-dir* "examples/")

;; Load ASDF if it isn't loaded
#-asdf(load (pathname (concatenate 'string (namestring *lispstat-external-dir*) "asdf")))

;; (pushnew #p"C:/Lisp/libs/" asdf-util:*source-dirs* :test #'equal)
;;(pushnew  *lispstat-asdf-dir*  asdf:*central-registry*))

