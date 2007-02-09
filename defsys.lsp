;;; -*- mode: lisp -*-

;;;; defsys -- System setup for CL version of Lisp-Stat
;;;; 
;;;; Copyright (c) 1991, by Luke Tierney. Permission is granted for
;;;; unrestricted use.

#+kcl
(dolist (f *features*)
  (if (symbolp f)
      (pushnew (intern (symbol-name f) 'keyword) *features*)))

#-:mcl(require :lspackages)
#-:mcl(require :lsmacros)

;;;;
;;;; Macintosh CL
;;;;

#+:mcl (pushnew :CLtL2 *features*)
#+:mcl (def-logical-directory "mcls;" "ccl;:MCLS:")
#+:mcl (setf *break-on-errors* nil)
#+:mcl (set-mac-default-directory "mcls;")
#+:mcl (setf *save-definitions* t)
#+:mcl (defpackage "COMMON-LISP" (:nicknames "CL" "LISP"))
#+:mcl (pushnew :stat-float-is-double-float *features*)
#+:mcl (require :ff)

;;;;
;;;; AKCL
;;;;

;#+:kcl (proclaim '(optimize (safety 2) (space 3) (speed 3)))
#+:kcl (setf *break-enable* nil)

#+:kcl (allocate 'cons 600)
#+:kcl (allocate 'cfun 1000)
#+:kcl (si:allocate-relocatable-pages 100)

;; **** This feature should only be used if the patches in num_sfun.c
;; **** and numlib.lsp habe been applied to AKCL -- see lsfloat.lsp for
;; **** more details.

;#+:kcl (pushnew :stat-float-is-double-float *features*)

;;;;
;;;; EXCL (Allegro)
;;;;

(setf *read-default-float-format* 'double-float)

;;;;
;;;; Switch to Lisp-Stat package
;;;;

#+:mcl (load "lspackages")
#+:mcl (load "lsmacros")

(in-package :lisp-stat)

(export '(*default-path* debug nodebug))

(defvar *common-lisp-stat-version* "1.0 Alpha 1")

(defvar *default-path* "./")

;;;;
;;;; Functions for switching into and out of debug mode
;;;;

(defun debug ()
  #+:kcl (setf *break-enable* t)
  #+:mcl (setf *break-on-errors* t))

(defun nodebug ()
  #+:kcl (setf *break-enable* nil)
  #+:mcl (setf *break-on-errors* nil))

;;;;
;;;; MCL definitions
;;;;

#+:mcl (setf *default-path* ":")

;;;;
;;;; AKCL definitions
;;;;

#+:kcl (setf  *clibs* 
	      #+:mips "lib/clib.a -lm_G0 -lc_G0"
	      #-:mips "lib/clib.a -lm -lc")

;;;;
;;;; EXCL definitions
;;;;

;;;;
;;;; Compilation and Loading Utilities
;;;;

(defvar *lsos-files* (list "lsobjects"))

(defvar *basic-files*
  (list #+:kcl "kclpatch"
	#+:mcl "mclglue"
	#+:excl "exclglue"
	"lsbasics"
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
