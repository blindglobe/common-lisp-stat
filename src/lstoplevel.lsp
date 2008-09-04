;;; -*- mode: lisp -*-
;;; Copyright (c) 2005--2007, by A.J. Rossini <blindglobe@gmail.com>
;;; See COPYRIGHT file for any additional restrictions (BSD license).
;;; Since 1991, ANSI was finally finished.  Edited for ANSI Common Lisp. 

(in-package :lispstat)

;;;;
;;;; AKCL Top Level (Modified from AKCL source file unixport/kcltop.lsp)
;;;;

#+:kcl
(import '(si::*quit-tag* si::*eof* si::*lisp-initialized* 
			 si::reset-stack-limits si::break-current))

(defvar +)
(defvar ++)
(defvar +++)
(defvar -)
(defvar *)
(defvar **)
(defvar ***)
(defvar /)
(defvar //)
(defvar ///)

#+:kcl
(defun ls-top-level ()
  (when (> (system:argc) 1)
        (setq system:*system-directory* (system:argv 1)))
  (let ((lslib (si:getenv "LSLIB")))
    (if lslib (setf *default-path* lslib)))
  (format t 
	  "AKCL (Austin Kyoto Common Lisp)~%~
           Contains Enhancements by W. Schelter~%~
	   Lisp-Stat ~a, Copyright (c) by Luke Tierney, 1990~%~
	   Type :q to continue after an error~2%"
	  *common-lisp-stat-version*)
  (setq si::*ihs-top* 1)

  (in-package 'system::user)
  (incf system::*ihs-top* 2)
  (let ((+ nil) (++ nil) (+++ nil)
        (- nil)
        (* nil) (** nil) (*** nil)
        (/ nil) (// nil) (/// nil))
    (setq *lisp-initialized* t)
    (catch *quit-tag* (when (probe-file "init.lsp") (load "init.lsp")))
    (catch *quit-tag* (when (probe-file "statinit.lsp") (load "statinit.lsp")))
    (loop
     (setq +++ ++ ++ + + -)
     (format t "~%~a>"
	     (if (eq *package* (find-package 'user)) 
		 "" 
	         (package-name *package*)))
     (reset-stack-limits)
     (when (catch *quit-tag*
	     (setq - (locally (declare (notinline read))
			      (read *standard-input* nil *eof*)))
	     (when (eq - *eof*) (bye))
	     (let ((values (multiple-value-list
			    (locally (declare (notinline eval)) (eval -)))))
	       (setq /// // // / / values *** ** ** * * (car /))
	       (fresh-line)
	       (dolist (val /)
		       (locally (declare (notinline prin1)) (prin1 val))
		       (terpri))
	       nil))
	   (terpri *error-output*)
	   (break-current)))))

;;;;
;;;; Macintosh CL Top Level
;;;; This does not quite properly work with the event loop of
;;;; the system.
;;;;

#|
#+:mcl
(defun ls-init-top-level ()
  (in-package cl-user)
  (setf + nil ++ nil +++ nil
        - nil
        * nil ** nil *** nil
        / nil // nil /// nil)
  (format t 
	  "Lisp-Stat ~a, Copyright (c) by Luke Tierney, 1990~%~
	   Type COMMAND-. to continue after an error~2%"
	  *common-lisp-stat-version*))

#+:mcl
(defun ls-top-level ()
  (catch :stat-abort
    (catch :abort
      (loop
        (setq +++ ++ ++ + + -)
        (format t "~%~a> "
                (if (eq *package* (find-package 'cl-user)) 
                  "" 
                  (package-name *package*)))
        (loop (if (listen *standard-input*) (return t))
              (event-dispatch))
        (setq - (locally (declare (notinline read))
                         (read *standard-input* t)))
        (if (consp -) (read-line *standard-input* t))
        (let ((*idle* nil)
              (values (multiple-value-list
                       (locally (declare (notinline eval)) (eval -)))))
          (setq /// // // / / values *** ** ** * * (car /))
          (fresh-line)
          (dolist (val /)
            (locally (declare (notinline prin1)) (prin1 val))
            (terpri))
          nil)))))
|#

;;;;
;;;; EXCL (Allegro) Top Level
;;;;

#+:excl
(defun ls-top-level-eval (expr)
  (setq +++ ++ ++ + + - - expr)
  (let ((values (multiple-value-list
		 (locally (declare (notinline eval)) (eval -)))))
    (setq /// // // / / values *** ** ** * * (car /))
    (car values)))

#+:excl
(defun ls-top-level ()
  (format t 
	  "Lisp-Stat ~a, Copyright (c) by Luke Tierney, 1990~%~
	   Type :reset to continue after an error~2%"
	  *common-lisp-stat-version*)

  (in-package 'user)
  (let ((tpl:*eval* 'ls-top-level-eval)
	(tpl:*prompt* "<cls> ")
	(*read-default-float-format* +stat-float-typing+)
	(+ nil) (++ nil) (+++ nil)
        (- nil)
        (* nil) (** nil) (*** nil)
        (/ nil) (// nil) (/// nil))
    (catch '*ls-quit-tag* (tpl:top-level-read-eval-print-loop))))

#+:excl
(defun exit-ls () (throw '*ls-quit-tag* nil))
