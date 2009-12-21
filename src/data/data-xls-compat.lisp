;;; -*- mode: lisp -*-

;;; Time-stamp: <2009-12-21 12:46:46 tony>
;;; Creation:   <2009-03-12 17:14:56 tony>
;;; File:       template.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2009--, AJ Rossini.  Currently licensed under MIT
;;;             license.  See file LICENSE.mit in top-level directory
;;;             for information.
;;; Purpose:    Template header file

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

;;; This organization and structure is new to the 21st Century
;;; version..   Think, "21st Century Schizoid Man".

(in-package :lisp-stat-data)

;; XLISPSTAT compatibility functions.

;;;;
;;;; Listing and Saving Variables and Functions (XLispStat compatibility)
;;;;

(defvar *variables* nil)
(defvar *ask-on-redefine* nil)

(defmacro def (symbol value)
  "Syntax: (def var form)
VAR is not evaluated and must be a symbol.  Assigns the value of FORM to
VAR and adds VAR to the list *VARIABLES* of def'ed variables. Returns VAR.
If VAR is already bound and the global variable *ASK-ON-REDEFINE*
is not nil then you are asked if you want to redefine the variable."
  `(progn
     (unless (and *ask-on-redefine*
		  (boundp ',symbol)
		  (not (y-or-n-p "Variable has a value. Redefine?")))
       (defparameter ,symbol ,value))
     (pushnew ',symbol *variables*)
     ',symbol))
  
(defun variables-list () 
  "Return list of variables as a lisp list of strings."
  (mapcar #'intern (sort-data (mapcar #'string *variables*))))

(defun variables ()
  "Args:()
Returns a list of the names of all def'ed variables to STREAM"
  (if *variables*
      (mapcar #'intern (sort-data (mapcar #'string *variables*)))))
  

(defun undef (v)
"Args: (v)
If V is the symbol of a defined variable the variable it is unbound and
removed from the list of defined variables. If V is a list of variable
names each is unbound and removed. Returns V."
  (dolist (s (if (listp v) v (list v)))
          (when (member s *variables*)
                (setq *variables* (delete s *variables*))
                (makunbound s)))
  v)

(defun read-data-file (&optional (file (open-file-dialog)))
"Args:  (file)
Returns a list of all lisp objects in FILE. FILE can be a string or a symbol,
in which case the symbol'f print name is used."
  (if file
      (let ((eof (gensym)))
        (with-open-file (f file)
          (if f
	      (do* ((r (read f nil eof) (read f nil eof))
		    (x (list nil))
		    (tail x (cdr tail)))
		   ((eq r eof) (cdr x))
		   (setf (cdr tail) (list r))))))))

;;; New definition to avoid stack size limit in apply
#|
 (defun read-data-columns (&optional (file (open-file-dialog))
                                    (cols (if file 
                                              (count-file-columns file))))
"Args: (&optional file cols)
Reads the data in FILE as COLS columns and returns a list of lists representing the columns."
  (if (and file cols)
      (transpose (split-list (read-data-file file) cols))))
|#

;;; FIXME:AJR:  ALL THE FOLLOWING NEED TO BE SOLVED BY PLATFORM-INDEP PATHNAME WORK! 
;;; FIXME:AJR: use either string or pathname.

(defun path-string-to-path (p s) 
    (pathname (concatenate 'string (namestring p) s)))

(defun load-data (file)
"Args: (file) as string
Read in data file from the System DATA library."
  (if (load (path-string-to-path *cls-data-dir* file))
      t
      (load (path-string-to-path *cls-data-dir* file))))

(defun load-example (file)
  "Args: (file) as string
Read in lisp example file from the System EXAMPLES library."
  (if (load (path-string-to-path *cls-examples-dir* file))
      t
      (load (path-string-to-path *cls-examples-dir* file))))

;;;
;;; Saving Variables and Functions
;;;
  
(defun savevar (vars file &optional (suffix ".lsp"))
  "Args: (vars-symbol-or-list file-name-root &optional suffix-string)

VARS is a symbol or a list of symbols. FILE-NAME-ROOT is a string (or
a symbol whose print name is used) not ending in SUFFIX (defaults to
\".lsp\"). The VARS and their current values are written to the file
FILE-NAME-ROOT.lsp in a form suitable for use with the load command."
  (with-open-file (f (concatenate 'string (namestring file) suffix)
		     :direction :output)
    (let ((vars (if (consp vars) vars (list vars))))
      (flet ((save-one (x)
	       (let ((v (symbol-value x)))
		 (if (objectp v) 
		     (format f "(def ~s ~s)~%" x (send v :save))
		   (format f "(def ~s '~s)~%" x v)))))
	    (mapcar #'save-one vars))
      vars)))
