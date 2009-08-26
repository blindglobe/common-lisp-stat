;;; -*- mode: lisp -*-

;;; Time-stamp: <2009-08-26 13:54:50 tony> 
;;; Creation:   <2005-08-xx 21:34:07 rossini> 
;;; File:       data.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2005--2009, AJ Rossini.  GPLv2
;;; Purpose:    data package for lispstat

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

;;; This organization and structure is new to the 21st Century
;;; version.

(in-package :lisp-stat-data)

;;; consider that data has 3 genotypic characteristics.  The first
;;; is storage form -- scalar, vector, array.  second would be datarep
;;; ("computer science simplistic data" type.  in particular integer,
;;; real, string, symbol.  The last would be statistical type
;;; ("usually handled by computer science approaches via metadata").
;;; augmenting datarep type with use in a statistical context,
;;; i.e. that would include nominal, ordinal, integer, continous,
;;; interval (orderable subtypes).  Clearly, the statistical type can
;;; be inherited, likewise the numerical type as well.  The form can
;;; be pushed up or simplified as necessary, but this can be
;;; challenging.

;;; The first approach considered is for CLS to handle this as
;;; lisp-only structures.  When we realize an "abstract" model, the
;;; data should be pushed into an appropriate form (either "en masse",
;;; or "on-demand") into a linear algebra framework.

;;; There is some excellent material on this by John Chambers in one
;;; of his earlier books.  Reference is being ignored to encourage
;;; people to read them all.  With all due respect to John, they've
;;; lasted quite well, but need to be updated.

;;; The purpose of this package is to manage data which will be
;;; processed by LispStat.  In particular, it will be important to
;;; register variables, datasets, relational structures, and other
;;; objects which could be the target for statistical modeling and
;;; inference.

(defvar *lisp-stat-data-table* (make-hash-table)
  "Marks up the data the could be used by.")

(defvar *lisp-stat-data-count* 0
  "number of items currently recorded.")

;;; Data (storage) Types, dt-{.*}
;;;
;;; Data types are the representation of data from a computer-science
;;; perspective, i.e. what it is that they contain, in the sense of
;;; scalars, arrays, networks, but not the actual values or
;;; statistical behavour of the values.  These types include
;;; particular forms of compound types (i.e. dataframe is array-like,
;;; but types differ, difference is row-wise, while array is a
;;; compound of elements of the same type.
;;; 
;;; This is completely subject to change, AND HAS.  We use a class
;;; heirarchy to generate the types.


;;; Statistical Variable Types, sv-{.*} or statistical-variable-{.*}
;;; 
;;; Statistical variable types work to represent the statistical
;;; category represented by the variable, i.e. nominal, ordinal,
;;; integral, continous, ratio.   This metadata can be used to hint at
;;; appropriate analysis methods -- or perhaps more critically, to
;;; define how these methods will fail in the final interrpretation.  

;;; originally, these were considered to be types, but now, we
;;; consider this in terms of abstract classes and mix-ins.



;;;;
;;;; Listing and Saving Variables and Functions
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
  
(defun savevar (vars file)
"Args: (vars file-name-root)
VARS is a symbol or a list of symbols. FILE-NAME-ROOT is a string (or a symbol
whose print name is used) not endinf in .lsp. The VARS and their current values
are written to the file FILE-NAME-ROOT.lsp in a form suitable for use with the
load command."
  (with-open-file (f (concatenate 'string (namestring file) ".lsp")
		     :direction :output)
    (let ((vars (if (consp vars) vars (list vars))))
      (flet ((save-one (x)
	       (let ((v (symbol-value x)))
		 (if (objectp v) 
		     (format f "(def ~s ~s)~%" x (send v :save))
		   (format f "(def ~s '~s)~%" x v)))))
	    (mapcar #'save-one vars))
      vars)))

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
        
