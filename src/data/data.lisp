;;; -*- mode: lisp -*-

;;; Time-stamp: <2008-11-16 20:24:10 tony> 
;;; Creation:   <2005-08-xx 21:34:07 rossini> 
;;; File:       data.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2005--2008, AJ Rossini.  BSD, LLGPL, or GPLv2, depending
;;;             on how it arrives.  
;;; Purpose:    data package for lispstat

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

;;; This organization and structure is new to the 21st Century
;;; version.

(in-package :lisp-stat-data)

;;; conside that data has 3 genotypic chracteristrics.  The first
;;; would be form -- scalar, vector, array.  second would be datarep
;;; ("computer science simplistic data" type.  in particular integer,
;;; real, string, symbol.  The last would be statistical type
;;; ("usually handled by computer sicience approaches via metadata").
;;; augmenting datarep type with use in a statistical context,
;;; i.e. that would include nominal, ordinal, integer, continous,
;;; interval (orderable subtypes).  Clearly, the statistical type can
;;; be inherited, likewise the numerical type as well.  The form can
;;; be pushed up or simplified as necessary, but this can be
;;; challenging.

;;; The first approach we will take for CLS is to handle this as
;;; lisp-only structures.  At the time of realization
;;; (?instantiation?) of an "abstract" model, the data should be
;;; pushed into an appropriate form (either "en masse", or
;;; "on-demand") into a linear algebra framework.

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
;;; perspective, i.e. what it is that they contain.  These types
;;; include particular forms of compound types (i.e. dataframe is
;;; array-like, but types differ, difference is row-wise, while array
;;; is a compound of elements of the same type.
;;; 
;;; This is completely subject to change.

;;Examples:
;;  (defun equidimensional (a)
;;    (or (< (array-rank a) 2)
;;        (apply #'= (array-dimensions a)))) =>  EQUIDIMENSIONAL
;;  (deftype square-matrix (&optional type size)
;;    `(and (array ,type (,size ,size))
;;          (satisfies equidimensional))) =>  SQUARE-MATRIX

(defun array-of-equal-dt-scalar-type (x) 
  ;; return dt-scalar-type which fits (more precise that works)
  (if x 
      'integer
      nil))

(defun array-of-equal-dt-scalar-type-within-column (x) 
  ;; return dt-scalar-type which fits (more precise that works)
  (if x 
      'integer
      nil))



(deftype dt-scalar (&optional type)
  `(or integer double complex symbol))

(deftype dt-array (&optional ndim dimlist)
  `(satisfies array-of-equal-dt-scalar-type))

(deftype dt-dataframe (&optional )
  `(satisfies array-of-equal-dt-scalar-type-within-column))

;(deftype dt-relationaldata ()
;  `(satisfies (foreach unit in relationalUnit
;	       (typep unit 'dt-dataframe))))


;;; Statistical Variable Types, sv-{.*}
;;; 
;;; Statistical variable types work to represent the statistical
;;; category represented by the variable, i.e. nominal, ordinal,
;;; integral, continous, ratio.   This metadata can be used to hint at
;;; appropriate analysis methods -- or perhaps more critically, to
;;; define how these methods will fail in the final interrpretation.  

(deftype sv-nominal (&optional n)
  ())

(deftype sv-ordinal (ordering &optional n)
  ())

(deftype sv-categorical ()
  `(satisfies (or sv-nominal sv-ordinal)))
;;(deftype sv-integer )
;;(deftype sv-real )  ;; precision could be a secondary component of real, rational, complex.
;;(deftype sv-rational )
;;(deftype sv-complex )
;;(deftype sv-continuous (or 'sv-integer 'sv-real 'sv-rational 'sv-complex)) ;; perhaps, call it "mostly contin..." 


;;; Data I/O

;; We can read 2 types of data -- those which are pure data, and those
;; which are impure (lisp-enabled, data as program as data thingy's).

(defparameter *lisp-stat-data-formats*
  '(csv tsv))

;; (defgeneric data-read (srce frmt)
;;   "read data from stream srce, in format frmt.")

;; (defgeneric data-write (srce frmt)
;;   "read data from stream srce, in format frmt.")

;; (defmacro with-data (body)
;;   "Stream-handling, maintaining I/O through object typing.")

;; design-wise should these be replaced with a "with-data" form? 

;;; These need to be elsewhere...!
;; DSV processing
;; XML processing

;;; DM operations should be somewhere else as well.
;;; Data Management

;; the goal is to have 2 operations which can be used to create new
;; data formats out of old ones.

;; (defgeneric data-subset (ds description) 
;;   "Take a dataset and make it smaller.")

;; (defgeneric data-relate (ds description) 
;;   "Take 2 or more datasets, and grow them into a bigger one through
;; relating them (i.e. merge is one example).")

;;; What should we be able to do? 

;;; Actions on a single dataset
;;* subset-dataset original-set
;; :list-of-columns :list-of-rows :list-of-rows-and-columns
;; :list-of-indices
;;* resample-dataset original-set ;
;; :by-rows :by-columns :row-weights :column-weights
;; :new-number-of-columns :new-number-of-rows 
;; 

;;; Actions based on 2 or more datasets
;;* concat-dataset set1 set2   ; no matching
;; :by-row :by-column :kronecker-product
;;* merge-dataset set1 set2
;; :match-on-column :match-on-row


;;; Data tools from "statistics.lsp" 

;;;;
;;;; Data File Reading 
;;;;

(defun count-file-columns (fname)
"Args: (fname)
Returns the number of lisp items on the first nonblank line of file FNAME."
  (with-open-file (f fname)
    (if f
        (let ((line (do ((line (read-line f) (read-line f))) 
                        ((or (null line) (< 0 (length line))) line))))
          (if line
              (with-input-from-string (s line)
                (do ((n 0 (+ n 1)) (eof (gensym))) 
                    ((eq eof (read s nil eof)) n))))))))

#+xlisp (defvar *xlisptable* *readtable*)

(if (not (fboundp 'open-file-dialog))
  #+dialogs
  (defun open-file-dialog () ;; why?(&optional set)
    (get-string-dialog "Enter a data file name:"))
  #-dialogs
  (defun open-file-dialog () ;; why? (&optional set)
    (error "You must provide a file name explicitly")))

(defun read-data-file (&optional (file (open-file-dialog t)))
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
(defun read-data-columns (&optional (file (open-file-dialog t))
                                    (cols (if file 
                                              (count-file-columns file))))
"Args: (&optional file cols)
Reads the data in FILE as COLS columns and returns a list of lists representing the columns."
  (if (and file cols)
      (transpose (split-list (read-data-file file) cols))))


;;; FIXME:AJR:  ALL THE FOLLOWING NEED TO BE SOLVED BY PLATFORM-INDEP PATHNAME WORK! 
;;; FIXME:AJR: use either string or pathname.

(defun path-string-to-path (p s) 
    (pathname (concatenate 'string (namestring p) s)))

(defun load-data (file)
"Args: (file) as string
Read in data file from the data examples library."
  (if (load (path-string-to-path *lispstat-data-dir* file))
      t
      (load (path-string-to-path *lispstat-data-dir* file))))

(defun load-example (file)
  "Args: (file) as string
Read in lisp example file from the examples library."
  (if (load (path-string-to-path *lispstat-example-dir* file))
      t
      (load (path-string-to-path *lispstat-example-dir* file))))

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
  `(unless (and *ask-on-redefine*
                (boundp ',symbol)
                (not (y-or-n-p "Variable has a value. Redefine?")))
    (if (boundp ',symbol)
	(setf ,symbol ,value)
	(defvar ,symbol ,value))
    (pushnew ',symbol *variables*)
    ',symbol))
  
(defun variables-list () 
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
        
