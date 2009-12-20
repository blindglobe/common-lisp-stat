;;; -*- mode: lisp -*-
;;; Copyright (c) 2008, by A.J. Rossini <blindglobe@gmail.com>
;;; See COPYRIGHT file for any additional restrictions (BSD license).
;;; Since 1991, ANSI was finally finished.  Edited for ANSI Common Lisp. 

;;; Time-stamp: <2009-12-20 22:27:42 tony> 
;;; Creation:   <2008-09-03 08:10:00 tony> 
;;; File:       import.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2007--2009, AJ Rossini.  GPLv2
;;; Purpose:    base structures for importing data into CLS

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

(in-package :lisp-stat-data)

;;; Data I/O

;; We can read 2 types of data -- those which are non-lisp-native
;; data, and those which are lisp-native (lisp-enabled, an extension
;; of lisp-serialized, i.e. data as program as data thingy's).

;; of the non-native, there could be raw sources (ascii file formats),
;; xml sources (xml -> lisp, possible with some preprocessing.

;;; Reading from DSV files:

;;; consider either the cybertyggyr-dsv package, or the rsm.string
;;; package.  The latter seems to actually work a bit at what we need
;;; to acccomplish, but the former is a challenge to get right when we
;;; need to think abut what it is that we need to get done.  The
;;; latter is also better licensed. i.e. BSD-style.  The latter is
;;; implemented through filename->dataframe 


(defparameter *lisp-stat-data-external-source-formats*
  '(csv tsv xml ;; ex of text-based (UTF, ASCII, or similar) formats
    sql ;; ex of RDBMS call
    fcs affy))  ;; ex of binary formats

(defparameter *lisp-stat-data-import-referencing-type*
  '(lisp-data-structure reference lisp-function))

(defgeneric data-import (source source-format referencing-type)
  (:documentation "read data from stream srce, in format srce-frmt; 
                   return a reftype, which could be a
                   lisp-data-structure, a reference to such, or a lisp
                   function which can be evaluated to generate
                   either."))

(defgeneric data-export (data target-format target-referencing-type)
  (:documentation "write data from stream srce, in format srce-frmt; 
                   return a reftype, which could be a
                   lisp-data-structure, a reference to such, or a lisp
                   function which can be evaluated to generate
                   either."))




;;; Potentially useful functions 

;; the following belongs here if we are working externally, but might
;; belong with data if we are working internlly

;; (defmacro with-data (body)
;;   "Stream-handling, maintaining I/O through object typing.")


;;;
;;; Related to data file reading 
;;;

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

(if (not (fboundp 'open-file-dialog))
  #+dialogs
  (defun open-file-dialog () ;; why?(&optional set)
    (get-string-dialog "Enter a data file name:"))
  #-dialogs
  (defun open-file-dialog () ;; why? (&optional set)
    (error "You must provide a file name explicitly")))

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
Read in data file from the data examples library."
  (if (load (path-string-to-path *cls-data-dir* file))
      t
      (load (path-string-to-path *cls-data-dir* file))))

(defun load-example (file)
  "Args: (file) as string
Read in lisp example file from the examples library."
  (if (load (path-string-to-path *cls-examples-dir* file))
      t
      (load (path-string-to-path *cls-examples-dir* file))))

;;;
;;; Saving Variables and Functions
;;;
  
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

        

;;; General modification approaches.

(defgeneric importData (source featureList)
  (:documentation "command to get data into CLS.  Specific methods
  will need to handle pathnames, internal data structures, and
  external services such as DBMS's.  We would like to be able to do
  thinks like: 
     (importData MyPathName '(:formattype 'csvString))
     (importData '(sqlConnection :server host.domain.net :port 666)
                 '(:formattype 'table   
  and so on."))


(defun pathname-example (name) 
  (let ((my-path (parse-namestring name)))
    (values (pathname-name my-path :case :common) 
            (pathname-name my-path :case :local))))

(defvar sourceTypes (list 'csv 'lisp 'tsv 'special)
  "list of possible symbols.

Thsees are used to specify source formats that might be supported for
input.  CSV and TSV are standard, LISP refers to forms, and SPECIAL
refers to a FUNCTION which parses as appropriately.")

#|
;;; WRONG LOGIC.
 (defmethod importData ((fileHandle pathname)
		       (fmt list)) ;sourceTypes))
  "File-based input for data.
Usually used by:
 (importData (parse-namestring 'path/to/file')
	     (list :format 'csv))

 (importData myPathName (list :format 'lisp))
."
  (let* ((fmtType (getf fmt :format))
	 (newData (getDataAsLists fileHandle fmtType)))
    (case fmtType
      ('csv (  ))
      ('tsv (  ))
      ('lisp ( ))
      ('special (let ((parserFcn (getf fmt :special-parser)))))
      (:default (error "no standard default importData format")))))
|#

(defmethod importData ((ds array) (fmt list))
  "mapping arrays into CLS data.")

#|
 (defmethod importData ((dsSpec DBMSandSQLextract)
		       (fmt mappingTypes))
  "mapping DBMS into CLS data.")

|#

(defun filename.dsv->dataframe (filename &optional
				(delimchar ",")
				(varnameheader 't)
				(docstring "This is an amusing dataframe array")
				(arraystorage-object 'dataframe-array))
  "Reads the DSV file FILENAME and returns a dataframe-array object.
By default, the delimiter is a ',' which can be changed.  
FIXME: could read first 2 lines, and logically guess if the first is variable name or not."
  (let ((csv-file-data (rsm.string:file->number-table
			filename 
			:delims delimchar)))
    (let ((var-name-list (if varnameheader
			     (car csv-file-data)
			     (make-labels "V"  (length (car csv-file-data)))))
	  (data-list (listoflist:listoflist->array (cdr csv-file-data))))
      (make-instance arraystorage-object ; 'dataframe-array, but all DF-likes have the following attrs
		 :storage data-list
		 :var-labels var-name-list
		 :doc docstring))))
