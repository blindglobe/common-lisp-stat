;;; -*- mode: lisp -*-
;;; Copyright (c) 2008--2012, by A.J. Rossini <blindglobe@gmail.com>
;;; See COPYRIGHT file for any additional restrictions (BSD license).
;;; Since 1991, ANSI was finally finished.  Edited for ANSI Common Lisp. 

;;; Time-stamp: <2012-11-24 17:10:03 tony> 
;;; Creation:   <2008-09-03 08:10:00 tony> 
;;; File:       import.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2007--2009, AJ Rossini.  GPLv2
;;; Purpose:    base structures for importing data into CLS

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

(in-package :cls-dataio)

;;; Data I/O

;; We can read 2 types of data -- those which are non-lisp-native
;; data, and those which are lisp-native (lisp-enabled, an extension
;; of lisp-serialized, i.e. data as program as data thingy's).

;; of the non-native, there could be raw sources (ascii file formats),
;; xml sources (xml -> lisp, possible with some preprocessing.

;;; Reading from DSV files:

;;; The latter seems to actually work a bit at what we need to
;;; acccomplish, is better licensed (i.e. BSD-style) and is now
;;; implemented through filename.dsv->dataframe

(defparameter *lisp-stat-data-external-source-formats*
  '(dsv xml ;; ex of text-based (UTF, ASCII, or similar) formats
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
  #-dialogs  (defun open-file-dialog () ;; why? (&optional set)
    (error "You must provide a file name explicitly")))


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

;; Support functions

(defun filename.dsv->dataframe2 (filename &optional
				(delimchar ",")
				(varnameheader 't)
				(docstring "This is an amusing dataframe array")
				)
  "Reads the DSV file FILENAME and returns a dataframe-array object.
By default, the delimiter is a ',' which can be changed.  FIXME: could
read first 2 lines, and logically guess if the first is variable name
or not.  If so, we'd probably like to return what was guessed and how
to use it next time if wanted."
  (let ((csv-file-data (rsm.string:file->number-table
			filename 
			:delims delimchar)))
    (let ((var-name-list (if varnameheader
			     (car csv-file-data)
			     (make-labels "V"  (length (car csv-file-data)))))
	  (data (listoflist:listoflist->array (cdr csv-file-data) )))
      (make-dataframe data
		 :varlabels var-name-list
		 :doc docstring))))



(defun convert-strings-to-data-types (cl-array)
  cl-array
#|
need to implement a clean...
  (for each column
    (set column (coerce column (determined type of column))))
|#
)


(defun filename.dsv->dataframe (filename
				&optional
				  (delimchar fare-csv:*separator*)
				  (varnameheader 't) ;; strip the first line for variable names
				  (docstring "This is default doc for an amusing dataframe array")
				  (arraystorage-object 'dataframe-array))
  ;; FIXME: fare-csv:read-csv-file returns strings, we need to convert some to numbers
  (let ((fare-csv:*separator* delimchar))  
    (let ((csv-file-data (fare-csv:read-csv-file filename))) 
      (let ((var-name-list (if varnameheader
			       (car csv-file-data)
			       (make-labels "V"  (length (car csv-file-data)))))
	    (data-list (if varnameheader
			   (listoflist:listoflist->array (cdr csv-file-data))
			   (listoflist:listoflist->array csv-file-data))))
	(make-instance arraystorage-object ; 'dataframe-array, but all DF-likes have the following attrs
		       :storage data-list ;; needs to be (convert-strings-to-data-types data-list)
		       :var-labels var-name-list
		       :doc docstring)))))


;; I have some elisp that will build this short of spec quite nicely for data files with fixed field formats
;; 
(defparameter *GHCN-TEMPERATURE-FIELDS*
  '( (0 10 :id (integer))
    (11 15 :year (integer))
    (15 18 :element (string))
    (19 23 :jan (integer))
    (24 24 :dmjan (string))
    (25 25 :qcjan (string))
    (26 26 :dsjan (string))
    (27 31 :feb (integer))
    (32 32 :dmfeb (string))
    (33 33 :qcfeb (string))
    (34 34 :dsfeb (string))
    (35 39 :mar (integer))
    (40 40 :dmmar (string))
    (41 41 :qcmar (string))
    (42 42 :dsmar (string))
    (43 47 :apr (integer))
    (48 48 :dmapr (string))
    (49 49 :qcapr (string))
    (50 50 :dsapr (string))
    (51 55 :may (integer))
    (56 56 :dmmay (string))
    (57 57 :qcmay (string))
    (58 58 :dsmay (string))
    (59 63 :jun (integer))
    (64 64 :dmjun (string))
    (55 65 :qcjun (string))
    (66 66 :dsjun (string))
    (67 71 :jul (integer))
    (72 72 :dmjul (string))
    (73 73 :qcjul (string))
    (74 74 :dsjul (string))
    (75 79 :aug (integer))
    (80 80 :dmaug (string))
    (81 81 :qcaug (string))
    (82 82 :dsaug (string))
    (83 87 :sep (integer))
    (88 88 :dmsep (string))
    (89 89 :qcsep (string))
    (90 90 :dssep (string))
    (91 95 :oct (integer))
    (96 96 :dmoct (string))
    (97 97 :qcoct (string))
    (98 98 :dsoct (string))
    (99 103 :nov (integer))
    (104 104 :dmnov (string))
    (105 105 :qcnov (string))
    (106 106 :dsnov (string))
    (107 111 :dec (integer))
    (112 112 :dmdec (string))
    (113 113 :qcdec (string))
    (114 114 :dsdec (string))
     ))


(defun file.fixed->dataframe (file  field-specification &optional (docstring "a fixed dataframe"))
  "this returns a record oriented file as a dataframe. The field-specification is of the form
<start end fieldname type>. where type is defined by data format validation  - ie string, integer, number, date and so on . dataframe metadata is created from this. At the moment different record types are not handled, though this is planned "
  (labels ((parse-line (line fields)
	   (loop for  ( from to fname type) in fields
		 collect  (data-format-validation:parse-input type  (subseq line from to))))
	 (read-file (file)
	   (with-open-file ( stream file :direction :input)
	     (loop for line = (read-line stream  nil nil)
		   while line
		   collect (parse-line line field-specification)))))
    
     (let ((data (listoflist:listoflist->array (read-file file)))
		(varlabels (mapcar #'third field-specification)))
	    (make-dataframe data
			    :varlabels varlabels
			    :doc docstring))))
;; (file.fixed->dataframe "fixed.txt", *GHCN-TEMPERATURE-FIELDS*)
