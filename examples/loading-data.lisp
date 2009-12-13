;;; -*- mode: lisp -*-

;;; Time-stamp: <2009-12-08 07:54:05 tony>
;;; Creation:   <2009-03-12 17:14:56 tony>
;;; File:       template.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2009--, AJ Rossini.  BSD, LLGPL, or GPLv2, depending
;;;             on how it arrives.  
;;; Purpose:    Template header file

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.


;; start within the common-lisp-stat-user 
(in-package :ls-user)

;; we'll be loading from directories in the CLS homedir, so we want to
;; make it easier to reach.  
(defparameter *my-cls-homedir* 
  "/home/tony/sandbox/CLS.git/" ; <- value with trailing directory separator
  "documentation: change this to localize") ; <- doc
;; so
(concatenate 'string *my-cls-homedir* "Data/example.csv")
;; implies
(defun localized-pathto (x)
  "return a string denoting the complete path.
FIXME: UNIX-centric (though might work on Mac OSX).  Might want to
return a pathspec, not a string/namespec"
  (check-type x string)
  (concatenate 'string *my-cls-homedir* x))



(progn
  ;; FIXME: Need to clean up data examples, licenses, attributions, etc.
  ;; The following breaks because we should use a package to hold
  ;; configuration details, and this would be the only package outside
  ;; of packages.lisp, as it holds the overall defsystem structure.
  (load-data "iris.lsp")  ;; (the above partially fixed).
  (variables)
  diabetes )




(progn
  ;; Importing data from DSV text files.

  (asdf:oos 'asdf:load-op 'rsm-string)
  (rsm.string:file->string-table
   (localized-pathto "Data/example-mixed.csv"))

  (rsm.string:file->number-table
   (localized-pathto "Data/example-numeric.csv"))

  (defparameter *my-df-2*
	(make-instance 'dataframe-array
		       :storage
		       (listoflist->array
			(transpose-listoflist 
			 (rsm.string:file->string-table
			  (localized-pathto "Data/example-mixed.csv"))))
		       :doc "This is an interesting dataframe-array"))
  *my-df-2*

  (defparameter *my-df-3*
	(make-instance 'dataframe-array
		       :storage
		       (listoflist->array
			(transpose-listoflist 
			 (rsm.string:file->number-table
			  (localized-pathto "Data/example-numeric.csv"))))
		       :doc "This is an interesting dataframe-array"))
  *my-df-3*

  ;; Need to the this using the make-dataframe example, or write a 
  ;; dsvfile->dataframe command.
  )

