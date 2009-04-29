;;; -*- mode: lisp -*-

;;; Time-stamp: <2009-04-23 07:14:56 tony>
;;; Creation:   <2009-03-12 17:14:56 tony>
;;; File:       template.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2009--, AJ Rossini.  BSD, LLGPL, or GPLv2, depending
;;;             on how it arrives.  
;;; Purpose:    Template header file

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.



(progn ;; FIXME: Need to clean up data examples, licenses, attributions, etc.
  ;; The following breaks because we should use a package to hold
  ;; configuration details, and this would be the only package outside
  ;; of packages.lisp, as it holds the overall defsystem structure.
  (load-data "iris.lsp")  ;; (the above partially fixed).
  (variables)
  diabetes )




(progn ;; Importing data from DSV text files.

  (defparameter *my-df-2*
	(make-instance 'dataframe-array
		       :storage
		       (listoflist->array
			(cybertiggyr-dsv::load-escaped
			 "/media/disk/Desktop/sandbox/CLS.git/Data/example-mixed.csv"))
		       :doc "This is an interesting dataframe-array"))
#|		       :case-labels (list "x" "y")
		       :var-labels (list "a" "b" "c" "d" "e")
|#  

  ;; a better approach is:
  (asdf:oos 'asdf:load-op 'rsm-string)
  (rsm.string:file->string-table
   "/media/disk/Desktop/sandbox/CLS.git/Data/example-mixed.csv")

  (rsm.string:file->number-table
   "/media/disk/Desktop/sandbox/CLS.git/Data/example-numeric.csv")

  (defparameter *my-df-2*
	(make-instance 'dataframe-array
		       :storage
		       (listoflist->array
			(transpose-listoflist 
			 (rsm.string:file->string-table
			  "/media/disk/Desktop/sandbox/CLS.git/Data/example-mixed.csv")))
		       :doc "This is an interesting dataframe-array"))
  *my-df-2*

  (defparameter *my-df-3*
	(make-instance 'dataframe-array
		       :storage
		       (listoflist->array
			(transpose-listoflist 
			 (rsm.string:file->number-table
			  "/media/disk/Desktop/sandbox/CLS.git/Data/example-numeric.csv")))
		       :doc "This is an interesting dataframe-array"))
  *my-df-3*

  ;; Need to the this using the make-dataframe example, or write a 
  ;; dsvfile->dataframe command.
  )

