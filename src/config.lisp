;;; -*- mode: lisp -*-

;;; Time-stamp: <2010-09-06 13:46:06 tony>
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

;;; enforce all floating reads as doubles
(setf *read-default-float-format* 'double-float)

;;; optimization settings
;; (proclaim '(optimize (safety 2) (space 3) (speed 3)))


(in-package :lisp-stat-config)

(defvar *common-lisp-stat-version* "1"
  "Version currently loaded and being used.  Need to make into a
  parseable object for version testing. Currently using integers.")

(defparameter *cls-home-dir*
  (directory-namestring
   (truename (asdf:system-definition-pathname :cls)))
  "Value considered \"home\" for the installation.  Requires the use of ASDF to find out where we are.")

#|
(setf *cls-home-dir*
      ;; #p"/cygdrive/c/local/sandbox/Lisp/CommonLispStat/"w
      ;; #p"/home/tony/sandbox/CommonLispStat.git/"
      #p"/home/tony/sandbox/CLS.git/")
|#

(macrolet ((ls-dir (root-str)
	     `(pathname (concatenate 'string
				     (namestring *cls-home-dir*) ,root-str)))

	   (ls-defdir (target-dir-var  root-str)
	     `(defvar ,target-dir-var (ls-dir ,root-str))))
  (ls-defdir *cls-asdf-dir* "ASDF/")
  (ls-defdir *cls-data-dir* "Data/")
  (ls-defdir *cls-external-dir* "external/")
  ;; reminder of testing
  ;;(macroexpand '(ls-defdir *cls-asdf-dir* "ASDF"))
  ;;(macroexpand-1 '(ls-defdir *cls-asdf-dir* "ASDF"))
  ;;(macroexpand-1 '(ls-dir "ASDF"))
  )
