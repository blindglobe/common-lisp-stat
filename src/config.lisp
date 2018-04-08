;;; -*- mode: lisp -*-

;;; Time-stamp: <2018-04-08 17:11:58 tony>
;;; Creation:   <2009-03-12 17:14:56 tony>
;;; File:       config.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2009--, AJ Rossini.  Currently licensed under MIT
;;;             license.  See file LICENSE.mit in top-level directory
;;;             for information.
;;; Purpose:    Configuration variables

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

;;; This organization and structure is new to the 21st Century
;;; version..   Think, "21st Century Schizoid Man".

;;; enforce all floating reads as doubles; at least to the extent that
;;; we can.  People can change if there is a scalable reason to save
;;; bits.

;; unfortunately not to be set by default!  Need to localise this a bit? 
;; (setf *read-default-float-format* 'double-float)

;;; optimization settings: at this point, premature to optimize.
;; (proclaim '(optimize (safety 2) (space 3) (speed 3)))

(in-package :lisp-stat-config)

(defvar *common-lisp-stat-version* "1"
  "Version currently loaded and being used.  Need to make into a
  parseable object for version testing. Currently using integers.")

(defparameter *cls-installation-home-dir*
  (directory-namestring
   (truename (asdf:system-definition-pathname :cls)))
  "Value considered \"home\" for the installation.  Requires the use
  of ASDF to find out where we are.  But ASDF is required no matter
  what.")

(macrolet ((ls-dir (root-str)
	     `(pathname (concatenate 'string
				     (namestring *cls-installation-home-dir*) ,root-str)))

	   (ls-defdir (target-dir-var  root-str)
	     `(defvar ,target-dir-var (ls-dir ,root-str))))
  (ls-defdir *cls-asdf-dir* "ASDF/")
  (ls-defdir *cls-data-dir* "Data/")
  (ls-defdir *cls-external-dir* "external/"))

#|
 Testing
 (macroexpand '(ls-defdir *cls-asdf-dir* "ASDF"))
 (macroexpand-1 '(ls-defdir *cls-asdf-dir* "ASDF"))
 (macroexpand-1 '(ls-dir "ASDF"))
|#
