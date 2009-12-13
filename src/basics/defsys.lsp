;;; -*- mode: lisp -*-

;;; Time-stamp: <2009-12-03 17:18:04 tony>
;;; Creation:   <2009-08-26 13:48:13 tony>
;;; File:       defsys.lsp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2009--, AJ Rossini.  BSD license
;;; Purpose:    basic settings for localization to for use.

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

;;; This organization and structure is new to the 21st Century
;;; version...  Think, "21st Century Schizoid Man".

(in-package :lisp-stat-config)

;;; optimization settings
;; (proclaim '(optimize (safety 2) (space 3) (speed 3)))

;;; enforce all floating reads as doubles
;; (setf *read-default-float-format* 'double-float)

(defvar *common-lisp-stat-version* "1.0 Alpha 1")

(defvar *default-path* "./")

(defvar *lispstat-home-dir* #p"/media/disk/Desktop/sandbox/CLS.git/"
	"Value considered \"home\" for our data")

(setf *lispstat-home-dir*
      ;; #p"/cygdrive/c/local/sandbox/Lisp/CommonLispStat/"w
      ;; #p"/home/tony/sandbox/CommonLispStat.git/"
      #p"/home/tony/sandbox/CLS.git/")
  
(defmacro ls-dir (root-str)
  `(pathname (concatenate 'string (namestring *lispstat-home-dir*) ,root-str)))

(defmacro ls-defdir (target-dir-var  root-str)
  `(defvar ,target-dir-var (ls-dir ,root-str)))

;;(macroexpand '(ls-defdir *lispstat-asdf-dir* "ASDF"))
;;(macroexpand-1 '(ls-defdir *lispstat-asdf-dir* "ASDF"))
;;(macroexpand-1 '(ls-dir "ASDF"))
  
(ls-defdir *lispstat-asdf-dir* "ASDF/")
(ls-defdir *lispstat-data-dir* "Data/")
(ls-defdir *lispstat-external-dir* "external/")
(ls-defdir *lispstat-examples-dir* "examples/")

;; Load ASDF if it isn't loaded
#-asdf(load (pathname (concatenate 'string (namestring *lispstat-external-dir*) "asdf")))

;; (pushnew #p"C:/Lisp/libs/" asdf-util:*source-dirs* :test #'equal)
;;(pushnew  *lispstat-asdf-dir*  asdf:*central-registry*))
