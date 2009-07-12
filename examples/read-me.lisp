;;; -*- mode: lisp -*-

;;; Time-stamp: <2009-07-12 07:55:04 tony>
;;; Creation:   <2009-03-12 17:14:56 tony>
;;; File:       read-me.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2009--, AJ Rossini.  BSD, LLGPL, or GPLv2, depending
;;;             on how it arrives.  
;;; Purpose:    Template for examples.

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

;;; This organization and structure is new to the 21st Century
;;; version..   Think, "21st Century Schizoid Man".

(in-package :ls-user) ; we do this in the CLS user playground, so we
		      ; have access to example data.


;;; Guidelines:

;; use license accord to requirements (based on included packages, or 

;; use DEFPARAMETER and special variables to define useful
;; structures.

;; Be prepared to have a clean-up function (or attach/wrap a cleanup
;; hook.

;; Consider the use of a demo or example package to avoid pollution of
;; the ls-user namespace to prevent the ls-user from becoming a luser.
