;;; -*- mode: lisp -*-

;;; Time-stamp: <2009-04-23 08:05:57 tony>
;;; Creation:   <2009-03-10 16:59:37 tony>
;;; File:       plot.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2009--, AJ Rossini.  BSD, LLGPL, or GPLv2, depending
;;;             on how it arrives.  
;;; Purpose:    visualization and plotting generics and methods.

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

;;; This organization and structure is new to the 21st Century
;;; version.
(defpackage #:cls-visualize
  (:nicknames #:plotting)
  (:use #:cl ))


(in-package :cls-visualize)

;; hacks and glory await!