;;; -*- mode: lisp -*-

;;; Time-stamp: <2014-04-08 09:54:25 tony>
;;; Creation:   <2014-04-08 09:49:19 tony>
;;; File:       024-dataframeBuilding-CLDATAFRAME.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2014--, AJ Rossini.  MIT license.  See LICENSE.mit
;;;             in top level directory for details.
;;; Purpose:    Experiments for using dataframe packages, as part of
;;;             scope/seek phase of development.

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

(in-package :cl-user)

;;; Load and create examples packages for the package structures.

(ql:quickload :cl-data-frame)
(ql:quickload :data-table)

(defpackage :cls-examples-cl-data-frame
  ;; (:export ) nothing
  (:use :common-lisp
	:cl-data-frame))

(defpackage :cls-examples-data-table
  ;; (:export ) nothing
  (:use :common-lisp
	:data-table))


;;; Now provide examples of how they could be used.

(in-package :cls-examples-cl-data-frame)
;; for Tamas' dataframe package





(in-package :cls-examples-data-table)
;; for Mirko's data table package




(in-package :cls-examples)
;; for my extension of lisp-matrix





;;; and more to come, hopefully Marco's version.
