;;; -*- mode: lisp -*-

;;; Time-stamp: <2014-04-01 10:49:47 tony>
;;; Creation:   <2009-03-12 17:14:56 tony>
;;; File:       XX-readMe.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2009--, AJ Rossini.  MIT license.  See LICENSE.mit
;;;             in top level directory for details.  However this
;;;             could be relicensed once modified, as only the
;;;             "templating" use is intended to be MIT licensed, not
;;;             the result of using the template!
;;; Purpose:    Template for examples, to describe how one should use
;;;             this directory.

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

(in-package :cls-examples)

;;; Guidelines:

;; use the ##-activity.lisp  naming scheme to drive what we load.

;; LICENSING: use license accord to requirements (based on included packages, or
;; what the author wants).  The default licensing is MIT, we want this
;; to be a general prototype that someone could commercialize or use
;; in a profit-making setting.  The goal of this project (from Tony's
;; perspective, others are free to think differently) is to ensure
;; that better computing is possible.  And that I have a system that I
;; like to use.

;; VARIABLES AND PLACES:  use DEFPARAMETER and special variables to define useful
;; structures.

;; NAMESPACE POLLUTION AND NAME COLLISIONS: Be prepared to have a
;; clean-up function (or attach/wrap a cleanup hook).  Better yet,
;; consider the use of a demo or example package to avoid pollution of
;; the ls-user namespace to prevent the ls-user from becoming a luser.

;; REFACTORING: As code in these files gets refined, it should get
;; moved into the core system as classes and functions demonstrate
;; usefulness.  All methods should start here, and get refactored into
;; the core as they become useful.
;;
;; Sometimes this is violated.
