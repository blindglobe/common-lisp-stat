;;; -*- mode: lisp -*-
;;; Copyright (c) 2005--2007, by A.J. Rossini <blindglobe@gmail.com>
;;; See COPYRIGHT file for any additional restrictions (BSD license).
;;; Since 1991, ANSI was finally finished.  Edited for ANSI Common Lisp. 


(in-package :cl-user)

(defpackage :lisp-stat-user
  (:documentation "Experimentation package for LispStat.  Serious work
should be packaged up elsewhere for reproducibility.")
  (:nicknames :ls-user)
  (:use :common-lisp
	:lisp-stat)
  (:shadowing-import-from :lisp-stat-object-system
			  slot-value call-method call-next-method))

(in-package :lisp-stat-user)

