;;; -*- mode: lisp -*-
;;; Copyright (c) 2007, by A.J. Rossini <blindglobe@gmail.com>
;;; See COPYRIGHT file for any additional restrictions (BSD license).
;;; Since 1991, ANSI was finally finished.  Edited for ANSI Common Lisp. 

;;; This is semi-external to lispstat core packages.  The dependency
;;; should be that lispstat packages are dependencies for the unit
;;; tests.  However, where they will end up is still to be
;;; determined. 

(in-package :lisp-stat-unittests)

(deftestsuite lisp-stat-ut-spec-fns (lisp-stat-ut) ())

;;;; Log-gamma function

(addtest (lisp-stat-ut-spec-fns) log-gamma-fn
	 (ensure-same 
	  (log-gamma 3.4)
	  1.0923280596789584
	  :test 'almost=))

