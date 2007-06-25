; -*- lisp -*-
(defpackage #:neldermead-asd
  (:use :cl :asdf))

(in-package :neldermead-asd)

(defsystem :neldermead
  :serial t
  :components ((:file "defpackage")
	       (:file "la")
	       (:file "neldermead")))