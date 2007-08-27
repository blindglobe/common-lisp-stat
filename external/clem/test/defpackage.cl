
(in-package #:cl-user)

(defpackage #:clem-test
  (:use #:cl #:asdf #:clem)
  (:export #:run-tests
	   #:run-bench
	   #:run-defmatrix-tests
	   ))

