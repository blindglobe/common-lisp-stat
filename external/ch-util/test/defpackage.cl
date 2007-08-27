
(in-package #:cl-user)

(defpackage #:ch-util-test
  (:use #:cl #:asdf #:ch-util)
  (:shadowing-import-from #:ch-util)
  (:export #:run-tests
	   ))

(in-package #:ch-util-test)
