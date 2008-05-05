(defpackage #:asdf-lift-test (:use #:asdf #:cl))
(in-package #:asdf-lift-test)

(defsystem lift-test
  :author "Gary Warren King <gwking@metabang.com>"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style License; see file COPYING for details"
  :description "Tests for LIsp Framework for Testing"
  :components ((:module 
		"setup"
		:pathname "test/"
		:components ((:file "packages")
			     (:file "lift-test"
				    :depends-on ("packages"))))
	       (:module 
		"test"
		:pathname "test/"
		:depends-on ("setup")
		:components ((:file "test-dynamic-variables")
			     (:file "equality-tests")
			     (:file "testsuite-expects")
			     (:file "finding-tests")
			     (:file "order-of-operations")
				     #+(or)
			     (:file "test-prototypes"))))  
  :depends-on (:lift))

(defmethod operation-done-p 
           ((o test-op)
            (c (eql (find-system 'lift-test))))
  (values nil))


