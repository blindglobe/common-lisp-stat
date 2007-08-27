(defpackage #:asdf-lift-test (:use #:asdf #:cl))
(in-package #:asdf-lift-test)

(defsystem lift-test
  :author "Gary Warren King <gwking@metabang.com>"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style License; see file COPYING for details"
  :description "Tests for LIsp Framework for Testing"
  :long-description "LIFT is yet another SUnit variant. These are some self tests."
  
  :components ((:module "test" 
                        :components ((:file "lift-test")
                                     (:file "test-dynamic-variables"
                                            :depends-on ("lift-test"))
                                     (:file "equality-tests"
                                            :depends-on ("lift-test"))
                                     (:file "finding-tests"
                                            :depends-on ("lift-test"))
				     #+(or)
                                     (:file "test-prototypes" 
                                            :depends-on ("lift-test")))))
  
  :depends-on (lift))

(defmethod operation-done-p 
           ((o test-op)
            (c (eql (find-system 'lift-test))))
  (values nil))


