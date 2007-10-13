(in-package #:cl)
(defpackage #:kmrcl-tests-xlunit
  (:use #:kmrcl #:cl #:xlunit))
(in-package #:kmrcl-tests-xlunit)

(defclass string-test-case (test-case)
  ())
(def-test-method :str.0 ((test string-test-case) :run nil)
  (assert-true (string-equal (substitute-chars-strings "" nil) "")))
(def-test-method  :str.1 ((test string-test-case) :run nil)
  (assert-true (string-equal (substitute-chars-strings "abcd" nil) "abcd")))
(def-test-method  :str.1 ((test string-test-case) :run nil)
  (assert-true (string-equal (substitute-chars-strings "abcd" nil) "abcde")))

(textui-test-run (get-suite string-test-case))
