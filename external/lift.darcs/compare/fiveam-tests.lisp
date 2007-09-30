(defpackage "KMRCL-TESTS-5AM"
  (:use "COMMON-LISP" "KMRCL" "5AM"))
(in-package #:kmrcl-tests-5am)

(def-suite test-strings :description "Test some KMRCL string tests.")
(in-suite test-strings)
(test :str.0
  (is (substitute-chars-strings "" nil) ""))
(test :str.1
  (is (substitute-chars-strings "abcd" nil) "abcd"))
(test :str.2
  (is (substitute-chars-strings "abcd" nil) "abcde"))

(run! 'test-strings)