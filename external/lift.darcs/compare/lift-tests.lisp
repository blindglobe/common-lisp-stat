(defpackage "KMRCL-TESTS-LIFT"
  (:use "COMMON-LISP" "KMRCL" "LIFT"))
(in-package #:kmrcl-tests-lift)

(deftestsuite test-strings () ())
(addtest :str.0
  (ensure-same (substitute-chars-strings "" nil) ""))
(addtest :str.1
  (ensure-same (substitute-chars-strings "abcd" nil) "abcd"))
(addtest :str.2 
  (ensure-same (substitute-chars-strings "abcd" nil) "abcde"))

(run-tests)
