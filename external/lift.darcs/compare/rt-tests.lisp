#|
can you change the equality test?
|#

(in-package #:cl)
(defpackage #:kmrcl-tests-rt
  (:use #:kmrcl #:cl #:rtest))
(in-package #:kmrcl-tests-rt)

(rem-all-tests)

(deftest :str.0 (substitute-chars-strings "" nil) "")
(deftest :str.1 (substitute-chars-strings "abcd" nil) "abcd")
(deftest :str.2 (substitute-chars-strings "abcd" nil) "abcde")

(do-tests)