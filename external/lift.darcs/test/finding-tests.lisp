(in-package #:lift-test)

;;; ---------------------------------------------------------------------------
;;; lift-test-ensure
;;; make sure that ensure and its friends work as expected
;;;
;;; The strategy here is to pair "regular" tests with meta-tests. The 
;;; regular tests are normal tests written using LIFT. The meta-tests
;;; use run-tests or run-tests to run the regular test and then grovel
;;; over the returned test-result to make sure it contains what it is
;;; supposed to.
;;; ---------------------------------------------------------------------------

(defpackage #:one-test-package
  (:use #:common-lisp #:lift))

(defpackage #:two-test-package 
  (:use #:common-lisp #:lift))

(deftestsuite one-test-package::test-finding () ())

(deftestsuite two-test-package::test-finding () ())

(deftestsuite test-find-test-suite (lift-test) ())

(addtest (test-find-test-suite)
  simple-searching
  (ensure-same (lift::find-testsuite 'test-find-test-suite)
	       'test-find-test-suite)
  (ensure-same (lift::find-testsuite 'one-test-package::test-finding)
	       'one-test-package::test-finding)
  (ensure-same (lift::find-testsuite 'two-test-package::test-finding)
	       'two-test-package::test-finding))

(addtest (test-find-test-suite)
  not-such-suite
  (ensure-condition lift::test-class-not-defined
    (lift::find-testsuite (gensym))))

(addtest (test-find-test-suite)
  two-many-tests
  (ensure-error (lift::find-testsuite "test-finding")))

