(in-package #:lift-test)

(defvar *a* 1)

(deftestsuite test-dynamic-variables (lift-test)
  ())

(deftestsuite test-dynamic-variables-1 (test-dynamic-variables)
  ()
  (:dynamic-variables (*a* 2))
  (:test (test-1 (ensure-same *a* 2))))

(deftestsuite test-dynamic-variables-2 (test-dynamic-variables)
  ()
  (:test (test-1 (ensure-same *a* 1)))
  (:test (test-2 (ensure *a*))))

