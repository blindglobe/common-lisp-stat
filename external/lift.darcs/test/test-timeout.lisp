(in-package #:lift-test)

(deftestsuite test-lift-timeouts (lift-test)
  ())

(deftestsuite test-timeout-helper () 
  ()
  :timeout 1)

(addtest (test-timeout-helper)
  success-1
  (ensure t))

(addtest (test-timeout-helper)
  timeout-1
  (sleep 2)
  (ensure t))




#|

(deftestsuite test-timeout ()
  ()
  (:timeout 1))

(addtest (test-timeout)
  test-should-pass
  (sleep 0.5)
  (ensure t))

(addtest (test-timeout)
  test-should-fail
  (sleep 1.5)
  (ensure t))
|#