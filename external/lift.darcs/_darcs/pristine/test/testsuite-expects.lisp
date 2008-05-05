(in-package #:lift-test)

(defvar *expect-error* nil)

(deftestsuite expect-error (lift-test)
  ())

(deftestsuite expect-error-helper ()
  ()
  (:expected-error *expect-error*))

(addtest (expect-error-helper)
  test-1
  (error "this is an error"))

(addtest (expect-error)
  test-expects-error
  (let* ((*expect-error* t)
	 (result (run-tests :suite 'expect-error-helper)))
    (ensure-same (lift::suites-run result) '(expect-error-helper) :test 'equal)
    (ensure-same (length (tests-run result)) 1)
    (ensure-same (length (errors result)) 0 :test '=))) 

(addtest (expect-error)
  test-does-not-expect-error
  (let* ((*expect-error* nil)
	 (result (run-tests :suite 'expect-error-helper)))
    (ensure-same (length (errors result)) 1 :test '=))) 

;;;;

(defvar *expect-failure* nil)

(deftestsuite expect-failure (lift-test)
  ())

(deftestsuite expect-failure-helper ()
  ()
  (:expected-failure *expect-failure*))

(addtest (expect-failure-helper)
  test-1
  (ensure-null "this is an failure"))

(addtest (expect-failure)
  test-expects-failure
  (let* ((*expect-failure* t)
	 (result (run-tests :suite 'expect-failure-helper)))
    (ensure-same (lift::suites-run result)
		 '(expect-failure-helper) :test 'equal)
    (ensure-same (length (tests-run result)) 1)
    (ensure-same (length (failures result)) 0 :test '=))) 

(addtest (expect-failure)
  test-does-not-expect-failure
  (let* ((*expect-failure* nil)
	 (result (run-tests :suite 'expect-failure-helper)))
    (ensure-same (length (failures result)) 1 :test '=))) 


