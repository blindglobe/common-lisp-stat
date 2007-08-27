(in-package lift)

(deftestsuite equality-test-1 ()
  ()
  (:equality-test '=)
  (:documentation "Ensure that equality-test is inherited"))

(addtest (equality-test-1) test-1
	 (ensure-same *lift-equality-test* '= :test #'equal))

(deftestsuite equality-test-2 (equality-test-1)
  ())

(addtest (equality-test-2) test-1
	 (ensure-same *lift-equality-test* '= :test #'equal))

(deftestsuite equality-test-3 (equality-test-1)
  ()
  (:equality-test 'equalp))

(addtest (equality-test-3) test-1
	 (ensure-same *lift-equality-test* 'equalp :test #'equal))

#|
(testsuite-methods 'equality-test-1)  
(generic-functions 'equality-test-1)

(run-tests :suite 'equality-test-1)
(list-tests)
(list-tests :start-at 'equality-test-1)
(run-tests :suite 'equality-test-2)
(run-tests :suite 'equality-test-3)
|#

