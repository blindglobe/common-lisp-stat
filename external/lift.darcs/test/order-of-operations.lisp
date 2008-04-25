(in-package #:lift-test)

;; see lift-test-setup-teardown tests too
(deftestsuite order-of-operations (lift-test)
  ()
  (:setup
   (setf *test-notepad* nil)))

(deftestsuite order-of-operations-helper ()
  ((slot (push :slot-init-parent *test-notepad*)))
  (:setup
   (push :setup-parent *test-notepad*))
  (:teardown 
   (push :teardown-parent *test-notepad*)))

(addtest (order-of-operations-helper)
  test-1
  (push :run-test-parent *test-notepad*))

(deftestsuite order-of-operations-helper-subclass (order-of-operations-helper)
  ((slot (push :slot-init-child *test-notepad*)))
  (:setup
   (push :setup-child *test-notepad*))
  (:teardown 
   (push :teardown-child *test-notepad*)))

(addtest (order-of-operations-helper-subclass)
  test-1
  (push :run-test-child *test-notepad*))

(addtest (order-of-operations) 
  run-parent-test
  (run-test :suite 'order-of-operations-helper
	    :name 'test-1
	    :result (make-test-result 'order-of-operations-helper :single))
  (ensure-same 
   (reverse *test-notepad*)
   (list :slot-init-parent :setup-parent :run-test-parent :teardown-parent)))

(addtest (order-of-operations) 
  run-child-test
  (run-test :suite 'order-of-operations-helper-subclass
	    :name 'test-1
	    :result (make-test-result 
		     'order-of-operations-helper-subclass :single))
  (ensure-same 
   (reverse *test-notepad*)
   (list :slot-init-parent
	 :slot-init-child 
	 :setup-parent
	 :setup-child 
	 :run-test-child
	 :teardown-child
	 :teardown-parent)))
