;;;-*- Mode: Lisp; Package: LIFT -*-

#| 

See file COPYING for license

|#

(in-package #:lift-test)

(deftestsuite lift-test () ())

;;; ---------------------------------------------------------------------------
;;; lift-test-ensure
;;; make sure that ensure and its friends work as expected
;;;
;;; The strategy here is to pair "regular" tests with meta-tests. The 
;;; regular tests are normal tests written using LIFT. The meta-tests
;;; use run-tests or run-tests to run the regular test and then grovel
;;; over the returned test-result to make sure it contains what it is
;;; supposed to.
;;;
;;; Note that if we don't pass in :report-pathname nil, then we'll get a lot
;;; of spurious extra report files...
;;; ---------------------------------------------------------------------------

(deftestsuite lift-test-ensure (lift-test) ())

(deftestsuite lift-test-ensure-helper () ())

(addtest (lift-test-ensure-helper)
  simple-ensure-test-1
  (ensure t))

(addtest (lift-test-ensure)
  simple-ensure-test-1
  (let ((tr (run-test :suite 'lift-test-ensure-helper
		      :name 'simple-ensure-test-1)))
    (ensure-same (length (tests-run tr)) 1)
    (ensure-null (failures tr))
    (ensure-null (errors tr))
    (ensure-same (test-mode tr) :single)
    (ensure-same (mapcar #'second (tests-run tr)) 
		 '(lift-test::simple-ensure-test-1))))

;;; ---------------------------------------------------------------------------

(addtest (lift-test-ensure-helper)
  simple-ensure-test-2
  (ensure nil))

(addtest (lift-test-ensure)
  simple-ensure-test-2
  (let ((tr (run-test :suite 'lift-test-ensure-helper
		      :name 'simple-ensure-test-2)))
    (ensure-same (length (tests-run tr)) 1 :report "Number of tests-run")
    (ensure-same (length (failures tr)) 1 :report "Number of failures")
    (ensure-null (errors tr) :report "Number of errors")
    (ensure-same (mapcar #'second (tests-run tr))
		 '(lift-test::simple-ensure-test-2))))

;;; ---------------------------------------------------------------------------

(addtest (lift-test-ensure-helper)
  simple-ensure-test-3
  (ensure (let ((x 0)) (/ x))))

(addtest (lift-test-ensure)
  simple-ensure-test-3
  (let ((tr (run-test :suite 'lift-test-ensure-helper
		      :name 'simple-ensure-test-3)))
    (ensure-same (length (tests-run tr)) 1)
    (ensure-same (length (failures tr)) 0)
    (ensure-same (length (errors tr)) 1)
    (ensure-same (mapcar #'second (tests-run tr)) 
		 '(lift-test::simple-ensure-test-3))))


;;; ---------------------------------------------------------------------------
;;; lift-test-setup-teardown
;;; make sure that setup and teardown happen in the right order
;;; ---------------------------------------------------------------------------

(deftestsuite lift-test-setup-teardown (lift-test) ())

(deftestsuite lift-test-setup-teardown-1 (lift-test-setup-teardown) ()
  (:setup (push 1 *test-notepad*))
  (:teardown (push :a *test-notepad*))
  (:tests (setup-teardown-1 (push 'test-1 *test-notepad*))))

(addtest (lift-test-setup-teardown)
  setup-teardown-1
  (setf *test-notepad* nil)
  (run-test
   :name 'setup-teardown-1
   :suite 'lift-test-setup-teardown-1
   :result (make-test-result 'lift-test-setup-teardown-1 :single))
  (ensure-same (reverse *test-notepad*)
               '(1 test-1 :a)))

(addtest (lift-test-setup-teardown) 
  setup-teardown-1-all
  (setf *test-notepad* nil)
  (run-tests 
   :suite 'lift-test-setup-teardown-1
   :result (make-test-result 'lift-test-setup-teardown-1 :multiple)
   :report-pathname nil)
  (ensure-same (reverse *test-notepad*)
               '(1 test-1 :a 1 2 test-2 :b :a 1 2 3 test-3 :c :b :a)))

(deftestsuite lift-test-setup-teardown-2 (lift-test-setup-teardown-1) ()
  (:setup (push 2 *test-notepad*))
  (:teardown (push :b *test-notepad*))
  (:tests (setup-teardown-2 (push 'test-2 *test-notepad*))))

(deftestsuite lift-test-setup-teardown-3 (lift-test-setup-teardown-2) ()
  (:setup (push 3 *test-notepad*))
  (:teardown (push :c *test-notepad*))
  (:tests (setup-teardown-3 (push 'test-3 *test-notepad*))))

(addtest (lift-test-setup-teardown) 
  setup-teardown-3
  (setf *test-notepad* nil)
  (run-test
   :name 'setup-teardown-3
   :suite 'lift-test-setup-teardown-3
   :result (make-test-result 'lift-test-setup-teardown-3 :single))
  (ensure-same (reverse *test-notepad*)
               '(1 2 3 test-3 :c :b :a)))

(addtest (lift-test-setup-teardown)
  setup-teardown-3-all
  (setf *test-notepad* nil)
  (run-tests 
   :suite 'lift-test-setup-teardown-3
   :result (make-test-result 'lift-test-setup-teardown-3 :multiple)
   :report-pathname nil)
  (ensure-same (reverse *test-notepad*)
               '(1 2 3 test-3 :c :b :a)))

;;; ---------------------------------------------------------------------------
;;; test ensure same
;;; ---------------------------------------------------------------------------

(deftestsuite lift-test-ensure-same (lift-test)
  ())

;;?? Gary King 2004-06-21: not really a test yet, more of a syntax works check
(addtest (lift-test-ensure-same)
  (ensure-same 2 2 :test =)
  (ensure-same 2 2 :test '=)
  (ensure-same 2 2 :test #'=))

;;; ---------------------------------------------------------------------------
;;; test single setup
;;; ---------------------------------------------------------------------------

(deftestsuite test-single-setup (lift-test) ())

;; helpers
(deftestsuite test-single-setup-helper () ())

(deftestsuite test-single-setup-child-a (test-single-setup-helper) () 
  (:setup (push :a *test-notepad*))
  (:test (test-1 (ensure t))))

(deftestsuite test-single-setup-child-a-1 (test-single-setup-child-a) () 
  (:setup (push :a-1 *test-notepad*))
  (:test (test-1 (ensure t)))
  (:test (test-2 (ensure t))))

(deftestsuite test-single-setup-child-b (test-single-setup-helper) ()
  (:setup (push :b *test-notepad*))
  (:test (test-1 (ensure t))))

(deftestsuite test-single-setup-child-b-1-ss (test-single-setup-child-b) ()
  (:run-setup :once-per-suite)
  (:setup (push :b-1 *test-notepad*))
  (:test (test-1 (ensure t)))
  (:test (test-2 (ensure t))))

(deftestsuite test-single-setup-child-b-1-a (test-single-setup-child-b-1-ss) ()
  (:setup (push :b-1-a *test-notepad*))
  (:test (test-1 (ensure t)))
  (:test (test-2 (ensure t))))

(deftestsuite test-single-setup-child-b-1-b (test-single-setup-child-b-1-ss) ()
  (:setup (push :b-1-b *test-notepad*))
  (:test (test-1 (ensure t)))
  (:test (test-2 (ensure t))))

(deftestsuite test-single-setup-child-c (test-single-setup-helper) ()
  (:setup (push :c *test-notepad*))
  (:test (test-1 (ensure t))))

(deftestsuite test-single-setup-child-c-1 (test-single-setup-child-c) ()
  (:setup (push :c-1 *test-notepad*))
  (:test (test-1 (ensure t))))

;;; ---------------------------------------------------------------------------

(addtest (test-single-setup)
  test-a-multiple-setup
  (setf *test-notepad* nil)
  (run-test :suite 'test-single-setup-child-a-1 :name 'test-1)
  (run-test :suite 'test-single-setup-child-a-1 :name 'test-2)
  (ensure-same *test-notepad* '(:a-1 :a :a-1 :a)))

(addtest (test-single-setup)
  test-b-single-setup-1
  (setf *test-notepad* nil)
  (run-test :suite 'test-single-setup-child-b-1-ss :name 'test-1)
  (run-test :suite 'test-single-setup-child-b-1-ss :name 'test-2)
  ;; single tests do all the setup so this should be exactly the same
  (ensure-same *test-notepad* '(:b-1 :b :b-1 :b)))

(addtest (test-single-setup)
  test-a-single-setup-2
  (setf *test-notepad* nil)
  (run-tests :suite 'test-single-setup-child-a-1 :do-children? nil
	     :report-pathname nil)
  (ensure-same *test-notepad* '(:a-1 :a :a-1 :a)))

(addtest (test-single-setup)
  test-a-single-setup-3
  (setf *test-notepad* nil)
  (run-tests :suite 'test-single-setup-child-a-1 
	     :run-setup :once-per-suite
	     :do-children? nil
	     :report-pathname nil)
  (ensure-same *test-notepad* '(:a-1 :a :a-1 :a)))

(addtest (test-single-setup)
  test-b-single-setup-2
  (setf *test-notepad* nil)
  (run-tests :suite 'test-single-setup-child-b-1-ss :do-children? nil
	     :report-pathname nil)
  (ensure-same *test-notepad* '(:b-1 :b)))

;;; ---------------------------------------------------------------------------
;;; warning behavior
;;; ---------------------------------------------------------------------------

(deftestsuite test-ignore-warnings (lift-test) ())

(deftestsuite test-ignore-warnings-helper () ())

(deftestsuite test-ignore-warnings-helper-warning (test-ignore-warnings-helper) ()
  (:test (do-it 
          (push :a *test-scratchpad*)
          (warn "Ouch")
          (push :b *test-scratchpad*))))

(deftestsuite test-ignore-warnings-helper-no-warning (test-ignore-warnings-helper) ()
  (:test (do-it
          (push :a *test-scratchpad*)
          (+ 2 2)
          (push :b *test-scratchpad*))))

(addtest (test-ignore-warnings)
  test-has-warning
  (run-test :suite 'test-ignore-warnings-helper-warning :name 'do-it)
  (ensure-same *test-scratchpad* '(:b :a)))

(addtest (test-ignore-warnings)
  test-has-no-warning
  (run-test :suite 'test-ignore-warnings-helper-no-warning :name 'do-it)
  (ensure-same *test-scratchpad* '(:b :a)))

;;; ---------------------------------------------------------------------------
;;; test-environment stays clean
;;; ---------------------------------------------------------------------------

(deftestsuite lift-test-environment-pristine (lift-test) ()
  (:setup (setf *test-environment* nil)))

(deftestsuite lift-test-environment-pristine-helper ()
  ((a 2)
   (b (* a a))))

(addtest (lift-test-environment-pristine-helper)
  do-it
  (ensure-same (* a a) b))

(addtest (lift-test-environment-pristine
	  :expected-failure "This is no longer guarenteed; I'm not sure yet whether or not this is a good thing.")
  test-1
  (run-test :suite 'lift-test-environment-pristine-helper :name 'do-it)
  (ensure (null *test-environment*)))


;;; ---------------------------------------------------------------------------
;;; test-creating-multiple-tests
;;; ---------------------------------------------------------------------------

(deftestsuite test-creating-multiple-tests (lift-test)
  ())

(deftestsuite test-creating-multiple-tests-helper ()
 ()
 (:tests ((ensure-same 1 1)
          (ensure-same 2 2))
         ((ensure-same 3 3))))

(addtest (test-creating-multiple-tests)
  test-1
  (ensure-same (testsuite-test-count 'test-creating-multiple-tests-helper) 2))

;;;;;

(defvar *dynamics-before-setup* :dbs)

(deftestsuite dynamics-before-setup (lift-test)
  ()
  :setup (setf *test-notepad* nil))

(deftestsuite dynamics-before-setup-helper ()
  ((slot (progn (push :slot *test-notepad*) :slot)))
  :dynamic-variables (*dynamics-before-setup* 
		      (progn (push :dynamics *test-notepad*) :dynamics))
  :setup (push :setup *test-notepad*))

(addtest (dynamics-before-setup-helper)
  test-1
  (push :test *test-notepad*)
  (ensure-same *dynamics-before-setup* :dynamics))

(addtest (dynamics-before-setup)
  test-1
  (run-test :suite 'dynamics-before-setup-helper
	    :name 'test-1)
  (ensure-same (reverse *test-notepad*)
	       '(:slot :dynamics :setup :test)))


;;;;;
;;; inherited functions

(deftestsuite test-inherited-functions-helper ()
  ()
  (:function 
   (really? (a b c)
	    (ensure-same (+ a b) c :test '=))))

(deftestsuite test-inherited-functions-pos (test-inherited-functions-helper)
  ()
  (:tests ((really? 1 2 3))
	  ((really? 4 5 9))))
	  
(deftestsuite test-inherited-functions-neg (test-inherited-functions-helper)
  ()
  (:tests ((really? -4 -2 -6))
	  ((really? -1 -1 -2))))

(deftestsuite test-inherited-functions (lift-test)
  ())

(addtest (test-inherited-functions)
  one
  (let ((tr (run-tests :suite 'test-inherited-functions-helper
		       :report-pathname nil)))
    (ensure-same (length (tests-run tr)) 4)
    (ensure-null (failures tr))
    (ensure-null (errors tr))))


;;;;;
;;; slot initialization takes place with every setup

(deftestsuite test-initialize-slots-helper ()
  ((slot (incf *test-notepad*))))

(addtest (test-initialize-slots-helper)
  one
  (ensure t))

(addtest (test-initialize-slots-helper)
  two
  (ensure-null nil))

(deftestsuite test-initialize-slots (lift-test)
  ()
  (:setup (setf *test-notepad* 0)))

(addtest (test-initialize-slots)
  slot-initform-evaluated-every-time
  (let ((tr (run-tests :suite 'test-initialize-slots-helper
		       :report-pathname nil)))
    (ensure-same (length (tests-run tr)) 2)
    (ensure-same *test-notepad* 2 :test '=)))

;;;;;
;;; errors during tests are reported in the test result

(defun cause-an-error ()
  (error "this is an error"))

(deftestsuite test-error-catching (lift-test)
  ())

(deftestsuite test-error-catching-helper-slot-init ()
  ((x (cause-an-error))))

(addtest (test-error-catching-helper-slot-init)
  slot-init
  (ensure t))

(addtest (test-error-catching)
  helper-slot-init
  (let ((result (run-test :suite 'test-error-catching-helper-slot-init
			  :name 'slot-init)))
    (ensure-same 1 (length (lift::suites-run result)))
    (ensure-same 1 (length (errors result)))))

;;;

(deftestsuite test-error-catching-helper-body ()
  ())

(addtest (test-error-catching-helper-body)
  body
  (cause-an-error))

(addtest (test-error-catching)
  helper-body
  (let ((result (run-test :suite 'test-error-catching-helper-body
			  :name 'body)))
    (ensure-same 1 (length (tests-run result)))
    (ensure-same 1 (length (errors result)))))

;;;

(deftestsuite test-error-catching-helper-setup ()
  ()
  (:setup
   (cause-an-error)))

(addtest (test-error-catching-helper-setup)
  setup
  (ensure t))

(addtest (test-error-catching)
  helper-setup
  (let ((result (run-test :suite 'test-error-catching-helper-setup
			  :name 'setup)))
    (ensure-same 1 (length (tests-run result)))
    (ensure-same 1 (length (errors result)))))

;;;

(deftestsuite test-error-catching-helper-teardown ()
  ()
  (:teardown
   (cause-an-error)))

(addtest (test-error-catching-helper-teardown)
  teardown
  (ensure t))

(addtest (test-error-catching)
  helper-teardown
  (let ((result (run-test :suite 'test-error-catching-helper-teardown
			  :name 'teardown)))
    (ensure-same 1 (length (tests-run result)))
    (ensure-same 1 (length (errors result)))))

;;;

(defvar *test-error-catching-helper*)

(deftestsuite test-error-catching-helper-dynamic-variables ()
  ()
  (:dynamic-variables
   (*test-error-catching-helper* (cause-an-error))))

(addtest (test-error-catching-helper-dynamic-variables)
  dynamic-variables
  (ensure t))

(addtest (test-error-catching)
  helper-dynamic-variables
  (let ((result (run-test :suite 'test-error-catching-helper-dynamic-variables
			  :name 'dynamic-variables)))
    (ensure-same 1 (length (lift::suites-run result)))
    (ensure-same 1 (length (errors result)))))

;;;

(deftestsuite test-error-catching-helper-equality-test ()
  ()
  (:equality-test
   (cause-an-error)))

(addtest (test-error-catching-helper-equality-test)
  equality-test
  (ensure t))

(addtest (test-error-catching)
  helper-equality-test
  (let ((result (run-test :suite 'test-error-catching-helper-equality-test
			  :name 'equality-test)))
    (ensure-same 0 (length (lift::suites-run result))) ;hmmm
    (ensure-same 1 (length (errors result)))))

;;;;

(deftestsuite test-interaction (lift-test)
  ()
  (:equality-test #'string=))

(addtest (test-interaction)
  run-test-sets-values
  (run-test :suite 'lift-test-ensure-helper :name 'simple-ensure-test-3)
  (ensure-same 
   (symbol-name lift::*current-test-case-name*)
   (symbol-name 'simple-ensure-test-3))
  (ensure-same 
   (symbol-name lift::*current-testsuite-name*)
   (symbol-name 'lift-test-ensure-helper)))

(addtest (test-interaction)
  run-tests-sets-values
  (run-tests :suite 'lift-test-ensure-helper
	     :report-pathname nil)
  (ensure-same 
   (symbol-name lift::*current-testsuite-name*)
   (symbol-name 'lift-test-ensure-helper))
  (ensure-same 
   (symbol-name lift::*current-test-case-name*)
   (symbol-name 'simple-ensure-test-3)))

(addtest (test-interaction)
  run-test-sets-values-nested
  (run-test :suite 'test-interaction :test-case 'run-tests-sets-values)
  (ensure-same 
   (symbol-name lift::*current-testsuite-name*)
   (symbol-name 'test-interaction))
  (ensure-same 
   (symbol-name lift::*current-test-case-name*)
   (symbol-name 'run-tests-sets-values)))

;;;;

(deftestsuite test-expected-errors (lift-test)
  ())

(deftestsuite test-expected-errors-helper ()
  ())

(addtest (test-expected-errors-helper
	  :expected-error t)
  test-1
  (error "this is an error"))

(addtest (test-expected-errors)
  test-passes
  (let ((result (run-tests :suite 'test-expected-errors-helper
			   :report-pathname nil)))
    (ensure-same 1 (length (tests-run result)))
    (ensure-same 0 (length (errors result)))
    (ensure-same 1 (length (expected-errors result)))
    ))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *test-expected-errors-helper-2* nil))

(deftestsuite test-expected-errors-helper-2 ()
  ())

(addtest (test-expected-errors-helper-2
	  :expected-error *test-expected-errors-helper-2*)
  test-1
  (error "this is an error"))

(addtest (test-expected-errors)
  test-expected-error-helper-true
  (let* ((*test-expected-errors-helper-2* t)
	 (result (run-tests :suite 'test-expected-errors-helper-2
			   :report-pathname nil)))
    (ensure-same 1 (length (tests-run result)))
    (ensure-same 0 (length (errors result)))
    (ensure-same 1 (length (expected-errors result)))
    ))

(addtest (test-expected-errors)
  test-expected-error-helper-false
  (let* ((*test-expected-errors-helper-2* nil)
	 (result (run-tests :suite 'test-expected-errors-helper-2
			   :report-pathname nil)))
    (ensure-same 1 (length (tests-run result)))
    (ensure-same 1 (length (errors result)))
    (ensure-same 0 (length (expected-errors result)))
    ))

(addtest (test-expected-errors)
  donot-break-on-errors
  ;; this is weird
  ;; I wonder if it's worth trying to abstract "up"
  (let ((*debugger-hook* (lambda (condition hook)
			   (declare (ignore hook))
			   (when (find-restart 'entered-debugger)
			     (invoke-restart 'entered-debugger condition))
			   (invoke-debugger condition))))
    (restart-case
      (let ((result (run-tests :suite 'test-expected-errors-helper 
			       :report-pathname nil
			       :break-on-errors? t)))
	(ensure-same 1 (length (tests-run result)))
	(ensure-same 0 (length (errors result)))
	(ensure-same 1 (length (expected-errors result)))
	)
    (entered-debugger (c)
      (declare (ignore c))
      (ensure-null "We should not be here")))))

;;;;

;;?? these pass but the cliquep test did not seem to be working. Why?
(deftestsuite test-scratchpad-resets (lift-test)
  ())

(deftestsuite test-scratchpad-resets-helper ()
  ()
  (:test
   (test-3 (push :test *test-scratchpad*)))
  (:test
   (test-4 (push :burt *test-scratchpad*))))

(addtest (test-scratchpad-resets)
  run-once-have-one
  (run-test :suite 'test-scratchpad-resets-helper :test-case 'test-3)
  (ensure-same '(:test) *test-scratchpad*))

(addtest (test-scratchpad-resets)
  run-twice-have-one
  (run-test :suite 'test-scratchpad-resets-helper :test-case 'test-3)
  (run-test :suite 'test-scratchpad-resets-helper :test-case 'test-3)
  (ensure-same '(:test) *test-scratchpad*))
 
(addtest (test-scratchpad-resets)
  run-twice-have-one-run-tests
  (run-tests :suite 'test-scratchpad-resets-helper :report-pathname nil)
  (run-tests :suite 'test-scratchpad-resets-helper :report-pathname nil)
  (ensure-same '(:burt) *test-scratchpad*))

;;;;

(deftestsuite test-break-on-failure (lift-test)
  ())

(deftestsuite test-break-on-failure-helper ()
  ()
  ;; :categories (foo bar)
  )

(addtest (test-break-on-failure-helper)
  failing-test
  (ensure-null "this fails"))

(addtest (test-break-on-failure)
  donot-break-on-failures
  (let* ((*test-break-on-failures?* nil)
	 (result (run-tests :suite 'test-break-on-failure-helper
			    :report-pathname nil)))
    (ensure-same 1 (length (tests-run result)))
    (ensure-same 1 (length (failures result)))))

(addtest (test-break-on-failure)
  do-break-on-failures
  (let* ((*test-break-on-failures?* t)
	 (*debugger-hook* (lambda (condition hook)
			    (declare (ignore hook))
			    (when (find-restart 'entered-debugger)
			      (invoke-restart 'entered-debugger condition))
			    (invoke-debugger condition)))
	 (result nil))
    (restart-case
	(setf result (run-tests :suite 'test-break-on-failure-helper
				:report-pathname nil))
      (entered-debugger (c)
	(declare (ignore c))
	(setf *test-scratchpad* t)))
    (ensure-null result)
    (ensure-same *test-scratchpad* t :test 'eq)))

;;;;

(deftestsuite ensure-no-warning (lift-test)
  ())

(deftestsuite ensure-no-warning-helper ()
  ())

(addtest (ensure-no-warning-helper)
  test-1
  (ensure-no-warning (ensure-same (+ 2 2) 4)))

(addtest (ensure-no-warning-helper)
  test-2
  (ensure-no-warning (ensure-same (+ 2 2) 4)
		     (warn "I like math")))

(addtest (ensure-no-warning)
  run-tests
  (let ((result (run-tests :suite 'ensure-no-warning-helper
			   :report-pathname nil)))
    (ensure-same (length (tests-run result)) 2)
    (ensure-same (length (failures result)) 1)
    (ensure-same (length (errors result)) 0)))


