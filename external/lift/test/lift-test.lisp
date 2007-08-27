;;;-*- Mode: Lisp; Package: LIFT -*-

#| 

See file COPYING for license

|#

(defpackage #:lift-test 
  (:use #:common-lisp #:lift)
  (:import-from #:lift
                #:failures
                #:errors
                #:tests-run
                #:test-mode
                #:test-interactive?
                #:make-test-result
                #:testsuite-test-count))
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
    (ensure-same (failures tr) nil)
    (ensure-same (errors tr) nil)
    (ensure-same (test-mode tr) :single)
;    (ensure-same (test-interactive? tr) nil)
    (ensure-same (mapcar #'first (tests-run tr)) 
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
    (ensure-same (errors tr) nil :report "Number of errors")
    (ensure-same (mapcar #'first (tests-run tr))
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
    (ensure-same (mapcar #'first (tests-run tr)) 
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
   :result (make-test-result 'lift-test-setup-teardown-1 :multiple))
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
   :result (make-test-result 'lift-test-setup-teardown-3 :multiple))
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
  (run-tests :suite 'test-single-setup-child-a-1 :do-children? nil)
  (ensure-same *test-notepad* '(:a-1 :a :a-1 :a)))

(addtest (test-single-setup)
  test-a-single-setup-2
  (setf *test-notepad* nil)
  (run-tests :suite 'test-single-setup-child-a-1 
	     :run-setup :once-per-suite
	     :do-children? nil)
  (ensure-same *test-notepad* '(:a-1 :a :a-1 :a)))

(addtest (test-single-setup)
  test-b-single-setup-2
  (setf *test-notepad* nil)
  (run-tests :suite 'test-single-setup-child-b-1-ss :do-children? nil)
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

(addtest (lift-test-environment-pristine)
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
	       '(:dynamics :slot :setup :test)))

