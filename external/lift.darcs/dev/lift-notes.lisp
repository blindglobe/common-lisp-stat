(in-package #:lift)

If a test suite isn't defined and try to define a test for it, you get an odd error instead 

#|
testsuite-setup
  testsuite-run
  testsuite-teardown

testsuite-run
  run each method of this suite using run-test-internal
  run child suites

run-test-internal
  start-test
  setup-test
    funcall test-method
    teardown-test
    end-test


;; sharing test code
(defmethod testsuite-run ((case test-with-generators) (result test-result))
  (loop for generators do
        (set-generators)
        (run-testsuite case result)))


;; single setup

My first thought (and attempt) was to have the test-suite do the setup for
single-setup? tests. This sort of works but gets wonky when a superclass has
it's own setup. It also doesn't make it easy to support making setup optional

My zeroth thought was to have a switch that each test checked.
  
  
  
  
  
|#
ok - test-teardown needs to remove vars from *test-environment*

in deftestsuite:     
  ;; create class first so that we can introspec on it
  ;;?? this is kind of weak

make sure that we have all the methods we need for
  generator tests, single setup tests, tests with timeout

could probably get rid of (need to have something that grabs super-classes then):
  (defun test-slot-names (test-name)
    (mopu-class-slot-names test-name))

custom error messages as conditions

better message
> Error: Failed assertion: (TEST-CLASS-P CLASS)
> While executing: CCL::%ASSERTION-FAILURE
> Type Command-/ to continue, Command-. to abort.
> If continued: test the assertion again.

a describe for test-mixins

make run-tests-internal a method
  make run-tests to the work
  possibly change *current-test-class* to a symbol always

make run-test a function, see above

simplify parsers

what should number-of-tests really do


Lift:

;;; ---------------------------------------------------------------------------
;;; sharing setup and teardown
;;; ---------------------------------------------------------------------------

;;;; currently, LIFT always shuffles cases within each class

(deftestsuite big-setup ()
  (:setup (long-slow-process))
  (:teardown (some-clean-up))
  :single-setup)


(addtest
  test-1
  )

(addtest
  test-2
  )

(deftestsuite foo-1 (big-setup)
  )

(addtest
  test-1-1
  )

(addtest
  test-1-2
  )
  
;;; ---------------------------------------------------------------------------
;;; want to use the same test on different things. So we need to have
;;; the same test body with different setup
;;; ---------------------------------------------------------------------------

(deftestsuite foo ()
  (<vars>)
  (:cases ...)
  (:setup ))


  
(subclasses* 'containers::abstract-queue)

(#<STANDARD-CLASS ABSTRACT-QUEUE> #<STANDARD-CLASS PRIORITY-QUEUE-HEAP>
 #<STANDARD-CLASS RING-BUFFER> #<STANDARD-CLASS BASIC-QUEUE>
 #<STANDARD-CLASS PRIORITY-QUEUE-ON-CONTAINER >)

No easy way to get, e.g., all the PQOCs b/c we don't know (and can't easily know)
which containers it can be on. Could add protocol.

(deftestsuite test-queue () ())

;;; ---------------------------------------------------------------------------

(deftestsuite test-queue-error-when-empty (test-queue)
  ((queue (make-container 'basic-queue)))
  (:test 
   (insert-item queue 2)
   (insert-item queue 4)
   (dequeue queue)
   (dequeue queue)
   (ensure-error (dequeue queue))))

;;; ---------------------------------------------------------------------------

(deftestsuite test-queue-error-when-empty (test-queue)
  ((queue (make-container 'basic-queue))))

(deftestsuite test-queue-error-when-empty (test-queue)
  ((queue :foreach (prototype-of 'abstract-queue))))

(deftestsuite test-queue-error-when-empty (test-queue)
  (queue)
  (:cases (queue (prototype-of 'abstract-queue))))

(addtest (test-queue-error-when-empty)
  (insert-item queue 2)
  (insert-item queue 4)
  (dequeue queue)
  (dequeue queue)
  (ensure-error (dequeue queue)))

;;; ---------------------------------------------------------------------------


;;; full factorial
(deftestsuite test-queue-error-when-empty (test-queue)
  (q1 q2 q3 q4)
  (:cases (queue-1 :foreach (prototype-of 'abstract-queue))
   (queue-2 :foreach (prototype-of 'abstract-queue))))

(deftestsuite test-queue-error-when-empty (test-queue)
  ((queue-1 :foreach (prototype-of 'abstract-queue))
   (queue-2 :foreach (prototype-of 'abstract-queue))))

(deftestsuite test-queue-error-when-empty (test-queue)
  ((queue-1 :foreach (prototype-of 'abstract-queue))
   (queue-2 :foreach (prototype-of 'abstract-queue))))

;;; as pairs
(deftestsuite test-queue-error-when-empty (test-queue)
  ((queue-1 :foreach (prototype-of 'abstract-queue))
   (queue-2 :andeach (prototype-of 'abstract-queue))))

;;; any generator will do
(deftestsuite foo ()
  ((number :foreach '(1 2 3 4 5 6))))

;;;; pros
;; simple syntax (so far!)
;;

;;;; Cons
;; conses up a list of prototypes instead of mapping them
;;   (I think that supporting mapping would create big changes in LIFT).
;;

;;;; Unknowns
;; what will this implement as?

;;; ---------------------------------------------------------------------------
;;; another way to handle setup for 'sequencial' tests
;;; ---------------------------------------------------------------------------
