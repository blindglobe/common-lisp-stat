(in-package #:lift-test)

(deftestsuite test-a ()
  (a)
  (:cases (a '(1 2))))

(defmethod initialize-prototypes :after ((test test-plus))
  (with-test-slots (a b)
    (setf (prototypes test)
          (list
           (list (let* ((a 1))
                   (cons 'a a))
                 (let* ((a 2))
                   (cons 'a a)))))))

(defmethod initialize-prototypes :after ((test test-plus))
  (with-test-slots (a b)
    (setf (prototypes test)
          (list
           (list (let* ((a 1))
                   (cons 'a a))
                 (let* ((a 2))
                   (cons 'a a)))))))

(defmethod initialize-prototypes :after ((test test-plus))
  (with-test-slots (a b)
    (setf (prototypes test)
          (let* ((a 0) (b 0))
            (list
             (list (cons 'a a) (cons 'b b)))))))

(deftestsuite test-a ()
  ((a '(1 2))))

(deftestsuite test-a ()
  (a)
  (:cases (a '((1 2)))))

(addtest (test-a)
  (format t "~%~A" a))
  
(deftestsuite test-b (test-a)
  (b)
  (:cases (b '(4 5 6))))

(addtest (test-b)
  (format t "~%  ~A x ~A" a b))


(deftestsuite test-e ()
  (a))

(defmethod initialize-prototypes :after ((test test-e))
  (setf (prototypes test)
        (list
         (list (cons 'a 1))
         (list (cons 'a 2)))))

(addtest (test-e)
  (format t "~%~A" a))
  
(deftestsuite test-f (test-e)
  (b))

(defmethod initialize-prototypes :after ((test test-f))
  (setf (prototypes test)
        (list
         (list (cons 'b 4))
         (list (cons 'b 5))
         (list (cons 'b 6)))))

(addtest (test-f)
  (format t "~%  ~A x ~A" a b))

1
  1 4
  1 5
  1 6
2 
  2 4
  2 5
  2 6


(defgeneric setup (suite)
  (:documentation "Setup at the testsuite-level")
  (:method ((suite test-mixin))
           (values)))

(defgeneric testsuite-teardown (suite)
  (:documentation "Cleanup at the testsuite level.")
  (:method ((suite test-mixin))
           (values)))

(defgeneric testsuite-run (suite result)
  (:documentation "Run the cases in this suite and it's children."))

(defgeneric setup-test (test-case)
  (:documentation "Setup for a test-case. By default it does nothing."))

(defgeneric teardown-test (test-case)
  (:documentation "Tear-down a test-case. By default it does nothing.")
  (:method-combination progn :most-specific-first))

(defgeneric testsuite-methods (test-case)
  (:documentation "Returns a list of the test methods defined for test. I.e.,
the methods that should be run to do the tests for this test."))

;;;;;;;;;;;;;;;;;;

(deftestsuite setup-and-slots-hierarchy-parent ()
  ((slot-parent (progn (push :slot-parent *test-scratchpad*) :a)))
  :setup (push :setup-parent *test-scratchpad*)
  :teardown (push :teardown-parent *test-scratchpad*))

(deftestsuite setups-and-slots-hierarchy-child
    (setup-and-slots-hierarchy-parent)
  ((slot-child (progn (push :slot-child *test-scratchpad*) :a)))
  :setup (push :setup-child *test-scratchpad*)
  :teardown (push :teardown-child *test-scratchpad*))



;;;;;;;;;;;;;;;;;;;;;



(defvar *dynamics-before-setup* :dbs)

(deftestsuite dynamics-before-setup ()
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

(run-test :break-on-errors? t)

  



(deftestsuite warnings-and-errors ()
  ())

(defun warnings-and-errors-function (mode)
  (ecase mode
    (:warn (warn "this is a warning all by itself"))
    (:error (error "this is an error all by itself"))
    (:warn-error (warn "first we warn") (error "then we error"))
    (:error-warn (error "first we error") (warn "then we warn"))))

(addtest (warnings-and-errors)
  warning-does-not-hide-error-1
  (ensure-error (warnings-and-errors-function :warn-error)))

(addtest (warnings-and-errors)
  warning-does-not-hide-error-2
  (ensure-warning (warnings-and-errors-function :warn-error)))



;;;;
