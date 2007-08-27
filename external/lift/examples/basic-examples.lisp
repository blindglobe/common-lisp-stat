;;;-*- Mode: Lisp; Package: LIFT -*-

#| simple-header

Copyright (c) 2001-2006 Gary Warren King (gwking@cs.umass.edu) 

Permission is hereby granted, free of charge, to any person obtaining a 
copy of this software and associated documentation files (the "Software"),
to deal in the Software without restriction, including without limitation
the rights to use, copy, modify, merge, publish, distribute, sublicense, 
and/or sell copies of the Software, and to permit persons to whom the 
Software is furnished to do so, subject to the following conditions: 

The above copyright notice and this permission notice shall be included in 
all copies or substantial portions of the Software. 

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL 
THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER 
DEALINGS IN THE SOFTWARE. 

|#

(in-package #:lift)

;;; ---------------------------------------------------------------------------
;;; a simple example
;;; ---------------------------------------------------------------------------

;;; define an empty testsuite
(deftestsuite lift-examples-1 () ())
;; => #<LIFT-EXAMPLES-1: no tests defined>

;;; and add a test to it
(addtest (lift-examples-1)
  (ensure-same (+ 1 1) 2))
;; => #<Test passed>

;;; add another test using ensure-error
(addtest (lift-examples-1)
  (ensure-error (let ((x 0)) (/ x))))
;; => #<Test passed>

;;; add another, slightly more specific test
(addtest (lift-examples-1)
  (ensure-condition division-by-zero (let ((x 0)) (/ x))))
;; => #<Test passed>

;;; run all the defined tests
(run-tests)
;; => #<Results for LIFT-EXAMPLES-1 [3 Successful tests]>


;;; ---------------------------------------------------------------------------
;;; a simple example using deftestsuites :tests clause
;;; ---------------------------------------------------------------------------

(deftestsuite lift-examples-2 () 
  ()
  (:tests
   ((ensure-same (+ 1 1) 2))
   ((ensure-error (let ((x 0)) (/ x))))
   ((ensure-condition division-by-zero (let ((x 0)) (/ x))))))


;;; ---------------------------------------------------------------------------
;;; testing a simple function
;;; ---------------------------------------------------------------------------

;; !!! Incorrect definition
(defun dotted-pair-p (putative-pair)
  (and (consp putative-pair)
       (cdr putative-pair)))

;;; ---------------------------------------------------------------------------

(deftestsuite test-dotted-pair-p ()
  ()
  (:tests
   ((ensure (dotted-pair-p '(a . b))))
   ((ensure (not (dotted-pair-p '(a b)))))
   ((ensure (not (dotted-pair-p :a))))
   ((ensure (not (dotted-pair-p '(a b . c)))))
   ((ensure (not (dotted-pair-p nil))))))
;; ==> #<Results for TEST-DOTTED-PAIR-P [5 Tests, 2 Failures]>

(describe (run-tests))
;; ==> (prints)
Test Report for TEST-DOTTED-PAIR-P: 5 tests run, 2 Failures.

Failure: TEST-2
  Condition: Ensure failed: (NOT (DOTTED-PAIR-P '(A B)))
             
  Code     : ((ENSURE (NOT (DOTTED-PAIR-P '(A B)))))
  
Failure: TEST-4
  Condition: Ensure failed: (NOT (DOTTED-PAIR-P '(A B . C)))
             
  Code     : ((ENSURE (NOT (DOTTED-PAIR-P '(A B . C)))))

;;; ---------------------------------------------------------------------------

;; !!! Correct the defintion and run tests again
(defun dotted-pair-p (putative-pair)
  (and (consp putative-pair)
       (cdr putative-pair)
       (not (consp (cdr putative-pair)))))

;;; ---------------------------------------------------------------------------

(describe (run-tests))
;; ==> Prints
Test Report for TEST-DOTTED-PAIR-P: 5 tests run, all passed!


;;; ---------------------------------------------------------------------------
;;; a test suite using slots
;;; ---------------------------------------------------------------------------

(defun nearly-zero-p (number &optional (tolerance 0.0001))
  (< (abs number) tolerance))

(progn
  (deftestsuite test-nearly-zero-p ()
    ((the-number-zero 0.0)
     (not-nearly-zero 10000.0)
     (close-to-zero 0.000000001)
     (close-but-no-cigar 0.01)))
  
  (addtest (test-nearly-zero-p)
    (ensure (nearly-zero-p the-number-zero)))
  
  (addtest (test-nearly-zero-p)
    (ensure (not (nearly-zero-p not-nearly-zero))))
  
  (addtest (test-nearly-zero-p)
    (ensure (nearly-zero-p close-to-zero)))
  
  (addtest (test-nearly-zero-p)
    (ensure (not (nearly-zero-p close-but-no-cigar))))
  
  (addtest (test-nearly-zero-p)
    (ensure (nearly-zero-p close-but-no-cigar 0.1))))


(deftestsuite lift-examples () ())

(addtest (lift-examples)
  (:documentation "This is the best test of all")
  (let ((foo 1)
        (faa 2)
        (bar 3))
    (setf foo (+ foo faa bar))
    (setf foo 2)
    (ensure (= (+ foo faa bar) (* foo faa bar)))))

(addtest (lift-examples)
  (:documentation "This is the best test of all")
  (let ((foo 1)
        (faa 2)
        (bar 3))
    (setf foo (+ foo faa bar))
    (setf foo 2)
    (ensure (= (+ foo faa bar) (* foo far bar)))))

(addtest (lift-examples)
  (ensure (= 2 3)))

(addtest (lift-examples)
  (ensure (= 2 2)))

(addtest (lift-examples)
  test-warning-2
  (ensure-warning (+ 2 3)))

(addtest (lift-examples)
  test-warning
  (ensure-warning (warn "Help!")))

(addtest (lift-examples)
  (:documentation "Testing ensure-same, should pass.")
  (ensure-same (values "1" "2" "3") (values "1" "2" "3") :test #'string-equal))

(addtest (lift-examples)
  (:documentation "Testing ensure-equal, should fail")
  (ensure-same (values "1" "2" "3") (values "1" "2" "3") :test #'eql))

(addtest (lift-examples)
  (ensure-error (warn "This test fails because a warning 
is not an error.")))

(addTest (lift-examples)
  (:documentation "This test will be logged as a 
failure because no error will be generated.")
  (ensure-warning (= 2 2)))

(addTest (lift-examples)
  (:documentation "This test succeeds!")
  (ensure-error (let ((x 0)) (print (/ 4 x)))))

(addTest (lift-examples)
  (:documentation "This test should fail.  Tests a bug where a warning would abort the test with no message.")
  (warn "A test warning")
  (ensure-same 1 2))

(run-tests :suite 'lift-examples)

;;; ---------------------------------------------------------------------------
;;; 
;;; ---------------------------------------------------------------------------

(deftestsuite more-lift-examples (lift-examples)
  ((var-1 1))
  (:documentation "More Examples")
  (:test (test-initial-slot-value (ensure (= var-1 1))))
  (:test ((ensure (= (1+ var-1) 2))))
  (:test ((setf var-1 0) (ensure (= (1+ var-1) 1))))
  (:test ((setf var-1 0) (ensure-warning (/ var-1))))
  (:test ((setf var-1 0) (/ var-1) :documentation "Wow")))

(deftestsuite more-lift-examples (lift-examples)
  ((var-1 1)
   (var-2 2)))

(addtest (more-lift-examples)
  test-initial-slot-value
  (ensure (= var-1 1)))
(remove-test)
(addtest (more-lift-examples)
  (ensure (= (1+ var-1) 2)))
(addtest (more-lift-examples)
  (setf var-1 0)
  (ensure (= (1+ var-1) 1)))
(addtest (more-lift-examples)
  (setf var-1 0)
  (ensure-warning (/ var-1)))


(addtest (more-lift-examples)
  test-initial-slot-value
  (ensure-same var-1 1))

(addtest (more-lift-examples)
  test-initial-slot-value
  (ensure-same "Hello" (concatenate 'string "he" "ll" "o")))

(addtest (more-lift-examples)
  test-initial-slot-value
  (ensure-same 1.23 1.23))

(addtest (more-lift-examples)
  test-initial-slot-value
  (ensure-same (floor 5/3) (values 1 2/3) :test #'=))


(addtest (more-lift-examples)
  test-initial-slot-value
  (ensure-same var-1 2))

(addtest (more-lift-examples)
  test-initial-slot-value
  (ensure-same var-1 1 :report "Var-1 is ~A, not 1." :args (list var-1)))

(addtest (more-lift-examples)
  test-initial-slot-value
  (ensure-same var-1 1 :report (lambda ()
                                 (format nil "Var-1 is ~A, not 1." var-1))))

(addtest (more-lift-examples)
  test-initial-slot-value
  (ensure-same var-1 1 :report ("Var-1 is ~A, not 1." var-1)))


;;; ---------------------------------------------------------------------------
;;; compare with fiveam
;;; ---------------------------------------------------------------------------

(deftestsuite my-suite ()
  ()
  (:documentation "My example suite")
  (:tests
   ((ensure-same 4 (+ 2 2)))
   ((ensure-same 0 (+ -1 1)))
   ((ensure-error (+ 'foo 4)))
   ((ensure-same 0 (+ 1 1) :report "This should fail."))))

