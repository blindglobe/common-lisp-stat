(in-package #:lift)

(deftestsuite test-case-generation () ())
(deftestsuite test-case-generation-simple (test-case-generation) ())

;;; ---------------------------------------------------------------------------
;;; test-process-cases-form
;;; ---------------------------------------------------------------------------

(deftestsuite test-process-cases-form () ())
(deftestsuite test-vars-from-assignment (test-process-cases-form) ())

(addtest (test-vars-from-assignment)
  test-1
  (ensure-same (vars-from-assignment '((:B ((A . 1) (B . 3)) ((A . 2) (B . 4)))))
               '(a b)))

(addtest (test-vars-from-assignment)
  test-2
  (ensure-same (vars-from-assignment (list '(c '(a b)) '(d (list 'x 'y))))
               '(c d)))

(addtest (test-vars-from-assignment)
  test-3
  (ensure-same (vars-from-assignment '((:B ((A . 1) (B . 3)) ((A . 2) (B . 4)))
                                       (:B ((C . A) (D . X)) ((C . B) (D . Y)))))
               '((a b) (c d))))

;;; ---------------------------------------------------------------------------

(deftestsuite test-values-from-assignment (test-process-cases-form) ())

(addtest (test-values-from-assignment)
  test-1
  (ensure-same (values-from-assignment '((:B ((A . 1) (B . 3)) ((A . 2) (B . 4)))))
               '((1 2) (3 4))))

(addtest (test-values-from-assignment)
  test-2
  (ensure-same (values-from-assignment (list '(c '(a b)) '(d (list 'x 'y))))
               '((a b) (x y))))

(addtest (test-values-from-assignment)
  test-3
  (ensure-same (values-from-assignment '((:B ((A . 1) (B . 3)) ((A . 2) (B . 4)))
                                         (:B ((C . A) (D . X)) ((C . B) (D . Y)))))
               '((1 2) (3 4) (a b) (x y))))

;;; ---------------------------------------------------------------------------

(deftestsuite test-standardize-cases-form (test-process-cases-form)
  ())

(addtest (test-standardize-cases-form)
  test-1
  (ensure-same 
   (standardize-cases-form '((q '(a b))))
   '(:cross (q '(a b)))))

;;; ---------------------------------------------------------------------------

(addtest (test-standardize-cases-form)
  test-2
  (ensure-same 
   (standardize-cases-form '((:map (a '(1 2 3 4 5)) (b '(9 8 7 6 5)))))
   '(:map (a '(1 2 3 4 5)) (b '(9 8 7 6 5)))))

;;; ---------------------------------------------------------------------------

(addtest (test-standardize-cases-form)
  test-3
  (ensure-same 
   (standardize-cases-form '((q '(a b)) (b '(1 2))))
   '(:cross (q '(a b)) (b '(1 2)))))

;;; ---------------------------------------------------------------------------

(addtest (test-process-cases-form)
  test-map-0
  (ensure-same 
   (process-cases-form :map '(c '(a b)))
   '(:b ((c . a)) ((c . b)))
   :test 'equal))

;;; ---------------------------------------------------------------------------

(addtest (test-process-cases-form)
  test-map1
  (ensure-same 
   (process-cases-form :map '(c '(a b)) '(d (list 'x 'y)))
   '(:b ((c . a) (d . x)) ((c . b) (d . y)))
   :test 'equal))

(addtest (test-process-cases-form)
  test-map2-a
  (ensure-same 
   (process-cases-form :map 
                       (process-cases-form :map '(a '(1 2)) '(b '(3 4)))
                       (process-cases-form :map '(c '(a b)) '(d '(x y))))
   '(:b ((a . 1) (b . 3) (c . a) (d . x)) 
     ((a . 2) (b . 4) (c . b) (d . y)))
   :test 'equal))

(addtest (test-process-cases-form)
  test-map2-a
  (ensure-same 
   (process-cases-form :map 
                       '(:b ((a . 1) (b . 3)) ((a . 2) (b . 4)))
                       '(:b ((c . a) (d . x)) ((c . b) (d . y))))
   '(:b ((a . 1) (b . 3) (c . a) (d . x)) 
     ((a . 2) (b . 4) (c . b) (d . y)))
   :test 'equal))

(addtest (test-process-cases-form)
  test-cross-0
  (ensure-same 
   (process-cases-form :cross '(c '(a b)))
   '(:b ((c . a)) ((c . b)))
   :test 'equal))

(addtest (test-process-cases-form)
  test-cross-1
  (ensure-same 
   (process-cases-form :cross '(c '(a b)) '(d (list 'x 'y)))
   '(:b
     ((c . a) (d . x))
     ((c . a) (d . y))
     ((c . b) (d . x))
     ((c . b) (d . y)))
   :test 'equal))

(addtest (test-process-cases-form)
  test-cross-b
  (ensure-same 
   (process-cases-form :cross 
                       '(:b ((a . 1) (b . 3)) ((a . 2) (b . 4)))
                       '(:b ((c . a) (d . x)) ((c . b) (d . y))))
   '(:B
     ((A . 1) (B . 3) (C . A) (D . X)) ((A . 1) (B . 3) (C . A) (D . Y))
     ((A . 1) (B . 3) (C . B) (D . X)) ((A . 1) (B . 3) (C . B) (D . Y))
     ((A . 1) (B . 4) (C . A) (D . X)) ((A . 1) (B . 4) (C . A) (D . Y))
     ((A . 1) (B . 4) (C . B) (D . X)) ((A . 1) (B . 4) (C . B) (D . Y))
     ((A . 2) (B . 3) (C . A) (D . X)) ((A . 2) (B . 3) (C . A) (D . Y))
     ((A . 2) (B . 3) (C . B) (D . X)) ((A . 2) (B . 3) (C . B) (D . Y))
     ((A . 2) (B . 4) (C . A) (D . X)) ((A . 2) (B . 4) (C . A) (D . Y))
     ((A . 2) (B . 4) (C . B) (D . X)) ((A . 2) (B . 4) (C . B) (D . Y)))
   :test 'equal))

(addtest (test-process-cases-form)
  test-cross-a
  (ensure-same 
   (process-cases-form :cross 
                       (process-cases-form :map '(a '(1 2)) '(b '(3 4)))
                       (process-cases-form :map '(c '(a b)) '(d '(x y))))
   '(:B
     ((A . 1) (B . 3) (C . A) (D . X)) ((A . 1) (B . 3) (C . A) (D . Y))
     ((A . 1) (B . 3) (C . B) (D . X)) ((A . 1) (B . 3) (C . B) (D . Y))
     ((A . 1) (B . 4) (C . A) (D . X)) ((A . 1) (B . 4) (C . A) (D . Y))
     ((A . 1) (B . 4) (C . B) (D . X)) ((A . 1) (B . 4) (C . B) (D . Y))
     ((A . 2) (B . 3) (C . A) (D . X)) ((A . 2) (B . 3) (C . A) (D . Y))
     ((A . 2) (B . 3) (C . B) (D . X)) ((A . 2) (B . 3) (C . B) (D . Y))
     ((A . 2) (B . 4) (C . A) (D . X)) ((A . 2) (B . 4) (C . A) (D . Y))
     ((A . 2) (B . 4) (C . B) (D . X)) ((A . 2) (B . 4) (C . B) (D . Y)))
   :test 'equal))


;;; ---------------------------------------------------------------------------
;;; some simple "real" tests
;;; ---------------------------------------------------------------------------

(deftestsuite test-addition ()
  (a b)
  (:cases (:map (a '(1 2 3 4 5))
                (b '(9 8 7 6 5))))
  (:test ((ensure-same (+ a b) 10 :test '=))))

(deftestsuite test-addition ()
  (a b)
  (:cases (a '(1 2 3 4 5))
          (b '(9 8 7 6 5)))
  (:test ((ensure-same (+ a b) (+ b a) :test '=))))

;;; ---------------------------------------------------------------------------
;;; 
;;; ---------------------------------------------------------------------------

(deftestsuite test-case-generation-simple-helper ()
  (a)
  (:cases (a '(1 2 3)))
  (:test (test-1 (push a *test-scratchpad*))))

(addtest (test-case-generation-simple)
  single-var-three-cases
  (let ((tr (run-test (test-case-generation-simple-helper) test-1)))
    (ensure-same (length (tests-run tr)) 3)
    (ensure-same *test-scratchpad* '(3 2 1))))

;;; ---------------------------------------------------------------------------

(deftestsuite test-case-generation-simple-helper-2 (test-case-generation-simple-helper)
  (b)
  (:cases (b '(4 5)))
  (:test (test-1 (push a *test-scratchpad*)
                 (push b *test-scratchpad*))))

(addtest (test-case-generation-simple)
  single-var-with-superclass
  (let ((tr (run-test (test-case-generation-simple-helper-2) test-1)))
    (ensure-same (length (tests-run tr)) 6)
    (ensure-same *test-scratchpad* '(1 4 1 5 2 4 2 5 3 4 3 5))))

;;; ---------------------------------------------------------------------------

(deftestsuite test-case-generation-simple-helper-3 ()
  (a b)
  (:cases (a '(1 2 3))
          (b '(4 5)))
  (:test (test-1 (push (list a b) *test-scratchpad*))))

(addtest (test-case-generation-simple)
  two-vars-cross-product
  (let ((tr (run-test (test-case-generation-simple-helper-3) test-1)))
    (ensure-same (length (tests-run tr)) 6)
    (ensure-same *test-scratchpad* '((1 4) (1 5) (2 4) (2 5) (3 4) (3 5)))))

;;; ---------------------------------------------------------------------------

(deftestsuite test-case-generation-simple-helper-4 ()
  (a b)
  (:cases (:map (a (1 2 3))
                (b (4 5 6))))
  (:test (test-1 (push (list a b) *test-scratchpad*))))

(addtest (test-case-generation-simple)
  two-vars-mapping
  (let ((tr (run-test (test-case-generation-simple-helper) test-1)))
    (ensure-same (length (tests-run tr)) 3)
    (ensure-same *test-scratchpad* '(3 2 1))))

;;; ---------------------------------------------------------------------------

(defun random-integers (count)
  (loop repeat count collect 
        (variates:integer-random variates:*random-generator* -9 10)))

(deftestsuite test-case-generation-simple-helper-5 ()
  (a b)
  (:cases (a (random-integers 3))
          (b (random-integers 2)))
  (:test (test-1 (push (list a b) *test-scratchpad*))))

(addtest (test-case-generation-simple)
  two-vars-cross-product-run-time
  (let ((tr (run-test (test-case-generation-simple-helper-5) test-1)))
    (ensure-same (length (tests-run tr)) 6)))

;;; ---------------------------------------------------------------------------
;;;
;;; ---------------------------------------------------------------------------

(deftestsuite test-case-generation-helper-1 ()
  (a)
  (:cases (a :exemplars-of fixnum))
  (:test (test-1 (push (list a) *test-scratchpad*))))

;;; ---------------------------------------------------------------------------

(deftestsuite test-case-generation-helper-2 ()
  (a)
  (:cases (a :samples-from standard-normal))
  (:test (test-1 (push (list a) *test-scratchpad*))))

;;; ---------------------------------------------------------------------------

(deftestsuite test-case-generation-helper-3 ()
  (a)
  (:cases (a :samples-from fixnum))
  (:test (test-1 (push (list a) *test-scratchpad*))))