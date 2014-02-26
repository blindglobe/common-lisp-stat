
;;; This file is used to support interactive testing for LISP-MATRIX
;;; it manages to remind (me) how to stage the development.

(ql:quickload :antik)
(ql:quickload :cls)

(in-package :lisp-stat-unittests)

(run-lisp-stat-tests)           ; long summary

(describe (run-lisp-stat-tests))           ; long summary
;; current: 55 tests, 3 errors, 16 failures


;;;; code for memory

;; (run-lisp-stat-tests)                      ; quick summary
;;(remove-test :test-case 'data-initialize :suite 'lisp-stat-ut)

;; EVERYTHING
;; (run-lisp-stat-tests)
;; (describe (run-lisp-stat-tests))

(describe (run-test :test-case 'bind2-dims-conditions))
(describe (run-test :test-case 'r-apply-columns))
(describe (run-test :test-case 'diagonalf-vectors))
(describe (run-test :test-case 'diagonal!-vectors))


;; VECTOR TESTS
;; (run-tests :suite 'lisp-matrix-ut-vectors)
;; (describe (run-tests :suite 'lisp-matrix-ut-vectors))
;; (run-test :test-case '   :suite 'lisp-matrix-ut-vectors)

;; REMINDER IF NEEDED
;; (remove-test :test-case 'data-initialize :suite 'lisp-matrix-ut)

