
(in-package #:ch-util-test)

(defun test-strcat ()
  (let ((s1 (strcat "this is " "a test")))
    (equal s1 "this is a test")))

(defun test-interncase ()
  (let ((s1 (interncase "testINGinternCase")))
    (format t "~&~A" s1)
    t))

(defun test-print-buffer ()
  (print-buffer (read-file-to-buffer "/etc/motd"))
  t)

(defun test-postincf ()
  (let ((moose 3))
    (print moose)
    (print (postincf moose))
    (print moose)
    (print (postincf moose 2))
    (print moose)
    ))

(defun run-tests ()
  (let ((run (make-test-run)))
    (run-test #'test-strcat "test-strcat" run)
    (run-test #'test-print-buffer "test-print-buffer" run)
    (run-test #'test-interncase "test-interncase" run)
    (run-test #'test-postincf "test-postincf" run)
    (format t "~&~A of ~A tests passed" (test-run-passed run) (test-run-tests run))
    ))
  
;;;  Lisp one liners:

; (let ((gstring (with-output-to-string (gstream) (run-program "ls" nil :output gstream)))) (print gstring))
