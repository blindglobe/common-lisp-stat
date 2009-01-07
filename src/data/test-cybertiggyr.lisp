;;;
;;; $Header: /home/gene/library/website/docsrc/lut/RCS/test.lisp,v 395.1 2008/04/20 17:25:47 gene Exp $
;;;
;;; Copyright (c) 2005 Gene Michael Stover.  All rights reserved.
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation; either version 2 of the
;;; License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301
;;; USA
;;;

(defpackage "CYBERTIGGYR-TEST"
  (:use "COMMON-LISP")
  (:export "*EXCLUDED-PACKAGES*"
	   "*PREFIX*"
	   "CHECK"
	   "DEFTEST"
	   "DISPOSITION"
	   "IS-A-UNIT-TEST"
	   "NOT-A-UNIT-TEST"
	   "RATE"
	   "RATETABLE"
	   "RUN"
	   "TEST-FUNCTION-P"
	   "TEST-FUNCTIONS"))

(in-package "CYBERTIGGYR-TEST")

;;;
;;; unexported helper functions & stoof
;;;

(defun symbol-name-starts-with (symbol starts-with)
  "Return true if & only if the name of the symbol begins with
the string bound to STARTS-WITH."
  (let ((len (length starts-with)))
    (and (>= (length (symbol-name symbol)) len)
	 (equal (subseq (symbol-name symbol) 0 len) starts-with))))

(defun symbol-bigname (symbol)
  "Return, as a string, the package name of the symbol & the name
of the symbol."
  (format nil "~A::~A" (package-name (symbol-package symbol)) symbol))

(defun make-failed-test-p (max strm)
  "Return a predicate which runs a test & tells whether it failed.
The predicate also prints a status to the character output stream
STRM."
  (let ((i 0))
    #'(lambda (test)
	;; Show which test we're about to run & what percentage
	;; of the test suit has been run.
	(format strm "~&~3D% ~A =>" (round (* (/ (incf i) max) 100))
		(symbol-bigname test))
	(finish-output strm)
	(let ((is-good (funcall test))) ; run the test
	  ;; Show the test's result.
	  (format strm " ~A" (if is-good "good" "FAILED"))
	  (not is-good)))))             ; compliment the result

;;;
;;; You could alter these values to fine-tune the behaviour of
;;; TEST-FUNCTION-P.  Adding packages to *EXCLUDED-PACKAGES* is
;;; safe, but altering *PREFIX* could be trouble.
;;;

(defvar *prefix* "TEST" "String prefix of test function names.")

(defvar *excluded-packages*
  (remove (find-package "COMMON-LISP-USER") (list-all-packages))
  "Packages whose functions are not eligible to be test functions.
Defaults to the packages that were loaded before this package, less
COMMON-LISP-USER.")

(defun test-function-p (symbol)
  "Return true if & only if SYMBOL is bound to a test function."
  (and (fboundp symbol)
       (not (eq (get symbol 'disposition) 'not-a-unit-test))
       (not (member (symbol-package symbol) *excluded-packages*))
       (or (eq (get symbol 'disposition) 'is-a-unit-test)
	   (symbol-name-starts-with symbol *prefix*))))
(setf (get 'test-function-p 'disposition) 'not-a-unit-test)

(defun test-functions ()
  "Return a list of symbols bound to test functions in any package."
  (let ((lst ()))
    (do-all-symbols (symbol)
      (when (test-function-p symbol) (push symbol lst)))
    (remove-duplicates (sort lst #'string-lessp :key #'symbol-bigname))))

(setf (get 'test-functions 'disposition) 'not-a-unit-test)

(defun run (&optional (strm *standard-output*))
  "Run all unit tests.  Print results to STRM.  Return true if & only
if all tests pass."
  (null
   (find-if
    ;; Search for a test function which fails...
    (make-failed-test-p (length (test-functions)) strm)
    ;; ...from the suite of test functions.
    (test-functions))))

(defmacro deftest (name &rest body)
  "Declare a unit test function.  For now, maps to DEFUN, but could
be implemented differently in the future."
  (if (symbol-name-starts-with name *prefix*)
      `(defun ,name ,@body)
    ;; else, We'll need to set DISPOSITION
    `(progn (setf (get ',name 'cybertiggyr-test:disposition)
		  'cybertiggyr-test:is-a-unit-test)
	    (defun ,name ,@body))))

(defun rate (fn)
  "Run function FN at least 3 times & at least 3 seconds.
Return triple whose FIRST is calls/second, SECOND is number
of calls, & THIRD is number of seconds.  All three numbers
will be positive.  They may be integers, ratios, or floating-
point, depending on details of the lisp system.  Time are
measured with GET-INTERNAL-REAL-TIME, but they are reported in
seconds."
  (declare (type function fn))
  (do ((start-time (get-internal-real-time))
       (seconds 0 (/ (- (get-internal-real-time) start-time)
		      internal-time-units-per-second))
       (count 0 (1+ count)))
      ((and (>= count 3) (>= seconds 3))
       (list (/ count seconds) count seconds))
      (funcall fn)))

(defun ratetable (names-and-fns strm)
  "Run RATE on a bunch of functios & return a LaTeX table in a 
string which shows the results of all of them.  Each element
in NAMES-AND-FNS is a list whose FIRST is the name of the function
in a string & whose SECOND is a function of no arguments whose
performance is to be tested."
  (format strm "\\begin{tabular}{|r|r|r|r|} \\hline")
  (format strm "~%{\\bf function} & {\\bf count} &")
  (format strm " {\\bf seconds} & {\\bf rate}")
  (format strm " \\\\ \\hline")
  (dolist (lst names-and-fns)
    (destructuring-bind (rate count seconds) (rate (second lst))
      (format strm "~%~A & ~D & ~,2E & ~,2E \\\\ \\hline"
	      (first lst) count seconds rate)))
  (format strm "~%\\end{tabular}")
  strm)

(defmacro check (expression)
  `(if ,expression
       t
     ;; else
     (progn
       (format t "~&Failure: ~S" ',expression)
       nil)))

;;; --- end of file ---
