;;;-*- Mode: Lisp; Package: LIFT-INTERFACE -*-

(defpackage "LIFT-INTERFACE"
  (:use 
   "LIFT"
   "COMMON-LISP"
   #+MCL       "CCL")
  (:import-from "LIFT"
                #:number-of-failures
                #:number-of-errors
                #:errors
                #:failures
                #:tests-run
                #:test-class-name
                #:run-tests-internal))

(in-package #:lift-interface)

(defvar *lift-report-window* nil)

(defclass lift-report-window (fred-window)
  ()
  (:default-initargs 
    :scratch-p t))

(defun show-last-test-results (result)  
  (unless (and *lift-report-window*
               (typep *lift-report-window* 'fred-window)
               (window-shown-p *lift-report-window*))
    (setf *lift-report-window* (make-instance 'lift-report-window)))
  (let* ((*print-length* nil)
         (*print-level* nil)
         (*print-array* t) 
         (win *lift-report-window*))
    (set-window-title 
     win 
     (format nil "~&Test Report for ~A: ~D test~:P run~:[~[~:;, ~:*~A Failure~:P~]~[~:;, ~:*~A Error~:P~]~;, All Passed~]" 
             (test-class-name result) (length (tests-run result))
             (not (or (failures result) (errors result)))
             (length (failures result))
             (length (errors result))))
     
    (select-all win)
    (clear win)
    (describe-object result win)
    (force-output win)
    (fred-update win)))

#+Ignore
(defmethod run-tests-internal :around ((case test-mixin) &key)
  (show-last-test-results (call-next-method)))

(u:define-around-advice run-tests show-results
  (show-last-test-results (u:call-next-advice)))
  
(run-tests :suite 'lift::test-lift)