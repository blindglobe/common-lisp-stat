;;;; kclpatch -- Additions to AKCL from CTtL2
;;;; 
;;;; Copyright (c) 1991, by Luke Tierney. Permission is granted for
;;;; unrestricted use.

(provide "kclpatch")

(in-package 'lisp-stat-basics)

;;;;
;;;; This file adds some functions from CLtL2 to AKCL. These functions
;;;; were not available in AKCL-1-600 but may become available at a
;;;; later date. I have added them to the LISP-STAT-BASICS package;
;;;; perhaps they ought to be put in the LISP package.
;;;;

(export '(function-lambda-expression realp fixnump))

(defun function-lambda-expression (f)
  (cond
   ((and (functionp f) (consp f))
    (case (first f)
     (lambda-block
      (values (cons 'lambda (nthcdr 2 f)) nil (second f)))
     (lambda-closure
      (values (cons 'lambda (nthcdr 4 f))
	      (not (and (null (second f)) 
			(null (third f))
			(null (fourth f))))
	      nil))
     (lambda-block-closure
      (values (cons 'lambda (nthcdr 5 f))
	      (not (and (null (second f)) 
			(null (third f))
			(null (fourth f))))
	      (fifth f)))
     (t (error "unknown function type"))))
    ((functionp f) (values nil t nil))
    (t (error "not a function - ~s" f))))

(defun realp (x) 
"Args: (x)
Returns T if X is a real number; NIL otherwise."
  (declare (inline rationalp floatp))
  (or (rationalp x) (floatp x)))

;;; Many CL's provide a fixnump somewhere. (A)KCL does not, so the one
;;; from ls-basics is exported here.
