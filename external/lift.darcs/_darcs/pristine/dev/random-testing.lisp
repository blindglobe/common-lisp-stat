(in-package #:lift)

;; we redefine the class and possibly method each time, ick.

(define-condition ensure-random-cases-failure (test-condition)
  ((total :initarg :total :initform 0)
   (problems :initarg :problems :initform nil))
  (:report (lambda (condition stream)
	     (format stream "Ensure-random-cases: ~d out of ~d failed. Failing values are: ~{~%  ~s~^, ~}" 
		     (length (slot-value condition 'problems))
		     (slot-value condition 'total)
		     (slot-value condition 'problems)))))

(defgeneric random-instance-for-suite (thing suite))

(defmacro defrandom-instance (instance-type suite &body body)
  `(progn
     (defclass ,instance-type () ())
     (defvar ,(intern (format nil "+~a+" instance-type) :lift)
       (make-instance ',instance-type))
     (defmethod random-instance-for-suite 
	 ((thing ,instance-type) (suite ,(if suite suite t)))
       ,@body)))

(defmacro ensure-random-cases (count (&rest vars-and-types)
			     &body body)
  (let ((problems (gensym)))
    (flet ((intern-type (type)
	     (intern (format nil "+~a+" type) :lift)))
      `(let ((,problems nil))
	 (loop repeat ,count do
	      (let (,@(mapcar 
		       (lambda (var-and-type)
			 `(,(first var-and-type) 
			    (random-instance-for-suite
			     ,(intern-type (second var-and-type))
			     *current-test*)))
		       vars-and-types))
		(restart-case
		    (progn ,@body
			   (princ #\. *debug-io*))
		  (ensure-failed (cond)
		    (declare (ignorable cond))
		    (princ #\* *debug-io*)
		    (push (list ,@(mapcar 
				   (lambda (var-and-type)
				     `(list ',(first var-and-type) 
					    ,(first var-and-type)))
				   vars-and-types)) ,problems)))))
	 (when ,problems
	   (let ((condition (make-condition 
			     'ensure-random-cases-failure
			     :total ,count 
			     :problems ,problems)))
	     (if (find-restart 'ensure-failed)
		 (invoke-restart 'ensure-failed condition) 
		 (warn condition))))))))

(defmacro ensure-random-cases+ (count (&rest vars) (&rest case-form)
				&body body)
  (let ((total (gensym))
	(problems (gensym)))
    `(let ((,problems nil) (,total 0))
       (loop repeat ,count do
	    (incf ,total)
	    (destructuring-bind ,vars ,case-form
	      (restart-case
		  (progn ,@body)
		(ensure-failed (cond)
		  (declare (ignore cond))
		  (push (list ,@vars) ,problems)))))
       (when ,problems
	 (let ((condition (make-condition 
			   'ensure-random-cases-failure
			   :total ,total
			   :problems ,problems)))
	   (if (find-restart 'ensure-failed)
	       (invoke-restart 'ensure-failed condition) 
	       (warn condition)))))))

;;; merge with deftestsuite macro
(pushnew :random-instance *deftest-clauses*)

(add-code-block 
 :random-instance 2 :methods
 (lambda () (def :random-instances)) 
 '((push (cleanup-parsed-parameter value) (def :random-instances)))
 'build-random-instances-method)

(defun build-random-instances-method ()
  `(let (#+allegro
	 (excl:*redefinition-warnings* nil))
     ,@(mapcar (lambda (instance)
		      (let ((atype (first instance))
			    (body (second instance)))
			`(defrandom-instance ,atype test-mixin ,body)))
		    (def :random-instances))))

(defgeneric random-number (suite min max))

(defgeneric random-element (suite sequence))

(defmethod random-number (suite min max)
  (declare (ignore suite))
  (+ min (random (- max min))))

(defmethod random-element (suite sequence)
  (elt sequence (random-number suite 0 (1- (length sequence)))))

(defrandom-instance an-integer test-mixin
  (random-number suite -100 100))

(defrandom-instance a-single-float test-mixin
  (random-number suite -100s0 100.0s0))

(defrandom-instance a-double-float test-mixin
  (random-number suite -100d0 100.0d0))

(defrandom-instance a-symbol test-mixin
  (random-element suite '(a hello a-c d_f |MiXeD|
			  -2<>#$%#)))



