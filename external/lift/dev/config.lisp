(in-package #:lift)

(defun run-tests-from-file (path)
  (setf *test-result*
	(let ((*package* *package*)
	      (*read-eval* nil)
	      (form nil)
	      (result (make-test-result path :multiple)))
	  (with-open-file (in path
			      :direction :input
			      :if-does-not-exist :error)
	    (let ((*lift-dribble-pathname* nil)
		  (*lift-debug-output* *debug-io*)
		  (*lift-standard-output* *standard-output*)
		  (*test-break-on-errors?* nil)
		  (*test-do-children?* t)
		  (*lift-equality-test* 'equal)
		  (*test-print-length* :follow-print)
		  (*test-print-level* :follow-print)
		  (*lift-if-dribble-exists* :append)
		  (*test-result* result))
	      (loop while (not (eq (setf form (read in nil :eof nil)) :eof)) 
		 collect
		   (handler-bind 
		       ((error (lambda (c) (format 
			       *error-output* 
			       "Error while running ~a from ~a: ~a"
			       form path c)
			  (invoke-debugger c))))
		     (destructuring-bind
			   (name &rest args &key &allow-other-keys)
			 form
		       (assert (typep name 'symbol))
		       (setf args (massage-arguments args))
		       (cond 
			 ;; check for preferences first (i.e., keywords)
			 ((eq (symbol-package name) 
			      (symbol-package :keyword))
			  ;; must be a preference
			  (handle-config-preference name args))
			 ((subtypep (find-testsuite name)
				    'lift:test-mixin)
			  (apply #'run-tests :suite name 
				 :result result args))
			 (t
			  (error "Don't understand '~s' while reading from ~s" 
				 form path)))))
		   #+(or)
		   (handler-case 
		     (destructuring-bind
			   (name &rest args &key &allow-other-keys)
			 form
		       (assert (typep name 'symbol))
		       (setf args (massage-arguments args))
		       (cond 
			 ;; check for preferences first (i.e., keywords)
			 ((eq (symbol-package name) 
			      (symbol-package :keyword))
			  ;; must be a preference
			  (handle-config-preference name args))
			 ((subtypep (find-testsuite name)
				    'lift:test-mixin)
			  (apply #'run-tests :suite name 
				 :result result args))
			 (t
			  (error "Don't understand '~s' while reading from ~s" 
				 form path))))
		   (error (c) (format 
			       *error-output* 
			       "Error while running ~a from ~a: ~a"
			       form path c)
			  (invoke-debugger c))))))
	  (values result))))

(defun massage-arguments (args)
  (loop for arg in args collect
       (cond ((eq arg '*standard-output*) *standard-output*)
	     (t arg))))

(defmethod handle-config-preference ((name t) args)
  (error "Unknown preference ~s (with arguments ~s)" 
	 name args))

(defmethod handle-config-preference ((name (eql :dribble)) args)
  (setf *lift-dribble-pathname* (first args)))

(defmethod handle-config-preference ((name (eql :debug-output)) args)
  (setf *lift-debug-output* (first args)))

(defmethod handle-config-preference ((name (eql :standard-output)) args)
  (setf *lift-standard-output* (first args)))

(defmethod handle-config-preference ((name (eql :break-on-errors?)) args)
  (setf *test-break-on-errors?* (first args)))

(defmethod handle-config-preference ((name (eql :do-children?)) args)
  (setf *test-do-children?* (first args)))

(defmethod handle-config-preference ((name (eql :equality-test)) args)
  (setf *lift-equality-test* (first args)))

(defmethod handle-config-preference ((name (eql :print-length)) args)
  (setf *test-print-length* (first args)))

(defmethod handle-config-preference ((name (eql :print-level)) args)
  (setf *test-print-level* (first args)))

(defmethod handle-config-preference ((name (eql :print-suite-names)) args)
  (setf *test-print-suite-names* (first args)))

(defmethod handle-config-preference ((name (eql :print-test-case-names)) args)
  (setf *test-print-test-case-names* (first args)))

(defmethod handle-config-preference ((name (eql :if-dribble-exists))
				     args)
  (setf *lift-if-dribble-exists* (first args)))

(defmethod handle-config-preference ((name (eql :report-property))
				     args)
  (setf (test-result-property *test-result* (first args)) (second args)))

(defmethod handle-config-preference ((name (eql :profiling-threshold))
				     args)
  (setf *profiling-threshold* (first args)))


(defmethod handle-config-preference ((name (eql :build-report))
				     args)
  (declare (ignore args))
  (let* ((dest (or (test-result-property *test-result* :full-pathname)
		   (asdf:system-relative-pathname 
		    (or (test-result-property *test-result* :relative-to)
			'lift)
		    (or (test-result-property *test-result* :name)
			"report.html"))))
	 (format (or (test-result-property *test-result* :format)
		     :html))
	 (unique-name (test-result-property *test-result* :unique-name)))
    (when (and unique-name (not (streamp dest)))
      (setf dest (unique-filename dest)))
    (with-standard-io-syntax 
      (let ((*print-readably* nil))
	(handler-case 
	    (cond
	      ((or (streamp dest) (writable-directory-p dest))
	       (format *debug-io* "~&Sending report (format ~s) to ~a" 
		       format dest)
	       (test-result-report
		*test-result*
		dest
		format))
	      (t
	       (format *debug-io* "~&Unable to write report (format ~s) to ~a" 
		       format dest)))
	  (error (c)
	    (format *debug-io*
		    "Error ~a while generating report (format ~s) to ~a"
		    c format dest)))))))
  
