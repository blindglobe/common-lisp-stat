(in-package #:lift)

(defvar *current-configuration-stream* nil)

(defvar *current-asdf-system-name* nil)

(eval-when (:load-toplevel :execute)
  (when (find-package :asdf)
    (defmethod asdf:perform :around ((operation asdf:test-op) (c asdf:system))
      (let ((*current-asdf-system-name* (asdf:component-name c)))
	(call-next-method)))))

(defun find-generic-test-configuration ()
  (let ((srp (and *current-asdf-system-name*
		  (find-package :asdf)
		  (intern (symbol-name 'system-relative-pathname) :asdf))))
    (cond (srp
	   (or (probe-file (funcall srp  
				    *current-asdf-system-name*
				    "lift-local.config"))
	       (probe-file (funcall srp  
				    *current-asdf-system-name*
				    "lift-standard.config"))
	       (error "Unable to find lift-local.config or lift-standard.config relative to the current system (~s)" *current-asdf-system-name*)))
	  (t
	   (error "Unable to use :generic configuration option either because ASDF is not loaded or because asdf:system-relative-pathname is not bound (maybe try updating?) or because the current system cannot be determined.")))))

(defun run-tests-from-file (path)
  (let ((real-path (cond ((eq path :generic)
			  (setf path (find-generic-test-configuration)))
			 (t
			  (probe-file path)))))
    (unless real-path
      (error "Unable to find configuration file ~s" path)) 
    (setf *test-result*
	  (let* ((*package* *package*)
		 (*read-eval* nil)
		 (result (make-test-result path :multiple))
		 (*lift-dribble-pathname* nil)
		 (*lift-debug-output* *debug-io*)
		 (*lift-standard-output* *standard-output*)
		 (*test-break-on-errors?* nil)
		 (*test-do-children?* t)
		 (*lift-equality-test* 'equal)
		 (*test-print-length* :follow-print)
		 (*test-print-level* :follow-print)
		 (*lift-if-dribble-exists* :append)
		 (*test-result* result))
	    (%run-tests-from-file path)))))

(defun %run-tests-from-file (path)
  (with-open-file (*current-configuration-stream* path
		      :direction :input
		      :if-does-not-exist :error)
    (let ((form nil))
      (loop while (not (eq (setf form (read *current-configuration-stream* 
					    nil :eof nil)) :eof)) 
	 collect
	 (handler-bind 
	     ((error (lambda (c) (format 
				  *error-output* 
				  "Error while running ~a from ~a: ~a"
				  form path c)
			     (invoke-debugger c))))
	   (destructuring-bind
		 (name &rest args)
	       form
	     (assert (typep name 'symbol) nil
		     "Each command must be a symbol and ~s is not." name)
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
		       :result *test-result* args))
	       (t
		(error "Don't understand '~s' while reading from ~s" 
		       form path))))))))
  (values *test-result*))

(defun massage-arguments (args)
  (loop for arg in args collect
       (cond ((and (symbolp arg)
		   (string= (symbol-name arg) (symbol-name '*standard-output*)))
	      *standard-output*)
	     (t arg))))

(defmethod handle-config-preference ((name t) args)
  (error "Unknown preference ~s (with arguments ~s)" 
	 name args))

(defmethod handle-config-preference ((name (eql :include)) args)
  (%run-tests-from-file (merge-pathnames (first args) 
					 *current-configuration-stream*)))

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
  (setf *test-print-testsuite-names* (first args)))

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
  
