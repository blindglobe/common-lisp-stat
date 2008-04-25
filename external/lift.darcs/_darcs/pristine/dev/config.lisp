(in-package #:lift)

(defvar *current-configuration-stream* nil)

(defvar *current-asdf-system-name* nil)

(eval-when (:load-toplevel :execute)
  (when (find-package :asdf)
    (defmethod asdf:perform :around ((operation asdf:test-op) (c asdf:system))
      (let ((*current-asdf-system-name* (asdf:component-name c)))
	(call-next-method)))))

(defun lift-relative-pathname (pathname &optional (errorp nil))
  "Merges pathname with either the path to the currently loading system
\(if there is one\) or the *default-pathname-defaults*."
  (let* ((asdf-package (find-package :asdf))
	 (srp-symbol (and asdf-package
			  (find-symbol (symbol-name 'system-relative-pathname) 
				       asdf-package)))
	 (srp (and *current-asdf-system-name* srp-symbol)))
    (labels ((try-it (path)
	       (let ((pathname (merge-pathnames pathname path)))
		 (if errorp (and pathname (probe-file pathname)) pathname))))
      (or (and srp (try-it (funcall srp *current-asdf-system-name* "")))
	  (try-it *default-pathname-defaults*)
	  (not errorp)
	  (and (not asdf-package)
	       (error "Unable to use :generic configuration option because ASDF is not loaded."))
	  (and (not srp-symbol)
	       (error "Unable to use :generic configuration option because asdf:system-relative-pathname is not function bound (maybe try updating ASDF?)"))
	  (and (not *current-asdf-system-name*)
	       (error "Unable to use :generic configuration option because because the current system cannot be determined. Are you using asdf:test-op?"))))))

(defun find-generic-test-configuration (&optional (errorp nil))
  (flet ((try-it (path)
	   (and path (probe-file path))))
    (or (try-it (lift-relative-pathname "lift-local.config" errorp))
	(try-it (lift-relative-pathname "lift-standard.config" errorp))
	(and errorp
	     (error "Unable to use :generic configuration file neither lift-local.config nor lift-standard.config can be found.")))))

(defun report-summary-pathname ()
  (unique-filename (generate-report-summary-pathname)))

(defmethod generate-report-summary-pathname ()
  (lift-relative-pathname "test-results/summary.sav"))

#+(or)
(generate-report-summary-pathname)

(defun run-tests-from-file (path)
  (let ((real-path (cond ((eq path :generic)
			  (setf path 
				(find-generic-test-configuration t)))
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
			     (print (get-backtrace c))
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

(defmethod handle-config-preference ((name (eql :count-calls-p))
				     args)
  (setf *count-calls-p* (first args)))

(defmethod handle-config-preference ((name (eql :log-pathname))
				     args)
  (setf *lift-report-pathname* (first args)))

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
	(handler-bind 
	    ((error 
	      (lambda (c)
		(format *debug-io*
			"Error ~a while generating report (format ~s) to ~a"
			c format dest)
		(format *debug-io*
			"~%~%Backtrace~%~%~s" 
			(get-backtrace c)))))
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
		     format dest))))))))
  
