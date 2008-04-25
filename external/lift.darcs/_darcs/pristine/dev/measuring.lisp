(in-package #:lift)

(eval-when (:compile-toplevel)
  (declaim (optimize (speed 3) (safety 1))))

(defmacro with-measuring ((var measure-fn) &body body)
  (let ((initial (gensym)))
    `(let ((,initial (,measure-fn)))
       ,@body
       (setf ,var (- (,measure-fn) ,initial)))))

(defmacro measure-time ((var) &body body)
  `(prog1
       (with-measuring (,var get-internal-real-time)
	 ,@body)
     (setf ,var (coerce (/ ,var internal-time-units-per-second) 
			'double-float))))

(defmacro measure-conses ((var) &body body)
  `(with-measuring (,var total-bytes-allocated)
     ,@body))

(defun measure-fn (fn &rest args)
  (declare (dynamic-extent args))
  (let ((bytes 0) (seconds 0) result)
    (measure-time (seconds)
      (measure-conses (bytes)
	(setf result (apply fn args))))
    (values seconds bytes result)))

(defmacro measure (seconds bytes &body body)
  (let ((result (gensym)))
    `(let (,result)
       (measure-time (,seconds)
	 (measure-conses (,bytes)
	   (setf ,result (progn ,@body))))
       (values ,result))))

(defmacro measure-time-and-conses (&body body)
  (let ((seconds (gensym))
	(conses (gensym))
	(results (gensym)))
    `(let ((,seconds 0) (,conses 0) ,results) 
       (setf ,results (multiple-value-list 
			    (measure ,seconds ,conses ,@body)))
       (values-list (nconc (list ,seconds ,conses)
			   ,results)))))

#+(or)
;; tries to handle multiple values (but fails since measure doesn't)
(defmacro measure-time-and-conses (&body body)
  (let ((seconds (gensym))
	(conses (gensym)))
    `(let ((,seconds 0) (,conses 0)) 
       (values-list (nconc (multiple-value-list 
			    (measure ,seconds ,conses ,@body))
			   (list ,seconds ,conses))))))

(defvar *profile-extra* nil)

(defparameter *benchmark-log-path*
  (asdf:system-relative-pathname 
   'lift "benchmark-data/benchmarks.log"))

(defvar *count-calls-p* nil)

(defvar *additional-markers* nil)

(defvar *profiling-threshold* nil)

(defmacro with-profile-report 
    ((name style &key (log-name *benchmark-log-path* ln-supplied?)
	   (count-calls-p *count-calls-p* ccp-supplied?)
	   (timeout nil timeout-supplied?))
     &body body)
  `(with-profile-report-fn 
       ,name ,style 
       (lambda ()
	 (progn ,@body))
       ,@(when ccp-supplied? 
	       `(:count-calls-p ,count-calls-p))
       ,@(when ln-supplied?
	       `(:log-name ,log-name))
       ,@(when timeout-supplied?
	       `(:timeout ,timeout))))

#+allegro
(defun cancel-current-profile (&key force?)
  (when (prof::current-profile-actual prof::*current-profile*)
    (unless force?
      (assert (member (prof:profiler-status) '(:inactive))))
    (prof:stop-profiler)
    (setf prof::*current-profile* (prof::make-current-profile))))

#+allegro
(defun current-profile-sample-count ()
   (ecase (prof::profiler-status :verbose nil)
    ((:inactive :analyzed) 0)
    ((:suspended :saved)
     (slot-value (prof::current-profile-actual prof::*current-profile*) 
		 'prof::samples))
    (:sampling (warn "Can't determine count while sampling"))))

#|
(prof:with-profiling ...
different reports
|#

#+allegro
(defun with-profile-report-fn 
    (name style fn &key (log-name *benchmark-log-path*)
			       (count-calls-p *count-calls-p*)
			       (timeout nil))
  (assert (member style '(:time :space :count-only)))
  (cancel-current-profile :force? t)
  (let* ((seconds 0.0) (conses 0)
	 results)
    (unwind-protect
	 (handler-case
	     (with-timeout (timeout)
	       (setf results
		     (multiple-value-list
		      (prof:with-profiling (:type style :count count-calls-p)
			(measure seconds conses (funcall fn))))))
	   (timeout-error 
	       (c)
	     (declare (ignore c))))
      ;; cleanup / ensure we get report
      (ensure-directories-exist log-name)
      ;;log 
      (with-open-file (output log-name
			      :direction :output
			      :if-does-not-exist :create
			      :if-exists :append)
	(with-standard-io-syntax
	  (let ((*print-readably* nil))
	    (terpri output)
	    (format output "\(~11,d ~20,s ~10,s ~10,s ~{~s~^ ~} ~s ~s\)"
		    (date-stamp :include-time? t) name 
		    seconds conses *additional-markers*
		    results (current-profile-sample-count)))))
      (when (> (current-profile-sample-count) 0)
	(let ((pathname (unique-filename
			 (merge-pathnames
			  (make-pathname 
			   :type "prof"
			   :name (format nil "~a-~a-" name style))
			  log-name))))
	  (let ((prof:*significance-threshold* 
		 (or *profiling-threshold* 0.01)))
	    (format t "~&Profiling output being sent to ~a" pathname)
	    (with-open-file (output pathname
				    :direction :output
				    :if-does-not-exist :create
				    :if-exists :append)
	      (format output "~&Profile data for ~a" name)
	      (format output "~&Date: ~a" 
		      (excl:locale-print-time (get-universal-time)
					      :fmt "%B %d, %Y %T" :stream nil))
	      (format output "~&  Total time: ~,2F; Total space: ~:d \(~:*~d\)"
		      seconds conses)
	      (format output "~%~%")
	      (when (or (eq :time style)
			(eq :space style))
		(prof:show-flat-profile :stream output)
		(prof:show-call-graph :stream output)
		(when count-calls-p
		  (format output "~%~%Call counts~%")
		  (let ((*standard-output* output))
		    (prof:show-call-counts))))
	      (when *profile-extra*
		(loop for thing in *profile-extra* do
		     (format output "~%~%")
		     (let ((*standard-output* output))
		       (prof:disassemble-profile thing)))))))))
    (values-list results)))

(defmacro while-counting-repetitions ((period) &body body)
  "Returns the count of the number of times `body` was executed during 
`period` seconds."
  (let ((gevent-count (gensym "count")))
    `(let ((,gevent-count 0))
       (declare (type fixnum ,gevent-count))
       (handler-case
	   (with-timeout (,period) 
	     (loop  
		(progn ,@body)
		(incf ,gevent-count)))
	 (timeout-error (c)
	   (declare (ignore c))
	   ,gevent-count)))))
  
(defun count-repetitions (fn period &rest args)
  (declare (dynamic-extent args))
  (let ((event-count 0))
    (handler-case
	(with-timeout (period) 
	  (loop  
	     (apply #'funcall fn args)
	     (incf event-count)))
      (timeout-error (c)
	(declare (ignore c))
	event-count))))
