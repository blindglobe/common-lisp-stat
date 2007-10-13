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

(defparameter *benchmark-file*
  (asdf:system-relative-pathname 
   'lift "benchmark-data/benchmarks.log"))

(defvar *collect-call-counts* nil)

(defvar *additional-markers* nil)

(defvar *profiling-threshold* nil)

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

;; FIXME -- functionify this!
#+allegro
(defmacro with-profile-report ((name style &key (log-name *benchmark-file*)
				     (call-counts-p *collect-call-counts*)) 
			       &body body)
  (assert (member style '(:time :space)))
  `(let ((seconds 0.0) (conses 0) result)
     (cancel-current-profile :force? t)
     (multiple-value-prog1
	 (prof:with-profiling (:type ,style :count ,call-counts-p)
	   (measure seconds conses ,@body))
       (ensure-directories-exist ,log-name)
       ;;log 
       (with-open-file (output ,log-name
			       :direction :output
			       :if-does-not-exist :create
			       :if-exists :append)
	 (with-standard-io-syntax
	   (let ((*print-readably* nil))
	     (terpri output)
	     (format output "\(~11,d ~20,s ~10,s ~10,s ~{~s~^ ~} ~s\)"
		     (date-stamp :include-time? t) ,name 
		     seconds conses *additional-markers*
		     result))))
       (when (> (current-profile-sample-count) 0)
	 (let ((pathname (unique-filename
			  (merge-pathnames
			   (make-pathname 
			    :type "prof"
			    :name (format nil "~a-~a-" ,name ,style))
			   ,log-name))))
	   (let ((prof:*significance-threshold* 
		  (or *profiling-threshold* 0.01)))
	     (format t "~&Profiling output being sent to ~a" pathname)
	     (with-open-file (output pathname
				     :direction :output
				     :if-does-not-exist :create
				     :if-exists :append)
	       (format output "~&Profile data for ~a" ,name)
	       (format output "~&Date: ~a" 
		       (excl:locale-print-time (get-universal-time)
					       :fmt "%B %d, %Y %T" :stream nil))
	       (format output "~&  Total time: ~,2F; Total space: ~:d \(~:*~d\)"
		       seconds conses)
	       (format output "~%~%")
	       (when (or (eq :time ,style)
			 (eq :space ,style))
		 (prof:show-flat-profile :stream output)
		 (prof:show-call-graph :stream output)
		 (when ,call-counts-p
		   (format output "~%~%Call counts~%")
		   (let ((*standard-output* output))
		     (prof:show-call-counts)))))))))))

#| OLD
;; integrate with LIFT

(pushnew :measure *deftest-clauses*)

(add-code-block
 :measure 1 :class-def
 (lambda () (def :measure)) 
 '((setf (def :measure) (cleanup-parsed-parameter value)))
 (lambda ()
   (pushnew 'measured-test-mixin (def :superclasses))
   nil))

(defclass measured-test-mixin ()
  ((total-conses :initform 0
		 :accessor total-conses)
   (total-seconds :initform 0
		  :accessor total-seconds)))
|#


#|
(defun test-sleep (period)	 
  (print (get-universal-time))
  (print
   (mp:process-wait-with-timeout 
    "wait-for-delay" period
    (lambda ()
      (sleep (1+ period)))))
  (print (get-universal-time)))

#+(or)
(test-sleep 2)
3392550276 
nil 
3392550281 

(defun test-gates (period)	 
  (print (get-universal-time))
  (let ((g (mp:make-gate nil)))
    (print
     (mp:process-wait-with-timeout 
      "wait-for-delay" period
      (lambda (gate)
	(mp:gate-open-p gate))
      g)))
  (print (get-universal-time)))

#+(or)
(test-gates 2)
3392550287 
nil 
3392550289 


|#

#|

(princ "ls" (shell-session-input-stream *ss*))
(terpri (shell-session-input-stream *ss*))
(force-output (shell-session-input-stream *ss*))

(read-shell-session-stream *ss* :output)

(shell-session-command *ss* "ls")

(shell-session-command *ss* "ps u")

(end-shell-session *ss*)

(compile 'read-from-stream-no-hang)

(with-input-from-string (s "hello there")
  (read-from-stream-no-hang s))

(read-shell-session-stream *ss* :output)

(setf *ss* (make-shell-session))

(count-repetitions-in-period 
 (lambda ()
   (shell-session-command *ss* "ps u")) 
 2.0)

(count-repetitions-in-period 
 (lambda ()
   (selected-metatilities::os-processes)) 
 2.0)

|#

#+(or)
(test-sleep-b 2)
