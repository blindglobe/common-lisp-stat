(in-package #:lift)

(declaim (optimize (speed 3) (safety 1)))

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

(defun measure (fn &rest args)
  (declare (dynamic-extent args))
  (let ((bytes 0) (seconds 0) result)
    (measure-time (seconds)
      (measure-conses (bytes)
	(setf result (apply fn args))))
    (values seconds bytes result)))

(defun benchmark (name fn &rest args)
  (declare (dynamic-extent args))
  (let ((seconds 0.0) (conses 0) result)
    (flet ((do-it ()
	     (setf (values seconds conses result)
		   (apply 'measure fn args))))
      (cond ((or (eq :time *profiling*)
		 (eq :space *profiling*))
	     (with-profiling (:type *profiling*) (do-it)))
	    ((eq :count *profiling*)
	     (with-profiling (:count t) (do-it)))
	    (t
	     (do-it)))
      (let ((date-stamp (get-universal-time)))
	(ensure-directories-exist *benchmark-file*)
	;;log 
	(with-open-file (output *benchmark-file* 
				:direction :output
				:if-does-not-exist :create
				:if-exists :append)
	  (with-standard-io-syntax
	    (let ((*print-readably* nil))
	      (terpri output)
	      (format output "\(~11,d ~20,s ~10,s ~10,s ~{~s~^ ~}~30,s ~s ~s\)"
		      date-stamp name 
		      seconds conses *additional-markers*
		      (if (symbolp fn) fn "<function>") args result))))
	(when *profiling*
	  (let ((pathname (merge-pathnames
			   (make-pathname 
			    :type "prof"
			    :name (format nil "~a-~a-~a"
					  name *profiling* date-stamp))
			   *benchmark-file*)))
	    (format t "~&Profiling output being sent to ~a" pathname)
	    (with-open-file (output pathname
				    :direction :output
				    :if-does-not-exist :create
				    :if-exists :append)
	      (format output "~&Profile data for ~a" name)
	      (format output "~&  Total time: ~,2F; Total space: ~:d \(~d\)"
		      seconds conses conses)
	      (format output "~&  Arguments:~{~&    ~s,~^~}" args)
	      (format output "~%~%")
	      (when (or (eq :time *profiling*)
			(eq :space *profiling*))
		(prof:show-flat-profile :stream output)
		(prof:show-call-graph :stream output))
	      (when (eq :count *profiling*)
		(let ((*standard-output* output)
		      (*print-readably* nil))
		  (prof:show-call-counts)))))))
      (list seconds conses result))))
