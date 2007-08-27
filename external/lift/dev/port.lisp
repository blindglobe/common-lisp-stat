(in-package #:lift)

(setf (documentation 'get-backtrace 'function)
  "This is the function that is used internally by Hunchentoot to
show or log backtraces.  It accepts a condition object ERROR and
returns a string with the corresponding backtrace.")

(defun ensure-directory (pathname)
  (merge-pathnames (make-pathname :name :unspecific
				  :type :unspecific)
		   pathname))

(defun writable-directory-p (directory)
  (let ((directory (ensure-directory directory)))
    (and (probe-file directory)
	 #+allegro
	 (excl.osi:access directory excl.osi:*w-ok*))))

#+allegro
(defun total-bytes-allocated ()
  (sys::gsgc-totalloc-bytes t))

#+(or digitool openmcl)
(defun total-bytes-allocated ()
  (ccl::total-bytes-allocated))

#+sbcl
(defun total-bytes-allocated ()
  (cl-user::get-bytes-consed))

#+mcl
(defun get-backtrace (error)
  (with-output-to-string (s)
    (let ((*debug-io* s))
      (format *terminal-io* "~@<An unhandled error condition has been signalled:~3I ~a~I~:@>~%~%"
              error)
      (ccl:print-call-history :detailed-p nil))))

#+allegro
(defun get-backtrace (error)
  (with-output-to-string (s)
    (with-standard-io-syntax
      (let ((*print-readably* nil)
            (*print-miser-width* 40)
            (*print-pretty* t)
            (tpl:*zoom-print-circle* t)
            (tpl:*zoom-print-level* nil)
            (tpl:*zoom-print-length* nil))
        (cl:ignore-errors
          (format *terminal-io* "~&~@<An unhandled error condition has been signalled:~3I ~a~I~:@>~%~%"
                  error))
        (cl:ignore-errors
          (let ((*terminal-io* s)
                (*standard-output* s))
            (tpl:do-command "zoom"
                            :from-read-eval-print-loop nil
                            :count t
                            :all t)))))))

#+(or)
(defun zoom-to-stream (condition output)
  (with-standard-io-syntax
    (let ((*print-readably* nil)
	  (*print-miser-width* 40)
	  (*print-pretty* t)
	  (tpl:*zoom-print-circle* t)
	  (tpl:*zoom-print-level* nil)
	  (tpl:*zoom-print-length* nil))
      (ignore-errors 
	(format *terminal-io* "Creating backtrace for ~a to ~a" 
		condition output))
      (flet ((zoom (s)
	       (ignore-errors
		 (let ((*terminal-io* s)
		       (*standard-output* s))
		   (tpl:do-command "zoom"
		     :from-read-eval-print-loop nil
		   :count t :all t)))))
	(cond ((streamp output)
	       (zoom output))
	      (t
	       (ensure-directories-exist output)
	       (with-open-file (s output :direction :output
				  :if-exists :supersede
				  :if-does-not-exist :create)
		 (zoom s))))))))

#+lispworks
(defun get-backtrace (error)
  (declare (ignore error))
  (with-output-to-string (s)
    (let ((dbg::*debugger-stack* (dbg::grab-stack nil :how-many most-positive-fixnum))
          (*debug-io* s)
          (dbg:*debug-print-level* nil)
          (dbg:*debug-print-length* nil))
      (dbg:bug-backtrace nil))))

#+sbcl
;; determine how we're going to access the backtrace in the next
;; function
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (find-symbol "*DEBUG-PRINT-VARIABLE-ALIST*" :sb-debug)
    (pushnew :hunchentoot-sbcl-debug-print-variable-alist *features*)))

#+sbcl
(defun get-backtrace (error)
  (declare (ignore error))
  (with-output-to-string (s)
    #+:hunchentoot-sbcl-debug-print-variable-alist
    (let ((sb-debug:*debug-print-variable-alist*
            (list* '(*print-level* . nil)
                   '(*print-length* . nil)
                   sb-debug:*debug-print-variable-alist*)))
      (sb-debug:backtrace most-positive-fixnum s))
    #-:hunchentoot-sbcl-debug-print-variable-alist
    (let ((sb-debug:*debug-print-level* nil)
          (sb-debug:*debug-print-length* nil))
      (sb-debug:backtrace most-positive-fixnum s))))

#+cmucl
(defun get-backtrace (error)
  (declare (ignore error))
  (with-output-to-string (s)
    (let ((debug:*debug-print-level* nil)
          (debug:*debug-print-length* nil))
      (debug:backtrace most-positive-fixnum s))))


