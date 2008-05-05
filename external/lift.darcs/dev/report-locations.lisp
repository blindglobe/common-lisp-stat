(in-package #:lift)

(defmethod generate-report-summary-pathname :around ()
  (let ((basepath (call-next-method)))
    (add-implementation-specific-directory-name basepath)))

#+(or)
(defmethod html-report-pathname :around (pathname)
  (declare (ignore pathname))
  (let ((basepath (call-next-method)))
    (add-implementation-specific-directory-name basepath)))

(defun add-implementation-specific-directory-name (basepath)
    (merge-pathnames
     (make-pathname 
      :directory `(:relative 
		   ,(asdf::implementation-specific-directory-name)))
   basepath))

