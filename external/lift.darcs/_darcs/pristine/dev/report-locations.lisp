(in-package #:lift)

(defmethod generate-report-summary-pathname :around ()
  (let ((basepath (call-next-method)))
    (merge-pathnames
     (make-pathname 
      :directory `(:relative 
		   ,(asdf::implementation-specific-directory-name)))
     basepath)))

