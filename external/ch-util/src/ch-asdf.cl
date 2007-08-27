;;;
;;; ch-asdf.cl -- various lisp utilities that make my life easier
;;;
;;; Author: Cyrus Harmon <ch-lisp@bobobeach.com>
;;; Time-stamp: <2006-07-31 14:46:56 sly>
;;;

(in-package :ch-util)

(defun pathname-in-parent (path system)
  (enough-namestring
   path
   (truename
    (merge-pathnames
     (make-pathname :directory `(:relative :up))
     (make-pathname
      :directory
      (pathname-directory
       (asdf:system-definition-pathname system)))))))

(defun list-module-files (module system)
  (cond ((string-equal (symbol-name (class-name (class-of module)))
                       "tinaa-directory")
         (list (pathname-in-parent (asdf:component-pathname module) system)))
        (t
         (append
          (typecase module
            (asdf:system (list
                          (pathname-in-parent
                           (truename (asdf:system-definition-pathname module))
                           system))))
          (mapcan #'(lambda (x)
                      (typecase x
                        (asdf:module (list-module-files x system))
                        (t (let ((path (asdf:component-pathname x)))
                             (when path
                               (list
                                (pathname-in-parent
                                 path
                                 system)))))))
                  (asdf:module-components module))))))

(defun make-dist (&rest module-keywords)
  (let ((modules (mapcar
		  #'(lambda (k) (asdf:find-system k))
		  module-keywords)))
    (let ((module (car modules))
	  (files-for-dist
	   (mapcan
	    #'(lambda (module)
                (list-module-files module module))
	    modules)))
      (with-open-file (gzip-file
		       (concatenate 'string
				    (asdf:component-name module)
				    "-"
				    (asdf:component-version module)
				  ".tar.gz")
		       :if-does-not-exist :create
		       :if-exists :supersede
		       :direction :output)
	(let ((*default-pathname-defaults*
	       (truename
		(merge-pathnames
		 (make-pathname :directory '(:relative :up))
		 (make-pathname
		  :directory
		  (pathname-directory
		   (asdf:system-definition-pathname module)))))))
	  (with-current-directory *default-pathname-defaults*
	    (ch-util:run-program 
	     "/usr/bin/gzip"
	     '("-c")
	     :input (ch-util:process-output-stream
		     (ch-util:run-program
		      "/usr/bin/tar"
		      (append
		       '("-cf" "-")
		       (print files-for-dist))
		      :wait nil
		      :output :stream
		      :error #p"/dev/null"
		      :if-error-exists :append))
	     :output gzip-file)))))))

(defun unregister-system (name)
  (remhash name asdf::*defined-systems*))
