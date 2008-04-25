(in-package #:common-lisp-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package '#:lift)
    (defpackage #:lift
      (:use #:common-lisp)
      (:import-from		     
       #+allegro #:mop
       #+clisp #:clos
       #+lispworks #:clos
       #+mcl #:ccl
       #+cmu #:clos-mop
       #+sbcl #:sb-mop
       #+scl #:clos
       #:class-direct-subclasses
       #:class-direct-superclasses
       #:class-precedence-list)
      (:export
       #:*benchmark-log-path*
       #:*lift-report-detail-hook*
       #:*lift-report-header-hook*
       #:*lift-report-footer-hook*
       #:with-profile-report
       #:describe-test-result
       #:count-repetitions
       #:while-counting-repetitions
       #:with-timeout))))

(unless (and (find-package :asdf)
	     (find-symbol (symbol-name 'system-relative-pathname) :asdf)
	     (fboundp (find-symbol
		       (symbol-name 'system-relative-pathname) :asdf)))
  (warn "LIFT uses asdf:system-relative-pathname which your version of ASDF 
doesn't seem to include. LIFT will define these for now but you may want to consider updating to the most recent version of ASDF (see http://www.cliki.net/asdf for details).")
  (intern (symbol-name 'system-source-file) :asdf)
  (intern (symbol-name 'system-source-directory) :asdf)
  (intern (symbol-name 'system-relative-pathname) :asdf)
  (export 'asdf::system-relative-pathname :asdf) 
  (defun asdf::system-source-file (system-name)
    (let ((system (asdf:find-system system-name)))
      (make-pathname 
       :type "asd"
       :name (asdf:component-name system)
       :defaults (asdf:component-relative-pathname system))))

  (defun asdf::system-source-directory (system-name)
    (make-pathname :name nil
		   :type nil
		   :defaults (asdf::system-source-file system-name)))

  (defun asdf::system-relative-pathname (system pathname &key name type)
    (let ((directory (pathname-directory pathname)))
      (when (eq (car directory) :absolute)
	(setf (car directory) :relative))
      (merge-pathnames
       (make-pathname :name (or name (pathname-name pathname))
		      :type (or type (pathname-type pathname))
		      :directory directory)
       (asdf::system-source-directory system)))))
  