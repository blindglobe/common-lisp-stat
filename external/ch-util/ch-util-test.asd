
(defpackage #:ch-util-test-system (:use #:asdf #:cl))
(in-package #:ch-util-test-system)

;;;;
;;;; The following section customizes asdf to work with filenames
;;;; with a .cl extension and to put fasl files in a separate
;;;; directory.
;;;;
;;;; To enable this behvior, use asdf component type
;;;;  :ch-util-test-cl-source-file
;;;;
(defclass ch-util-test-cl-source-file (cl-source-file) ())

(defparameter *fasl-directory*
  (make-pathname :directory '(:relative #+sbcl "sbcl-fasl"
			      #+openmcl "openmcl-fasl"
			      #-(or sbcl openmcl) "fasl")))

(defmethod source-file-type ((c ch-util-test-cl-source-file) (s module)) "cl")

(defmethod asdf::output-files :around ((operation compile-op) (c ch-util-test-cl-source-file))
  (list (merge-pathnames *fasl-directory* (compile-file-pathname (component-pathname c)))))

(defsystem #:ch-util-test
  :name "ch-util-test"
  :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
  :version #.(with-open-file
                 (vers (merge-pathnames "version.lisp-expr" *load-truename*))
               (read vers))
  :licence "BSD"
  :description "Tests for ch-util"
  :depends-on (:ch-util)
  :components
  ((:module :test
	    :components
	    ((:ch-util-test-cl-source-file "defpackage")
	     (:ch-util-test-cl-source-file "test-ch-util" :depends-on ("defpackage"))))))

