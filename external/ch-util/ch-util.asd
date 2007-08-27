
(defpackage #:ch-util-system (:use #:asdf #:cl))
(in-package #:ch-util-system)

;;;;
;;;; The following section customizes asdf to work with filenames
;;;; with a .cl extension and to put fasl files in a separate
;;;; directory.
;;;;
;;;; To enable this behvior, use asdf component type
;;;;  :ch-util-cl-source-file
;;;;
(defclass ch-util-cl-source-file (cl-source-file) ())

(defparameter *fasl-directory*
  (make-pathname :directory '(:relative #+sbcl "sbcl-fasl"
			      #+openmcl "openmcl-fasl"
			      #-(or sbcl openmcl) "fasl")))

(defmethod source-file-type ((c ch-util-cl-source-file) (s module)) "cl")

(defmethod asdf::output-files :around ((operation compile-op) (c ch-util-cl-source-file))
  (list (merge-pathnames *fasl-directory* (compile-file-pathname (component-pathname c)))))

(defsystem #:ch-util
  :name "ch-util"
  :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
  :version #.(with-open-file
                 (vers (merge-pathnames "version.lisp-expr" *load-truename*))
               (read vers))
  :licence "BSD"
  :description "Miscellaneous Utility Functions from Cyrus Harmon"
  :components
  ((:static-file "version" :pathname #p"version.lisp-expr")
   (:module :src
	    :components
	    ((:ch-util-cl-source-file "defpackage")
	     (:ch-util-cl-source-file "ch-util" :depends-on ("defpackage"))
	     (:ch-util-cl-source-file "lists" :depends-on ("defpackage"))
	     (:ch-util-cl-source-file "macros" :depends-on ("defpackage"))
	     (:ch-util-cl-source-file "testharness" :depends-on ("defpackage"))
	     (:ch-util-cl-source-file "hash-table" :depends-on ("defpackage"))
	     (:ch-util-cl-source-file "array" :depends-on ("defpackage"))
	     (:ch-util-cl-source-file "sequence" :depends-on ("defpackage"))
	     (:ch-util-cl-source-file "vector" :depends-on ("defpackage"))
	     (:ch-util-cl-source-file "bytebuffer" :depends-on ("defpackage"))
	     (:ch-util-cl-source-file "filesystem" :depends-on ("defpackage"))
	     (:ch-util-cl-source-file "debug" :depends-on ("defpackage"))
	     (:ch-util-cl-source-file "ch-asdf" :depends-on ("defpackage" "filesystem"))))
   (:static-file "bootstrap" :pathname #p"bootstrap.cl")
   (:static-file "COPYRIGHT")
   (:static-file "README")
   (:static-file "make-dist" :pathname #.(make-pathname :name "make-dist" :type "sh"))))

