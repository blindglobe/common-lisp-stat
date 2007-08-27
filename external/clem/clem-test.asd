
(defpackage #:clem-test-system (:use #:asdf #:cl))
(in-package #:clem-test-system)

;;;;
;;;; The following section customizes asdf to work with filenames
;;;; with a .cl extension and to put fasl files in a separate
;;;; directory.
;;;;
;;;; To enable this behvior, use asdf component type
;;;;  :clem-test-cl-source-file
;;;;
(defclass clem-test-cl-source-file (cl-source-file) ())

(defmethod source-file-type ((c clem-test-cl-source-file) (s module)) "cl")

(defparameter *fasl-directory*
  (make-pathname :directory '(:relative #+sbcl "sbcl-fasl"
			      #+openmcl "openmcl-fasl"
			      #-(or sbcl openmcl) "fasl")))

(defmethod asdf::output-files :around ((operation compile-op) (c clem-test-cl-source-file))
  (list (merge-pathnames *fasl-directory* (compile-file-pathname (component-pathname c)))))


(defsystem :clem-test
  :name "clem-test"
  :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
  :version #.(with-open-file
                 (vers (merge-pathnames "version.lisp-expr" *load-truename*))
               (read vers))
  :depends-on (ch-util clem)
  :components
  ((:module :test
	    :components
	    ((:clem-test-cl-source-file "defpackage")
	     (:clem-test-cl-source-file "test-clem" :depends-on ("defpackage"))
	     (:clem-test-cl-source-file "test-clem2" :depends-on ("defpackage"))
	     (:clem-test-cl-source-file "test-clem3" :depends-on ("defpackage"))
	     (:clem-test-cl-source-file "test-defmatrix" :depends-on ("defpackage"))
	     (:clem-test-cl-source-file "test-transform" :depends-on ("defpackage"))
	     (:clem-test-cl-source-file "test-convolve" :depends-on ("defpackage"))
	     (:clem-test-cl-source-file "bench-matrix" :depends-on ("defpackage"))
	     (:clem-test-cl-source-file "test-hprod" :depends-on ("defpackage"))
	     ))))

