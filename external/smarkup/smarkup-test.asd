


(asdf:operate 'asdf:load-op "ch-asdf")

(defpackage #:smarkup-test-system (:use #:cl #:asdf #:ch-asdf))
(in-package #:smarkup-test-system)

(defclass smarkup-test-cl-source-file (ch-asdf:ch-cl-source-file) ())

(defsystem :smarkup-test
  :name "smarkup-test"
  :author "Cyrus Harmon <cyrus@bobobeach.com>"
  :licence "BSD"
  :version #.(with-open-file
                 (vers (merge-pathnames "version.lisp-expr" *load-truename*))
               (read vers))
  :description "S-Expression-based Markup Utilities"
  :depends-on (:ch-asdf :ch-util :bibtex :smarkup)
  :components
  ((:module
    :test
    :components
    ((:smarkup-test-cl-source-file "defpackage")
     (:smarkup-test-cl-source-file "smarkup-test" :depends-on (:defpackage))
     (:static-file "sample-bib" :pathname #p"sample.bib")
     (:static-file "sample-sexp" :pathname #p"sample.sexp")))))
