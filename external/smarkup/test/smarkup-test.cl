
(in-package #:smarkup-test)

(defun test-xhtml-file ()
  (let ((smarkup-file (ch-asdf:asdf-lookup-path "asdf:/smarkup-test/test/sample-sexp"))
        (xhtml-file (merge-pathnames #p"sample.xhtml"
                                     (ch-asdf:asdf-lookup-path "asdf:/smarkup-test/test"))))
    (with-open-file (stream smarkup-file)
      (let ((sexp (read stream)))
        (let ((filtered (smarkup::apply-filters
                         sexp
                         '(:lisp :smarkup-metadata :html-metadata :ref))))
          (smarkup::render-as :xhtml filtered xhtml-file))))))
