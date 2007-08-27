;;; -*- Mode: lisp; outline-regexp: ";;;;;*"; indent-tabs-mode: nil -*-;;;
;;;
;;; file: smarkup-asdf.cl
;;; author: cyrus harmon
;;;

;;;
;;; smarkup ASDF classes and methods for various component types
;;; and operations on them

(in-package :smarkup)

(defclass filtered-object (ch-asdf:object-from-variable)
  ((filters :accessor object-filters :initarg :filters)))

(defmethod perform ((op compile-op) (c filtered-object))
  (call-next-method)
  (setf (symbol-value (ch-asdf::object-symbol c))
        (apply-filters
         (symbol-value (ch-asdf::object-symbol c))
         (object-filters c))))

(defmethod component-relative-pathname ((component filtered-object)))

(defmethod component-pathname ((component filtered-object)))

(defparameter *pdflatex-program* "pdflatex")
(defparameter *pdflatex-program-path*
  (let ((found (sb-ext:find-executable-in-search-path
                *pdflatex-program*)))
    (unless found
      (setf found 
            #+darwin "/opt/local/bin/pdflatex"
            #-darwin "/usr/local/bin/pdflatex"))
    found))

(defclass object-latex-file (ch-asdf:object-from-variable generated-file) ())

(defmethod perform ((op ch-asdf::generate-op) (c object-latex-file))
  (call-next-method)
  (render-as :latex
             (symbol-value (ch-asdf::object-symbol c))
             (component-pathname c)))

(defmethod perform ((operation compile-op) (c object-latex-file))
  (with-component-directory (c)
    (let ((unix-path (ch-util::unix-name (component-pathname c))))
      (ch-util::run-program *pdflatex-program-path*
                            (list unix-path))
      ;; we have to do this twice to get the references right!
      ;; maybe 3x?
      (ch-util::run-program *pdflatex-program-path*
                            (list unix-path)))))

(defmethod operation-done-p ((o ch-asdf::generate-op) (c object-latex-file))
  (declare (optimize (debug 3)))
  (let ((on-disk-time
         (file-write-date (component-pathname c)))
        (obj (asdf::find-component
              (asdf::component-parent c)
              (asdf::coerce-name (ch-asdf::object-input-object c)))))
    
    (let ((obj-date (asdf::component-property obj 'ch-asdf::last-loaded)))
      (and on-disk-time
           obj-date
           (>= on-disk-time obj-date)))))

(defclass object-xhtml-file (ch-asdf:object-from-variable source-file) ())

(defmethod perform ((op compile-op) (c object-xhtml-file))
  (call-next-method)
  (let ((sexp (symbol-value (ch-asdf::object-symbol c)))
        (file (component-pathname c)))
    (render-as :xhtml sexp file)))

(defmethod perform ((op load-op) (c object-xhtml-file))
  (call-next-method)
  (ch-util::firefox-open (ch-util::unix-name (component-pathname c))))

(defclass object-cl-pdf-file (ch-asdf:object-from-variable pdf-file) ())

(defmethod perform ((op compile-op) (c object-cl-pdf-file))
  (call-next-method)
  (let ((sexp (symbol-value (ch-asdf::object-symbol c)))
        (file (component-pathname c)))
    (render-as :cl-pdf sexp file)))

(defmethod perform ((op load-op) (c object-cl-pdf-file))
  (call-next-method)
  (ch-util::app-open (unix-name (component-pathname c))))

