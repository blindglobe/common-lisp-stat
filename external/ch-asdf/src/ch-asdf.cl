;;
;; file: ch-asdf.cl
;; author: cyrus harmon
;;

(in-package :ch-asdf)

(defparameter *c-compiler* "gcc")

(defclass clean-op (operation) ())

(defmethod perform ((operation clean-op) (c component))
  nil)

(defclass ch-cl-source-file (cl-source-file) ())

(defparameter *fasl-directory*
  (make-pathname :directory '(:relative #+sbcl "sbcl-fasl"
			      #+openmcl "openmcl-fasl"
			      #-(or sbcl openmcl) "fasl")))

(defmethod source-file-type ((c ch-cl-source-file) (s module)) "cl")

(defmethod output-files :around ((operation compile-op) (c ch-cl-source-file))
  (list (merge-pathnames *fasl-directory* (compile-file-pathname (component-pathname c)))))

(defclass ch-lisp-source-file (cl-source-file) ())

(defparameter *fasl-directory*
  (make-pathname :directory '(:relative #+sbcl "sbcl-fasl"
			      #+openmcl "openmcl-fasl"
			      #-(or sbcl openmcl) "fasl")))

(defmethod output-files :around ((operation compile-op) (c ch-lisp-source-file))
  (list (merge-pathnames *fasl-directory* (compile-file-pathname (component-pathname c)))))

;;;; C source file compilation section
;;;; ripped from sb-posix.asd in the sbcl source code

(defclass unix-dso (module)
  ((dso-name :accessor dso-name :initarg :dso-name)
   (dso-directory :accessor dso-directory :initarg :dso-directory)
   (include-directories :accessor include-directories :initarg :include-directories :initform nil)
   (link-library-directories :accessor link-library-directories :initarg :link-library-directories :initform nil)
   (link-libraries :accessor link-libraries :initarg :link-libraries :initform nil)
   (dso-type :accessor dso-type :initarg :dso-type :initform
             ;; fill appropriate OS specific types in here
             #+darwin "so"
             #-darwin "so")))

(defmethod input-files ((operation compile-op) (dso unix-dso))
  (mapcar #'component-pathname (module-components dso)))

(defmethod output-files ((operation compile-op) (dso unix-dso))
  (let ((dir (component-pathname dso)))
    (list
     (make-pathname :type (dso-type dso)
		    :name (if (slot-boundp dso 'dso-name)
                              (dso-name dso)
                              (car (last (pathname-directory dir))))
                    :directory (cond
                                 ((slot-boundp dso 'dso-directory)
                                  (let ((dso-pathname
                                         (merge-pathnames (dso-directory dso)
                                                          (component-pathname dso))))
                                    (ensure-directories-exist dso-pathname)
                                    (pathname-directory dso-pathname)))
                                 ((and (slot-boundp dso 'dso-name)
                                       (absolute-path-p (dso-name dso)))
                                  nil)
                                 (t (butlast (pathname-directory dir))))
		    :defaults dir))))

(defmethod perform :after ((operation compile-op) (dso unix-dso))
  (let ((dso-name (unix-name (car (output-files operation dso)))))
    (unless (zerop
	     (run-shell-command
	      "~A ~A -o ~S ~{~S ~}"
              *c-compiler*
	      (concatenate 'string
                           ;; This really should be specified as an initarg of the unix-dso
                           ;; rather than hard coded here!
                           ;; e.g. :components (... (:unix-library "R" :library-directory *r-dir*))
			   (sb-ext:posix-getenv "EXTRA_LDFLAGS")
			   " "
                           (format nil " ~{-L~A~^ ~} " (link-library-directories dso))
                           #-darwin
                           (format nil " ~{-Xlinker -rpath -Xlinker ~A~^ ~} " (link-library-directories dso))
                           (format nil " ~{-l~A~^ ~} " (link-libraries dso))
			   #+sunos " -shared -lresolv -lsocket -lnsl "
			   #+darwin " -bundle "
			   #-(or darwin sunos) " -shared ")
	      dso-name
	      (mapcar #'unix-name
		      (mapcan (lambda (c)
				(output-files operation c))
			      (module-components dso)))))
      (error 'operation-error :operation operation :component dso))))

;;;; Unix executables
;;;;
(defclass unix-executable (module)
  ((include-directories :accessor include-directories :initarg :include-directories :initform nil)
   (link-library-directories :accessor link-library-directories :initarg :link-library-directories :initform nil)
   (link-libraries :accessor link-libraries :initarg :link-libraries :initform nil)
   (source-files :accessor source-files :initarg :source-files :initform nil)))

(defmethod input-files ((operation compile-op) (executable unix-executable))
  (declare (optimize (debug 3)))
  (let ((files
         (mapcan #'(lambda (obj)
                     (output-files operation (get-sibling-component executable obj)))
                 (source-files executable))))
    (append (mapcar #'unix-name files)
            (mapcar #'component-pathname (module-components executable)))))

(defmethod output-files ((operation compile-op) (executable unix-executable))
  (list (component-pathname executable)))

#+nil
(defmethod operation-done-p ((o compile-op) (c unix-executable))
  nil)

(defmethod perform :after ((operation compile-op) (executable unix-executable))
  (let ((executable-name (unix-name (car (output-files operation executable)))))
    (unless (zerop
	     (run-shell-command
	      "~A ~A ~A -o ~S ~{~S ~}"
              *c-compiler*
              *c-compiler-options*
	      (concatenate 'string
                           ;; This really should be specified as an initarg of the unix-executable
                           ;; rather than hard coded here!
                           ;; e.g. :components (... (:unix-library "R" :library-directory *r-dir*))
			   (sb-ext:posix-getenv "EXTRA_LDFLAGS")
			   " "
                           (format nil " ~{-L~A~^ ~} " (link-library-directories executable))
                           #-darwin
                           (format nil " ~{-Xlinker -rpath -Xlinker ~A~^ ~} " (link-library-directories executable))
                           (format nil " ~{-l~A~^ ~} " (link-libraries executable))
                           (format nil " ~{~A~^ ~} " (input-files operation executable)))
	      executable-name
              nil
              #+nil
	      (mapcar #'unix-name
		      (mapcan (lambda (c)
				(output-files operation c))
			      (module-components executable)))))
      (error 'operation-error :operation operation :component executable))))

(defmethod component-depends-on ((op compile-op) (c unix-executable))
  (append (call-next-method)
          (mapcar #'(lambda (x)
                      `(compile-op ,x))
                  (source-files c))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; C Header Files
(defclass c-header-file (source-file) ())

(defmethod perform ((op compile-op) (c c-header-file)))

(defmethod perform ((op load-op) (c c-header-file)))

(defmethod source-file-type ((c c-header-file) (s module)) "h")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; C Source Files

;;; if this goes into the standard asdf, it could reasonably be extended
;;; to allow cflags to be set somehow
(defmethod output-files ((op compile-op) (c c-source-file))
  (list 
   (make-pathname :type "o" :defaults
		  (component-pathname c))))

(defgeneric get-include-directories (c))

(defmethod get-include-directories ((c c-source-file))
  (when (and 
         (slot-exists-p (component-parent c) 'include-directories)
         (slot-boundp (component-parent c) 'include-directories))
    (mapcar
     #'unix-name
     (include-directories
      (component-parent c)))))

;;;
;;; removed this bit here:
;;; #+nil "~{-isystem ~A~^ ~}"
;;; #+nil (mapcar #'unix-name (system-include-directories c))

(defparameter *c-compiler-options* "-Wall")

(defmethod perform ((op compile-op) (c c-source-file))
  (unless
      (= 0 (run-shell-command
            (concatenate 'string
                         (format nil "~A ~A ~A -o ~S -c ~S"
                                 *c-compiler*
                                 *c-compiler-options*
                                 (concatenate
                                  'string
                                  (format nil "~{-I~A~^ ~}" (get-include-directories c))
                                  " " (sb-ext:posix-getenv "EXTRA_CFLAGS")
                                  " -fPIC")
                                 (unix-name (car (output-files op c)))
                                 (unix-name (component-pathname c))))))
    (error 'operation-error :operation op :component c)))

(defmethod perform ((op load-op) (c c-source-file)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ASM Source Files
;;;
;;; NASTY cut-and-paste job here!

(defclass asm-source-file (source-file) ())

(defmethod source-file-type ((c asm-source-file) (s module)) "s")

(defmethod output-files ((op compile-op) (c asm-source-file))
  (list 
   (make-pathname :type "o" :defaults
		  (component-pathname c))))

(defgeneric get-include-directories (c))

(defmethod get-include-directories ((c asm-source-file))
  (when (and 
         (slot-exists-p (component-parent c) 'include-directories)
         (slot-boundp (component-parent c) 'include-directories))
    (mapcar
     #'unix-name
     (include-directories
      (component-parent c)))))

(defmethod perform ((op compile-op) (c asm-source-file))
  (unless
      (= 0 (run-shell-command
            (concatenate 'string
                         (format nil "~A ~A -o ~S -c ~S"
                                 *c-compiler*
                                 (concatenate
                                  'string
                                  (format nil "~{-I~A~^ ~}" (get-include-directories c))
                                  " " (sb-ext:posix-getenv "EXTRA_ASMFLAGS")
                                  " -fPIC")
                                 (unix-name (car (output-files op c)))
                                 (unix-name (component-pathname c))))))
    (error 'operation-error :operation op :component c)))

(defmethod perform ((op load-op) (c asm-source-file)))

(defmethod perform ((o load-op) (c unix-dso))
  (let ((co (make-instance 'compile-op)))
    (let ((filename (car (output-files co c))))
      #+cmu (ext:load-foreign filename)
      #+sbcl (sb-alien:load-shared-object filename))))

;;;; ASDF hackery for generating components, generated source files
;;;; and other neat stuff.

;;;
;;; generate-op
(defclass generate-op (asdf:operation) ())

;;;
;;; generated-component for components that generate files
(defclass generated-component (asdf:component) ())


(defmethod perform ((op generate-op) (c component)))

(defmethod perform ((op generate-op) (c generated-component)))

(defmethod component-depends-on ((op compile-op) (c generated-component))
  (append (call-next-method)
          `((generate-op ,(component-name c)))))

(defmethod component-depends-on ((op load-op) (c generated-component))
  (append (call-next-method)
          `((generate-op ,(component-name c)))))


(defmethod perform :before ((operation generate-op) (c generated-component))
  (map nil #'ensure-directories-exist (output-files operation c)))

;;;
;;; generated-file - not all components will have files associated
;;; with them. for those that do, use this subclass of
;;; generated-component.
(defclass generated-file (generated-component source-file) ())

(defmethod asdf::component-relative-pathname ((component generated-file))
  (let ((relative-pathname (slot-value component 'asdf::relative-pathname)))
    (if relative-pathname
        relative-pathname
        (let ((*default-pathname-defaults*
               (asdf::component-parent-pathname component)))
          (make-pathname
           :name (component-name component))))))

(defclass generated-source-file (generated-file) ())

(defmethod operation-done-p ((o operation) (c generated-source-file))
  (let ((in-files (input-files o c)))
    (if in-files
        (and (every #'probe-file in-files)
             (call-next-method))
        (call-next-method))))

(defmethod source-file-type ((c generated-file) (s module)) "")

(defmethod perform ((op compile-op) (c generated-file)))

(defmethod perform ((op load-op) (c generated-file)))

;;; pdf files

(defclass pdf-file (source-file) ())
(defmethod source-file-type ((c pdf-file) (s module)) "pdf")

(defmethod perform ((operation compile-op) (c pdf-file)))

(defmethod perform ((operation load-op) (c pdf-file))
  (ch-util::app-open (unix-name (component-pathname c))))

(defmethod operation-done-p ((o load-op) (c pdf-file))
  nil)


;;; css files

(defclass css-file (static-file) ())
(defmethod source-file-type ((c css-file) (s module)) "css")

;;; xhtml files

(defclass xhtml-file (html-file) ())
(defmethod source-file-type ((c xhtml-file) (s module)) "xhtml")

;;; tiff files

(defclass tiff-file (static-file) ())
(defmethod source-file-type ((c tiff-file) (s module)) "tiff")

;;; jpeg files

(defclass jpeg-file (static-file) ())
(defmethod source-file-type ((c jpeg-file) (s module)) "jpg")

;;; png files

(defclass png-file (static-file) ())
(defmethod source-file-type ((c png-file) (s module)) "png")

;;; markup files

(defclass markup-file (source-file) ())
(defmethod source-file-type ((c markup-file) (s module)) "gmarkup")

(defclass markup-latex-file (generated-source-file) ())
(defmethod source-file-type ((c markup-latex-file) (s module)) "tex")

(defclass markup-pdf-file (pdf-file generated-source-file) ())
(defclass markup-xhtml-file (xhtml-file) ())

;;; tinaa documentation

(defclass tinaa-directory (module) ())

;;; Need a generic ASDF object that reads a file and associates an
;;; in-memory object with the file. It should cache the creation date
;;; of the object and reload the object if the modification date of
;;; the file is newer than the creation date of the in-memory object.

(defun get-sibling-component (comp sib)
  (asdf:find-component (asdf:component-parent comp)
                       (asdf::coerce-name sib)))

(defclass object-component (generated-component)
  ((symbol :accessor object-symbol :initarg :symbol)))

(defmethod operation-done-p ((o generate-op) (c object-component))
  t)

(defmethod source-file-type ((c object-component) (s module)) nil)

(defun make-symbol-from-name (name)
  (intern (string (read-from-string name))))

(defmethod shared-initialize :after ((c object-component) slot-names
                                     &key force
                                     &allow-other-keys)
  (declare (ignore force))
  (when (slot-boundp c 'asdf::name)
    (unless (slot-boundp c 'symbol)
      (setf (object-symbol c)
            (make-symbol-from-name (asdf::component-name c))))))

(defmethod perform ((op compile-op) (c object-component)))
(defmethod perform ((op load-op) (c object-component))
  (setf (component-property c 'last-loaded)
        (get-universal-time)))

(defmethod operation-done-p ((o compile-op) (c object-component))
  t)

(defmethod operation-done-p ((o load-op) (comp object-component))
  (every #'identity
         (loop for (dep-op dep-comp) in
              (asdf::component-depends-on o comp)
              collect (asdf::operation-done-p
                       (make-instance dep-op)
                       (get-sibling-component comp dep-comp)))))

;;; An object-from-file is the file-based representation of an object. The
;;; load-op 
(defclass object-from-file (object-component source-file)
  ((load-date :accessor object-load-date :initarg :load-date)))

(defmethod perform ((op compile-op) (c object-from-file))
  (setf (asdf:component-property c 'last-compiled)
        (get-universal-time))
  (with-open-file (input-stream (component-pathname c))
    (setf (symbol-value (object-symbol c))
          (read input-stream)))
  (call-next-method))

(defmethod perform ((op generate-op) (c object-from-file))
  (setf (asdf::component-property c 'last-generated)
        (get-universal-time)))

;;; this needs to check the file date!!!!
(defmethod operation-done-p ((o generate-op) (c object-from-file))
  (let ((on-disk-time
         (file-write-date (component-pathname c)))
        (my-last-load-time (asdf::component-property c 'last-loaded)))
    (and on-disk-time
         my-last-load-time
         (>= my-last-load-time on-disk-time))))


(defclass object-to-file (object-component)
  ((write-date :accessor object-write-date :initarg :write-date)))


(defclass object-from-variable (object-component)
  ((input-object :accessor object-input-object :initarg :input-object)))

(defmethod component-depends-on ((op generate-op) (c object-from-variable))
  (append (call-next-method)
          `((load-op , (asdf::coerce-name (object-input-object c))))))

(defmethod component-depends-on ((op compile-op) (c object-from-variable))
  (append (call-next-method)
          `((load-op ,(asdf::coerce-name (object-input-object c))))))

(defmethod operation-done-p ((o generate-op) (c object-from-variable))
  (let ((input-object-last-load-time
         (asdf::component-property
          (find-component (component-parent c)
                          (asdf::coerce-name (object-input-object c)))
          'last-loaded))
        (my-last-generate-time (asdf::component-property c 'last-generated)))
    (and input-object-last-load-time
         my-last-generate-time
         (>= my-last-generate-time input-object-last-load-time))))

(defmethod operation-done-p ((o compile-op) (c object-from-variable))
  (let ((my-last-generate-time (asdf::component-property c 'last-generated))
        (my-last-compile-time (asdf::component-property c 'last-compiled)))
    (and my-last-generate-time
         my-last-compile-time
         (>= my-last-compile-time my-last-generate-time))))

(defmethod operation-done-p ((o load-op) (c object-from-variable))
  (let ((my-last-compile-time (asdf::component-property c 'last-compiled))
        (my-last-load-time (asdf::component-property c 'last-loaded)))
    (and my-last-compile-time
         my-last-load-time
         (>= my-last-load-time my-last-compile-time))))

(defmethod perform ((op generate-op) (c object-from-variable))
  (setf (asdf:component-property c 'last-generated)
        (get-universal-time))
  (let ((sexp
       (symbol-value
        (object-symbol
         (find-component (component-parent c)
                         (asdf::coerce-name (object-input-object c)))))))
  (setf (symbol-value (object-symbol c)) sexp)))


(defmethod perform ((op compile-op) (c object-from-variable))
  (setf (asdf:component-property c 'last-compiled)
        (get-universal-time)))

(defmethod perform ((op load-op) (c object-from-variable))
    (setf (asdf:component-property c 'last-loaded)
        (get-universal-time)))

(defclass load-only-file-mixin ()
   ())

(defclass load-only-cl-source-file (load-only-file-mixin cl-source-file)
   ())

(defmethod perform ((op compile-op) (component load-only-file-mixin))
   nil)

(defmethod perform ((op load-op) (component load-only-cl-source-file))
   (load (component-pathname component)))

;;; graphviz dot-files

(defparameter *dot-program* "dot")
(defparameter *dot-program-path*
  (let ((found (sb-ext:find-executable-in-search-path
                *dot-program*)))
    (unless found
      (setf found 
            #+darwin "/opt/local/bin/dot"
            #-darwin "/usr/local/bin/dot"))
    found))

(defclass graphviz-dot-file (generated-source-file) ())

(defmethod source-file-type ((c graphviz-dot-file) (s module)) "dot")

(defmethod output-files ((operation compile-op) (c graphviz-dot-file))
  (list
   (merge-pathnames (make-pathname :type "png")
                    (compile-file-pathname (component-pathname c)))))

(defmethod perform ((op compile-op) (c graphviz-dot-file))
  (run-shell-command
   "~A ~A -o~A ~A"
   *dot-program-path*
   "-Tpng"
   (ch-asdf:unix-name (car (output-files op c)))
   (ch-asdf:unix-name (component-pathname c))))


;;; benchmarking stuff

(defclass benchmark-op (operation) ())

(defmethod perform ((operation benchmark-op) (c component))
  (oos 'load-op c))



