;;
;; file: asdf-util.cl
;; author: cyrus harmon
;;

(in-package :ch-asdf)

(defun unix-name (pathname)
  (namestring 
   (typecase pathname
     (logical-pathname (translate-logical-pathname pathname))
     (t pathname))))

(defun absolute-path-p (path)
  (when (listp (pathname-directory path))
    (eql (car (car (pathname-directory path)))
         :absolute)))

(defun asdf-lookup (path)
  (cond ((and path (listp path))
         (reduce #'asdf:find-component (cdr path)
                 :initial-value (asdf:find-system (car path))))
        ((stringp path)
         (let ((uri (puri:parse-uri path)))
           (when uri
             (let ((scheme (puri:uri-scheme uri)))
               (when (and (or (null scheme)
                              (eql scheme :asdf))
                          (puri:uri-parsed-path uri))
                 (asdf-lookup (cdr (puri:uri-parsed-path uri))))))))))

(defun asdf-lookup-path (path)
  (component-pathname (asdf-lookup path)))

(defun merge-asdf-path (name path)
  (merge-pathnames name (asdf-lookup-path path)))

(defmacro with-component-directory ((component) &body body)
  `(ch-util::with-current-directory
       (make-pathname
        :directory (pathname-directory
                    (component-pathname ,component)))
     ,@body))

(flet ((asdf-op (op component) 
         (typecase component
           (string
            (asdf:operate 'asdf:load-op (asdf-lookup component)))
           (t (asdf:operate 'asdf:load-op component)))))
  (defun asdf-load (component)
    (asdf-op 'asdf:load-op component))
  (defun asdf-compile (component)
    (asdf-op 'asdf:compile-op component)))

