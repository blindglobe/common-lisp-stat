;;;
;;; filesystem.cl -- various lisp utilities that make my life easier
;;;
;;; Author: Cyrus Harmon <ch-lisp@bobobeach.com>
;;;

(in-package :ch-util)

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-posix))

#+openmcl
(defun pwd ()
  (ccl::current-directory-name))

(defun component-present-p (value)
  (and value (not (eql value :unspecific))))

;;; I don't remember where I got this from. Probably from KMR somewhere
;;; along the line...
(defun pathname-as-directory (pathname)
  "Return a pathname reperesenting the given pathname in `directory
  form', i.e. with all the name elements in the directory component and
  NIL in the name and type components. Can not be used on wild
  pathnames. Returns its argument if name and type are both nil or
  :unspecific."
  (setf pathname (pathname pathname))
  (when (wild-pathname-p pathname)
    (error "Can't reliably convert wild pathnames to directory names."))
  (cond 
   ((or (component-present-p (pathname-name pathname))
	(component-present-p (pathname-type pathname)))
    (make-pathname 
     :directory (append (pathname-directory pathname) (list
						       (file-namestring pathname)))
     :name      nil
     :type      nil
     :defaults pathname))
   (t pathname)))

;;; I don't remember where I got this from. Probably from KMR somewhere
;;; along the line...
(defun list-directory (dirname)
  "Return a list of the contents of the directory named by dirname.
  Names of subdirectories will be returned in `directory normal form'.
  Unlike CL:DIRECTORY, LIST-DIRECTORY does not accept wildcard
  pathnames; `dirname' should simply be a pathname that names a
  directory. It can be in either file or directory form."
  (let ((wildcard (make-pathname :name :wild 
				 :type :wild
				 :defaults (pathname-as-directory
					    dirname))))
    
    (declare (ignorable wildcard))
    #+openmcl
    ;; OpenMCl by default doesn't return subdirectories at all. But
    ;; when prodded to do so with the special argument :directories,
    ;; it returns them in directory form.
    (directory wildcard :directories t)
    #-openmcl (directory wildcard)))

(defun ls (&optional (dirname ""))
  (list-directory dirname))

(defmacro with-open-file-preserving-case (&rest args)
  `(let ((*readtable* (copy-readtable)))
    (setf (readtable-case *readtable*) :preserve)
    (with-open-file ,@args)))

(defparameter *tmp-file-directory* (make-pathname :directory '(:absolute "tmp")))

(defun tmp-file-name (&key (prefix "tmp."))
  (concatenate 'string prefix (format nil "~8,'0',X" (random #xffffffff))))
  
(defun tmp-file (&key (name (tmp-file-name)))
  (merge-pathnames name *tmp-file-directory*))

(defun remove-keyword-args (list &rest remove)
  (loop for x on list by #'cddr when (not (member (car x) remove)) append (list (car x) (cadr x))))
  
(defmacro with-temporary-file ((path stream &rest options &key (delete t) &allow-other-keys) &body body)
  `(let ((,path (tmp-file)))
     (prog1
	 (with-open-file (,stream ,path ,@(remove-keyword-args options :delete))
	   ,@body)
       ,(when delete `(delete-file ,path)))))

;;; from antifuchs on #lisp via paste.lisp.org
;;; http://paste.lisp.org/display/9527
(defmacro with-current-directory (dir &body body)
  `(unwind-protect (progn
		     #+sbcl
                     (sb-posix:chdir ,dir)
                     (let ((*default-pathname-defaults* ,dir))
                       ,@body))
     #+sbcl (sb-posix:chdir *default-pathname-defaults*)))

(defmacro run-program (&rest args)
  #+sbcl `(sb-ext::run-program ,@args))

(defmacro run-program-asynchronously (&rest args)
  #+sbcl `(sb-ext::run-program ,@args :wait nil))


(defun app-open (&rest args)
  #+darwin (run-program "/usr/bin/open" (mapcar #'(lambda (x) (if (pathnamep x) (unix-name x) x)) args)))

(defun safari-open (&rest args)
  #+darwin
  (apply #'app-open (list* "-a" "/Applications/Safari.app"
			   (mapcar #'(lambda (x) (if (pathnamep x) (unix-name x) x)) args))))

(defun firefox-open (&rest args)
  #+darwin
  (apply #'app-open (list* "-a" "/Users/sly/Applications/Camino.app"
                           (mapcar #'(lambda (x) (if (pathnamep x) (unix-name x) x)) args))))


(defparameter *pdf-viewer*
  #+linux "kpdf"
  #+darwin "Preview.app")

(defparameter *pdf-viewer-path*
  (let ((found (sb-ext:find-executable-in-search-path
                *pdf-viewer*)))
    (unless found
      (setf found 
            #+darwin "/Applications/Preview.app"
            #-darwin "/usr/bin/kpdf"))
    found))

(defun pdf-open (&rest args)
  #+darwin
  (apply #'app-open "-a" *pdf-viewer-path* (mapcar #'unix-name args))
  #-darwin
  (run-program-asynchronously *pdf-viewer-path* 
	       (mapcar #'(lambda (x)
			   (if (pathnamep x) (unix-name x) x)) args))
  )

(defparameter *html-viewer*
  #+linux "konqueror"
  #+darwin "Safari.app")

(defparameter *html-viewer-path*
  (let ((found (sb-ext:find-executable-in-search-path
                *html-viewer*)))
    (unless found
      (setf found 
            #+darwin "/Applications/Safari.app"
            #-darwin "/usr/bin/konqueror"))
    found))

(defun html-open (&rest args)
  (run-program-asynchronously *html-viewer-path* 
	       (mapcar #'(lambda (x)
			   (if (pathnamep x) (unix-name x) x)) args)))

(defmacro process-output-stream (&rest args)
  #+sbcl `(sb-ext::process-output ,@args))

(defun prefix (seq suffix)
  "Removes the prefix of seq that occurs before suffix. Return
  values are the prefix and the position at which the suffix
  begins in the original sequence, or the original sequence and
  NIL if the suffix is not in seq"
  (let ((pos (search suffix seq)))
    (if pos
        (values (subseq seq 0 pos) pos)
        (values seq nil))))

(defun subdirectories (path)
  (loop for d in
       (directory
        (make-pathname :name :wild :type nil :defaults path))
     when (string-equal (file-namestring d) "") collect d))

(defun unix-name (pathname)
  (namestring 
   (typecase pathname
     (logical-pathname (translate-logical-pathname pathname))
     (t pathname))))

(defun map-files-in-directory (function
			       destination-directory
			       &key file-type)
  (mapcar function
	  (directory
	   (merge-pathnames
	    (make-pathname
	     :name :wild
	     :type (if file-type file-type :wild))
	    destination-directory))))

(defun pathname-lessp (p1 p2)
  (string-lessp (pathname-name p1)
                (pathname-name p2)))

