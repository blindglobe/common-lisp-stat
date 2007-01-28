;;;; lsmacros -- Various macros
;;;; 
;;;; Copyright (c) 1991, by Luke Tierney. Permission is granted for
;;;; unrestricted use.
;;;;

(provide "lsmacros")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;              Macros for LISP-STAT-BASICS Package
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;
;;;; Package Setup
;;;;

#+:CLtL2
(progn
  (defpackage "LISP-STAT-BASICS"
    (:nicknames "LS-BASICS")
    (:use "COMMON-LISP" "LISP-STAT-OBJECT-SYSTEM"))

  (in-package lisp-stat-basics))
#-:CLtL2
(in-package 'lisp-stat-basics 
	    :nicknames '(ls-basics)
	    :use '(lisp lsos))

;;;;
;;;; Floating Point Macros
;;;;

(defmacro declare-double (&rest vars) `(declare (long-float ,@vars)))

;;;;
;;;; Macros for Defining Vectorized Funcitons
;;;;

(defmacro make-vectorized-function (sym fcn)
  `(defun ,sym (&rest args)
     (apply #'map-elements #',fcn args)))

(defmacro fixup-vectorized-doc-list (sym)
  `(let ((doc (documentation ',sym 'function)))
     (if doc (list (format nil "~s~%Vectorized.")))))

(defmacro make-rv-function (sym fcn &rest args)
  (cond
   ((and args (= (length args) 1))
    `(defun ,sym (,@args)
       ,@(fixup-vectorized-doc-list fcn)
       (declare (inline cmpndp ,fcn ,sym recursive-map-elements list))
       (if (cmpndp ,@args)
	   (recursive-map-elements #',fcn #',sym ,@args)
	   (,fcn ,@args))))
   (args
    `(defun ,sym (,@args)
       ,@(fixup-vectorized-doc-list fcn)
       (declare (inline cmpndp ,fcn ,sym recursive-map-elements list))
       (if ,(cons 'or (mapcar #'(lambda (x) (list 'cmpndp x)) args))
	   (recursive-map-elements #',fcn #',sym ,@args)
	   (,fcn ,@args))))
   (t
    `(defun ,sym (&optional (x nil has-x) (y nil has-y) &rest args)
       ,@(fixup-vectorized-doc-list fcn)
       (declare (inline cmpndp ,fcn ,sym recursive-map-elements list))
       (if has-x
	   (if has-y
	       (if (or args (cmpndp x) (cmpndp y))
		   (apply #'recursive-map-elements #',fcn #',sym x y args)
		   (,fcn x y))
	       (if (cmpndp x)
		   (recursive-map-elements #',fcn #',sym x)
		   (,fcn x)))
	   (,fcn))))))

(defmacro make-rv-function-1 (sym fcn &rest args)
  (cond
   ((and args (= (length args) 1))
    `(defun ,sym (,@args)
       ,@(fixup-vectorized-doc-list fcn)
       (declare (inline cmpndp ,fcn ,sym recursive-map-elements list))
       (if (cmpndp ,@args)
	   (recursive-map-elements #',fcn #',sym ,@args)
	   (,fcn ,@args))))
   (args
    `(defun ,sym (,@args)
       ,@(fixup-vectorized-doc-list fcn)
       (declare (inline cmpndp ,fcn ,sym recursive-map-elements list))
       (if ,(cons 'or (mapcar #'(lambda (x) (list 'cmpndp x)) args))
	   (recursive-map-elements #',fcn #',sym ,@args)
	   (,fcn ,@args))))
   (t
    `(defun ,sym (x &optional (y nil has-y) &rest args)
       ,@(fixup-vectorized-doc-list fcn)
       (declare (inline cmpndp ,fcn ,sym recursive-map-elements list))
       (if has-y
         (if (or args (cmpndp x) (cmpndp y))
           (apply #'recursive-map-elements #',fcn #',sym x y args)
           (,fcn x y))
         (if (cmpndp x)
           (recursive-map-elements #',fcn #',sym x)
           (,fcn x)))))))
