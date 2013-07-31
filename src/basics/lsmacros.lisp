;;; -*- mode: lisp -*-
;;; Copyright (c) 2005--2007, by A.J. Rossini <blindglobe@gmail.com>
;;; See COPYRIGHT file for any additional restrictions (BSD license).
;;; Since 1991, ANSI was finally finished.  Edited for ANSI Common Lisp. 

;;;; lsmacros -- Various macros
;;;; 
;;;; Copyright (c) 1991, by Luke Tierney. Permission is granted for
;;;; unrestricted use.
;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;              Macros for LISP-STAT-BASICS Package
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :lisp-stat-macros)

;;; Floating Point Macros

(defmacro declare-double (&rest vars) `(declare (long-float ,@vars)))

;;; Macros for Defining Vectorized Funcitons

(defmacro make-vectorized-function (sym fcn)
  `(defun ,sym (&rest args)
     (apply #'map-elements #',fcn args)))

(defmacro fixup-vectorized-doc-list (sym)
  `(let ((doc (documentation ',sym 'function)))
    (if doc (list (format nil "~s~%Vectorized." ,sym)))))

;;; Exported

;; recursively vectorizes (rv) functions in dists and lispstat-math. 
(defmacro make-rv-function (sym fcn &rest args)
  (cond
   ((and args (= (length args) 1))
    `(defun ,sym (,@args)
       ,@(fixup-vectorized-doc-list fcn)
       (declare (inline compound-data-p ,fcn ,sym recursive-map-elements list))
       (if (compound-data-p ,@args)
	   (recursive-map-elements #',fcn #',sym ,@args)
	   (,fcn ,@args))))
   (args
    `(defun ,sym (,@args)
       ,@(fixup-vectorized-doc-list fcn)
       (declare (inline compound-data-p ,fcn ,sym recursive-map-elements list))
       (if ,(cons 'or (mapcar #'(lambda (x) (list 'compound-data-p x)) args))
	   (recursive-map-elements #',fcn #',sym ,@args)
	   (,fcn ,@args))))
   (t
    `(defun ,sym (&optional (x nil has-x) (y nil has-y) &rest args)
       ,@(fixup-vectorized-doc-list fcn)
       (declare (inline compound-data-p ,fcn ,sym recursive-map-elements list))
       (if has-x
	   (if has-y
	       (if (or args (compound-data-p x) (compound-data-p y))
		   (apply #'recursive-map-elements #',fcn #',sym x y args)
		   (,fcn x y))
	       (if (compound-data-p x)
		   (recursive-map-elements #',fcn #',sym x)
		   (,fcn x)))
	   (,fcn))))))

(defmacro make-rv-function-1 (sym fcn &rest args)
  (cond
   ((and args (= (length args) 1))
    `(defun ,sym (,@args)
       ,@(fixup-vectorized-doc-list fcn)
       (declare (inline compound-data-p ,fcn ,sym recursive-map-elements list))
       (if (compound-data-p ,@args)
	   (recursive-map-elements #',fcn #',sym ,@args)
	   (,fcn ,@args))))
   (args
    `(defun ,sym (,@args)
       ,@(fixup-vectorized-doc-list fcn)
       (declare (inline compound-data-p ,fcn ,sym recursive-map-elements list))
       (if ,(cons 'or (mapcar #'(lambda (x) (list 'compound-data-p x)) args))
	   (recursive-map-elements #',fcn #',sym ,@args)
	   (,fcn ,@args))))
   (t
    `(defun ,sym (x &optional (y nil has-y) &rest args)
       ,@(fixup-vectorized-doc-list fcn)
       (declare (inline compound-data-p ,fcn ,sym recursive-map-elements list))
       (if has-y
         (if (or args (compound-data-p x) (compound-data-p y))
           (apply #'recursive-map-elements #',fcn #',sym x y args)
           (,fcn x y))
         (if (compound-data-p x)
           (recursive-map-elements #',fcn #',sym x)
           (,fcn x)))))))
