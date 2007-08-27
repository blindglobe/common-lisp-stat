
;;; I'm trying to figure out how to generalize some matrix operations
;;; using SBCL's nice fast non-consing math operations.
;;; 
;;; My first (well, the previous one anyway) attempt at this involved
;;; big hairy macros for each type. I'm trying to simplify things a
;;; bit with smaller functions with proper type definitions that get
;;; called as appropriate. I still, however, run into the problem that
;;; at some point I need a declare statement to tell the compiler what
;;; type of thing I'm working with. Perhaps this is a stupid idea.  In
;;; my defense, the previous approach does work and gives me nice fast
;;; matrix math, it just involves a whole lot of compiled functions. I
;;; still think there's some merit to trying to simplify/clean this up
;;; a bit.

(in-package :clem-test)

;;; I need *my-pacakge* so the intern statement below doesn't intern
;;; the symbol into CL-USER and I put it up here to remind myself of
;;; this fact in case I switch packages
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *my-package* :clem-test))

;;; macro to define typed accessor functions
(defmacro make-accessors (element-type
			  &optional (element-name (symbol-name element-type)))
  (let ((funcname (intern
		   (string-upcase
		    (concatenate 'string "mref-" element-name)) *my-package*)))
    `(progn
       (declaim (inline ,funcname))
       (defun ,funcname (a i j)
	 (declare (type (simple-array ,element-type (* *)) a))
	 (aref a i j))
       
       (declaim (inline (setf ,funcname)))
       (defun (setf ,funcname) (v a i j)
	 (declare (type (simple-array ,element-type (* *)) a))
	 (setf (aref a i j) v)))))

;;; local macro to call make-accessors. saves a (very) few keystrokes
;;; but makes it easier to do more to these later if necessary.
(macrolet ((frob (&rest args)
	     `(make-accessors ,@args)))
  (frob double-float)
  (frob single-float)
  (frob (unsigned-byte 8) "ub8")
  (frob (unsigned-byte 16) "ub16")
  (frob (unsigned-byte 32) "ub32")
  (frob (signed-byte 8) "sb8")
  (frob (signed-byte 16) "sb16")
  (frob (signed-byte 32) "sb32")
  (frob fixnum)
  (frob bit))

;;; get the accessor functions for an array
;;; returns multiple-values the getter and the setter
(defun get-accessors (a)
  (typecase a
    ((simple-array double-float (* *))
     (values #'mref-double-float #'(setf mref-double-float)))
    ((simple-array single-float (* *))
     (values #'mref-single-float #'(setf mref-single-float)))

    ((simple-array (unsigned-byte 8) (* *))
     (values #'mref-ub8 #'(setf mref-ub8)))
    ((simple-array (unsigned-byte 16) (* *))
     (values #'mref-ub16 #'(setf mref-ub16)))
    ((simple-array (unsigned-byte 32) (* *))
     (values #'mref-ub32 #'(setf mref-ub32)))

    ((simple-array (signed-byte 8) (* *))
     (values #'mref-sb8 #'(setf mref-sb8)))
    ((simple-array (signed-byte 16) (* *))
     (values #'mref-sb16 #'(setf mref-sb16)))
    ((simple-array (signed-byte 32) (* *))
     (values #'mref-sb32 #'(setf mref-sb32)))

    ((simple-array fixnum (* *))
     (values #'mref-fixnum #'(setf mref-fixnum)))
    ((simple-array bit (* *))
     (values #'mref-bit #'(setf mref-bit)))))

;;; test arrays
(defparameter uba (make-array '(1024 1024) :element-type '(unsigned-byte 8) :initial-element 2))
(defparameter dfa (make-array '(1024 1024) :element-type 'double-float :initial-element 2d0))

;;; test functions to see if this works

;;; the old school way - works no consing - double floats
(defun array-test-1 (a)
  (let ((getter #'mref-double-float)
	(setter #'(setf mref-double-float)))
    (destructuring-bind (r c) (array-dimensions a)
      (dotimes (i r)
	(dotimes (j c)
	  (funcall setter (* (funcall getter a i j) 2.0d0) a i j))))))

;;; the old school way - works no consing - unsigned bytes
(defun array-test-2 (a)
  (let ((getter #'mref-ub8)
	(setter #'(setf mref-ub8)))
    (destructuring-bind (r c) (array-dimensions a)
      (dotimes (i r)
	(dotimes (j c)
	  (funcall setter (* (funcall getter a i j) 2) a i j))))
    a))

(defun array-test-3 (a)
;;  (declare (optimize (speed 3) (space 0)))
  (multiple-value-bind (getter setter) (get-accessors a)
;;    (declare (type (function ((simple-array double-float (* *)) * *) double-float) getter)
;;	     (type (function (double-float (simple-array double-float (* *)) * *) double-float) setter))
    (destructuring-bind (r c) (array-dimensions a)
      (dotimes (i r)
	(dotimes (j c)
	  (funcall setter (* (funcall getter a i j) 2.0d0)) a i j)))))
