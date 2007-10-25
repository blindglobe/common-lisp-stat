;;; -*- mode: lisp -*-
;;; Copyright (c) 2005--2007, by A.J. Rossini <blindglobe@gmail.com>
;;; See COPYRIGHT file for any additional restrictions (BSD license).
;;; Since 1991, ANSI was finally finished.  Edited for ANSI Common Lisp. 

;;; compound -- Compound data and element-wise mapping functions
;;; 
;;; Copyright (c) 1991, by Luke Tierney. Permission is granted for
;;; unrestricted use.
;;;

;;;
;;; Package Setup
;;;

(in-package :cl-user)

(defpackage :lisp-stat-compound-data
  (:use :common-lisp
	:lisp-stat-object-system
	:lisp-stat-sequence)
  (:import-from :lisp-stat-fastmap fastmap)
  (:shadowing-import-from :lisp-stat-object-system
			  slot-value
			  call-next-method call-method)
  (:export compound-data-p compound-data-proto
	   compound-object-p
	   compound-data-seq compound-data-length

	   element-list element-seq

	   sort-data order rank

	   recursive-map-elements
	   ;; export sequence-related functionality

	   ;; export matrix-related functionality (not sure??)
	   ))

(in-package :lisp-stat-compound-data)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                    Internal Support Functions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cmpndp (x)
  "Predicate to determine if argument is compound. Most common
non-compound types are checked first."
  (declare (inline numberp symbolp stringp consp arrayp array-total-size))
  (cond ((or (numberp x) (symbolp x) (stringp x)) nil)
         ((or (consp x) (and (arrayp x) (< 0 (array-total-size x)))) t)
         (t (compound-object-p x))))

(defun find-compound-data (list)
  "Returns first compound data item in LIST or NIL if there is none."
  (dolist (x list) (if (cmpndp x) (return x))))

(defun any-compound-elements (seq)
  "Checks for a compound element."
  (cond ((consp seq) (dolist (x seq) (if (cmpndp x) (return x))))
	((vectorp seq)
	 (let ((n (length seq)))
	   (declare (fixnum n))
	   (dotimes (i n)
	     (declare (fixnum i))
	     (let ((x (aref seq i)))
	       (if (cmpndp x) (return x))))))
	(t (error "argument must be a list or vector"))))

(defun compound-data-sequence (x)
  "Returns sequence of data values for X."
  (declare (inline consp vectorp arrayp make-array array-total-size))
  (cond
   ((or (consp x) (vectorp x)) x)
   ((arrayp x) (make-array (array-total-size x) :displaced-to x))
   (t (send x :data-seq))))

(defmacro sequence-type (x) `(if (consp ,x) 'list 'vector))

(defun make-compound-data (shape sequence)
"Construct a compound data item to match the shape of the first
argument." 
  (let ((n (length (compound-data-sequence shape))))
    (if (/= n (length sequence)) (error "compound data not the same shape"))
    (cond
     ((consp shape) (if (consp sequence) sequence (coerce sequence 'list)))
     ((vectorp shape)
      (if (vectorp sequence) sequence (coerce sequence 'vector)))
     ((arrayp shape)
      (make-array (array-dimensions shape)
		  :displaced-to (coerce sequence 'vector)))
     (t (send shape :make-data sequence)))))

(defun make-circle (x)
  "Make a circular list of one element."
  (declare (inline cons rplacd))
  (let ((x (cons x nil)))
    (rplacd x x)
    x))

(defun check-compound (x)
  "Signals an error if X is not compound."
  (if (not (cmpndp x)) (error "not a compound data item - ~a" x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MAP-ELEMENTS function
;;; Applies a function to arguments. If all arguments are simple (i. e.
;;; not compound) then MAP-ELEMENTS acts like funcall. Otherwise all
;;; compound arguments must be of the same shape and simple arguments
;;; are treated as if they were compound arguments of the appropriate 
;;; shape. This is implemented by replacin all simple arguments by
;;; circular lists of one element.
;;;
;;; This implementation uses FASTMAP, a version of MAP that is assumed 
;;; to
;;;
;;;     a) work reasonable fast on any combination of lists and vectors
;;;        as its arguments
;;;
;;;     b) not hang if at least one of its arguments is not a circular
;;;        list.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fixup-map-elements-arglist (args)
  (do* ((args args (rest args))
        (x (car args) (car args)))
       ((null args))
    (declare (inline car))
    (setf (car args)
          (if (cmpndp x) (compound-data-sequence x) (make-circle x)))))
            
(defun map-elements (fcn &rest args)
"Args: (fcn &rest args)
Applies FCN elementwise. If no arguments are compound MAP-ELEMENTS
acts like FUNCALL. Compound arguments must all be the same shape. Non 
compound arguments, in the presence of compound ones, are treated as 
if they were of the same shape as the compound items with constant data
values."
  (let ((first-compound (find-compound-data args)))
    (cond ((null first-compound) (apply fcn args))
	  (t (fixup-map-elements-arglist args)
	     (let* ((seq (compound-data-sequence first-compound))
		    (type (sequence-type seq)))
               (make-compound-data first-compound
				   (apply #'fastmap type fcn args)))))))
		     
(defun recursive-map-elements (base-fcn fcn &rest args)
"Args: (base-fcn fcn &rest args)
The same idea as MAP-ELEMENTS, except arguments are in a list and the
base and recursive cases can use different functions. Modified to check
for second level of compounding and use base-fcn if there is none."
  (let ((first-compound (find-compound-data args)))
    (cond ((null first-compound) (apply base-fcn args))
	  (t (fixup-map-elements-arglist args)
	     (let* ((seq (compound-data-sequence first-compound))
		    (type (sequence-type seq))
		    (f (if (any-compound-elements seq) fcn base-fcn)))
               (make-compound-data first-compound
				   (apply #'fastmap type f args)))))))
      

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;             Public Predicate and Accessor Functions
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compound-data-p (x)
"Args: (x)
Returns T if X is a compound data item, NIL otherwise."
  (cmpndp x))

(defun compound-data-seq (x)
"Args (x)
Returns data sequence in X."
  (check-compound x)
  (compound-data-sequence x))

(defun compound-data-length (x)
"Args (x)
Returns length of data sequence in X."
  (check-compound x)
  (length (compound-data-sequence x)))

(defun element-list (x)
  (cond 
   ((compound-data-p x)
    (let ((x (concatenate 'list (compound-data-seq x)))) ; copies sequence
      (cond
       ((any-compound-elements x)
        (do ((next x (rest next)))
            ((not (consp next)))
          (setf (first next) (element-list (first next))))
        (do ((result (first x))
             (last (last (first x)))
             (next (rest x) (rest next)))
            ((not (consp next)) result)
          (setf (rest last) (first next))
          (setf last (last (first next)))))
       (t x))))
   (t (list x))))

(defun element-seq (x)
"Args: (x)
Returns sequence of the elements of compound item X."
  (check-compound x)
  (let ((seq (compound-data-seq x)))
    (if (any-compound-elements seq) (element-list seq) seq)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;                     Compound Data Objects
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defproto compound-data-proto)

(defmeth compound-data-proto :data-length (&rest args) nil)
(defmeth compound-data-proto :data-seq (&rest args) nil)
(defmeth compound-data-proto :make-data (&rest args) nil)
(defmeth compound-data-proto :select-data (&rest args) nil)

(defun compound-object-p (x) (kind-of-p x compound-data-proto))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;                         Sorting Functions
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sort-data (x)
"Args: (sequence)
Returns a sequence with the numbers or strings in the sequence X in order."
  (flet ((less (x y) (if (numberp x) (< x y) (string-lessp x y))))
    (stable-sort (copy-seq (compound-data-seq x)) #'less)))

(defun order (x)
"Args (x)
Returns a sequence of the indices of elements in the sequence of numbers
or strings X in order."
  (let* ((seq (compound-data-seq x))
	 (type (if (consp seq) 'list 'vector))
	 (i -1))
    (flet ((entry (x) (setf i (+ i 1)) (list x i))
	   (less (a b)
		 (let ((x (first a))
		       (y (first b)))
		   (if (numberp x) (< x y) (string-lessp x y)))))
      (let ((sorted-seq (stable-sort (map type #'entry seq) #'less)))
	(map type #'second sorted-seq)))))

;; this isn't destructive -- do we document destructive only, or any
;; variant?
(defun rank (x)
"Args (x)
Returns a sequence with the elements of the list or array of numbers or
strings X replaced by their ranks."
  (let ((ranked-seq (order (order x))))
    (make-compound-data (compound-data-shape x) ranked-seq)))
