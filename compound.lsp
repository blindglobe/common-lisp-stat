;;;; compound -- Compound data and element-wise mapping functions
;;;; 
;;;; Copyright (c) 1991, by Luke Tierney. Permission is granted for
;;;; unrestricted use.
;;;;

;;;;
;;;; Package Setup
;;;;

(in-package #:lisp-stat-basics)

(export '(compound-data-p map-elements compound-data-seq
	  compound-data-length element-seq compound-data-proto))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                    Internal Support Functions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Predicate to determine if argument is compound. Most common
;;; non-compound types are checked first.
(defun cmpndp (x)
  (declare (inline numberp symbolp stringp consp arrayp array-total-size))
  (cond ((or (numberp x) (symbolp x) (stringp x)) nil)
         ((or (consp x) (and (arrayp x) (< 0 (array-total-size x)))) t)
         (t (compound-object-p x))))

;;; Returns first compound data item in LIST or NIL if there is none.
(defun find-compound-data (list)
  (dolist (x list) (if (cmpndp x) (return x))))

;;; Checks for a compound element
(defun any-compound-elements (seq)
  (cond ((consp seq) (dolist (x seq) (if (cmpndp x) (return x))))
	((vectorp seq)
	 (let ((n (length seq)))
	   (declare (fixnum n))
	   (dotimes (i n)
	     (declare (fixnum i))
	     (let ((x (aref seq i)))
	       (if (cmpndp x) (return x))))))
	(t (error "argument must be a list or vector"))))


;;; Returns sequence of data values for X.
(defun compound-data-sequence (x)
  (declare (inline consp vectorp arrayp make-array array-total-size))
  (cond
   ((or (consp x) (vectorp x)) x)
   ((arrayp x) (make-array (array-total-size x) :displaced-to x))
   (t (send x :data-seq))))

(defmacro sequence-type (x) `(if (consp ,x) 'list 'vector))

;;;; Construct a compound data item to match the shape of the first argument.
(defun make-compound-data (shape sequence)
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

;;; Make a circular list of one element
(defun make-circle (x)
  (declare (inline cons rplacd))
  (let ((x (cons x nil)))
    (rplacd x x)
    x))

;;; Signals an error if X is not compound
(defun check-compound (x)
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

;;; COMPOUND-DATA-P function
(defun compound-data-p (x)
"Args: (x)
Returns T if X is a compound data item, NIL otherwise."
  (cmpndp x))

;;; COMPOUND-DATA-SEQ function
(defun compound-data-seq (x)
"Args (x)
Returns data sequence in X."
  (check-compound x)
  (compound-data-sequence x))

;;; COMPOUND-DATA-LENGTH function
(defun compound-data-length (x)
"Args (x)
Returns length of data sequence in X."
  (check-compound x)
  (length (compound-data-sequence x)))

;;; ELEMENT-SEQ function
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
