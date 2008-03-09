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
	:lisp-stat-types)
  (:shadowing-import-from :lisp-stat-object-system
			  slot-value
			  call-next-method call-method)
  (:export compound-data-p *compound-data-proto*
	   compound-object-p
	   compound-data-seq compound-data-length
	   element-list element-seq
	   sort-data order rank
	   recursive-map-elements map-elements repeat
	   check-sequence
	   get-next-element make-next-element set-next-element
	   sequencep iseq ordered-nneg-seq
	   select split-list which
	   difference rseq))

(in-package :lisp-stat-compound-data)

;;; Sequences are part of ANSI CL, being a supertype of vector and
;;; list (ordered set of things).
;;; 
;;; Need to use the interenal structure when possible -- silly to be
;;; redundant!  However, this means we need to understand what
;;; sequences were intending to do, which I'm not clear on yet.

;;; The original ordering, object-wise, was to have compound
;;; functionality passed into sequences, into other data sources.
;;; However, at this point, we will see about inverting this and
;;; having basic data types pushed through compound, to simplify
;;; packaging.  In this vein, we have created a compound package to
;;; contain the compound data and sequence structures.  Probably need
;;; to clean this up even more.


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
				   (apply #'map type fcn args)))))))
		     
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
				   (apply #'map type f args)))))))
      

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

(defun compound-data-shape (x)
  "Needed but undefined??"
  x)


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

(defvar *compound-data-proto*)
(defproto *compound-data-proto*)

;;; FIXME: These need to be defined!!
(defmeth *compound-data-proto* :data-length (&rest args) nil)
(defmeth *compound-data-proto* :data-seq (&rest args) nil)
(defmeth *compound-data-proto* :make-data (&rest args) nil)
(defmeth *compound-data-proto* :select-data (&rest args) nil)

(defun compound-object-p (x) (kind-of-p x *compound-data-proto*))



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
    (make-compound-data
     ;; compound-data-shape is undefined?
     (compound-data-shape x) ranked-seq)))



;;;
;;; REPEAT function
;;;

(defun repeat (a b)
"Args: (vals times)
Repeats VALS. If TIMES is a number and VALS is a non-null, non-array atom,
a list of length TIMES with all elements eq to VALS is returned. If VALS
is a list and TIMES is a number then VALS is appended TIMES times. If
TIMES is a list of numbers then VALS must be a list of equal length and 
the simpler version of repeat is mapped down the two lists.
Examples: (repeat 2 5)                 returns (2 2 2 2 2)
          (repeat '(1 2) 3)            returns (1 2 1 2 1 2)
	  (repeat '(4 5 6) '(1 2 3))   returns (4 5 5 6 6 6)
	  (repeat '((4) (5 6)) '(2 3)) returns (4 4 5 6 5 6 5 6)"
  (cond ((compound-data-p b)
	 (let* ((reps (coerce (compound-data-seq (map-elements #'repeat a b))
			      'list))
		(result (first reps))
		(tail (last (first reps))))
	   (dolist (next (rest reps) result)
		   (when next
			 (setf (rest tail) next)
			 (setf tail (last next))))))
	(t (let* ((a (if (compound-data-p a) 
			 (coerce (compound-data-seq a) 'list)
		         (list a)))
		  (result nil))
	     (dotimes (i b result)
		      (let ((next (copy-list a)))
			(if result (setf (rest (last next)) result))
			(setf result next)))))))
;;;
;;; WHICH function
;;;

(defun which (x)
"Args: (x)
Returns a list of the indices where elements of sequence X are not NIL."
  (let ((x (list (compound-data-seq x)))
	(result nil)
	(tail nil))
    (flet ((add-result (x)
             (if result (setf (rest tail) (list x)) (setf result (list x)))
	     (setf tail (if tail (rest tail) result)))
	   (get-next-element (seq-list i)
	     (cond ((consp (first seq-list))
		    (let ((elem (first (first seq-list))))
		      (setf (first seq-list) (rest (first seq-list)))
		      elem))
		   (t (aref (first seq-list) i)))))
	  (let ((n (length (first x))))
	    (dotimes (i n result)
		     (if (get-next-element x i) (add-result i)))))))

;;;                      Type Checking Functions

(defun check-sequence (a)
  ;; FIXME:AJR: does this handle consp as well?  (Luke had an "or"
  ;; with consp).
  (if (not (typep a 'sequence))
      (error "not a sequence - ~s" a)))

;;;                       Sequence Element Access


;;; (elt x i) -- NOT.  This is more like "pop".
(defun get-next-element (x i)
  "Get element i from seq x.  FIXME: not really??"
  (let ((myseq (first x)))
    (if (consp myseq)
        (let ((elem (first myseq)))
          (setf (first x) (rest myseq))
          elem)
      (aref myseq i))))

;;; (setf (elt x i) v)
(defun set-next-element (x i v)
  (let ((seq (first x)))
    (cond ((consp seq)
           (setf (first seq) v)
           (setf (first x) (rest seq)))
          (t (setf (aref seq i) v)))))

(defun make-next-element (x) (list x))


;;;                         Sequence Functions


;; to prevent breakage.
(defmacro sequencep (x) 
  (typep x 'sequence))

(defun iseq (a &optional b)
"Args: (n &optional m)
Generate a sequence of consecutive integers from a to b.
With one argumant returns a list of consecutive integers from 0 to N - 1.
With two returns a list of consecutive integers from N to M.
Examples: (iseq 4) returns (0 1 2 3)
          (iseq 3 7)  returns (3 4 5 6 7)
          (iseq 3 -3) returns (3 2 1 0 -1 -2 -3)"
  (if b
      (let ((n (+ 1 (abs (- b a))))
	    (x nil))
	(dotimes (i n x)
		 (setq x (cons (if (< a b) (- b i) (+ b i)) x))))
      (cond 
       ((= 0 a) nil)
       ((< a 0) (iseq (+ a 1) 0))
       ((< 0 a) (iseq 0 (- a 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;               Subset Selection and Mutation Functions
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun old-rowmajor-index (index indices dim olddim)
  "translate row major index in resulting subarray to row major index
in the original array."
  (declare (fixnum index))
  (let ((rank (length dim))
        (face 1)
        (oldface 1)
        (oldindex 0))
    (declare (fixnum rank face oldface))

    (dotimes (i rank)
      (declare (fixnum i))
      (setf face (* face (aref dim i)))
      (setf oldface (* oldface (aref olddim i))))
  
    (dotimes (i rank)
      (declare (fixnum i))
      (setf face (/ face (aref dim i)))
      (setf oldface (/ oldface (aref olddim i)))
      (incf oldindex
	    (* oldface (aref (aref indices i) (floor (/ index face))))) ;;*** is this floor really needed???
      (setf index (rem index face)))
    oldindex))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;               Subset Selection and Mutation Functions
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun subarray-select (a indexlist &optional (values nil set_values))
  "extract or set subarray for the indices from a displaced array." 
  (let ((indices nil)
        (index)
        (dim)
        (vdim)
        (data)
        (result_data)
        (olddim)
        (result)
        (rank 0)
        (n 0)
        (k 0))
    (declare (fixnum rank n))

    (if (or (sequencep a) (not (arrayp a))) (error "not an array - ~a" a))
    (if (not (listp indexlist))  (error "bad index list - ~a" indices))
    (if (/= (length indexlist)  (array-rank a))
	(error "wrong number of indices"))
    
    (setf indices (coerce indexlist 'vector))
    
    (setf olddim (coerce (array-dimensions a) 'vector))
    
    ;; compute the result dimension vector and fix up the indices
    (setf rank (array-rank a))
    (setf dim (make-array rank))
    (dotimes (i rank)
      (declare (fixnum i))
      (setf index (aref indices i))
      (setf n (aref olddim i))
      (setf index (if (fixnump index) (vector index) (coerce index 'vector)))
      (setf k (length index))
      (dotimes (j k)
        (declare (fixnum j))
        (if (<= n (check-nonneg-fixnum (aref index j)))
          (error "index out of bounds - ~a" (aref index j)))
        (setf (aref indices i) index))
      (setf (aref dim i) (length index)))
    
    ;; set up the result or check the values
    (let ((dim-list (coerce dim 'list)))
      (cond 
       (set_values
        (cond
         ((compound-data-p values)
          (if (or (not (arrayp values)) (/= rank (array-rank values)))
            (error "bad values array - ~a" values))
          (setf vdim (coerce (array-dimensions values) 'vector))
          (dotimes (i rank)
            (declare (fixnum i))
            (if (/= (aref vdim i) (aref dim i))
              (error "bad value array dimensions - ~a" values)))
          (setf result values))
         (t (setf result (make-array dim-list :initial-element values)))))
       (t (setf result (make-array dim-list)))))

    ;; compute the result or set the values
    (setf data (compound-data-seq a))
    (setf result_data (compound-data-seq result))
    (setf n (length result_data))
    (dotimes (i n)
      (declare (fixnum i))
      (setf k (old-rowmajor-index i indices dim olddim))
      (if (or (> 0 k) (>= k (length data))) (error "index out of range"))
      (if set_values
        (setf (aref data k) (aref result_data i))
        (setf (aref result_data i) (aref data k))))
  
    result))


;;;; is x an ordered sequence of nonnegative positive integers?
(defun ordered-nneg-seq(x)
  ;; FIXME -- sbcl warning about unreachable code, might be a logic error here. 
  (if (sequencep x)
      (let ((n (length x))
            (cx (make-next-element x))
            (m 0))
        (dotimes (i n t)
          (let ((elem (check-nonneg-fixnum (get-next-element cx i))))
            (if (> m elem) (return nil) (setf m elem)))))))

;;;; select or set the subsequence corresponding to the specified indices
(defun sequence-select(x indices &optional (values nil set-values))
  ;; FIXME -- sbcl warning about unreachable code, might be a logic error here. 
  (let ((rlen 0)
        (dlen 0)
        (vlen 0)
        (data nil)
        (result nil))
    (declare (fixnum rlen dlen vlen))

    ;; Check the input data
    (check-sequence x)
    (check-sequence indices)
    (if set-values (check-sequence values))
    
    ;; Find the data sizes
    (setf data (if (ordered-nneg-seq indices) x (coerce x 'vector)))
    (setf dlen (length data))
    (setf rlen (length indices))
    (when set-values
      (setf vlen (length values))
      (if (/= vlen rlen) (error "value and index sequences do not match")))

    ;; set up the result/value sequence
    (setf result
          (if set-values
              values
              (make-sequence (if (listp x) 'list 'vector) rlen)))

    ;; get or set the sequence elements 
    (if set-values
      (do ((nextx x)
           (cr (make-next-element result))
           (ci (make-next-element indices))
           (i 0 (+ i 1))
           (j 0)
           (index 0))
          ((>= i rlen))
        (declare (fixnum i j index))
        (setf index (get-next-element ci i))
	(if (<= dlen index) (error "index out of range - ~a" index))
        (let ((elem (get-next-element cr i)))
          (cond
           ((listp x)
            (when (> j index)
              (setf j 0)
              (setf nextx x))
            (do ()
                ((not (and (< j index) (consp nextx))))
              (incf j 1)
              (setf nextx (rest nextx)))
            (setf (first nextx) elem))
           (t (setf (aref x index) elem)))))
      (do ((nextx data)
           (cr (make-next-element result))
           (ci (make-next-element indices))
           (i 0 (+ i 1))
           (j 0)
           (index 0)
           (elem nil))
          ((>= i rlen))
        (declare (fixnum i j index))
        (setf index (get-next-element ci i))
	(if (<= dlen index) (error "index out of range - ~a" index))
	(cond
         ((listp data) ;; indices must be ordered
          (do ()
              ((not (and (< j index) (consp nextx))))
            (incf j 1)
            (setf nextx (rest nextx)))
          (setf elem (first nextx)))
         (t (setf elem (aref data index))))
	(set-next-element cr i elem)))
  
    result))

;;;
;;; SELECT function
;;;

(defun select (x &rest args)
"Args: (a &rest indices)
A can be a list or an array. If A is a list and INDICES is a single number
then the appropriate element of A is returned. If  is a list and INDICES is
a list of numbers then the sublist of the corresponding elements is returned.
If A in an array then the number of INDICES must match the ARRAY-RANK of A.
If each index is a number then the appropriate array element is returned.
Otherwise the INDICES must all be lists of numbers and the corresponding
submatrix of A is returned. SELECT can be used in setf."
  (cond
   ((every #'fixnump args)
    (if (listp x) (nth (first args) x) (apply #'aref x args)))
   ((sequencep x) (sequence-select x (first args)))
   (t (subarray-select x args))))


;; Built in SET-SELECT (SETF method for SELECT)
;; FIXME: This should be done cleaner, check the spec, something like
;; (defun (setf select) (x &rest args)...)
(defun set-select (x &rest args)
  (let ((indices (butlast args))
        (values (first (last args))))
    (cond
     ((typep x 'sequence)
      (if (not (consp indices)) (error "bad indices - ~a" indices))
      (let* ((indices (first indices))
             (i-list (if (fixnump indices) (list indices) indices))
             (v-list (if (fixnump indices) (list values) values)))
        (sequence-select x i-list v-list)))
     ((arrayp x)
      (subarray-select x indices values))
     (t (error "bad argument type - ~a" x)))
    values))

(defsetf select set-select)

;;;;
;;;; Basic Sequence Operations
;;;;

(defun difference (x)
"Args: (x)
Returns differences for a sequence X."
  (let ((n (length x)))
    (- (select x (iseq 1 (1- n))) (select x (iseq 0 (- n 2))))))

(defun rseq (a b num)
"Args: (a b num)
Returns a list of NUM equally spaced points starting at A and ending at B."
  (+ a (* (values-list (iseq 0 (1- num))) (/ (float (- b a)) (1- num)))))



(defun split-list (x n)
"Args: (list cols)
Returns a list of COLS lists of equal length of the elements of LIST.
Example: (split-list '(1 2 3 4 5 6) 2) returns ((1 2 3) (4 5 6))"
  (check-one-fixnum n)
  (if (/= (rem (length x) n) 0) (error "length not divisible by ~a" n))
  (flet ((next-split ()
           (let ((result nil)
                 (end nil))
             (dotimes (i n result)
               (declare (fixnum i))
               (let ((c-elem (list (first x))))
                 (cond ((null result)
                        (setf result c-elem)
                        (setf end result))
                       (t 
                        (setf (rest end) c-elem)
                        (setf end (rest end)))))
               (setf x (rest x))))))
    (let ((result nil)
          (end nil)
          (k (/ (length x) n)))
      (declare (fixnum k))
      (dotimes (i k result)
        (declare (fixnum i))
        (let ((c-sub (list (next-split))))
          (cond ((null result)
                 (setf result c-sub)
                 (setf end result))
                (t 
                 (setf (rest end) c-sub)
                 (setf end (rest end)))))))))
