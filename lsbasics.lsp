;;; -*- mode: lisp -*-
;;; Copyright (c) 2005--2007, by A.J. Rossini <blindglobe@gmail.com>
;;; See COPYRIGHT file for any additional restrictions (BSD license).
;;; Since 1991, ANSI was finally finished.  Edited for ANSI Common Lisp. 

;;;; lsbasics -- Low level Lisp-Stat functions
;;;; 
;;;; Copyright (c) 1991, by Luke Tierney. Permission is granted for
;;;; unrestricted use.

;;;;
;;;; Package Setup
;;;;

(defpackage #:lisp-stat-basics
  (:nicknames #:ls-basics)
  (:use #:common-lisp #:lisp-stat-object-system)
  (:shadowing-import (package-shadowing-symbols 'lisp-stat-object-system))
  (:export
   ;; lispstat-basics.lisp
   sequencep copy-vector copy-array iseq which repeat select
   permute-array sum prod count-elements mean if-else sample sort-data
   order rank
   ;; matrices.lisp
   matrixp num-rows num-cols matmult identity-matrix diagonal row-list
   column-list inner-product outer-product cross-product transpose
   bind-columns bind-rows
   ;; linalg.lisp
   chol-decomp lu-decomp lu-solve determinant inverse sv-decomp
   qr-decomp rcondest make-rotation 
   fft make-sweep-matrix sweep-operator ax+y numgrad numhess
   split-list eigen
   ;; in linalg.lisp, possibly not supported by matlisp
   spline kernel-dens kernel-smooth
   ;; lispstat-macros
   make-rv-function make-rv-function-1 
   ;; lispstat-float
   #:*stat-float-typing* #:*stat-cfloat-typing* #:*stat-float-template*
   #:machine-epsilon  
   ;; dists
   log-gamma uniform-rand normal-cdf normal-quant normal-dens
   normal-rand bivnorm-cdf cauchy-cdf cauchy-quant cauchy-dens
   cauchy-rand gamma-cdf gamma-quant gamma-dens gamma-rand
   chisq-cdf chisq-quant chisq-dens chisq-rand beta-cdf beta-quant
   beta-dens beta-rand t-cdf t-quant t-dens t-rand f-cdf f-quant
   f-dens f-rand poisson-cdf poisson-quant poisson-pmf poisson-rand 
   binomial-cdf binomial-quant binomial-pmf binomial-rand
   ;;
   ))

(in-package #:lisp-stat-basics))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;                      Type Checking Functions
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fixnump (x)
"Args: (x)
Returns T if X is a fixnum; NIL otherwise."
  (declare (inline typep))
  (typep x 'fixnum))

(defun check-nonneg-fixnum (x)
  (if (and (fixnump x) (<= 0 x)) x (error "not a non-negative fixnum")))

(defun check-one-fixnum (x)
  (if (not (fixnump x)) (error "not a fixnum - ~a" x)))

(defun check-one-real (a)
  (if (not (or (rationalp a) (floatp a))) (error "not a real number ~s" a)))

(defun check-one-number (a)
  (if (not (numberp a)) (error "not a number ~s" a)))

(defun check-sequence (a)
  (if (not (or (vectorp a) (consp a))) (error "not a sequence - ~s" a)))

(defun check-matrix (a)
  (if (not (and (arrayp a) (= (array-rank a) 2)))
      (error "not a matrix - ~s" a)))

(defun check-square-matrix (a)
  (check-matrix a)
  (let ((m (array-dimension a 0))
	(n (array-dimension a 1)))
    (if (/= n m) (error "not a square matrix - ~s" a))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;                       Sequence Element Access
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-next-element (x i)
  (let ((seq (first x)))
    (if (consp seq)
        (let ((elem (first seq)))
          (setf (first x) (rest seq))
          elem)
        (aref seq i))))

(defun set-next-element (x i v)
  (let ((seq (first x)))
    (cond ((consp seq)
           (setf (first seq) v)
           (setf (first x) (rest seq)))
          (t (setf (aref seq i) v)))))

(defun make-next-element (x) (list x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;          Array to Row-Major Data Vector Conversion Functions
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun array-data-vector (a)
"Args: (a)
Displaces array A to a vector"
  (make-array (array-total-size a)
	      :displaced-to a
	      :element-type (array-element-type a)))

(defun vector-to-array (v dims)
"Args: (v dims)
Displaces vector V to array with dimensions DIMS"
  (make-array dims
	      :displaced-to v
	      :element-type (array-element-type v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;                           Copying Functions
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; COPY-VECTOR function
;;;

(defun copy-vector (x)
"Args: (x)
Returns a copy of the vector X"
  (copy-seq x))

;;;
;;; COPY-ARRAY function
;;;

(defun copy-array (a)
"Args: (a)
Returns a copy of the array A"
  (vector-to-array (copy-seq (array-data-vector a))
		   (array-dimensions a)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;                         Sequence Functions
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; sequence predicate SEQUENCEP
;;;

(defun sequencep (x) 
"Args: (x)
Returns NIL unless X is a list or vector."
  (or (listp x) (vectorp x)))

;;;
;;; ISEQ - generate a sequence of consecutive integers from a to b
;;;

(defun iseq (a &optional b)
"Args: (n &optional m)
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

;;;;
;;;; WHICH function
;;;;

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

;;;;
;;;; REPEAT function
;;;;

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
			    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;               Subset Selection and Mutation Functions
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; is x an ordered sequence of nonnegative positive integers?
(defun ordered-nneg-seq(x)
  (if (sequencep x)
      (let ((n (length x))
            (cx (make-next-element x))
            (m 0))
        (dotimes (i n t)
          (let ((elem (check-nonneg-fixnum (get-next-element cx i))))
            (if (> m elem) (return nil) (setf m elem)))))))

;;;; select or set the subsequence corresponding to the specified indices
(defun sequence-select(x indices &optional (values nil set-values))
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

;;;;
;;;; SELECT function
;;;;

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
(defun set-select (x &rest args)
  (let ((indices (butlast args))
        (values (first (last args))))
    (cond
     ((sequencep x)
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;                    Array Permutation Functions
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun permute-indices (x y perm check) 
  "Args: (x y perm check).
permute x into y using perm; all should be vectors; If check is TRUE
the routine will check to make sure no indices are reused, but x
will be destroyed."
  (let ((rank (length x)))
    (declare (fixnum rank))
    (dotimes (i rank)
      (declare (fixnum i))
      (let ((k (aref perm i)))
        (if (not (fixnump k)) (error "bad permutation sequence - ~a" perm))
        (if (or (< k 0) (>= k rank))
	    (error "bad permutation sequence - ~a" perm))
        (setf (aref y i) (aref x k))
	;; to insure dimensions are not re-used
        (if check (setf (aref x k) NIL))))))

(defun indices-from-rowmajor (a k result)
  "Args: (a k result).
Compute indices in a from rowmajor index k, put in vector result."
  (declare (fixnum k))

  (if (not (arrayp a)) (error "not an array - ~a" a))
  (if (or (> 0 k) (>= k (array-total-size a))) (error "index out of range"))

  (let ((face 1)
        (rank (array-rank a))
        (dim (array-dimensions a)))
    (declare (fixnum face rank))
    (let ((cdim (make-next-element dim)))
      (dotimes (i rank)
        (declare (fixnum i))
        (setf face (* face (get-next-element cdim i)))))
    (let ((cdim (make-next-element dim)))
      (dotimes (i rank)
        (setf face (/ face (get-next-element cdim i)))
        (setf (aref result i) (floor (/ k face)))
        (setf k (rem k face))))))

(defun translate-index (i result x perm indices oldindices ilist)
  "Args: (i result x perm indices oldindices ilist).
Translate row major index in original array to row major index in new
array. Use indices vectors and ilist for temporary storage."
  (declare (fixnum i))
  (let ((rank (array-rank x)))
    (declare (fixnum rank))
    (indices-from-rowmajor x i oldindices)
    (permute-indices oldindices indices perm nil)
    (do ((next ilist (rest next))
         (k 0 (+ k 1)))
        ((not (and (< k rank) (consp next))))
      (setf (first next) (aref indices k)))
    (apply #'array-row-major-index result ilist)))

(defun permute-array (x perm)
  "Args: (a p)
Returns a copy of the array A permuted according to the permutation P."
  (if (not (arrayp x)) (error "not an array - ~a" x))
  (check-sequence perm)
  (if (/= (length perm) (array-rank x)) 
    (error "bad permutation sequence - ~a" perm))
  (let* ((perm (coerce perm 'vector))
         (rank (array-rank x))
         (dim (make-array rank))
         (olddim (coerce (array-dimensions x) 'vector)))
    (declare (fixnum rank))
    ;; construct new dimension vector
    (permute-indices olddim dim perm t)
    ;; make result array and the index vectors and lists */
    (let* ((result (make-array (coerce dim 'list)))
          (indices (make-array rank))
          (oldindices (make-array rank))
          (ilist (make-list rank))
          (data (compound-data-seq x))
          (result_data (compound-data-seq result))
          (n (length data)))
      (declare (fixnum n))
      (dotimes (i rank)
        (declare (fixnum i))
        (setf (aref oldindices i) (list nil)))
      ;; fill in the result
      (if (/= n (length result_data)) (error "bad data"))
      (dotimes (i n result)
        (declare (fixnum i))
        (let ((k (translate-index i result x perm indices oldindices ilist)))
          (declare (fixnum k))
          (setf (aref result_data k) (aref data i)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;              SUM, PROD, COUNT-ELEMENTS, and MEAN Functions
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sum-1 (x)
  (if (numberp x)
    x
    (let ((seq (compound-data-seq x))
          (sum 0))
      (if (consp seq)
        (dolist (x seq sum)
          (setf sum (+ sum (if (numberp x) x (sum-1 x)))))
        (let ((n (length seq)))
          (declare (fixnum n))
          (dotimes (i n sum)
            (declare (fixnum i))
            (let ((x (aref seq i)))
              (setf sum (+ sum (if (numberp x) x (sum-1 x)))))))))))

(defun sum (&rest args)
"Args: (&rest number-data)
Returns the sum of all the elements of its arguments. Returns 0 if there
are no arguments. Vector reducing."
  (if args
    (sum-1 (if (rest args) args (first args)))
    0))

(defun prod-1 (x)
  (if (numberp x)
    x
    (let ((seq (compound-data-seq x))
          (prod 1))
      (if (consp seq)
        (dolist (x seq prod)
          (setf prod (* prod (if (numberp x) x (prod-1 x)))))
        (let ((n (length seq)))
          (declare (fixnum n))
          (dotimes (i n prod)
            (declare (fixnum i))
            (let ((x (aref seq i)))
              (setf prod (* prod (if (numberp x) x (prod-1 x)))))))))))

(defun prod (&rest args)
"Args: (&rest number-data)
Returns the product of all the elements of its arguments. Returns 1 if there
are no arguments. Vector reducing."
  (if args
    (prod-1 (if (rest args) args (first args)))
    1))

(defun count-elements (x)
"Args: (number &rest more-numbers)
Returns the number of its arguments. Vector reducing"
  (if (compound-data-p x)
    (let ((seq (compound-data-seq x))
          (count 0))
      (if (consp seq)
        (dolist (x seq count)
          (incf count (if (compound-data-p x) (count-elements x) 1)))
        (let ((n (length seq)))
          (declare (fixnum n))
          (dotimes (i n count)
            (declare (fixnum i))
            (let ((x (aref seq i)))
              (incf count (if (compound-data-p x) (count-elements x) 1)))))))
    1))

(defun mean (x)
"Args: (x)
Returns the mean of the elements x. Vector reducing."
  (let ((mean 0.0)
        (count 0.0))
    (labels ((add-to-mean (x)
              (let ((count+1 (+ count 1.0)))
                (setf mean (+ (* (/ count count+1) mean) (* (/ count+1) x)))
                (setf count count+1)))
             (find-mean (x)
               (if (numberp x)
                 (add-to-mean x)
                 (let ((seq (compound-data-seq x)))
                   (if (consp seq)
                     (dolist (x seq)
                       (if (numberp x) (add-to-mean x) (find-mean x)))
                     (let ((n (length seq)))
                       (dotimes (i n)
		         (declare (fixnum i))
                         (let ((x (aref seq i)))
                           (if (numberp x)
			       (add-to-mean x)
			       (find-mean x))))))))))
      (find-mean x)
      mean)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;                    IF-ELSE and SAMPLE Functions
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun if-else (a x y)
"Args: (first x y)
Takes simple or compound data items FIRST, X and Y and returns result of
elementswise selecting from X if FIRST is not NIL and from Y otherwise."
  (flet ((base-if-else (a x y) (if a x y)))
    (recursive-map-elements #'base-if-else #'if-else a x y)))

(defun sample (x ssize &optional replace)
"Args: (x n &optional (replace nil))
Returns a list of a random sample of size N from sequence X drawn with or
without replacement."
  (check-sequence x)
  (let ((n (length x))
	(x (if (consp x) (coerce x 'vector) (copy-vector x)))
	(result nil))
    (if (< 0 n)
	(dotimes (i ssize result)
		 (let ((j (if replace (random n) (+ i (random (- n i))))))
		   (setf result (cons (aref x j) result))
		   (unless replace     ;; swap elements i and j
			   (let ((temp (aref x i)))
			     (setf (aref x i) (aref x j))
			     (setf (aref x j) temp))))))))

