;;; -*- mode: lisp -*-

;;; Copyright (c) 2005--2012, by A.J. Rossini <blindglobe@gmail.com>
;;; Copyright (c) 2012, by David Hodge <davidbhodge@gmail.com>

;;; See COPYRIGHT file for any additional restrictions (BSD license).
;;; Since 1991, ANSI was finally finished.  Edited for ANSI Common Lisp.

;;; XLisp-ism's removed to focus on Common Lisp.  Original source from:
;;;; statistics.lsp XLISP-STAT statistics functions
;;;; XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney
;;;; Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz
;;;; You may give out copies of this software; for conditions see the file
;;;; COPYING included with this distribution.

(in-package :lisp-stat-descriptive-statistics)

;;;
;;; Basic Summary Statistics
;;;

(defgeneric mean (x)
  (:documentation "compute the mean of lists, vectors, various objects")
  (:method ((x list) )
    (/ (reduce #'+ x)
       (length x)))
  (:method ((x sequence))
   (/ (reduce #'+ x)
       (length x)) )
  (:method ((x vector-like))
      ;; (defparameter *x* (make-vector 5 :initial-contents '((1d0 2d0 3d0 4d0 5d0))))
    (/ (loop for i from 0 to (- (nelts x) 1)
	  summing (vref x i))
       (nelts x)))
#| ;; a more traditional versino of the above... 
  (:method ((x vector-like))
    (let ((n (nelts x))
	  (type (if (col-vector-p x) :column :row)))
      (/ (gemm x (ones
		  (ecase type (:row 1) (:column n))
		  (ecase type (:row n) (:column 1))))
	 n)))
|#
  )

(defun mean-fn (x)
"Args: (x)
Returns the mean of the elements x. Vector reducing.

FIXME: Why is this so complex?  When figure out, put into generic."
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




(defun sum-vector (v)
  (loop for i from 0 below (nelts v)
	summing (vref v i)))

(defun reduce-vector (function v)
  "reduce a vector with function yielding a scalar"
  (loop with result = (vref v 0)
	for i from 1 below (nelts v) do
	  (setf result (funcall function result (vref v i)))
	finally (return result)))

(defun reduce-matrix (function m &key (dim :row))
  "reduce a matrix yielding a vector"
  (flet ((reduce-dimension (m row-or-column number-elts)
	   (loop with result = (make-matrix 1 number-elts)
	   for i from 0 below number-elts
		 do (setf (vref result i)
			  (reduce-vector function (funcall row-or-column m i)))
	   finally (return result))))
    
    (cond
      ((eq dim :row)
       (reduce-dimension m #'row (nrows m)))
      ((eq dim :col)
       (reduce-dimension m #'col (ncols m)))
      (t (error "reduce-matrix: invalid dimension specified only row or column supported")))))


(defun map-vector (function v)
  (loop with result = (make-matrix 1 (nelts v))
	for i from 0 below (nelts v)
	do (setf (vref result i) (funcall function (vref v i)))
	finally (return result)))

(defun map-matrix (function m)
  (loop with result = (apply  #'make-matrix (matrix-dimensions m))
	for i from 0 below (nrows m) do
	  (loop for j from 0 below (ncols m) do
	    (setf (mref result i j) (funcall function (mref m i j))))
	finally (return result)))

(defun matrix-mean (m)
  "the mean of the columns of a matrix, returned as a vector"
  (map-vector #'(lambda (x) (/ x (nrows m)))
	      (reduce-matrix #'+ m :dim :col)))


(defun matrix-sd (m)
  "standard deviation of the matrix"
  (sqrt (variance (data m))))

(defun matrix-covariance (m)
  "This is actually quite nice. Just not sure if its correct. too much consing though"
  (let* ((rows  (nrows m))
	 (deviation-scores (m- m
			       (m* (ones rows rows) (map-matrix #'(lambda (x) (/ x rows)) m )))))
    (m* (transpose-matrix deviation-scores)
	(map-matrix #'(lambda (x) (/ x rows)) deviation-scores))))

;; We do the variance, since the STANDARD-DEVIATION is simply the root
;; (for interesting definitions of "simply"), and VARIANCE is a bit
;; more general.
(defgeneric variance (x)
  (:documentation "compute the variance of the entity X, i.e. if a
  scalar, vector, or (covariance) matrix.")
  (:method ((x sequence))
    (let ((n (length x))
	  (r (- x (mean x))))
      (sqrt (* (mean (* r r)) (/ n (- n 1))))))
  (:method ((x vector-like))
    ;; FIXME!!!
    (let* ((negone nil) ;; FIXME!
	   (negresid  (axpy (mean x) negone x))) ; -mu + x = x - mu
      (/ (loop for i from 0 to (- (nelts x) 1)
	    summing (* (vref negresid i)
		       (vref negresid i)))
	 (- (nelts negresid) 1))))
  (:method ((x matrix-like))
    (matrix-covariance x)))



(defgeneric standard-deviation (x)
  (:documentation "Compute standard deivation of entity X.")
  (:method ((x vector-like)) (sqrt (variance x)))
  (:method ((x list))
    ;; if elements are not  numeric, error, otherwise
    (sqrt (variance x)))
  (:method ((x matrix-like))
    (matrix-sd x)))

(defun standard-deviation-old (x)
"Args: (x)
Returns the standard deviation of the elements x. Vector reducing.

FIXME AJR: This should be redone as square-root of the variance, and
defer to that structure for computation.  The justification for this
'redo' is that it holds for many generalized variance structures such
as variance-covariance matrices."
  (let ((n (count-elements x))
        (r (- x (mean x))))
    (sqrt (* (mean (* r r)) (/ n (- n 1))))))

(defun calculate-quantile (x p)
  "a helper function for quantile"
  (let ((np (* p (1- (length x))))
	(x-copy x))
    (mean (list (elt (sort  x-copy #'<) (floor np))
		(elt (sort  x-copy #'<) (ceiling np))))));; aref does not work for lists.

(defgeneric quantile (x p)
  (:documentation "Returns the P-th quantile(s) of sequence X as a
  native lisp list (which could be fed into a vector-like or similar
  for the final format).  

  NOT OPTIMIZED.  This approach uses the native Common Lisp sort,
  which is destructive.  We can optimize space usage by using some
  form of rank computations instead.")
  (:method ((x sequence) (p real))
    (calculate-quantile x p)) ;; dbh - dry.
  (:method ((x vector-like) (p real))  ;; average of sorted elements.  Could store compile
    (let ((np (* p (- (nelts x) 1)))
	  (x-copy x))
      (mean (list (vref (sort x-copy #'<) (floor np))
		  (vref (sort x-copy #'<) (ceiling np))))))
  (:method ((x sequence) (p sequence))
    (loop for the-p in p collect (calculate-quantile (coerce  x 'vector) the-p)) )
  (:method ((x vector-like) (p sequence))
    (error "FIXME: generalize."))
  (:method ((x vector-like) (p vector-like))
    (error "FIXME: generalize.")))

;;; things to build on top of quantile.
;;
;; Do as generics if possible, keeping in mind that quantile returns a
;; raw lisp list.  Nearly all of these use native lisp structures for speed,
;; such as raw lists for single and vector values, and arrays
;; (simple?) for matrix-values  and array-valued results.

(defun median (x) 
  "Return median of X, using whatever the quantile generic function supports."
  (quantile x 0.5))

(defun interquartile-range (x) 
  "Returns the interquartile range of the elements of X."
  (reduce #'- (quantile x (list 0.75 0.25))))

(defun fivnum (x) 
  "Returns the five number summary (min, 1st quartile, medinan, 3rd quartile,
max) of the elements X."
  (quantile x (list 0 .25 .5 .75 1)))

;;;; Sampling / Resampling

(defgeneric sample (x n &optional replace weights)
  (:documentation "Draw a sample of size n from sequence x, with/out
  replacement, with weights per element of x as described as a
  probability mass distribution vector.  Length of weights should
  match length of X.  Unclear if (= X (uniq X))? (possible issue).
  Return value shoud match the input values.")
  (:method ((x sequence) (n fixnum)
	    &optional replace weights)
    (declare (ignorable weights))
    (sample-fn x n replace weights))
  (:method ((x vector-like) (n fixnum)
	    &optional replace weights)
    (declare (ignorable weights replace))
    (error "SAMPLE for vector-likes not implemented yet.")))

(defun sample-fn (x ssize &optional replace weights)
  "Args: (x n &optional (replace nil))
Returns a list of a random sample of size N from sequence X drawn with or
without replacement.

IMPROVE: need to implement sampling with weights."
  (declare (ignorable weights)) ;; need to implement
  (check-sequence x)
  (let ((n (length x))
	(x (if (consp x) (coerce x 'vector) (copy-seq x)))
	(result nil))
    (if (< 0 n)
	(dotimes (i ssize result)
		 (let ((j (if replace (random n) (+ i (random (- n i))))))
		   (setf result (cons (aref x j) result))
		   (unless replace     ;; swap elements i and j
			   (let ((temp (aref x i)))
			     (setf (aref x i) (aref x j))
			     (setf (aref x j) temp))))))))



