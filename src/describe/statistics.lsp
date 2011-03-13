;;; -*- mode: lisp -*-
;;; Copyright (c) 2005--2009, by A.J. Rossini <blindglobe@gmail.com>
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
  (:method ((x list))
    (/ (reduce #'+ x)
       (length x)))
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


;; We do the variance, since the STANDARD-DEVIATION is simply the root
;; (for interesting definitions of "simply"), and VARIANCE is a bit
;; more general.
(defgeneric variance (x)
  (:documentation "compute the variance of the entity X, i.e. if a
  scalar, vector, or (covariance) matrix.")
  (:method ((x list))
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
    (error "FIXME: define variance for matrices as covariance (?).")))

(defun covariance-matrix (&rest args)
"Args: (&rest args)
Returns the sample covariance matrix of the data columns in ARGS. ARGS may
consist of lists, vectors or matrices."
  (let ((columns (apply #'append 
                        (mapcar #'(lambda (x) 
				    (if (typep x 'matrix-like)
					(list-of-columns x)
					(list x)))
                                args))))
    (/ (cross-product (reduce #'bind2 
			      (- columns (mapcar #'mean columns))))
       (- (length (car columns)) 1))))

(defgeneric standard-deviation (x)
  (:documentation "Compute standard deivation of entity X.")
  (:method ((x vector-like)) (sqrt (variance x)))
  (:method ((x list))
    ;; if elements are not  numeric, error, otherwise
    (sqrt (variance x)))
  (:method ((x matrix-like))
    (error "FIXME: define SD for matrix-like objects")))

(defun standard-deviation-fn (x)
  "Function front-end to generic.  Do we really need this?"
  (standard-deviation x))

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

(defgeneric quantile (x p)
  (:documentation "Returns the P-th quantile(s) of sequence X as a
  native lisp list (which could be fed into a vector-like or similar
  for the final format).  

  NOT OPTIMIZED.  This approach uses the native Common Lisp sort,
  which is destructive.  We can optimize space usage by using some
  form of rank computations instead.")
  (:method ((x sequence) (p real))
    (let ((np (* p (- (length x) 1)))
	  (x-copy x))
      (mean (list (aref x-copy (floor np))
		  (aref x-copy (ceiling np)))))) ;; aref work in general for lists too, or use nth?
  (:method ((x vector-like) (p real))  ;; average of sorted elements.  Could store compile
    (let ((np (* p (- (nelts x) 1)))
	  (x-copy x))
      (mean (list (vref (sort x-copy #'<) (floor np))
		  (vref (sort x-copy #'<) (ceiling np))))))
  (:method ((x sequence) (p sequence))
    (error "FIXME: generalize.   Basically, do mapcar or similar across a vector."))
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
    (sample-fn x n replace))
  (:method ((x vector-like) (n fixnum)
	    &optional replace weights)
    (error "SAMPLE for vector-likes not implemented yet.")))

(defun sample-fn (x ssize &optional replace)
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



