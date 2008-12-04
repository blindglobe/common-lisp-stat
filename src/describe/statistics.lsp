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
    (let ((n (nelts x))
	  (type (if (column-vector-p x) :column :row)))
      (/ (gemm x (ones
		  (ecase type (:row 1) (:column n))
		  (ecase type (:row n) (:column 1))))
	 n))))

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


;; We do the variance, since the SD is simply the root.
(defgeneric variance (x)
  (:documentation "compute the variance of the entity X, i.e. if a
  scalar, vector, or matrix.")
  (:method ((x list))
    (let ((n (length x))
	  (r (- x (mean x))))
      (sqrt (* (mean (* r r)) (/ n (- n 1))))))
  (:method ((x vector-like))))

(defgeneric standard-deviation (x)
  (:documentation "Compute standard deivation of entity X.")
  (:method ((x vector-like)) (sqrt (variance x)))
  (:method ((x list))
    ;; if elements are not  numeric, error, otherwise
    (sqrt (variance x)))
  (:method ((x matrix-like))
    (error "FIXME: define SD for matrix-like objects")))

(defun standard-deviation-fn (x)
"Args: (x)
Returns the standard deviation of the elements x. Vector reducing.

FIXME AJR: This should be redone as square-root of the variance, and
defer to that structure for computation."
  (let ((n (count-elements x))
        (r (- x (mean x))))
    (sqrt (* (mean (* r r)) (/ n (- n 1))))))

(defgeneric quantile (x p)
  (:documentation "Returns the P-th quantile(s) of sequence X.")
  (:method ((x sequence) (p sequence))
    (let* ((x (sort-data x))
	   (n (length x))
	   (np (* p (- n 1)))
	   (low (floor np))
	   (high (ceiling np)))
      (/ (+ (select x low) (select x high)) 2)))
  (:method ((x sequence) (p real))
    (error "FIXME: implement.")))


;;; things to build on top of quantiles...!

;; Args: (x)
;; Returns the median of the elements of X.
(defmacro median (x) 
  `(quantile ,x 0.5))
;; (macroexpand '(median (list 1 2 3)))
;; (median (list 1 2 3))


;; Args: (number-data) 
;; Returns the interquartile range of the elements of X.
(defmacro interquartile-range (x) 
  `(reduce #'- (quantile ,x '(0.75 0.25))))

(defun fivnum (x) 
"Args: (number-data)
Returns the five number summary (min, 1st quartile, medinan, 3rd quartile,
max) of the elements X."
  (quantile x '(0 .25 .5 .75 1)))

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
    (/ (cross-product (reduce #'bind-columns 
                             (- columns (mapcar #'mean columns))))
       (- (length (car columns)) 1))))

;;;; Sampling / Resampling

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



