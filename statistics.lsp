;;; -*- mode: lisp -*-
;;; Copyright (c) 2005--2007, by A.J. Rossini <blindglobe@gmail.com>
;;; See COPYRIGHT file for any additional restrictions (BSD license).
;;; Since 1991, ANSI was finally finished.  Edited for ANSI Common Lisp.

;;; XLisp-ism's removed to focus on Common Lisp.  Original source from:
;;;; statistics.lsp XLISP-STAT statistics functions
;;;; XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney
;;;; Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz
;;;; You may give out copies of this software; for conditions see the file
;;;; COPYING included with this distribution.

(in-package :cl-user)

(defpackage :lisp-stat-descriptive-statistics
 (:use :common-lisp
       :lisp-stat-data
       :lisp-stat-math
       :lisp-stat-compound-data
       :lisp-stat-matrix
       :lisp-stat-linalg-data
       :lisp-stat-linalg
       :lisp-stat-basics)
   (:shadowing-import-from :lisp-stat-math
      expt + - * / ** mod rem abs 1+ 1- log exp sqrt sin cos tan
      asin acos atan sinh cosh tanh asinh acosh atanh float random
      truncate floor ceiling round minusp zerop plusp evenp oddp 
      < <= = /= >= > complex conjugate realpart imagpart phase
      min max logand logior logxor lognot ffloor fceiling
      ftruncate fround signum cis)
   (:export standard-deviation quantile median interquartile-range
	    fivnum sample))

(in-package :lisp-stat-descriptive-statistics)

;;;
;;; Basic Summary Statistics
;;;

(defun standard-deviation (x)
"Args: (x)
Returns the standard deviation of the elements x. Vector reducing."
  (let ((n (count-elements x))
        (r (- x (mean x))))
    (sqrt (* (mean (* r r)) (/ n (- n 1))))))


;; FIXME the following assume that we are using the vector based functions
(defun quantile (x p)
"Args: (x p)
Returns the P-th quantile(s) of sequence X. P can be a number or a sequence."
  (let* ((x (sort-data x))
         (n (length x))
         (np (* p (- n 1)))
         (low (floor np))
         (high (ceiling np)))
    (/ (+ (select x low) (select x high)) 2)))
    
(defun median (x) 
"Args: (x)
Returns the median of the elements of X."
  (quantile x 0.5))

(defun interquartile-range (x) 
"Args: (number-data)
Returns the interquartile range of the elements of X."
  (reduce #'- (quantile x '(0.75 0.25))))

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
				    (if (matrixp x) (column-list x) (list x)))
                                args))))
    (/ (cross-product (apply #'bind-columns 
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

