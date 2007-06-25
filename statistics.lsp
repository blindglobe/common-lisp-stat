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
       :lisp-stat-data)
 (:export ;; descriptive stats
          standard-deviation quantile median interquartile-range
	  fivnum

	  sample 

	  ;; the following are more matrix-centric
	  covariance-matrix matrix print-matrix solve
	  backsolve eigenvalues eigenvectors accumulate cumsum combine
	  lowess))

(in-package :lisp-stat-descriptive-statistics)

;;;;
;;;; Basic Summary Statistics
;;;;

(defun standard-deviation (x)
"Args: (x)
Returns the standard deviation of the elements x. Vector reducing."
  (let ((n (count-elements x))
        (r (- x (mean x))))
    (sqrt (* (mean (* r r)) (/ n (- n 1))))))

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
  (apply #'- (quantile x '(0.75 0.25))))

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

;;;;
;;;; Linear Algebra Functions
;;;;

(defun matrix (dim data)
"Args: (dim data)
returns a matrix of dimensions DIM initialized using sequence DATA
in row major order." 
  (let ((dim (coerce dim 'list))
        (data (coerce data 'list)))
    (make-array dim :initial-contents (split-list data (nth 1 dim)))))

(defun print-matrix (a &optional (stream *standard-output*))
"Args: (matrix &optional stream)
Prints MATRIX to STREAM in a nice form that is still machine readable"
  (unless (matrixp a) (error "not a matrix - ~a" a))
  (let ((size (min 15 (max (map-elements #'flatsize a)))))
    (format stream "#2a(~%")
    (dolist (x (row-list a))
            (format stream "    (")
            (let ((n (length x)))
              (dotimes (i n)
                       (let ((y (aref x i)))
                         (cond
                           ((integerp y) (format stream "~vd" size y))
                           ((floatp y) (format stream "~vg" size y))
                           (t (format stream "~va" size y))))
                       (if (< i (- n 1)) (format stream " "))))
            (format stream ")~%"))
    (format stream "   )~%")
    nil))

(defun solve (a b)
"Args: (a b)
Solves A x = B using LU decomposition and backsolving. B can be a sequence
or a matrix."
  (let ((lu (lu-decomp a)))
    (if (matrixp b)
        (apply #'bind-columns 
               (mapcar #'(lambda (x) (lu-solve lu x)) (column-list b)))
        (lu-solve lu b))))
        
(defun backsolve (a b)
"Args: (a b)
Solves A x = B by backsolving, assuming A is upper triangular. B must be a
sequence. For use with qr-decomp."
  (let* ((n (length b))
         (sol (make-array n)))
    (dotimes (i n)
             (let* ((k (- n i 1))
                    (val (elt b k)))
               (dotimes (j i)
                        (let ((l (- n j 1)))
                          (setq val (- val (* (aref sol l) (aref a k l))))))
               (setf (aref sol k) (/ val (aref a k k)))))
    (if (listp b) (coerce sol 'list) sol)))

(defun eigenvalues (a) 
"Args: (a)
Returns list of eigenvalues of square, symmetric matrix A"
  (first (eigen a)))

(defun eigenvectors (a) 
"Args: (a)
Returns list of eigenvectors of square, symmetric matrix A"
  (second (eigen a)))

(defun accumulate (f s)
"Args: (f s)
Accumulates elements of sequence S using binary function F.
(accumulate #'+ x) returns the cumulative sum of x."
  (let* ((result (list (elt s 0)))
         (tail result))
    (flet ((acc (dummy x)
                (rplacd tail (list (funcall f (first tail) x)))
                (setf tail (cdr tail))))
      (reduce #'acc s))
    (if (vectorp s) (coerce result 'vector) result)))

(defun cumsum (x)
  "Args: (x)
Returns the cumulative sum of X."
  (accumulate #'+ x))

(defun combine (&rest args) 
  "Args (&rest args) 
Returns sequence of elements of all arguments."
  (copy-seq (element-seq args)))

(defun lowess (x y &key (f .25) (steps 2) (delta -1) sorted)
"Args: (x y &key (f .25) (steps 2) delta sorted)
Returns (list X YS) with YS the LOWESS fit. F is the fraction of data used for
each point, STEPS is the number of robust iterations. Fits for points within
DELTA of each other are interpolated linearly. If the X values setting SORTED
to T speeds up the computation."
  (let ((x (if sorted x (sort-data x)))
        (y (if sorted y (select y (order x))))
        (delta (if (> delta 0.0) delta (/ (- (max x) (min x)) 50))))
    (list x)));; (|base-lowess| x y f steps delta))))
