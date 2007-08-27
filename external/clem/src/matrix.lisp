;;; matrix.lisp
;;;
;;; Copyright (c) 2004-2006 Cyrus Harmon (ch-lisp@bobobeach.com)
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

(in-package :clem)


;;; matrix protocol

(defgeneric dim (m)
  (:documentation "Returns a list containg the number of
  elments in each dimension of the matrix."))

(defgeneric rows (m)
  (:documentation "Returns the number of rows in the matrix."))

(defgeneric cols (m)
  (:documentation "Returns the number of columns in the matrix."))

(defgeneric val (m i j)
  (:documentation "Returns the value of the element in the ith row of
  the jth column of the matrix m."))

;;; This is totally bogys. Why do we have both mref and val? I'm
;;; assuming that val should go away!
(defgeneric mref (m &rest indices)
  (:documentation "Returns the value of the element in the ith row of
  the jth column of the matrix m."))

(defgeneric (setf mref) (v m &rest indices)
  (:documentation "Set the value of the specified element at row row
  and col col of matrix m to be v."))

(defgeneric row-major-mref (m index))

(defgeneric (setf row-major-mref) (v m index))

(defgeneric move-element (m i1 j1 n i2 j2)
  (:documentation "Copy the contents of the element at row i1, column
  j1, in matrix m to the element at row i2, column j2, in matrix n."))

(defgeneric set-val (m i j v &key coerce)
  (:documentation "Sets the value of the element at row i, column j of
  matrix m to v."))

(defgeneric set-val (m i j v &key coerce))

;;;; level-1 interface

;;; arithmetic functions

(defgeneric m+ (&rest matrices)
  (:documentation "Element-wise addition of matrices. All matrices
  must be of the same dimensions. Returns a matrix of the same size as
  each matrix in matrices, whoses elements are the some of the
  corresponding elements from each matrix in matrices."))

(defgeneric m- (&rest matrices)
  (:documentation "When passed a single matrix, returns a new
  matrix of the same dimensions as matrix whose elemnts are the
  result of perforing a unary minus (or sign inversion) on each
  element in the matrix. When passed more than one matrix,
  performs element-wise subtraction of each matrix after the
  first from the first matrix and returns a new matrix containing
  these values."))

(defgeneric m* (&rest matrices)
  (:documentation "General purpose matrix multiplication
  operator. If one argument is supplied, returns that matrix. If
  two arguments are supplied, if both are matrices, performs a
  matrix multiplication equivalent to multiplying the first
  matrix by the second matrix. if one argument is a matrix and
  the other a number, m* scales the matrix by the numeric
  argument. If more than two arguments are supplied, the first
  two arguments are treated as in the two argument case and the
  results are then multiplied by the remaining arguments such
  that (m* m1 m2 m3) == (m* (m* m1 m2) m3)."))

(defgeneric m.* (&rest matrices)
  (:documentation "Hadamard multiplication operator. Performs an
  element-wise multiplication of the elements in each matrix."))

;;; logical functions

(defgeneric mlognot (m &key in-place)
  (:documentation "Performs element-wise logical negation of the
  matrix m. If in-place is nil, returns a new matrix with the
  resulting values, otherwise, destructively modifies matrix
  m."))

(defgeneric mlognot-range (m startr endr startc endc &key in-place))

(defgeneric mlogand-range (m1 m2 startr endr startc endc &key in-place))
(defgeneric mlogand (m1 m2 &key in-place))

(defgeneric mlogior-range (m1 m2 startr endr startc endc &key in-place))
(defgeneric mlogior (m1 m2 &key in-place))

(defgeneric mlogxor-range (m1 m2 startr endr startc endc &key in-place))
(defgeneric mlogxor (m1 m2 &key in-place))

;;; FIXME!! fix bitnor
(defgeneric mbitnor-range (m1 m2 startr endr startc endc))
(defgeneric mbitnor (m1 m2))

;;; extrema (min and max)

(defgeneric min-range (m startr endr startc endc))
(defgeneric max-range (m startr endr startc endc))
(defgeneric sum-range (m startr endr startc endc))
(defgeneric min-val (m))
(defgeneric max-val (m))

;;; exponential functions

(defgeneric mat-square (u))
(defgeneric mat-square! (u))
(defgeneric mat-sqrt (u))
(defgeneric mat-sqrt! (u))

;;; sum

(defgeneric sum (m))
(defgeneric sum-cols (m &key matrix-class))
(defgeneric sum-rows (m &key matrix-class))
(defgeneric sum-square-range (m startr endr startc endc))
(defgeneric sum-square (m))

;;; statistics

(defgeneric mean-range (m startr endr startc endc))
(defgeneric mean (m))
(defgeneric variance-range (m startr endr startc endc))
(defgeneric variance (m))
(defgeneric sample-variance-range (m startr endr startc endc))
(defgeneric sample-variance (m))


;;; normalization

(defgeneric normalize (u &key normin normax copy))
(defgeneric norm-0-255 (u &key copy))
(defgeneric norm-0-1 (u &key copy))

;;;; level-0 interface

(defgeneric matrix-move-range-2d (m n startr1 endr1 startc1 endc1
                                 startr2 endr2 startc2 endc2))

(defgeneric matrix-move-range-2d-constrain
    (m n startr1 endr1 startc1 endc1
       startr2 endr2 startc2 endc2))

(defgeneric matrix-move (m n &key constrain))

(defgeneric mat-mult-3-block (m n p))


;;; log generic functions

(defgeneric mlog-range (m startr endr startc endc &optional base))

(defgeneric mlog-range! (m startr endr startc endc &optional base))

(defgeneric mlog (m &optional base))

(defgeneric mlog! (m &optional base))

;;; hadamard product

(defgeneric mat-hprod-range (m n startr endr startc endc))

(defgeneric mat-hprod (m n))

(defgeneric mat-hprod-range! (m n startr endr startc endc))

(defgeneric mat-hprod! (m n))

;;; add

(defgeneric mat-add (a b &key in-place))
(defgeneric mat-add-range (m n startr endr startc endc &key in-place))

;;; abs

(defgeneric mabs (u))

(defgeneric mabs-range (m startr endr startc endc))

(defgeneric mabs-range! (m startr endr startc endc))

;;; transformation

(defgeneric %transfrom-matrix (m n xfrm &key background interpolation))

;;; discrete convolution

(defgeneric %discrete-convolve (u v z &key norm-v))

(defgeneric %separable-discrete-convolve (m h1 h2 z1 z2 &key truncate norm-v matrix-class))

;;;;;

;;; Miscellaneous class utilities
    
#+openmcl
(defun compute-class-precedence-list (class)
  (ccl:class-precedence-list class))

(defun subclassp (c1 c2)
  (subtypep (class-name c1) (class-name c2)))

(defgeneric matrix-precedence-list (c)
  (:method ((c class))
    (let ((mpl (compute-class-precedence-list c))
	  (mc (find-class 'matrix)))
      (mapcan #'(lambda (x)
		  (if (subclassp x mc) (when x (list x))))
              mpl)
      )))

(defgeneric closest-common-matrix-class (m1 &rest mr)
  (:method ((m1 matrix) &rest mr)
    (car (apply #'ch-util:closest-common-ancestor
                (mapcar #'(lambda (x) (matrix-precedence-list (class-of x)))
                        (cons m1 mr))))))

;;; gory implementation details follow

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; coerce/fit

(defgeneric fit (m val))
(defmethod fit ((m matrix) val)
  (declare (ignore m))
  val)

(defgeneric fit-value (val m))
(defmethod fit-value (val (m matrix))
  (declare (ignore m))
  val)

(defmethod dim ((m matrix)) (matrix-dimensions m))

(defmethod rows ((m matrix))
  (let ((vals (matrix-vals m)))
    (if (and vals
             (> (length (array-dimensions vals)) 0))
        (array-dimension vals 0)
        1)))

(defmethod cols ((m matrix))
  (let ((vals (matrix-vals m)))
    (if (and vals
             (> (length (array-dimensions vals)) 1))
        (array-dimension vals 1)
        1)))

(declaim (inline set-val))
(defmethod val ((m matrix) i j) (aref (matrix-vals m) i j))


;;; rvref treats the matrix as a row-vector (that is a
;;; 1 x n matrix). we should throw an error if this is not the case.
;;; TODO: add rvref methods for row-vector, column-vector, and scalar
;;; classes, erroring as appropriate.
(defgeneric rvref (rv i))
(defmethod rvref ((rv matrix) i) (aref (matrix-vals rv) 1 i))

;;; cvref treats the matrix as a row-vector (that is an
;;; n x 1 matrix). we should throw an error if this is not the case.
;;; TODO: add cvref methods for row-vector, column-vector, and scalar
;;; classes, erroring as appropriate.
(defgeneric cvref (cv i))
(defmethod cvref ((cv matrix) i) (aref (matrix-vals cv) i 1))


(defmethod move-element ((m matrix) i1 j1 (n matrix) i2 j2)
  (setf (mref n i2 j2) (mref m i1 j1)))

(defmethod set-val ((m matrix) i j v &key (coerce t))
  (setf (aref (matrix-vals m) i j)
	(if coerce
	    (coerce v (element-type (class-of m)))
	    v)))

(defmethod set-val-fit ((m matrix) i j v &key (truncate nil))
  (setf (mref m i j) (if truncate (truncate v) v)))


(defmethod transpose ((m matrix))
  (destructuring-bind (rows cols) (dim m)
    (declare (type fixnum rows cols))
    (let ((tr (make-instance (class-of m) :rows cols :cols rows)))
      (dotimes (i rows)
        (declare (type fixnum i))
	(dotimes (j cols)
          (declare (type fixnum j))
	  (set-val tr j i (val m i j))))
      tr)))

(defmethod transpose ((m double-float-matrix))
  (destructuring-bind (rows cols) (dim m)
    (declare (type fixnum rows cols))
    (let ((tr (make-instance 'double-float-matrix :rows cols :cols rows)))
      (with-typed-mref (m double-float)
        (with-typed-mref (tr double-float)
          (dotimes (i rows)
            (dotimes (j cols)
              (setf (mref tr j i) (mref m i j))))))
      tr)))

(defgeneric mat-mult (a b))
(defmethod mat-mult ((a matrix) (b matrix))
  (destructuring-bind (ar ac) (dim a)
    (destructuring-bind (br bc) (dim b)
      (if (= ac br)
          (let* ((c (make-instance (class-of a) :rows ar :cols bc)))
            (dotimes (i ar)
              (dotimes (j bc)
                (let ((v 0))
                  (dotimes (r ac)
                    (incf v (* (val a i r) (val b r j))))
                  (set-val c i j v))))
            c)
          (error 'matrix-argument-error
                 :format-control
                 "Incompatible matrix dimensions in mat-mult (~S x ~S) * (~S x ~S)."
                 :format-arguments (list ar ac br bc))))))

(declaim (inline inferior))
(defun inferior (a b)
  (if (< a b) a b))

(declaim (inline superior))
(defun superior (a b)
  (if (> a b) a b))

(declaim (inline constrain))
(defun constrain (min val max)
  (if (and min max)
    (inferior (superior min val) max)
    val))

;;; FIXME - figure out what to do about truncate
(defgeneric mat-copy-into (a c &key truncate constrain))
(defmethod mat-copy-into ((a matrix) (c matrix) &key truncate constrain)
  (declare (ignore truncate constrain))
  (destructuring-bind (m n) (dim a)
    (dotimes (i m)
      (declare (type fixnum i))
      (dotimes (j n)
        (declare (type fixnum j))
        (move-element a i j c i j)))
    c))
  
(defgeneric mat-copy-proto-dim (a m n &key initial-element))
(defmethod mat-copy-proto-dim ((a matrix) (m fixnum) (n fixnum)
                               &key (initial-element nil initial-element-supplied-p))
  (apply #'make-instance (class-of a) :rows m :cols n
         (when initial-element-supplied-p
           (list :initial-element initial-element))))
  

(defgeneric mat-copy-proto (a))
(defmethod mat-copy-proto ((a matrix))
  (destructuring-bind (m n) (dim a)
    (make-instance (class-of a) :rows m :cols n)))

(defgeneric mat-copy (a &rest args))
(defmethod mat-copy ((a matrix) &rest args)
  (let ((c (mat-copy-proto a)))
    (apply #'mat-copy-into a c args)
    c))

(defgeneric mat-scalar-op (a b op))
(defmethod mat-scalar-op ((a matrix) (b matrix) op)
  (and (equal (dim a) (dim b))
       (destructuring-bind (m n) (dim a)
	 (let ((c (mat-copy a)))
	   (dotimes (i m c)
	     (dotimes (j n)
	       (set-val c i j (funcall op (val a i j) (val b i j)))))))))

(defmethod mat-scalar-op ((a number) (b matrix) op)
  (destructuring-bind (m n) (dim b)
    (let ((c (mat-copy b)))
      (dotimes (i m c)
        (dotimes (j n)
          (set-val c i j (funcall op a (val b i j))))))))

(defmethod mat-scalar-op ((a matrix) (b number) op)
  (destructuring-bind (m n) (dim a)
    (let ((c (mat-copy a)))
      (dotimes (i m c)
        (dotimes (j n)
          (set-val c i j (funcall op (val a i j) b)))))))
	
(defgeneric mat-subtr (a b &key in-place result-type))

(defgeneric swap-rows (a k l))
(defmethod swap-rows ((a matrix) k l)
  (let* ((da (dim a))
	 (n (second da)))
    (dotimes (j n)
      (let ((h (val a k j)))
	(set-val a k j (val a l j))
	(set-val a l j h)))))

(defgeneric swap-cols (a k l))
(defmethod swap-cols ((a matrix) k l)
  (let* ((da (dim a))
	 (m (first da)))
    (dotimes (i m)
      (let ((h (val a i k)))
	(set-val a i k (val a i l))
	(set-val a i l h)))))

(defgeneric map-col (a k f))
(defmethod map-col ((a matrix) k f)
  (destructuring-bind (m n) (dim a)
    (declare (ignore n))
    (dotimes (i m)
      (set-val a i k (funcall f (val a i k))))))

(defgeneric map-row (a k f))
(defmethod map-row ((a matrix) k f)
  (destructuring-bind (m n) (dim a)
    (declare (ignore m))
    (dotimes (j n)
      (set-val a k j (funcall f (val a k j))))))

(macrolet ((frob (name op)
             `(progn
                ;;; define the row op
                (defgeneric ,(ch-util:make-intern (concatenate 'string "scalar-" name "-row"))
                    (a k q))
                (defmethod ,(ch-util:make-intern (concatenate 'string "scalar-" name "-row"))
                    ((a matrix) k q)
                  (map-row a k #'(lambda (x) (apply ,op (list x q))))
                  a)
                ;;; define the column op
                (defgeneric ,(ch-util:make-intern (concatenate 'string "scalar-" name "-col"))
                    (a k q))
                (defmethod ,(ch-util:make-intern (concatenate 'string "scalar-" name "-col"))
                    ((a matrix) k q)
                  (map-col a k #'(lambda (x) (apply ,op (list x q))))
                  a))))
  (frob "mult" #'*)
  (frob "divide" #'/)
  (frob "double-float-divide" #'/)
  (frob "single-float-divide" #'/))

(defgeneric scalar-mult (m q))
(defmethod scalar-mult ((m matrix) q)
  (dotimes (i (first (dim m)) m)
    (scalar-mult-row m i q))
  m)

(defgeneric scalar-mult-copy (m q))
(defmethod scalar-mult-copy ((a matrix) q)
  (let ((m (mat-copy a)))
    (scalar-mult m q)))

(defgeneric scalar-divide (a q))
(defmethod scalar-divide ((a matrix) q)
  (dotimes (i (first (dim a)) a)
    (scalar-divide-row a i q))
  a)

(defgeneric scalar-divide-copy (a q))
(defmethod scalar-divide-copy ((a matrix) q)
  (let ((m (mat-copy a)))
    (scalar-divide m q)))

(defgeneric zero-matrix (j k &key matrix-class))
(defmethod zero-matrix ((j fixnum) (k fixnum) &key (matrix-class 'matrix))
  (make-instance matrix-class :rows j :cols k))

(defgeneric identity-matrix (k &key matrix-class))
(defmethod identity-matrix ((k fixnum) &key (matrix-class 'matrix))
  (let* ((a (zero-matrix k k :matrix-class matrix-class))
         (one (coerce 1 (element-type (class-of a)))))
    (dotimes (i k a)
      (set-val a i i one))))

(defgeneric concat-matrix-cols (a b &key matrix-type))
(defmethod concat-matrix-cols ((a matrix) (b matrix) &key matrix-type)
  (let ((da (dim a)) (db (dim b)))
    (cond
      ((equal (first da) (first db))
       (let ((c (make-instance (if matrix-type
				   matrix-type
				   (class-of a)) :rows (first da) :cols (+ (second da) (second db)))))
	 (let  ((m (first da))
		(n (second da))
		(q (second db)))
	   (dotimes (i m)
	     (dotimes (j n)
	       (set-val c i j (val a i j) :coerce t)))
	   (dotimes (i m)
	     (dotimes (j q)
	       (set-val c i (+ j n) (val b i j) :coerce t))))
	 c))
      (t nil))))

(defgeneric subset-matrix-cols (a x y &key (matrix-type)))
(defmethod subset-matrix-cols ((a matrix) x y &key (matrix-type))
  (let ((da (dim a)))
    (cond
     ((< x y)
      (let* ((m (first da))
	     (n (- y x))
	     (c (make-instance (if matrix-type
				   matrix-type
				   (class-of a)) :rows (first da) :cols (- y x))))
	(dotimes (i m)
	  (dotimes (j n)
	    (set-val c i j (val a i (+ j x)))))
	c))
     (t nil))))

;;; this should be optimized via a type-specific method
;;; for each matrix class
(defgeneric get-first-non-zero-row-in-col (a j &optional start))
(defmethod get-first-non-zero-row-in-col ((a matrix) j &optional (start 0))
  (let ((n (first (dim a))))
    (do ((i start (+ i 1)))
	((or (= i n)
	     (not (= (val a i j) 0)))
	 (if (= i n)
	     nil
	   i)))))

;;; this should be optimized via a type-specific method
;;; for each matrix class
(defgeneric invert-matrix (a))
(defmethod invert-matrix ((a matrix))
  (let* ((n (second (dim a)))
	 (c (concat-matrix-cols a (identity-matrix n) :matrix-type 'double-float-matrix)))
    (do*
	((y 0 (+ y 1)))
	((or (= y n) (not c)))
      (let ((z (get-first-non-zero-row-in-col c y y)))
	(cond
	 ((not z) (setf c nil))
	 (t
	  (if (> z y)
	      (swap-rows c y z))
	  (scalar-divide-row c y (val c y y))
	  (do*
	      ((i 0 (+ i 1)))
	      ((= i n))
	    (unless (= i y)
	      (let ((k (val c i y)))
		(dotimes (j (* n 2))
		  (set-val c i j (+ (val c i j)
				    (* (- k) (val c y j))))))))))))
    (unless (not c)
      (subset-matrix-cols c n (+ n n) :matrix-type 'double-float-matrix))))

(defgeneric transpose-matrix (a))
(defmethod transpose-matrix ((a matrix))
  (let* ((da (dim a))
	 (m (first da))
	 (n (second da))
	 (c (make-instance (class-of a) :rows n :cols m)))
    (dotimes (i m)
      (dotimes (j n)
	(set-val c j i (val a i j))))
    c))

(defgeneric add-row (m &key values initial-element)
  (:method ((m matrix) &key values initial-element)
    (if (adjustable m)
	(progn
	  (if (null initial-element)
	      (setf initial-element (initial-element m)))
	  (let ((d (dim m)))
	    (setf (matrix-vals m)
		  (adjust-array (matrix-vals m) (list (+ 1 (first d)) (second d))
				:initial-element initial-element))
	    (if values
		(do
		 ((l values (cdr l))
		  (i 0 (+ i 1)))
		 ((not l))
		  (set-val m (first d) i (car l))))))
	(error 'matrix-error :message "Tried to add-row to non-adjustable matrix ~A" m))))

(defgeneric add-col (m &key values initial-element)
  (:method ((m matrix) &key values initial-element)
    (if (adjustable m)
	(progn
	  (if (null initial-element)
	      (setf initial-element (initial-element m)))
	  (let ((d (dim m)))
	    (setf (matrix-vals m)
		  (adjust-array (matrix-vals m) (list (first d) (+ 1 (second d)))
				:initial-element initial-element))
	    (if values
		(do
		 ((l values (cdr l))
		  (i 0 (+ i 1)))
		 ((not l))
		  (set-val m i (second d) (car l))))))
	(error 'matrix-error :message "Tried to add-col to non-adjustable matrix ~A" m))))

(defgeneric reshape (m rows cols &key initial-element)
  (:method ((m matrix) rows cols &key initial-element)
    (if (adjustable m)
	(progn
	  (if (null initial-element)
	      (setf initial-element (initial-element m)))
	  (setf (matrix-vals m)
		(adjust-array (matrix-vals m) (list rows cols)
			      :initial-element initial-element)))
	(error 'matrix-error :message "Tried to reshape non-adjustable matrix ~A" m))))

(defgeneric horzcat (m1 &rest mr)
  (:method ((m1 matrix) &rest mr)
    (let ((rows (apply #'max (mapcar #'rows (cons m1 mr))))
          (cols (apply #'+ (mapcar #'cols (cons m1 mr)))))
      (let ((z (make-instance (apply #'closest-common-matrix-class m1 mr)
                              :rows rows
                              :cols cols)))
        (let ((coff 0))
          (dolist (x (cons m1 mr))
            (map-set-range z 0 (- (rows x) 1) coff (+ coff (- (cols x) 1))
                           #'(lambda (v i j) (declare (ignore v)) (val x i (- j coff))))
            (incf coff (cols x))
            ))
        z))))

(defgeneric vertcat (m1 &rest mr)
  (:method ((m1 matrix) &rest mr)
    (let ((rows (apply #'+ (mapcar #'rows (cons m1 mr))))
          (cols (apply #'max (mapcar #'cols (cons m1 mr)))))
      (let ((z (make-instance (apply #'closest-common-matrix-class m1 mr)
                              :rows rows
                              :cols cols)))
        (let ((roff 0))
          (dolist (x (cons m1 mr))
            (map-set-range z roff (+ roff (- (rows x) 1)) 0 (- (cols x) 1)
                           #'(lambda (v i j) (declare (ignore v)) (val x (- i roff) j)))
            (incf roff (rows x))
            ))
        z))))

(defgeneric pad-matrix (m))
(defmethod pad-matrix ((m matrix))
  (cond
    ((> (rows m) (cols m))
     (let ((delta (- (rows m) (cols m))))
       (horzcat (make-instance (class-of m)
                               :rows (rows m)
                               :cols (ceiling (/ delta 2)))
                m
                (make-instance (class-of m)
                               :rows (rows m)
                               :cols (floor (/ delta 2))))))
    ((> (cols m) (rows m))
     (let ((delta (- (cols m) (rows m))))
       (vertcat (make-instance (class-of m)
                               :rows (ceiling (/ delta 2))
                               :cols (cols m))
                m
                (make-instance (class-of m)
                               :rows (floor (/ delta 2))
                               :cols (cols m)))))))

(defgeneric set-row (m r values))
(defmethod set-row ((m matrix) r values)
  (do
      ((l values (cdr l))
       (i 0 (+ i 1)))
      ((not l))
    (set-val m r i (car l))))

(defgeneric set-col (m c values))

(defmethod set-col ((m matrix) c values)
  (do
   ((l values (cdr l))
    (i 0 (+ i 1)))
   ((not l))
    (set-val m i c (car l))))

(defgeneric get-row-list (m r &optional start))

(defmethod get-row-list ((m matrix) r &optional (start 0))
  (cond
    ((< start (second (dim m)))
     (cons (val m r start)
           (get-row-list m r (+ 1 start))))
    (t nil)))

(defgeneric get-col-list (m c &optional start))

(defmethod get-col-list ((m matrix) c &optional (start 0))
  (cond
    ((< start (first (dim m)))
     (cons (val m start c)
           (get-col-list m c (+ 1 start))))
    (t nil)))

(defgeneric map-set-val (a f))

(defmethod map-set-val ((m matrix) f)
  (loop for i from 0 below (matrix-total-size m)
     do (setf (row-major-mref m i)
              (funcall f (row-major-mref m i))))
  m)

;;; FIXME THIS IS BROKEN!
(defgeneric map-set-val-fit (a f &key truncate))
(defmethod map-set-val-fit ((a matrix) f &key (truncate t))
  (destructuring-bind (m n) (dim a)
    (declare (dynamic-extent m n) (fixnum m n))
    (dotimes (i m)
      (declare (dynamic-extent i) (fixnum i))
      (dotimes (j n)
        (declare (dynamic-extent j) (fixnum j))
	(set-val-fit a i j (funcall f (val a i j)) :truncate truncate))))
  a)

(defgeneric map-set-val-copy (a f))
(defmethod map-set-val-copy ((a matrix) f)
  (destructuring-bind (ar ac) (dim a)
    (declare (dynamic-extent ar ac) (fixnum ar ac))
    (let* ((b (mat-copy-proto a)))
      (dotimes (i ar)
        (declare (dynamic-extent i) (fixnum i))
	(dotimes (j ac)
          (declare (dynamic-extent j) (fixnum j))
	  (set-val b i j (funcall f (val a i j)))))
      b)))

(defgeneric map-range (a startr endr startc endc f))
(defmethod map-range ((a matrix)
                      (startr fixnum)
                      (endr fixnum)
                      (startc fixnum)
                      (endc fixnum)
                      f)
  (declare (dynamic-extent startr endr startc endc)
	   (fixnum startr endr startc endc))
  (do ((i startr (1+ i)))
      ((> i endr))
    (declare (dynamic-extent i) (type fixnum i))
    (do ((j startc (1+ j)))
	((> j endc))
      (declare (dynamic-extent j) (type fixnum j))
      (funcall f (val a i j) i j))))

(defgeneric map-set-range (a startr endr startc endc func))
(defmethod map-set-range ((a matrix)
                          (startr fixnum)
                          (endr fixnum)
                          (startc fixnum)
                          (endc fixnum)
                          f)
  (declare (dynamic-extent startr endr startc endc)
	   (fixnum startr endr startc endc))
  (do ((i startr (1+ i)))
      ((> i endr))
    (declare (dynamic-extent i) (fixnum i))
    (do ((j startc (1+ j)))
	((> j endc))
      (declare (dynamic-extent j) (fixnum j))
      (set-val a i j (funcall f (val a i j) i j)))))

(defgeneric random-matrix (rows cols &key matrix-class limit)
  (:documentation "Create a matrix of type <matrix-class> having
<rows> rows and <cols> cols. The values of the matrix will be
random numbers of the appropriate type between 0 and <limit>."))

(defmethod random-matrix ((rows fixnum) (cols fixnum) &key
                          (matrix-class 'matrix)
                          (limit 1.0d0))
  (declare (dynamic-extent rows cols)
	   (fixnum rows cols))
  (let ((a (make-instance matrix-class :rows rows :cols cols)))
    (map-set-val a #'(lambda (x) (declare (ignore x)) (random limit)))
    a))


(defun count-range (startr endr startc endc)
  (* (1+ (- endr startr)) (1+ (- endc startc))))  

(defun double-float-divide (&rest args)
  (apply #'/ (mapcar #'(lambda (x) (coerce x 'double-float)) args)))

(defgeneric subset-matrix (u startr endr startc endc))
(defmethod subset-matrix ((u matrix) startr endr startc endc)
  (destructuring-bind (ur uc) (dim u)
    (cond
      ((and (<= startr endr ur) (<= startc endc uc))
       (let* ((m (1+ (- endr startr)))
              (n (1+ (- endc startc)))
              (c (mat-copy-proto-dim u m n)))
         (dotimes (i m)
           (dotimes (j n)
             (set-val c i j (val u (+ i startr) (+ j startc)))))
         c))
      (t nil))))

(defgeneric map-matrix-copy (a f &key matrix-class))
(defmethod map-matrix-copy ((a matrix) f &key (matrix-class (class-of a)))
  (destructuring-bind (m n) (dim a)
    (let* ((b (make-instance matrix-class :rows m :cols n)))
      (dotimes (i m)
	(dotimes (j n)
	  (set-val b i j (funcall f a i j))))
      b)))

(defgeneric array->matrix (a &key matrix-class))
(defmethod array->matrix ((a array) &key (matrix-class 'matrix))
  (let ((d (array-dimensions a))
        (element-type (element-type (find-class matrix-class)))
	(m))
    (cond ((= (length d) 2)
	   (destructuring-bind (ar ac) d
	     (cond
#|	      ((and (= ar 1) (= ac 1))
	       (setf m (scalar (aref a 0 0))))
	      ((= ac 1)
	       (setf m (array->col-vector a)))
	      ((= ar 1)
	       (setf m (array->row-vector a)))
|#
	      (t 
	       (setf m (make-instance matrix-class :rows ar :cols ac))
	       (let ((k 0))
		 (dotimes (i ar)
		   (dotimes (j ac)
		     (set-val m i j (aref a i j) :coerce t)
		     (incf k))))))))
          ((> (length d) 0)
           (setf m (make-instance matrix-class :dimensions d))
           (loop for i from 0 below (matrix-total-size m)
              do (setf (row-major-mref m i)
                       (coerce 
                        (row-major-aref a i)
                        element-type)))))
    m))

(defgeneric matrix->list (m))
(defmethod  matrix->list ((m matrix))
  (destructuring-bind (mr mc) (dim m)
    (loop for i below mr
       append (loop for j below mc
                   collect (mref m i j)))))

(defgeneric mat-trim (m k))
(defmethod mat-trim ((m matrix) k)
  (destructuring-bind (mr mc) (dim m)
    (subset-matrix m k (- mr k 1) k (- mc k 1))))


