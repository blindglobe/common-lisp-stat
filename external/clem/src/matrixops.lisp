;;;
;;; file: matrixops.cl
;;; author: cyrus harmon
;;;

;;;
;;; 2004-05-07 - This class contains matrix operations such as
;;;              gaussian-blur, gradient, etc...
;;;
;;;              Relies on the matrix package for matrix datatypes
;;;              and core functions such as discrete-convolve
;;;

(in-package :clem)


;;; discrete-convolve takes two matrices and returns
;;; a new matrix which is the convolution of the two matrices.
;;; To convolve, for each row,col of the matrix u, overlay matrix v
;;; the current cell and take the sum of the product of all of the
;;; u,v pairs. Note that v is rotated 180 degrees wrt u so that
;;; if we are calculating the value of (1,1) in the convolution of
;;; 3x3 matrices, the first number we would sum would be (0,0) x (2,2)
;;; not (0,0) x (0,0)

;;; discrete-convolve takes two matrices and returns
;;; a new matrix which is the convolution of the two matrices.
;;; To convolve, for each row,col of the matrix u, overlay matrix v
;;; the current cell and take the sum of the product of all of the
;;; u,v pairs. Note that v is rotated 180 degrees wrt u so that
;;; if we are calculating the value of (1,1) in the convolution of
;;; 3x3 matrices, the first number we would sum would be (0,0) x (2,2)
;;; not (0,0) x (0,0)


(defgeneric discrete-convolve (u v &key truncate norm-v matrix-class))

(defmethod discrete-convolve ((u matrix) (v matrix)
			      &key (truncate nil) (norm-v t)
			      (matrix-class nil))
  ;; (declare (optimize (speed 3) (safety 0) (space 0)))
  ;;; ur, uc, vr, vc are the number of rows and columns in u and v
  (print 'unspecialized-discrete-convolve!)
  (destructuring-bind (ur uc) (dim u)
    (declare (dynamic-extent ur uc) (fixnum ur uc))
    (destructuring-bind (vr vc) (dim v)
      (declare (fixnum vr vc) (dynamic-extent vr vc))
      ;;; need a new matrix z to hold the values of the convolved matrix
      ;;; dim z should be dim u + dim v - 1
      (let ((zr (+ ur vr (- 1)))
	    (zc (+ uc vc (- 1))))
	(declare (fixnum zr zc) (dynamic-extent zr zc))
	(unless matrix-class
	  (setf matrix-class (type-of u)))
	(let ((z (make-instance matrix-class :rows zr :cols zc))
	      (vsum (sum v)))
	  (dotimes (i zr)
	    (let ((ustartr (max 0 (- i vr -1)))
		  (uendr (min (- ur 1) i))
		  (vstartr (- vr (max (- vr i) 1)))
		  (vendr (- vr (min (- zr i) vr))))
	      (dotimes (j zc)
		(let ((ustartc (max 0 (- j vc -1)))
		      (uendc (min (- uc 1) j))
		      (vstartc (- vc (max (- vc j) 1)))
		      (vendc (- vc (min (- zc j) vc)))
		      (acc 0))
		  (let ((normval (if (and norm-v (or (not (= vendr vendc 0))
						     (< vstartr (- vr 1))
						     (< vstartc (- vc 1))))
				     (let ((rsum (sum-range v vendr vstartr vendc vstartc)))
				       (if (not (= rsum 0))
					   (/ vsum rsum)
					 0))
				   nil)))
		    (do ((urow ustartr (1+ urow))
			 (vrow vstartr (1- vrow)))
			((> urow uendr))
		      (declare (fixnum urow vrow))
		      (declare (dynamic-extent urow vrow))
		      (do ((ucol ustartc (1+ ucol))
			   (vcol vstartc (1- vcol)))
			  ((> ucol uendc))
			(declare (fixnum ucol vcol))
			(declare (dynamic-extent ucol vcol))
			(let ((uv (val u urow ucol))
			      (vv (val v vrow vcol)))
			  (declare (dynamic-extent uv vv))
			  (incf acc (* uv vv))
			  )))
		    (if normval
			(setf acc (fit-value (* acc normval) z)))
		    (if truncate
			(set-val-fit z i j acc :truncate truncate)
			(set-val z i j acc)))))))
	  z)))))

(defun gaussian-kernel (k sigma)
  (let* ((d (1+ (* 2 k)))
	 (a (make-instance 'double-float-matrix :rows d :cols d))
	 (q (* 2 sigma sigma))
	 (z (/ (* pi q))))
    (dotimes (i d)
      (dotimes (j d)
	(set-val a i j
		 (* (exp (- (/ (+ (* (- i k) (- i k))
				  (* (- j k) (- j k)))
			       q)))
		    z))))
    (scalar-divide a (sum-range a 0 (- d 1) 0 (- d 1)))
    a))


(defun gaussian-kernel-1d (k sigma)
  (let* ((d (1+ (* 2 k)))
	 (a (make-instance 'double-float-matrix :rows d :cols 1))
	 (q (* 2 sigma sigma))
	 (z (/ (* pi q))))
    (dotimes (i d)
      (set-val a i 0
               (* (exp (- (/ (+ (* (- i k) (- i k))
                                (* (- i k) (- i k)))
                             q)))
                  z)))
    (scalar-divide a (sum-range a 0 (- d 1) 0 0))
    a))

(defgeneric separable-discrete-convolve (m h1 h2 &key truncate norm-v))

(defmethod separable-discrete-convolve (m h1 h2 &key (truncate nil) (norm-v nil))
  (let* ((m1 (discrete-convolve m h1 :truncate truncate :norm-v norm-v))
         (m2 (discrete-convolve m1 h2 :truncate truncate :norm-v norm-v)))
    m2))

(defun gaussian-blur (m &key (k 2) (sigma 1) (truncate nil))
  (let* ((hr (gaussian-kernel-1d k sigma))
         (hc (transpose hr)))
    (separable-discrete-convolve m hr hc :truncate truncate)))

(defparameter *x-derivative-conv-matrix*
  (transpose (array->sb8-matrix #2A((1 0 -1)(1 0 -1)(1 0 -1)))))

(defun x-derivative (m &key (matrix-class 'double-float-matrix) (truncate t))
  (let ((m1 (copy-to-matrix-type m matrix-class))
	(m2 (copy-to-matrix-type *x-derivative-conv-matrix* matrix-class)))
    (mat-scale (discrete-convolve m1 m2
                                  :truncate truncate :matrix-class matrix-class)
               (/ 3))))

(defparameter *y-derivative-conv-matrix*
  (array->sb8-matrix #2A((1 0 -1)(1 0 -1)(1 0 -1))))

(defun y-derivative (m &key (matrix-class 'double-float-matrix) (truncate t))
  (mat-scale
   (discrete-convolve (copy-to-matrix-type m matrix-class) 
                      (copy-to-matrix-type *y-derivative-conv-matrix* matrix-class)
                      :truncate truncate :matrix-class matrix-class)
   (/ 3)))

(defun gradmag (m &key (truncate nil))
  (let ((xd (x-derivative m :truncate truncate))
	(yd (y-derivative m :truncate truncate)))
    (mat-square! xd)
    (mat-square! yd)
    (mat-sqrt! (mat-add xd yd :in-place t))))

(defun graddir (m &key (truncate nil))
  (let ((xd (x-derivative m :truncate truncate))
	(yd (y-derivative m :truncate truncate)))
    (destructuring-bind (ur uc) (dim xd)
      (dotimes (i ur)
        (dotimes (j uc)
          (set-val xd i j
                   (if (zerop (val yd i j))
                       (val yd i j)
                       (atan (/ (val xd i j) (val yd i j))))))))
    xd))

(defparameter *laplacian-conv-matrix*
  (array->sb8-matrix #2A((0 1 0)(1 -4 1)(0 1 0))))

(defun laplacian (m &key (matrix-class 'double-float-matrix) (truncate t))
  (mat-scale
   (discrete-convolve (copy-to-matrix-type m matrix-class) 
                      (copy-to-matrix-type *laplacian-conv-matrix* matrix-class)
                      :truncate truncate :matrix-class matrix-class)
   (/ 4)))

(defparameter *laplacian-conv-matrix-2*
  (array->sb8-matrix #2A((0 0 1 0 0)
                         (0 0 0 0 0)
                         (1 0 -4 0 1)
                         (0 0 0 0 0)
                         (0 0 1 0 0))))


(defun laplacian-2 (m &key (matrix-class 'double-float-matrix) (truncate t))
  (mat-scale
   (discrete-convolve (copy-to-matrix-type m matrix-class) 
                      (copy-to-matrix-type *laplacian-conv-matrix-2* matrix-class)
                      :truncate truncate :matrix-class matrix-class)
   (/ 4)
   :in-place t))


(defun variance-window (a &key (k 2))
  (destructuring-bind (m n) (dim a)
    (let ((zm (1- m))
	  (zn (1- n)))
      (map-matrix-copy a #'(lambda (m i j) (variance-range
					    m
					    (max 0 (- i k))
					    (min zm (+ i k))
					    (max 0 (- j k))
					    (min zn (+ j k))))
		       :matrix-class 'double-float-matrix))))

(defun sample-variance-window (a &key (k 1) (truncate nil))
  ;;; FIXME remove truncate, I think
  (declare (ignore truncate))
  (destructuring-bind (m n) (dim a)
    (let ((zm (1- m))
	  (zn (1- n)))
      (map-matrix-copy a #'(lambda (m i j) (sample-variance-range
					    m
					    (max 0 (- i k))
					    (min zm (+ i k))
					    (max 0 (- j k))
					    (min zn (+ j k))))))))

(defgeneric morphological-op (u v f))
(defmethod morphological-op ((u matrix) (v matrix) f)
  ;;; ur, uc, vr, vc are the number of rows and columns in u and v
  (destructuring-bind (ur uc) (dim u)
    (destructuring-bind (vr vc) (dim v)
      ;;; need a new matrix z to hold the values of the convolved matrix
      ;;; dim z should be dim u + dim v - 1
      (let ((zr (+ ur vr (- 1)))
	    (zc (+ uc vc (- 1))))
	(let ((z (make-instance (class-of u) :rows zr :cols zc)))
	  (dotimes (i zr)
	    (let ((ustartr (max 0 (- i vr -1)))
		  (uendr (min (- ur 1) i))
		  (vstartr (- vr (max (- vr i) 1)))
;		  (vendr (- vr (min (- zr i) vr)))
		  )
	      (dotimes (j zc)
		(let ((ustartc (max 0 (- j vc -1)))
		      (uendc (min (- uc 1) j))
		      (vstartc (- vc (max (- vc j) 1)))
		      (acc '()))
;		      (vendc (- vc (min (- zc j) vc))))
;		  (print (list i j ";" ustartr uendr ";" ustartc uendc
;			       ";" vstartr vendr ";" vstartc vendc))
		  (do ((urow ustartr (1+ urow))
		       (vrow vstartr (1- vrow)))
		      ((> urow uendr))
		    (do ((ucol ustartc (1+ ucol))
			 (vcol vstartc (1- vcol)))
			((> ucol uendc))
		      (setf acc (funcall f acc (val u urow ucol) (val v vrow vcol)))))
;		      (setf acc (max acc (* (val u urow ucol) (val v vrow vcol))))))
		  (set-val-fit z i j acc)))))
	  z)))))

(defun separable-morphological-op (m h f)
  (let ((rowstart (floor (/ (1- (rows h)) 2)))
	(rowend (floor (/ (rows h) 2)))
	(colstart (floor (/ (1- (cols h)) 2)))
	(colend (floor (/ (cols h) 2))))
    
    (declare (dynamic-extent rowstart rowend colstart colend)
	     (index-type rowstart rowend colstart colend))
    
    (let ((h1 (subset-matrix h rowstart rowend 0 (1- (cols h))))
	  (h2 (subset-matrix h 0 (1- (rows h)) colstart colend)))
      (let* 
	  ((m1 (morphological-op m h1 f))
	   (m2 (morphological-op m1 h2 f)))
	m2))))

(defgeneric dilate (u v))
(defmethod dilate ((u matrix) (v matrix))
  (separable-morphological-op u v #'(lambda (acc uval vval)
				      (let ((opval (+ uval vval)))
					(cond
					 ((null acc) opval)
					 (t (max acc opval)))))))

(defgeneric erode (u v))
(defmethod erode ((u matrix) (v matrix))
  (separable-morphological-op u v #'(lambda (acc uval vval)
				      (let ((opval (- uval vval)))
					(cond
					 ((null acc) opval)
					 (t (min acc opval)))))))

#+nil
(defmethod dilate-orig ((u matrix) r)
;;; ur, uc, vr, vc are the number of rows and columns in u and v
  (destructuring-bind (ur uc) (dim u)
    (let ((z (make-instance (class-of u) :rows ur :cols uc)))
      (dotimes (i ur)
        (let ((ustartr (max 0 (- i r)))
              (uendr (min (- ur 1) (+ i r))))
          (dotimes (j uc)
            (let ((ustartc (max 0 (- j r)))
                  (uendc (min (- uc 1) (+ j r)))
                  (max-val (val u i j)))
              (do ((urow ustartr (1+ urow)))
                  ((> urow uendr))
                (do ((ucol ustartc (1+ ucol)))
                    ((> ucol uendc))
                  (setf max-val (max max-val (val u urow ucol)))))
              (set-val z i j max-val)))))
      z)))


#+nil
(defmethod erode-orig ((u matrix) r)
;;; ur, uc, vr, vc are the number of rows and columns in u and v
  (destructuring-bind (ur uc) (dim u)
    (let ((z (make-instance (class-of u) :rows ur :cols uc)))
      (dotimes (i ur)
        (let ((ustartr (max 0 (- i r)))
              (uendr (min (- ur 1) (+ i r))))
          (dotimes (j uc)
            (let ((ustartc (max 0 (- j r)))
                  (uendc (min (- uc 1) (+ j r)))
                  (min-val (val u i j)))
              (do ((urow ustartr (1+ urow)))
                  ((> urow uendr))
                (do ((ucol ustartc (1+ ucol)))
                    ((> ucol uendc))
                  (setf min-val (min min-val (val u urow ucol)))))
              (set-val z i j min-val)))))
      z)))

(defgeneric threshold (u tval &key minval maxval))
(defmethod threshold ((u matrix) (tval number) &key (minval 0) (maxval 255))
  (map-set-val-copy u #'(lambda (x) (if (> x tval)
					maxval
                                        minval))))

(defgeneric binary-threshold (u tval))
(defmethod binary-threshold ((u matrix) (tval number))
  (destructuring-bind (rows cols)
      (clem:dim u)
    (let ((thresh (make-instance 'bit-matrix :rows (rows u) :cols (cols u))))
      (dotimes (i rows)
        (dotimes (j cols)
          (setf (mref thresh i j)
                (if (> (mref u i j) tval) 1 0))))
      thresh)))

(defgeneric complement-matrix (u &key maxval))
(defmethod complement-matrix ((u matrix) &key (maxval (max-val u)))
  (destructuring-bind (rows cols)
      (clem:dim u)
    (let ((comp (mat-copy-proto u)))
      (dotimes (i rows)
        (dotimes (j cols)
          (setf (mref comp i j) (- maxval (mref u i j)))))
      comp)))
  
(defun standard-deviation (&rest matrices)
  (flet ((square (q)
           (* q q)))
    (when matrices
      (let ((x (make-instance 'double-float-matrix
                              :rows (rows (car matrices))
                              :cols (cols (car matrices))))
            (sd (make-instance 'double-float-matrix
                               :rows (rows (car matrices))
                               :cols (cols (car matrices))))
            (n (length matrices)))
        (dotimes (i (rows sd))
          (dotimes (j (cols sd))
            (loop for m in matrices
               do
               (incf (mref x i j) (mref m i j)))))
        (mat-scale x (/ n) :in-place t)
        (dotimes (i (rows sd))
          (dotimes (j (cols sd))
            (loop for m in matrices
               do
                 (incf (mref sd i j) (square (- (mref m i j) (mref x i j)))))))
        (mat-scale sd (/ (1- n)) :in-place t)
        (mat-sqrt sd)))))


(defun matrix-means (&rest matrices)
  (cond ((null matrices) nil)
        ((= (length matrices) 1) (car matrices))
        (t
         (when matrices
           (let ((x (make-instance 'double-float-matrix
                                   :rows (rows (car matrices))
                                   :cols (cols (car matrices))))
                 (n (length matrices)))
             (dotimes (i (rows x))
               (dotimes (j (cols x))
                 (loop for m in matrices
                    do
                    (incf (mref x i j) (mref m i j)))))
             (mat-scale x (/ n) :in-place t)
             x)))))

(defun matrix-medians (matrices)
  (cond ((null matrices) nil)
        ((= (length matrices) 1) (car matrices))
        (t
         (when matrices
           (let ((x (make-instance 'double-float-matrix
                                   :rows (rows (car matrices))
                                   :cols (cols (car matrices)))))
             (dotimes (i (rows x))
               (dotimes (j (cols x))
                 (loop for m in matrices
                    with l
                    do (push (mref m i j) l)
                    finally (setf (mref x i j)
                                  (coerce (ch-util::median l)
                                          'double-float)))))
             x)))))

(defgeneric matrix-l2-distance (b0 b1 &key dest))

;; we could probably make this faster by passing in a double-float-matrix that we
;; could use as a temporary destination matrix
(defmethod matrix-l2-distance ((b0 matrix) (b1 matrix) &key dest)
  (declare (optimize (speed 3)
                     (safety 0)))
  (let ((n (the fixnum (* (the fixnum (clem:rows b0)) (the fixnum (clem:cols b0))))))
    (declare (type fixnum n))
    (/
     ;; if we have a destination matrix, use it, otherwise we need to allocate one
     (the double-float
       (if dest
           (clem:sum
            (clem:mat-square!
             (progn
               (clem::matrix-move b0 dest)
               (clem:mat-subtr dest b1 :in-place t)
               dest)))
           (clem:sum
            (clem:mat-square
             (clem::copy-to-double-float-matrix (clem:m- b0 b1))))))
     n)))
