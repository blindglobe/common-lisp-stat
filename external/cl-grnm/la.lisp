(in-package :neldermead)

;; Simple matrix-vector facility. Optimized for staying out of the way.
(defmacro with-matrix-dimensions (varforms &body body)
  (let ((form `(progn ,@body)))
    (dolist (vform varforms)
      (setf form
            `(destructuring-bind ,(car vform)
                 (array-dimensions ,(cadr vform))
	       (declare (fixnum ,@(car vform))
			(ignorable ,@(car vform)))
               ,form)))
    form))

(defun ip (v w &optional (start 0))
  (let ((l (length v))
	(ax 0.0d0))
    (dotimes (i (- l start))
      (incf ax (* (aref v (+ i start))
		  (aref w (+ i start)))))
    ax))

(defun norm (v &optional (start 0))
    (sqrt (ip v v start)))

(defun make-matrix (m n)
  (make-array (list m n)
	      :element-type 'double-float
	      :initial-element 0.0d0))

(defun make-vector (n &key (initial-element 0.0d0))
  (make-array n
	      :element-type 'double-float
	      :initial-element initial-element))

(defun v*c (vector constant)
  (let* ((n (length vector))
	 (res (make-vector n)))
    (dotimes (i n)
      (setf (aref res i)
	    (* constant (aref vector i))))
    res))

(defun v+w*c (v w c)
  (assert (= (length v) (length w)))
  (let* ((n (length v))
	 (res (make-vector n)))
    (dotimes (i n)
      (setf (aref res i)
	    (+ (aref v i)
	       (* c (aref w i)))))
    res))

(defun qr-factorization (mat &key (with-q t))
  (declare (type (simple-array double-float (* *)) mat)
	   (optimize (speed 3) (safety 0)))
  (with-matrix-dimensions (((m n) mat))
    (let ((q (when with-q
	       (let ((pq (make-matrix m n)))
		 (declare (type (simple-array double-float (* *)) pq))
		 (dotimes (i (min m n))
		   (setf (aref pq i i) 1.0d0))
		 pq))))

      (loop for i from 0 below (- m 1) do
	    (loop for j from (+ i 1) below m do
		  (let ((a (aref mat i i))
			 (b (aref mat j i)))
		    (when (/= b 0.0d0)
		      (let* ((r (sqrt (+ (* a a) (* b b))))
			    (c (/ a r))
			    (s (/ b r)))

		    (loop for k from i below m do
			  (let ((olda (aref mat i k))
				(oldb (aref mat j k)))

			    (setf (aref mat i k)
				  (+ (* c olda) (* s oldb))
				  (aref mat j k)
				  (+ (* (- s) olda) (* c oldb)))))

		    (when with-q
		      (let ((q q))
			(declare (type (simple-array double-float (* *)) q))
			(loop for k from 0 below m do
			      (let ((olda (aref q k i))
				    (oldb (aref q k j)))
			      
				(setf (aref q k i)
				      (+ (* c olda) (* s oldb))
				      (aref q k j)
				      (+ (* (- s) olda) (* c oldb))))))))))))
	    
      (values mat q))))
