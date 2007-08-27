
(in-package :clem)

;;; slow fallback methods
;;; FIXME NEED FAST METHODS!!

(defmethod sum ((m matrix))
  (let ((acc 0))
    (loop for i from 0 below (matrix-total-size m)
       do (incf acc (row-major-mref m i)))
    acc))

(defmethod sum-cols ((m matrix) &key (matrix-class (class-of m)))
  (let ((mr (rows m)) (mc (cols m)))
    (let ((n (make-instance matrix-class :rows 1 :cols mc)))
      (dotimes (i mr)
        (dotimes (j mc)
          (incf (mref n 0 j) (mref m i j))))
      n)))

(defmethod sum-rows ((m matrix) &key (matrix-class (class-of m)))
  (let ((mr (rows m)) (mc (cols m)))
    (let ((n (make-instance matrix-class :rows mr :cols 1)))
      (dotimes (i mr)
        (dotimes (j mc)
          (incf (mref n i 0) (mref m i j))))
      n)))

(defmethod sum-square-range ((m matrix) (startr fixnum) (endr fixnum) (startc fixnum) (endc fixnum))
  (declare (dynamic-extent startr endr startc endc)
	   (fixnum startr endr startc endc))
  (let ((acc 0))
    (map-range m startr endr startc endc
	       #'(lambda (v i j)
		   (declare (ignore i j))
		   (incf acc (* v v))))
    acc))

(defmethod sum-square ((m matrix))
  (destructuring-bind (mr mc) (dim m)
    (sum-square-range m 0 (- mr 1) 0 (- mc 1))))

(defmethod sum-range ((m matrix) (startr fixnum) (endr fixnum) (startc fixnum) (endc fixnum))
  (declare (dynamic-extent startr endr startc endc)
	   (fixnum startr endr startc endc))
  (let ((acc 0))
    (map-range m startr endr startc endc
	       #'(lambda (v i j)
		   (declare (ignore i j))
		   (incf acc v)))
    acc))

;;; faster type-specific methods

(defmacro %%sum-range (m startr endr startc endc element-type accumulator-type)
  `(let ((acc (coerce 0 ',accumulator-type))
         (a (matrix-vals ,m)))
     (declare (type ,accumulator-type acc)
              (type (simple-array ,element-type *) a))
     (do ((i ,startr (1+ i)))
         ((> i ,endr))
       (declare (dynamic-extent i) (type fixnum i))
       (do ((j ,startc (1+ j)))
           ((> j ,endc))
         (declare (dynamic-extent j) (type fixnum j))
         (setf acc (+ acc (aref a i j)))))
     acc))

(macrolet ((frob-sum (matrix-type accumulator-type)
             (let ((element-type (element-type (find-class matrix-type))))
               `(progn
                  (defmethod sum ((m ,matrix-type))
                    (let ((acc (coerce 0 ',accumulator-type)))
                      (declare (type ,accumulator-type acc))
                      (with-typed-mref (m ,element-type)
                        (loop for i from 0 below (matrix-total-size m)
                           do (incf acc (row-major-mref m i))))
                      acc))

                  (defmethod sum-square ((m ,matrix-type))
                    (let ((acc (coerce 0 ',accumulator-type)))
                      (declare (type ,accumulator-type acc))
                      (with-typed-mref (m ,element-type)
                        (loop for i from 0 below (matrix-total-size m)
                           do (incf acc
                                    (* (row-major-mref m i)
                                       (row-major-mref m i)))))
                      acc))))))
  (frob-sum double-float-matrix double-float)
  (frob-sum single-float-matrix single-float)

  (frob-sum ub8-matrix (unsigned-byte 32))
  (frob-sum ub16-matrix (unsigned-byte 32))
  (frob-sum ub32-matrix (unsigned-byte 32))

  (frob-sum sb8-matrix (signed-byte 32))
  (frob-sum sb16-matrix (signed-byte 32))
  (frob-sum sb32-matrix (signed-byte 32))

  (frob-sum fixnum-matrix (signed-byte 32))
  (frob-sum bit-matrix (signed-byte 32))
  (frob-sum integer-matrix integer)
  (frob-sum real-matrix real)
  (frob-sum number-matrix number))

(macrolet
    ((frob-sum-range (matrix-type accumulator-type)
       (let ((element-type (element-type (find-class matrix-type))))
	 `(defmethod sum-range ((m ,matrix-type)
                                (startr fixnum) (endr fixnum)
                                (startc fixnum) (endc fixnum))
            (%%sum-range m startr endr startc endc
                         ,element-type ,accumulator-type)))))
  (frob-sum-range double-float-matrix double-float)
  (frob-sum-range single-float-matrix single-float)

  (frob-sum-range ub8-matrix (unsigned-byte 32))
  (frob-sum-range ub16-matrix (unsigned-byte 32))
  (frob-sum-range ub32-matrix (unsigned-byte 32))

  (frob-sum-range sb8-matrix (signed-byte 32))
  (frob-sum-range sb16-matrix (signed-byte 32))
  (frob-sum-range sb32-matrix (signed-byte 32))

  (frob-sum-range fixnum-matrix (signed-byte 32))
  (frob-sum-range bit-matrix (signed-byte 32)))


(macrolet
    ((frob-sum-square-range (matrix-type accumulator-type)
       (let ((element-type (element-type (find-class matrix-type))))
	 `(defmethod sum-square-range ((m ,matrix-type) (startr fixnum) (endr fixnum) (startc fixnum) (endc fixnum))
	    (let ((acc (coerce 0 ',accumulator-type))
		  (a (matrix-vals m)))
	      (declare (type ,accumulator-type acc)
		       (type (simple-array ,element-type *) a))
	      (do ((i startr (1+ i)))
		  ((> i endr))
		(declare (dynamic-extent i) (type fixnum i))
		(do ((j startc (1+ j)))
		    ((> j endc))
		  (declare (dynamic-extent j) (type fixnum j))
		  (incf acc (* (aref a i j) (aref a i j)))))
	      acc)))))

  (frob-sum-square-range double-float-matrix double-float)
  (frob-sum-square-range single-float-matrix single-float)

  (frob-sum-square-range ub8-matrix (unsigned-byte 32))
  (frob-sum-square-range ub16-matrix (unsigned-byte 32))
  (frob-sum-square-range ub32-matrix (unsigned-byte 32))

  (frob-sum-square-range sb8-matrix (signed-byte 32))
  (frob-sum-square-range sb16-matrix (signed-byte 32))
  (frob-sum-square-range sb32-matrix (signed-byte 32))

  (frob-sum-square-range fixnum-matrix (signed-byte 32))
  (frob-sum-square-range bit-matrix (signed-byte 32)))

