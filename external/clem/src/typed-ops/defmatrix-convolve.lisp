;;;
;;; file: defmatrix-conolve.cl
;;; author: cyrus harmon
;;;

(in-package :clem)

(defmacro def-matrix-convolve (type-1 type-2 accumulator-element-type dest-type)
  (let ((element-type-1 (element-type (find-class `,type-1)))
	(element-type-2 (element-type (find-class `,type-2)))
	(dest-element-type (element-type (find-class `,dest-type))))
    (let ((one (coerce 1 accumulator-element-type))
          (zero (coerce 0 accumulator-element-type)))
      `(progn
         (defmethod %discrete-convolve ((u ,type-1)
                                        (v ,type-2)
                                        (z ,dest-type)
                                        &key
                                        (norm-v t))
           (declare (optimize (speed 3) (safety 0) (space 0)))
           ;; ur, uc, vr, vc are the number of rows and columns in u and v
           (let ((ur (rows u)) (uc (cols u)) (vr (rows v)) (vc (cols v)))
             (declare (type fixnum ur uc vr vc))
             ;; need a new matrix z to hold the values of the convolved matrix
             ;; dim z should be dim u + dim v - 1
             (let ((zr (+ ur vr (- 1)))
                   (zc (+ uc vc (- 1))))
               ;; now that we really have z we should get the dimensions and check
               ;; instead!
               (declare (type fixnum zr zc))
               (let ((uval (matrix-vals u))
                     (vval (matrix-vals v))
                     (zval (matrix-vals z))
                     (vsum (sum v)))
                 (declare (type (simple-array ,element-type-1 *) uval)
                          (type (simple-array ,element-type-2 *) vval)
                          (type (simple-array ,dest-element-type *) zval)
                          (type ,accumulator-element-type vsum))
                 (dotimes (i zr)
                   (declare (type fixnum i))
                   (let ((ustartr (max 0 (the fixnum (- i vr -1))))
                         (uendr (min (- ur 1) i))
                         (vstartr (- vr (max (the fixnum (- vr i)) 1)))
                         (vendr (- vr (min (- zr i) vr))))
                     (declare (type fixnum ustartr uendr vstartr vendr))
                     (dotimes (j zc)
                       (declare (type fixnum j))
                       (let ((ustartc (max 0 (the fixnum (- j vc -1))))
                             (uendc (min (- uc 1) j))
                             (vstartc (- vc (max (the fixnum (- vc j)) 1)))
                             (vendc (- vc (min (- zc j) vc)))
                             (acc ,zero))
                         (declare (type fixnum ustartr uendr vstartr vendr)
                                  (type ,accumulator-element-type acc))
                         (let ((normval
                                (if (and norm-v
                                         (or (not (= vendr vendc 0))
                                             (< vstartr (the fixnum (1- vr)))
                                             (< vstartc (the fixnum (1- vc)))))
                                    (let ((rsum
                                           (%%sum-range v vendr vstartr vendc vstartc
                                                        ,element-type-2
                                                        ,(car (slot-value
                                                               `,(find-class `,type-2)
                                                               'clem::accumulator-type)))))
                                      (declare (type ,accumulator-element-type rsum))
                                      (if (not (= rsum ,zero))
                                          (/ vsum rsum)
                                          ,zero))
                                    ,one)))
                           (declare (type ,accumulator-element-type normval))
                           (do ((urow ustartr (the fixnum (1+ urow)))
                                (vrow vstartr (the fixnum (1- vrow))))
                               ((> urow uendr))
                             (declare (type fixnum urow vrow))
                             (do ((ucol ustartc (the fixnum (1+ ucol)))
                                  (vcol vstartc (the fixnum (1- vcol))))
                                 ((> ucol uendc))
                               (declare (type fixnum ucol vcol))
                               (let ((uv (aref uval urow ucol))
                                     (vv (aref vval vrow vcol)))
                                 (declare (type ,element-type-1 uv)
                                          (type ,element-type-2 vv))
                                 (incf acc (* uv vv))
                                 )))

                           (setf (aref zval i j)
                                 (maybe-truncate
                                  (* acc normval)
                                  ,accumulator-element-type
                                  ,dest-element-type)))))))
                 z))))

         (defmethod discrete-convolve ((u ,type-1) (v ,type-2)
                                       &key (truncate nil) (norm-v t)
                                       (matrix-class ',dest-type))
           (declare (ignore truncate))
           ;; ur, uc, vr, vc are the number of rows and columns in u and v
           (let ((ur (rows u)) (uc (cols u)) (vr (rows v)) (vc (cols v)))
             (let ((zr (+ ur vr (- 1)))
                   (zc (+ uc vc (- 1))))
               (unless matrix-class
                 (setf matrix-class (type-of u)))
               (let ((z (make-instance matrix-class :rows zr :cols zc)))
                 (%discrete-convolve u v z :norm-v norm-v)))))

         (defmethod %separable-discrete-convolve ((m ,type-1) (h1 ,type-2) (h2 ,type-2)
                                                 (z1 matrix) (z2 matrix)
                                                 &key (truncate nil) (norm-v nil)
                                                 (matrix-class (class-of m)))
           (declare (ignore truncate matrix-class))
           (let ((mr (rows m)) (mc (cols m)) (hr (rows h1)) (hc (cols h2)))
             (declare (ignore mr mc hr hc))
             (%discrete-convolve m h2 z1 :norm-v norm-v)
             (%discrete-convolve z1 h1 z2 :norm-v norm-v)))

         
         (defmethod separable-discrete-convolve ((m ,type-1) (h1 ,type-2) (h2 ,type-2)
                                                 &key (truncate nil) (norm-v nil)
                                                 (matrix-class (class-of m)))
           (declare (ignore truncate))
           (let ((mr (rows m)) (mc (cols m)) (hr (rows h1)) (hc (cols h2)))
             (let ((z1r mr) (z1c (+ mc hc -1))
                   (z2r (+ mr hr -1)) (z2c (+ mc hc -1)))
               (let ((z1 (make-instance matrix-class :rows z1r :cols z1c))
                     (z2 (make-instance matrix-class :rows z2r :cols z2c)))
                 (%separable-discrete-convolve m h1 h2 z1 z2 :norm-v norm-v)))))))))

(macrolet ((frob (type-1 type-2 type-3 type-4)
             `(progn
                (def-matrix-convolve ,type-1 ,type-2 ,type-3 ,type-4))))
  (frob double-float-matrix double-float-matrix double-float double-float-matrix)
  (frob single-float-matrix single-float-matrix single-float single-float-matrix)
  (frob ub8-matrix ub8-matrix (unsigned-byte 8) ub8-matrix)
  (frob ub8-matrix double-float-matrix double-float ub8-matrix)
  (frob ub8-matrix single-float-matrix single-float ub8-matrix))

