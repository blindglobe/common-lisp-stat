
(in-package :clem-test)

(defun test-affine-transform ()
  (let ((xfrm (make-affine-transformation :x-shift -2d0 :y-shift -3d0 :theta (* -.25 pi)))
        (coord1 (transpose (array->double-float-matrix #2A((10.0d0 20.0d0 1.0d0)))))
        (coord2 (transpose (array->double-float-matrix #2A((2d0 2d0 1d0))))))
    (let ((inv-xfrm (invert-matrix xfrm)))
      (print-matrix (mat-mult xfrm coord1))
      (print-matrix inv-xfrm)
      (let ((hc (mat-mult inv-xfrm coord2)))
        hc))))

(defun test-affine-transform-2 ()
  (let ((t1 (clem:make-affine-transformation
             :x-shift 20d0 :y-shift 20d0 :theta (/ (* 120 pi) 180) :x-scale (log 2) :y-scale (log 2)))
        (t2 (clem:make-affine-transformation
             :x-shift 100d0 :y-shift 100d0)))
    (clem:print-matrix t1)
    (clem:print-matrix t2)
    (clem:print-matrix (clem:mat-mult t2 t1))))

