;;; transform.lisp
;;; affine transformations for the clem matrix package
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

(defclass affine-transformation (double-float-matrix)
  ((dimensions :initarg :dimensions :initform '(3 3) :type (or list null)))
  (:metaclass standard-matrix-class)
  (:documentation "a matrix that represents an affine-transformation"))

(defgeneric transform-matrix
    (m n xfrm &key u v x y
       interpolation background)
  (:documentation
   "applies the affine transform xfrm to the contents of matrix m
    and places the contents in n. The default supported classes
    of interpolation are :quadratic, :bilinear
    and :nearest-neighbor."))

(defgeneric copy-affine-transformation (xfrm))
(defgeneric move-affine-transformation (src dest))

(defun transform-coord (x y xfrm)
  "applies the affine transformation xfrm to the point {x,y} and
  returns the position of the point after applying the transformation"
  (let ((coord1 (make-instance 'double-float-matrix :rows 3 :cols 1)))
    (setf (mref coord1 0 0) (coerce x 'double-float)
	  (mref coord1 1 0) (coerce y 'double-float)
	  (mref coord1 2 0) 1d0)
    (let ((coord2 (mat-mult xfrm coord1)))
      (values (mref coord2 0 0) (mref coord2 1 0)))))

(defun compute-bounds (x1 y1 x2 y2 xfrm)
  "takes a region bound by x1 and x2 on the x-axis and y1 and y2 on
  the y-axis and returns the coordinates of the bounding rectangle
  after applying the affine transform xfrm"
  (multiple-value-bind (p1 q1)
      (transform-coord x1 y1 xfrm)
    (multiple-value-bind (p2 q2)
        (transform-coord x2 y2 xfrm)
      (multiple-value-bind (p3 q3)
          (transform-coord x1 y2 xfrm)
        (multiple-value-bind (p4 q4)
            (transform-coord x2 y1 xfrm)
          (values (min p1 p2 p3 p4) ;; x1'
                  (min q1 q2 q3 q4) ;; y1'
                  (max p1 p2 p3 p4) ;; x2'
                  (max q1 q2 q3 q4))))))) ;; y2'

;;; I need to rethink what to do about the output matrix for the
;;; moment I pass it in and it is the same size as the input matrix. I
;;; should probably compute the required size of the thing and make a
;;; new matrix as apporpriate.
;;;
;;; Ok, rethinking...
;;;
;;; we have an input matrix, an output matrix and an affine
;;; transformation, represented by a matrix.
;;; this is enough to go on but it would probably be nice to offer
;;; more parameters to make affine transforms easier to use.
;;;
;;; the problem is that both input and output matrices themselves can
;;; be considered as living in coordinate spaces. One approach would
;;; be to leave this as is, ranging from 0 to rows - 1 rows and 0 to
;;; cols - 1 cols. Alternatively, we can allow input and output
;;; coordinates, onto which the affine transform is applied and the
;;; appropriate transformed matrix generated. We probably also need to
;;; specify a pixel-space coordinate for the input and output matrices
;;; as well, although there are lots of possible to interpret
;;; those. Let'stick to the matrix-space coordinates and figure those
;;; at first:
;;;
;;; m = input matrix
;;; mr = input matrix rows - 1, mc = input matrix cols -1
;;;
;;; n = output matrix
;;; nr = output matrix rows - 1, nc = output matrix cols - 1
;;;
;;; xfrm - affine transformation, specifies the mapping of points from
;;; input space to output space
;;;
;;; u = (u1 . u2) begin and end x coordinates of input matrix
;;; v = (v1 . v2) begin and end y coordinates of input matrix
;;; 
;;; x = (x1 . x2) begin and end x coordinates of output matrix
;;; y = (y1 . y2) begin and end y coordinates of output matrix
;;; 
;;; examples:
;;;
;;; keeping the transformed matrix fully in the new matrix:
;;;  2x doubling transformation
;;;  u = (0 . 100), v = (0 . 100), x = (0 . 200), y = (0 . 200)
;;;
;;;  2x doubling of a matrix shifted
;;;  u = (100 . 200), v = (100 . 200), x = (200 . 400), y = (200 . 400)

(defmethod transform-matrix (m n xfrm
                             &key u v x y
                             (interpolation :nearest-neighbor interpolation-supplied-p)
                             (background nil background-supplied-p))
  "applies the affine transform xfrm to the contents of matrix m
   and places the contents in n. The default supported
   classes. The default supported classes of interpolation
   are :quadratic, :bilinear and :nearest-neighbor. If no
   interpolation is supplied, the default is :nearest-neighbor."
  (let ((xfrm-shift (mat-copy xfrm)))
    (unless u
      (setf u (cons 0 (cols m))))
    (unless v
      (setf v (cons 0 (rows m))))    
    (multiple-value-bind (x1 y1 x2 y2)
        (compute-bounds (car u) (car v) (cdr u) (cdr v) xfrm)
      (unless x
        (setf x (cons (floor x1) (ceiling x2))))
      (unless y
        (setf y (cons (floor y1) (ceiling y2)))))

    ;; Need to rework math to do the right thing here!

    (let ((pre-shift1 (make-affine-transformation
                       :y-shift (car v) :x-shift (car u)))
          (pre-shift2 (make-affine-transformation
                       :y-scale (/ (- (cdr v) (car v)) (rows m))
                       :x-scale (/ (- (cdr u) (car u)) (cols m)))))
      (setf xfrm-shift (mat-mult xfrm-shift (mat-mult pre-shift1 pre-shift2))))
    (let ((post-shift (make-affine-transformation
                       :y-shift (- (car y)) :x-shift (- (car x))))
          (post-shift2 (make-affine-transformation
                        :y-scale (/ (rows n) (- (cdr y) (car y))) 
                        :x-scale (/ (cols n) (- (cdr x) (car x))))))
      (setf xfrm-shift (mat-mult post-shift (mat-mult post-shift2 xfrm-shift))))
    (apply #'%transform-matrix m n xfrm-shift
           (append
            (when background-supplied-p (list :background background))
            (when interpolation-supplied-p (list :interpolation interpolation))))))


(defmethod set-affine-transformation-parameters ((xfrm affine-transformation)
                                                 &key
                                                 (y-shift 0d0)
                                                 (x-shift 0d0)
                                                 (theta 0d0)
                                                 (y-scale 1d0)
                                                 (x-scale 1d0)
                                                 (y-shear 0d0)
                                                 (x-shear 0d0))
  (setf (mref xfrm 0 0) (- (* (cos theta) x-scale)
                           (* (sin theta) y-scale y-shear)))
  (setf (mref xfrm 0 1) (- (* (cos theta) x-scale x-shear)
                           (* (sin theta) y-scale)))
  (setf (mref xfrm 0 2) (coerce x-shift 'double-float))

  (setf (mref xfrm 1 0) (+ (* (sin theta) x-scale)
                           (* (cos theta) y-scale y-shear)))
  (setf (mref xfrm 1 1) (+ (* (sin theta) x-scale x-shear)
                           (* (cos theta) y-scale)))
  (setf (mref xfrm 1 2) (coerce y-shift 'double-float))
  
  (setf (mref xfrm 2 0) 0d0)
  (setf (mref xfrm 2 1) 0d0)
  (setf (mref xfrm 2 2) 1d0)
  xfrm)

(defmethod set-affine-transformation-matrix ((xfrm affine-transformation) (m matrix))
  (dotimes (i (rows m))
    (dotimes (j (cols m))
      (setf (mref xfrm i j) (mref m i j))))
  xfrm)

(defmethod copy-affine-transformation ((xfrm affine-transformation))
  (mat-copy xfrm))

(defmethod invert-affine-transformation ((xfrm affine-transformation))
  (let ((inv (copy-affine-transformation xfrm)))
    (set-affine-transformation-matrix inv (clem:invert-matrix xfrm))))

(defmethod move-affine-transformation ((src affine-transformation)
				       (dest affine-transformation))
  (mat-copy-into src dest))

(defmethod shared-initialize :after
    ((object affine-transformation) slot-names &rest args)
  (declare (ignore slot-names args)))

;;; Creates 3x3 matrix that represents an affine transformation.
;;; since we have an arbitarty 2d matrix (row-major order) and we
;;; haven't really fixed x and y axes, we have some choice as
;;; to how to represent this. Current convention is that
;;; y == row and x == col, so rows 0 and 1 of this matrix are
;;; swapped WRT the usual parameterization of this kind of
;;; affine transformation matrix.
(defun make-affine-transformation (&key
				   (x-shift 0d0)
				   (y-shift 0d0)
				   (x-scale 1.0d0)
				   (y-scale 1.0d0)
				   (x-shear 0.0d0)
				   (y-shear 0.0d0)
				   (theta 0d0))
  (let ((xfrm (make-instance 'affine-transformation)))
    (set-affine-transformation-parameters xfrm
                                          :x-shift x-shift
                                          :y-shift y-shift
                                          :theta theta
                                          :x-scale x-scale
                                          :y-scale y-scale
                                          :x-shear x-shear
                                          :y-shear y-shear)
    xfrm))


(defgeneric affine-transform (mat xfrm &key u v x y interpolation background matrix-class))
(defmethod affine-transform ((mat matrix)
                             (xfrm affine-transformation)
                             &key
                             u v x y
                             (interpolation nil interpolation-supplied-p)
                             (background nil background-supplied-p)
                             (matrix-class (class-of mat)))
  (unless u (setf u (cons 0 (cols mat))))
  (unless v (setf v (cons 0 (rows mat)))) 
  (multiple-value-bind (x1 y1 x2 y2)
      (compute-bounds (car u) (car v) (cdr u) (cdr v) xfrm)
    (unless x (setf x (cons (floor x1) (ceiling x2))))
    (unless y (setf y (cons (floor y1) (ceiling y2)))))
  (let ((rows (if y (truncate (- (cdr y) (car y)))
                  (rows mat)))
        (cols  (if x (truncate (- (cdr x) (car x)))
                   (cols mat))))
    (let ((m (make-instance matrix-class
                            :rows rows
                            :cols cols
                            :initial-element
                            (coerce 0 (element-type (class-of mat))))))
      (apply #'transform-matrix mat m xfrm
             (append
              (when u (list :u u))
              (when v (list :v v))
              (when x (list :x x))
              (when y (list :y y))
              (when background-supplied-p
                (list :background background))
              (when interpolation-supplied-p
                (list :interpolation interpolation))))
      m)))

(defun resize-matrix (m y x &key (interpolation :bilinear))
  (let ((oldy (rows m))
        (oldx (cols m)))
    (let ((xfrm (make-affine-transformation :x-scale (/ x oldx)
                                            :y-scale (/ y oldy))))
      (let ((n (affine-transform
                m xfrm
                :interpolation interpolation
                :u `(0 . ,oldx) :v `(0 . ,oldy)
                :x `(0 . ,x) :y `(0 . ,y))))
        n))))

(defmethod mat-mult ((m affine-transformation)
                      n)
  (let ((p (make-instance 'clem::affine-transformation))
        (r (call-next-method)))
    (if (equal (dim n) '(3 3))
        (progn
          (set-affine-transformation-matrix p r)
          p)
        r)))

(defmethod mat-add ((m affine-transformation)
                    (n affine-transformation) &key in-place)
  (let ((p (make-instance 'clem::affine-transformation))
        (r (call-next-method)))
    (set-affine-transformation-matrix p r)))

(defmethod mat-subtr :around
    ((m affine-transformation)
     n &key in-place (result-type 'clem::affine-transformation))
  (declare (ignorable matrix-class))
  (let ((p (make-instance result-type))
        (r (call-next-method)))
    (set-affine-transformation-matrix p r)))

(defmethod mat-hprod ((m affine-transformation)
                      n)
  (let ((p (make-instance 'clem::affine-transformation))
        (r (call-next-method)))
    (if (equal (dim n) '(3 3))
        (set-affine-transformation-matrix p r)
        r)))


(defclass affine-transformation-7-parameters ()
  ((y-shift :accessor y-shift :initarg :y-shift :initform 0d0 :type double-float)
   (x-shift :accessor x-shift :initarg :x-shift :initform 0d0 :type double-float)
   (theta :accessor theta :initarg :theta :initform 0d0 :type double-float)
   (y-scale :accessor y-scale :initarg :y-scale :initform 0d0 :type double-float)
   (x-scale :accessor x-scale :initarg :x-scale :initform 0d0 :type double-float)
   (y-shear :accessor y-shear :initarg :y-shear :initform 0d0 :type double-float)
   (x-shear :accessor x-shear :initarg :x-shear :initform 0d0 :type double-float))
  (:documentation "a set of parameters for use in
  (over-) parameterizing an affine transformation by use of seven
  parameters, x-shift, y-shift, theta, x-scale, y-scale, x-shear,
  and y-shear."))

(defun make-affine-transformation-matrix-from-7-parameters (transvec)
  (with-slots (y-shift x-shift theta y-scale x-scale y-shear x-shear) transvec
    (clem:make-affine-transformation :y-shift y-shift
                                     :x-shift x-shift
                                     :theta theta
                                     :y-scale (exp y-scale)
                                     :x-scale (exp x-scale)
                                     :y-shear y-shear
                                     :x-shear x-shear)))

(declaim (inline transformation-parameter))
(declaim (ftype (function (affine-transformation-7-parameters fixnum) double-float) transforamtion-parameter))

(defun transformation-parameter (xfrm i)
  (declare (type fixnum i))
  (ecase i
    (0 (/ (the double-float (y-shift xfrm)) 100d0))
    (1 (/ (the double-float (x-shift xfrm)) 100d0))
    (2 (theta xfrm))
    (3 (y-scale xfrm))
    (4 (x-scale xfrm))
    (5 (y-shear xfrm))
    (6 (x-shear xfrm))))

(declaim (ftype (function (double-float clem:affine-transformation fixnum)
                          clem:affine-transformation) (setf transforamtion-parameter)))
(declaim (inline (setf transformation-parameter)))
(defun (setf transformation-parameter) (v xfrm i)
  (declare (type double-float v)
           (type fixnum i))
  (ecase i
    (0 (setf (y-shift xfrm) (* v 100d0)))
    (1 (setf (x-shift xfrm) (* v 100d0)))
    (2 (setf (theta xfrm) v))
    (3 (setf (y-scale xfrm) v))
    (4 (setf (x-scale xfrm) v))
    (5 (setf (y-shear xfrm) v))
    (6 (setf (x-shear xfrm) v))))


(defun decf-transformation-parameters (src delta)
  (with-slots (y-shift x-shift theta y-scale x-scale y-shear x-shear) src
    (decf y-shift (y-shift delta))
    (decf x-shift (x-shift delta))
    (decf theta (theta delta))
    (decf y-scale (y-scale delta))
    (decf x-scale (x-scale delta))
    (decf y-shear (y-shear delta))
    (decf x-shear (x-shear delta))
    src))

(defun copy-affine-transformation-7-parameters (src)
  (let ((dest (make-instance 'affine-transformation-7-parameters)))
    (setf (y-shift dest) (y-shift src)
          (x-shift dest) (x-shift src)
          (theta dest) (theta src)
          (y-scale dest) (y-scale src)
          (x-scale dest) (x-scale src)
          (y-shear dest) (y-shear src)
          (x-shear dest) (x-shear src))
    dest))

(defun move-affine-transformation-7-parameters (src dest)
  (setf (y-shift dest) (y-shift src)
        (x-shift dest) (x-shift src)
        (theta dest) (theta src)
        (y-scale dest) (y-scale src)
        (x-scale dest) (x-scale src)
        (y-shear dest) (y-shear src)
        (x-shear dest) (x-shear src))
  dest)

(defun get-affine-transformation-7-parameters-properties (transvec)
  (with-slots (y-shift x-shift theta y-scale x-scale y-shear x-shear)
      transvec
    (list :y-shift y-shift
          :x-shift x-shift
          :theta theta
          :y-scale y-scale
          :x-scale x-scale
          :y-shear y-shear
          :x-shear x-shear)))
