;;; -*- Mode: lisp; outline-regexp: ";;;;;*"; indent-tabs-mode: nil -*-;;;
;;;
;;; file: defmatrix-transform.cl
;;; author: cyrus harmon
;;;

(in-package :clem)

(defconstant +epsilon+ 0.00001d0)

(defmacro def-matrix-transform (type-1 type-2 transform-type)
  (let ((element-type-1 (element-type (find-class `,type-1)))
	(element-type-2 (element-type (find-class `,type-2)))
	(transform-element-type (element-type (find-class `,transform-type))))
    (let ((zero (coerce 0 `,element-type-2))
	  (one (coerce 1 `,transform-element-type))
	  (half (coerce 0.5d0 `,transform-element-type))
          (epsilon (coerce +epsilon+ `,transform-element-type)))
      `(progn
	 (defmethod %transform-matrix ((m ,type-1) (n ,type-2) (xfrm ,transform-type)
				      &key (background ,zero)
                                       (interpolation :nearest-neighbor))
	   (let ((mr (rows m)) (mc (cols m))
		 (nr (rows n)) (nc (cols n)))
	     (declare (type fixnum mr mc nr nc)
                      (type ,element-type-2 background)
		      (optimize (speed 3) (safety 0)))
	     (let  ((inv-xfrm (clem::invert-matrix xfrm))
		    (coord1 (make-instance ',transform-type :rows 3 :cols 1))
		    (coord2 (make-instance ',transform-type :rows 3 :cols 1))
                    #+nil (mrf (coerce mr ',transform-element-type))
                    #+nil (mcf (coerce mc ',transform-element-type)))
	       (let ((a (clem::matrix-vals m))
		     (b (clem::matrix-vals n))
		     (c (clem::matrix-vals coord1))
		     (d (clem::matrix-vals coord2))
		     (g (clem::matrix-vals inv-xfrm)))
		 (declare (type (simple-array ,element-type-1 *) a)
			  (type (simple-array ,element-type-2 *) b)
			  (type (simple-array ,transform-element-type *) c d g)
                          (optimize (speed 3) (safety 0)))
		 (setf (aref c 2 0) ,one)
                 (dotimes (i nr)
                   (declare (type fixnum i))
                   (setf (aref c 0 0) (coerce i ',transform-element-type))
                   (dotimes (j nc)
                     (declare (type fixnum j))
                     (setf (aref c 1 0) (coerce j ',transform-element-type))
;;; the slow(er) way to do this is:
;;;   (setf (aref d 0 0) ,zero
;;;         (aref d 1 0) ,zero
;;;         (aref d 2 0) ,zero)
;;; (clem::mat-mult3 inv-xfrm coord1 coord2)
;;;
;;; but since we don't need the full matrix multiply,
;;; based on what we know is in the affine
;;; transformation matrix, we can get away with
;;; fewer operations (Foley et al., 1996, p. 213)
                     (setf (aref d 1 0) (the ,transform-element-type
                                          (+ (* (aref c 1 0) (aref g 0 0))
                                             (* (aref c 0 0) (aref g 0 1))
                                             (aref g 0 2)))
                           (aref d 0 0) (the ,transform-element-type
                                          (+ (* (aref c 1 0) (aref g 1 0))
                                             (* (aref c 0 0) (aref g 1 1))
                                             (aref g 1 2))))
;;; this does nearest neighbor interpolation
;;; we should also offer a way to do bilinear,
;;; bicubic, etc... interpolation
                     (case interpolation
                       ((:quadratic)
                        (if (and
                             (< ,(coerce most-negative-fixnum `,transform-element-type)
                                (aref d 0 0)
                                ,(coerce most-positive-fixnum `,transform-element-type))
                             (< ,(coerce most-negative-fixnum `,transform-element-type)
                                (aref d 1 0)
                                ,(coerce most-positive-fixnum `,transform-element-type)))
                            (multiple-value-bind (l ry)
                                
                                (truncate (the ,transform-element-type
                                            (+ (the (,transform-element-type
                                                     ,(coerce most-negative-fixnum `,transform-element-type)
                                                     ,(coerce most-positive-fixnum `,transform-element-type))
                                                 (aref d 0 0))
                                               ,epsilon)))
                              (declare (type fixnum l)
                                       (type ,transform-element-type ry))
                              (multiple-value-bind (k rx)
                                  (truncate (the ,transform-element-type
                                              (+ 
                                               (the (,transform-element-type
                                                     ,(coerce most-negative-fixnum `,transform-element-type)
                                                     ,(coerce most-positive-fixnum `,transform-element-type))
                                                 (aref d 1 0))
                                               ,epsilon)))
                                (declare (type fixnum k)
                                         (type ,transform-element-type rx))
                                (cond
                                  ((and (< -1 l mr)
                                        (< -1 k mc))
                                   (let ((l0 (max (the fixnum (1- l)) 0))
                                         (l2 (min (the fixnum (1+ l)) (the fixnum (1- mr))))
                                         (k0 (max (the fixnum (1- k)) 0))
                                         (k2 (min (the fixnum (1+ k)) (the fixnum (1- mc)))))
                                     (declare (type fixnum l0 l2 k0 k2))
                                     (setf (aref b i j)
                                           (maybe-truncate
                                            (quadratic-interpolate
                                             (aref a l0 k0)
                                             (aref a l0 k)
                                             (aref a l0 k2)
                                             (aref a l k0)
                                             (aref a l k)
                                             (aref a l k2)
                                             (aref a l2 k0)
                                             (aref a l2 k)
                                             (aref a l2 k2)
                                             ry rx
                                             ,transform-element-type)
                                            ,transform-element-type
                                            ,element-type-2
                                            ))))
                                  (t
                                   (setf (aref b i j) background)))))
                            (setf (aref b i j) background)))
                       ((:bilinear :bi-linear)
                        (if (and
                             (< ,(coerce most-negative-fixnum `,transform-element-type)
                                (aref d 0 0)
                                ,(coerce most-positive-fixnum `,transform-element-type))
                             (< ,(coerce most-negative-fixnum `,transform-element-type)
                                (aref d 1 0)
                                ,(coerce most-positive-fixnum `,transform-element-type)))
                            (multiple-value-bind (l ry)
                                (floor (the ,transform-element-type
                                         (+ (the (,transform-element-type
                                                  ,(coerce most-negative-fixnum `,transform-element-type)
                                                  ,(coerce most-positive-fixnum `,transform-element-type))
                                              (aref d 0 0))
                                            ,epsilon)))
                              (declare (type fixnum l)
                                       (type ,transform-element-type ry))
                              (multiple-value-bind (k rx)
                                  (floor (the ,transform-element-type
                                           (+ 
                                            (the (,transform-element-type
                                                  ,(coerce most-negative-fixnum `,transform-element-type)
                                                  ,(coerce most-positive-fixnum `,transform-element-type))
                                              (aref d 1 0))
                                            ,epsilon)))
                                (declare (type fixnum k)
                                         (type ,transform-element-type rx))
                                (cond
                                  ((and (< -1 l mr)
                                        (< -1 k mc))
                                   (let ((l1 (the fixnum (min (the fixnum (1+ l)) (the fixnum (1- mr)))))
                                         (k1 (the fixnum (min (the fixnum (1+ k)) (the fixnum (1- mc))))))
                                     (declare (type fixnum l1 k1))
                                     (setf (aref b i j)
                                           (maybe-truncate
                                            (bilinear-interpolate
                                             (aref a l k)
                                             (aref a l k1)
                                             (aref a l1 k)
                                             (aref a l1 k1)
                                             ry rx)
                                            ,transform-element-type
                                            ,element-type-2
                                            ))))
                                  (t
                                   (setf (aref b i j) background)))))
                            (setf (aref b i j) background)))
                       ((nil :nearest-neighbor)
                        (let ((oldx (the fixnum (truncate (+ (aref d 0 0) ,half))))
                              (oldy (the fixnum (truncate (+ (aref d 1 0) ,half)))))
                          (declare (type fixnum oldx oldy))
                          (if (and (< -1 oldx mr)
                                   (< -1 oldy mc))
                              (setf (aref b i j) (aref a oldx oldy))
                              (setf (aref b i j) background))))
                       (t (error 'matrix-argument-error
                                 :format-control "Unknown interpolation type: ~A"
                                 :format-arguments (list  interpolation)))))))))
	   n)))))

(macrolet ((frob (type-1 type-2 type-3)
	     `(def-matrix-transform ,type-1 ,type-2 ,type-3)))
  (frob double-float-matrix double-float-matrix double-float-matrix)
  (frob single-float-matrix single-float-matrix double-float-matrix)
  (frob single-float-matrix single-float-matrix single-float-matrix)
  (frob ub8-matrix ub8-matrix double-float-matrix)
  (frob ub16-matrix ub16-matrix double-float-matrix)
  (frob ub32-matrix ub32-matrix double-float-matrix)
  (frob sb8-matrix sb8-matrix double-float-matrix)
  (frob sb16-matrix sb16-matrix double-float-matrix)
  (frob sb32-matrix sb32-matrix double-float-matrix)
  (frob fixnum-matrix fixnum-matrix double-float-matrix)
  (frob bit-matrix bit-matrix double-float-matrix))
