
(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+sbcl
  (if (find-package 'sb-mop)
      (pushnew :clem-sbcl-mop cl:*features*)
      (pushnew :clem-sbcl-pcl cl:*features*)))
  

(defpackage #:clem
  (:use #:cl #:asdf
        #+clem-sbcl-mop #:sb-mop
        #+(and cmu pcl) #:clos-mop)
  (:export
   #:matrix
   #:dim
   #:rows
   #:cols
   #:val
   #:set-val
   #:mref
   
   #:mat-equal
   #:print-range
   #:print-matrix
   #:transpose
   #:mat-mult
   #:mat-hprod
   #:mat-hprod!
   #:mat-copy-into
   #:mat-add
   #:mat-subtr
   #:swap-rows
   #:swap-cols
   #:map-col
   #:map-row
   #:invert-matrix

   #:horzcat
   #:vertcat
   #:pad-matrix

   #:scalar-divide
   #:scalar-mult-col
   #:scalar-mult-row
   #:scalar-divide-col
   #:scalar-divide-row
   #:scalar-double-float-divide-col
   #:scalar-double-float-divide-row
   #:scalar-single-float-divide-col
   #:scalar-single-float-divide-row

   #:random-matrix
   #:zero-matrix
   #:identity-matrix

   #:mat-square
   #:mat-square!
   #:mat-sqrt
   #:mat-sqrt!

   #:normalize
   #:norm-0-255
   #:norm-0-1

   #:map-matrix

   #:sum-range
   #:sum
   #:sum-square-range
   #:sum-square
   
   #:max-val
   #:min-val

   #:mat-copy-proto

   ;;; temporary (maybe) matrix conversion utility functions
   #:copy-to-ub8-matrix
   #:copy-to-ub16-matrix
   #:copy-to-ub32-matrix
   #:copy-to-double-float-matrix
   #:copy-to-fixnum-matrix
   #:copy-to-bit-matrix


   ;;; matrix operations
   
   #:discrete-convolve
   #:separable-discrete-convolve

   #:gaussian-blur
   #:gaussian-blur-word
   
   #:gaussian-kernel

   #:dilate
   #:erode

   #:threshold
   #:binary-threshold

   #:x-derivative
   #:y-derivative
   #:gradmag
   #:laplacian
   
   #:variance-window
   #:sample-variance-window

   ;;; from matrixutils.cl
   #:array->matrix
   #:mat-trim
   
   ;;; typed-matrix stuff
   #:typed-matrix
   #:map-matrix-fit

   #:defmatrixtype

   #:sb8-matrix
   #:sb8-scalar
   #:array->sb8-matrix
   #:random-sb8-matrix

   #:unsigned-byte-matrix
   #:ub8-matrix
   #:ub8-scalar
   #:array->ub8-matrix
   #:random-ub8-matrix

   #:sb16-matrix
   #:array->sb16-matrix
   #:random-sb16-matrix

   #:ub16-matrix
   #:array->ub16-matrix
   #:random-ub16-matrix

   #:sb32-matrix
   #:array->sb32-matrix
   #:random-sb32-matrix

   #:ub32-matrix
   #:array->ub32-matrix
   #:random-ub32-matrix

   #:fixnum-matrix
   #:array->fixnum-matrix
   #:random-fixnum-matrix

   #:float-matrix
   
   #:single-float-matrix
   #:array->single-float-matrix
   #:random-single-float-matrix

   #:double-float-matrix
   #:array->double-float-matrix
   #:random-double-float-matrix

   #:t-matrix
   #:array->t-matrix
   #:random-t-matrix

   #:bit-matrix
   #:array->bit-matrix
   #:random-bit-matrix

   #:integer-matrix
   #:array->integer-matrix
   #:random-integer-matrix

   #:real-matrix
   #:array->real-matrix
   #:random-real-matrix
   
   #:complex-matrix
   #:array->complex-matrix
   #:random-complex-matrix

   #:number-matrix
   
   ;;; affine transformation stuff
   #:affine-transformation
   #:make-affine-transformation

   ;; 7-parameter parameterization of an affine transformation
   #:affine-transformation-7-parameters
   #:make-affine-transformation-matrix-from-7-parameters
   #:transformation-parameter
   #:decf-transformation-parameters
   #:copy-affine-transformation-7-parameters
   #:move-affine-transformation-7-parameters
   #:get-affine-transformation-7-parameters-properties

   #:y-scale
   #:x-scale
   #:theta
   #:y-shift
   #:x-shift
   #:y-shear
   #:x-shear

   #:affine-transform
   #:transform-matrix

   #:compute-bounds
   #:transform-coord
   
   #:subset-matrix

   #:mat-scale
   #:mat-scale-range
   #:mat-abs
   #:mat-abs-range
   #:mlog
   #:mlog-range

   #:matrix-move
   #:matrix-move-range-2d
   
   ;;; typed matrix operations

   #:m+
   #:m-
   #:m*
   #:m.*

   ;;; logical operations

   #:mlogior
   #:mlogand
   #:mlogxor

   ;; matrix looping operator
   #:mloop

   #:matrix-means
   #:matrix-medians
   ))

(defpackage :clem-user
  (:use #:cl #:clem))

