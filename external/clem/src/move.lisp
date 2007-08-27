;;; move.lisp
;;; macros, functions and methods for moving elements between
;;; matrices.
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

;;; fallback version for when we don't have type information

(defmethod matrix-move ((m matrix) (n matrix) &key constrain)
  (if (= (matrix-total-size m)
         (matrix-total-size n))
      (if constrain
          (let ((min (minval (class-of n)))
                (max (maxval (class-of n))))
            (dotimes (i (matrix-total-size m))
              (setf (row-major-mref n i)
                    (constrain min (row-major-mref m i) max))))
          (dotimes (i (matrix-total-size m))
            (setf (row-major-mref n i)
                  (row-major-mref m i))))
      (error 'matrix-argument-error
             :format-control
             "Incompatible matrix dimensions in matrix-move ~S => ~S."
             :format-arguments (list (matrix-dimensions m)
                                     (matrix-dimensions n)))))

(defmethod matrix-move ((m matrix) (n integer-matrix) &key constrain)
  (if (= (matrix-total-size m)
         (matrix-total-size n))
      (if constrain
          (let ((min (minval (class-of n)))
                (max (maxval (class-of n))))
            (dotimes (i (matrix-total-size m))
              (setf (row-major-mref n i)
                    (constrain min (truncate (row-major-mref m i)) max))))
          (dotimes (i (matrix-total-size m))
            (setf (row-major-mref n i)
                  (truncate (row-major-mref m i)))))
      (error 'matrix-argument-error
             :format-control
             "Incompatible matrix dimensions in matrix-move ~S => ~S."
             :format-arguments (list (matrix-dimensions m)
                                     (matrix-dimensions n)))))

(defmacro def-matrix-move (type-1 type-2)
  (let ((element-type-1 (element-type (find-class `,type-1)))
	(element-type-2 (element-type (find-class `,type-2)))
        (min (minval (find-class `,type-2)))
        (max (maxval (find-class `,type-2))))
    `(progn
       (defmethod matrix-move-range-2d ((m ,type-1) (n ,type-2)
                                        startr1 endr1 startc1 endc1
                                        startr2 endr2 startc2 endc2)
         (declare (optimize (speed 3) (safety 0))
                  (type fixnum startr1 endr1 startc1 endc1
                        startr2 endr2 startc2 endc2))
         (with-typed-matrix-vals (m ,element-type-1 a)
           (with-typed-matrix-vals (n ,element-type-2 b)
             (do ((i startr1 (1+ i))
                  (k startr2 (1+ k)))
                 ((or (>= i endr1) (>= k endr2)))
               (declare (dynamic-extent i k) (type fixnum i k))
               (do ((j startc1 (1+ j))
                    (l startc2 (1+ l)))
                   ((or (>= j endc1) (>= l endc2)))
                 (declare (dynamic-extent j l) (type fixnum j l))
                 (setf (aref b k l)
                       (maybe-truncate
                        (aref a i j)
                        ,element-type-1 ,element-type-2))))))
         n)

       (defmethod matrix-move-range-2d-constrain ((m ,type-1) (n ,type-2) 
                                                  startr1 endr1 startc1 endc1
                                                  startr2 endr2 startc2 endc2)
         (with-matrix-vals (m ,element-type-1 a)
           (with-matrix-vals (n ,element-type-2 b)
             (do ((i startr1 (1+ i))
                  (k startr2 (1+ k)))
                 ((or (>= i endr1) (>= k endr2)))
               (declare (dynamic-extent i k) (type fixnum i k))
               (do ((j startc1 (1+ j))
                    (l startc2 (1+ l)))
                   ((or (>= j endc1) (>= l endc2)))
                 (declare (dynamic-extent j l) (type fixnum j l))
                 (setf (aref b k l) ,(if (eql element-type-1 element-type-2)
                                         `(constrain ,min (aref a i j) ,max)
                                         `(maybe-truncate (constrain ,min (aref a i j) ,max)
                                                          ,element-type-1 ,element-type-2)))))))
         n)

       (defmethod matrix-move ((m ,type-1) (n ,type-2) &key constrain)
         (destructuring-bind (mr mc) (dim m)
           (cond (constrain
                  (matrix-move-range-2d-constrain m n
                                                  0 mr 0 mc
                                                  0 mr 0 mc))
                 (t
                  (matrix-move-range-2d m n
                                        0 mr 0 mc
                                        0 mr 0 mc))))))))

(macrolet ((frob (type-1 type-2)
             `(progn
                (def-move-element ,type-1 ,type-2)
                (def-matrix-move ,type-1 ,type-2))))
  
  (frob double-float-matrix double-float-matrix)
  (frob double-float-matrix single-float-matrix)
  (frob double-float-matrix ub8-matrix)
  (frob double-float-matrix ub16-matrix)
  (frob double-float-matrix ub32-matrix)
  (frob double-float-matrix sb8-matrix)
  (frob double-float-matrix sb16-matrix)
  (frob double-float-matrix sb32-matrix)
  (frob double-float-matrix bit-matrix)
  (frob double-float-matrix fixnum-matrix)
  (frob double-float-matrix real-matrix)
  (frob double-float-matrix complex-matrix)
  (frob double-float-matrix number-matrix)

  (frob single-float-matrix single-float-matrix)
  (frob single-float-matrix ub8-matrix)
  (frob single-float-matrix ub16-matrix)
  (frob single-float-matrix ub32-matrix)
  (frob single-float-matrix sb8-matrix)
  (frob single-float-matrix sb16-matrix)
  (frob single-float-matrix sb32-matrix)
  (frob single-float-matrix bit-matrix)
  (frob single-float-matrix fixnum-matrix)
  (frob single-float-matrix real-matrix)
  (frob single-float-matrix complex-matrix)
  (frob single-float-matrix number-matrix)

  (frob ub8-matrix ub8-matrix)
  (frob ub16-matrix ub16-matrix)
  (frob ub32-matrix ub32-matrix)

  (frob ub8-matrix bit-matrix)
  (frob ub16-matrix bit-matrix)
  (frob ub32-matrix bit-matrix)

  (frob sb8-matrix bit-matrix)
  (frob sb16-matrix bit-matrix)
  (frob sb32-matrix bit-matrix)
  
  (frob sb32-matrix ub8-matrix)
  (frob sb32-matrix ub16-matrix))

(macrolet ((frob (type-1 type-2)
             `(progn
                (def-move-element ,type-1 ,type-2)
                (def-matrix-move ,type-1 ,type-2))))

  (frob single-float-matrix double-float-matrix)

  (frob ub8-matrix double-float-matrix)
  (frob ub8-matrix single-float-matrix)

  (frob ub16-matrix double-float-matrix)
  (frob ub16-matrix single-float-matrix)

  (frob ub32-matrix double-float-matrix)
  (frob ub32-matrix single-float-matrix)

  (frob sb8-matrix double-float-matrix)
  (frob sb8-matrix single-float-matrix)

  (frob sb16-matrix double-float-matrix)
  (frob sb16-matrix single-float-matrix)

  (frob sb32-matrix double-float-matrix)
  (frob sb32-matrix single-float-matrix)

  (frob fixnum-matrix double-float-matrix)
  (frob fixnum-matrix single-float-matrix)

  (frob bit-matrix double-float-matrix)
  (frob bit-matrix single-float-matrix)
  (frob bit-matrix ub8-matrix)
  (frob bit-matrix ub16-matrix)
  (frob bit-matrix ub32-matrix)
  (frob bit-matrix sb8-matrix)
  (frob bit-matrix sb16-matrix)
  (frob bit-matrix sb32-matrix))

(macrolet ((frob (type-1 type-2)
             `(progn
                (def-move-element ,type-1 ,type-2)
                (def-matrix-move ,type-1 ,type-2))))

  (frob complex-matrix real-matrix)
  (frob complex-matrix complex-matrix)

  (frob real-matrix ub8-matrix)
  (frob real-matrix real-matrix)
  (frob real-matrix complex-matrix)
  (frob real-matrix double-float-matrix)
  (frob real-matrix single-float-matrix))
  
