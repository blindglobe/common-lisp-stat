;;; add.lisp
;;; macros, functions and methods for matrix addition
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

;;; slow version
(defmethod mat-add ((a matrix) (b matrix) &key in-place)
  (if in-place
      (error "not yet supported")
      (mat-scalar-op a b #'+)))

;;; faster version
(defmacro def-matrix-add-range (type-1 type-2 accumulator-type &key suffix)
  (let ((element-type-1 (element-type (find-class `,type-1)))
	(element-type-2 (element-type (find-class `,type-2)))
	(accumulator-element-type (element-type (find-class `,accumulator-type))))
    `(progn
       (defmethod ,(ch-util:make-intern (concatenate 'string "mat-add-range" suffix))
	   ((m ,type-1) (n ,type-2) startr endr startc endc &key in-place)
	 (destructuring-bind (mr mc) (dim m)
           (if in-place
               (progn
                 (clem::mloop-range (((m ,element-type-1 a)
                                      (n ,element-type-2 b))
                                     startr endr startc endc i j)
                   (setf (aref a i j) (+ (aref a i j) (aref b i j))))
                 m)
               (let ((p (make-instance ',accumulator-type :rows mr :cols mc)))
                 (clem::mloop-range (((m ,element-type-1 a)
                                      (n ,element-type-2 b)
                                      (p ,accumulator-element-type c))
                                     startr endr startc endc i j)
                   (setf (aref c i j) (+ (aref a i j) (aref b i j))))
                 p)))))))


(macrolet ((frob (type-1 type-2 type-3 &key suffix)
	     `(progn
                (def-binary-op "mat-add" + ,type-1 ,type-2 ,type-3)
		(def-matrix-add-range ,type-1 ,type-2 ,type-3 :suffix ,suffix))))
  
  (frob double-float-matrix double-float-matrix double-float-matrix)
  (frob double-float-matrix single-float-matrix double-float-matrix)
  (frob double-float-matrix ub8-matrix double-float-matrix)
  (frob double-float-matrix ub16-matrix double-float-matrix)
  (frob double-float-matrix ub32-matrix double-float-matrix)
  (frob double-float-matrix sb8-matrix double-float-matrix)
  (frob double-float-matrix sb16-matrix double-float-matrix)
  (frob double-float-matrix sb32-matrix double-float-matrix)
  (frob double-float-matrix bit-matrix double-float-matrix)
  (frob double-float-matrix fixnum-matrix double-float-matrix)
  
  (frob single-float-matrix single-float-matrix single-float-matrix)
  (frob single-float-matrix ub8-matrix single-float-matrix)
  (frob single-float-matrix ub16-matrix single-float-matrix)
  (frob single-float-matrix ub32-matrix single-float-matrix)
  (frob single-float-matrix sb8-matrix single-float-matrix)
  (frob single-float-matrix sb16-matrix single-float-matrix)
  (frob single-float-matrix sb32-matrix single-float-matrix)
  (frob single-float-matrix bit-matrix single-float-matrix)
  (frob single-float-matrix fixnum-matrix single-float-matrix)
  
  (frob ub8-matrix ub8-matrix ub8-matrix)
  (frob ub16-matrix ub16-matrix ub16-matrix)
  (frob ub32-matrix ub8-matrix ub32-matrix)
  (frob ub32-matrix ub32-matrix ub32-matrix)
  
  (frob sb8-matrix sb8-matrix sb16-matrix)
  (frob sb16-matrix sb16-matrix sb32-matrix)
  (frob sb32-matrix sb8-matrix sb32-matrix)
  (frob sb32-matrix sb32-matrix sb32-matrix)
  
  (frob ub8-matrix bit-matrix ub8-matrix)
  (frob ub16-matrix bit-matrix ub16-matrix)
  (frob ub32-matrix bit-matrix ub32-matrix)
  
  (frob sb8-matrix bit-matrix sb8-matrix)
  (frob sb16-matrix bit-matrix sb16-matrix)
  (frob sb32-matrix bit-matrix sb32-matrix)
  
  (frob sb32-matrix ub8-matrix sb32-matrix)
  (frob sb32-matrix ub16-matrix sb32-matrix)
  
  (frob real-matrix real-matrix real-matrix)
  (frob real-matrix double-float-matrix real-matrix)
  (frob real-matrix single-float-matrix real-matrix)
  (frob real-matrix integer-matrix real-matrix)
  
  (frob integer-matrix integer-matrix integer-matrix)
   
  (frob complex-matrix complex-matrix complex-matrix)
  (frob complex-matrix integer-matrix complex-matrix)
  (frob complex-matrix real-matrix complex-matrix))


(macrolet ((frob (type-1 type-2 type-3)
	     `(progn
                (def-binary-op "mat-add" + ,type-1 ,type-2 ,type-3 :allow-in-place nil))))
  (frob single-float-matrix double-float-matrix double-float-matrix)

  (frob bit-matrix bit-matrix ub32-matrix)

  (frob ub8-matrix double-float-matrix double-float-matrix)
  (frob ub8-matrix single-float-matrix single-float-matrix)

  (frob ub16-matrix double-float-matrix double-float-matrix)
  (frob ub16-matrix single-float-matrix single-float-matrix)

  (frob ub32-matrix double-float-matrix double-float-matrix)
  (frob ub32-matrix single-float-matrix single-float-matrix)

  (frob sb8-matrix double-float-matrix double-float-matrix)
  (frob sb8-matrix single-float-matrix single-float-matrix)

  (frob sb16-matrix double-float-matrix double-float-matrix)
  (frob sb16-matrix single-float-matrix single-float-matrix)

  (frob sb32-matrix double-float-matrix double-float-matrix)
  (frob sb32-matrix single-float-matrix single-float-matrix)

  (frob fixnum-matrix double-float-matrix double-float-matrix)
  (frob fixnum-matrix single-float-matrix single-float-matrix)

  (frob bit-matrix double-float-matrix double-float-matrix)
  (frob bit-matrix single-float-matrix single-float-matrix))


(defmacro def-matrix-add-number (type-1 type-2 accumulator-type &key suffix (allow-in-place t))
  (let ((element-type-1 (element-type (find-class `,type-1)))
	(accumulator-element-type (element-type (find-class `,accumulator-type))))
    `(progn
       (defmethod ,(ch-util:make-intern (concatenate 'string "mat-add-range" suffix))
	   ((m ,type-1) (n ,type-2) startr endr startc endc &key in-place)
         (declare (type ,type-2 n))
	 (destructuring-bind (mr mc) (dim m)
           (if in-place
               ,(if allow-in-place
                    `(with-typed-mref (m ,element-type-1)
                      (do ((i startr (1+ i)))
                          ((> i endr))
                        (declare (dynamic-extent i) (type fixnum i))
                        (do ((j startc (1+ j)))
                            ((> j endc))
                          (declare (dynamic-extent j) (type fixnum j))
                          (setf (mref m i j)
                                (+ (mref m i j) n))))
                      m)
                    `(error 'matrix-argument-error
                            :format-control
                            "in-place operation not allowed (~S of ~S and ~S)"
                            :format-arguments (list '+ ',type-1 ',type-2)))
               (let ((p (make-instance ',accumulator-type :rows mr :cols mc)))
                 (with-typed-mref (m ,element-type-1)
                   (with-typed-mref (p ,accumulator-element-type)
                     (do ((i startr (1+ i)))
                         ((> i endr))
                       (declare (dynamic-extent i) (type fixnum i))
                       (do ((j startc (1+ j)))
                           ((> j endc))
                         (declare (dynamic-extent j) (type fixnum j))
                         (setf (mref p i j)
                               (+ (mref m i j) n))))))
                 p))))
       
       (defmethod ,(ch-util:make-intern (concatenate 'string "mat-add" suffix))
	   ((m ,type-1) (n ,type-2) &key in-place)
         (if in-place
             ,(if allow-in-place
                  `(with-typed-mref (m ,element-type-1)
                     (loop for i from 0 below (matrix-total-size m)
                        do (setf (row-major-mref m i)
                                 (+ (row-major-mref m i) n)))
                     m)
                  `(error 'matrix-argument-error
                          :format-control
                          "in-place operation not allowed (~S of ~S and ~S"
                          :format-arguments (list '+ ',type-1 ',type-2)))
             (let ((p (make-instance ',accumulator-type :dimensions (matrix-dimensions m))))
               (with-typed-mref (m ,element-type-1)
                 (with-typed-mref (p ,accumulator-element-type)
                   (loop for i from 0 below (matrix-total-size m)
                      do (setf (row-major-mref p i)
                               (+ (row-major-mref m i) n)))))
               p))))))

(macrolet ((frob (type-1 type-2 type-3 &key suffix)
	     `(progn
		(def-matrix-add-number ,type-1 ,type-2 ,type-3 :suffix ,suffix))))
  (frob double-float-matrix double-float double-float-matrix)
  (frob double-float-matrix single-float double-float-matrix)
  (frob double-float-matrix integer double-float-matrix)

  (frob integer-matrix integer integer-matrix))

(macrolet ((frob (type-1 type-2 type-3 &key suffix)
	     `(progn
		(def-matrix-add-number ,type-1 ,type-2 ,type-3 :suffix ,suffix :allow-in-place nil))))

  (frob ub8-matrix integer integer-matrix)
  (frob ub8-matrix double-float double-float-matrix)
  (frob ub8-matrix single-float single-float-matrix))

(defmacro def-matrix-add-scalar (type-1 type-2 accumulator-type &key suffix)
  (let ((element-type-1 (element-type (find-class `,type-1)))
        (element-type-2 (element-type (find-class `,type-2)))
	(accumulator-element-type (element-type (find-class `,accumulator-type))))
    `(progn
       (defmethod ,(ch-util:make-intern (concatenate 'string "mat-add-range" suffix))
	   ((m ,type-1) (n ,type-2) startr endr startc endc &key in-place)
         (declare (type ,type-2 n))
         (let ((val (clem::scalar-val n)))
           (declare (type ,element-type-2 val))
           (destructuring-bind (mr mc) (dim m)
             (if in-place
                 (progn
                   (with-typed-mref (m ,element-type-1)
                     (do ((i startr (1+ i)))
                         ((> i endr))
                       (declare (dynamic-extent i) (type fixnum i))
                       (do ((j startc (1+ j)))
                           ((> j endc))
                         (declare (dynamic-extent j) (type fixnum j))
                         (setf (mref m i j)
                               (+ (mref m i j) val)))))
                   m)
                 (let ((p (make-instance ',accumulator-type :rows mr :cols mc)))
                   (with-typed-mref (m ,element-type-1)
                     (with-typed-mref (p ,accumulator-element-type)
                       (do ((i startr (1+ i)))
                           ((> i endr))
                         (declare (dynamic-extent i) (type fixnum i))
                         (do ((j startc (1+ j)))
                             ((> j endc))
                           (declare (dynamic-extent j) (type fixnum j))
                           (setf (mref p i j)
                                 (+ (mref m i j) val))))))
                   p)))))
         
       (defmethod ,(ch-util:make-intern (concatenate 'string "mat-add" suffix))
	   ((m ,type-1) (n ,type-2) &key in-place)
         (let ((val (clem::scalar-val n)))
           (declare (type ,element-type-2 val))
           (if in-place
               (with-typed-mref (m ,element-type-1)
                 (loop for i from 0 below (matrix-total-size m)
                    do (setf (row-major-mref m i)
                             (+ (row-major-mref m i) val)))
                 m)
               (let ((p (make-instance ',accumulator-type :dimensions (matrix-dimensions m))))
                 (with-typed-mref (m ,element-type-1)
                   (with-typed-mref (p ,accumulator-element-type)
                     (loop for i from 0 below (matrix-total-size m)
                        do (setf (row-major-mref p i)
                                 (+ (row-major-mref m i) val)))))
                 p)))
	 ))))

(macrolet ((frob (type-1 type-2 type-3 &key suffix)
	     `(progn
                (def-matrix-add-scalar ,type-1 ,type-2 ,type-3 :suffix ,suffix))))
  
  (frob ub8-matrix bit-scalar ub8-matrix)
  (frob ub8-matrix sb8-scalar ub8-matrix)
  (frob ub8-matrix ub8-scalar ub8-matrix))

