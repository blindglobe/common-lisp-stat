;;; subtr.lisp
;;; macros, functions and methods for matrix element access
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

;;; first the slow versions

(defmethod mat-subtr ((m matrix) (n matrix) &key in-place result-type)
  ;;; FIXME how about some sanity check on the args here please?
  (if in-place
      (progn
        (loop for i from 0 below (matrix-total-size m)
           do (setf (row-major-mref m i)
                    (- (row-major-mref m i)
                       (row-major-mref n i))))
        m)
      (progn
        (let ((p (if result-type
                     (make-instance result-type :dimensions (matrix-dimensions m))
                     (mat-copy-proto m))))
          (loop for i from 0 below (matrix-total-size m)
             do (setf (row-major-mref p i)
                      (- (row-major-mref m i)
                         (row-major-mref n i))))
          p))))

(defmethod mat-subtr ((m number) (n matrix) &key in-place result-type)
  (if in-place
      `(error 'matrix-argument-error
              :format-control
              "in-place operation not allowed (~S of ~S and ~S"
              :format-arguments (list '- m n))
      (progn
        (let ((p (if result-type
                     (make-instance result-type :dimensions (matrix-dimensions n))
                     (mat-copy-proto n))))
          (loop for i from 0 below (matrix-total-size n)
             do (setf (row-major-mref p i)
                      (- m
                         (row-major-mref n i))))
          p))))

(defmethod mat-subtr ((m matrix) (n number) &key in-place result-type)
  (if in-place
      (progn
        (loop for i from 0 below (matrix-total-size m)
           do (setf (row-major-mref m i)
                    (- (row-major-mref m i) n)))
        m)
      (progn
        (let ((p (if result-type
                     (make-instance result-type :dimensions (matrix-dimensions m))
                     (mat-copy-proto m))))
          (loop for i from 0 below (matrix-total-size m)
             do (setf (row-major-mref p i)
                      (- (row-major-mref m i) n)))
          p))))


(defgeneric %get-subtr-matrix-class (a b))
(defgeneric mat-subtr-range3 (m n p startr endr startc endc))

(defmacro def-matrix-subtr (type-1 type-2 accumulator-type &key suffix)
  (let ((element-type-1 (element-type (find-class `,type-1)))
	(element-type-2 (element-type (find-class `,type-2)))
	(accumulator-element-type (element-type (find-class `,accumulator-type))))
    `(progn

       (defmethod %get-subtr-matrix-class ((a ,type-1) (b ,type-2))
         ',accumulator-type)
       
       (defmethod ,(ch-util:make-intern (concatenate 'string "mat-subtr-range3" suffix))
	   ((m ,type-1) (n ,type-2) (p ,accumulator-type) startr endr startc endc)
         (with-matrix-vals (m ,element-type-1 a)
           (with-matrix-vals (n ,element-type-2 b)
             (with-matrix-vals (p ,accumulator-element-type c)
               (do ((i startr (1+ i)))
                   ((> i endr))
                 (declare (dynamic-extent i) (type fixnum i))
                 (do ((j startc (1+ j)))
                     ((> j endc))
                   (declare (dynamic-extent j) (type fixnum j))
                   (setf (aref c i j)
                         (- (aref a i j) (aref b i j))))))))
         p))))

(macrolet ((frob (type-1 type-2 type-3 &key suffix)
	     `(progn
		(def-matrix-subtr ,type-1 ,type-2 ,type-3 :suffix ,suffix))))
  
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
  (frob ub8-matrix ub8-matrix sb16-matrix)
  (frob ub16-matrix ub16-matrix ub16-matrix)
  (frob ub16-matrix ub16-matrix sb32-matrix)
  (frob ub32-matrix ub32-matrix ub32-matrix)
  (frob ub32-matrix ub32-matrix sb32-matrix)

  (frob sb8-matrix sb8-matrix sb8-matrix)
  (frob sb8-matrix sb8-matrix sb16-matrix)
  (frob sb16-matrix sb16-matrix sb32-matrix)
  (frob sb16-matrix sb16-matrix sb32-matrix)
  (frob sb32-matrix sb32-matrix sb32-matrix)
  (frob sb32-matrix sb32-matrix sb32-matrix)

  (frob ub8-matrix bit-matrix ub8-matrix)
  (frob ub16-matrix bit-matrix ub16-matrix)
  (frob ub32-matrix bit-matrix ub32-matrix)

  (frob sb8-matrix bit-matrix sb8-matrix)
  (frob sb8-matrix bit-matrix sb16-matrix)
  (frob sb16-matrix bit-matrix sb16-matrix)
  (frob sb32-matrix bit-matrix sb32-matrix)
  
  (frob sb32-matrix ub8-matrix sb32-matrix)
  (frob sb32-matrix ub16-matrix sb32-matrix)
  
  (frob single-float-matrix double-float-matrix double-float-matrix)

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
  (frob bit-matrix double-float-matrix double-float-matrix)
  (frob bit-matrix single-float-matrix single-float-matrix)
  (frob bit-matrix bit-matrix bit-matrix))

(defgeneric mat-subtr-range (m n start endr startc endc &key in-place result-type))

(defmethod mat-subtr-range ((m typed-mixin) (n typed-mixin) startr endr startc endc &key in-place
                            (result-type (%get-subtr-matrix-class m n)))
  (destructuring-bind (mr mc) (dim m)
    (if in-place
        (mat-subtr-range3 m n m startr endr startc endc)
        (let ((p (make-instance result-type :rows mr :cols mc)))
          (mat-subtr-range3 m n p startr endr startc endc)))))

(defmethod mat-subtr :around ((m matrix) (n matrix)
                              &key (in-place nil in-place-supplied-p)
                              (result-type (%get-subtr-matrix-class m n)))
  (if (compute-applicable-methods #'mat-subtr-range (list m n 0 0 0 0))
      (destructuring-bind (mr mc) (dim m)
        (apply #'mat-subtr-range
               m n 0 (1- mr) 0 (1- mc) :result-type result-type
               (when in-place-supplied-p `(:in-place ,in-place))))
      (call-next-method)))

