;;; abs.lisp
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

(defmethod mabs ((u matrix))
  (map-set-val-copy u #'(lambda (x) (abs x))))

;;; faster version

(defmacro def-matrix-abs (type-1 accumulator-type &key suffix)
  (let ((element-type-1 (element-type (find-class `,type-1)))
	(accumulator-element-type (element-type (find-class `,accumulator-type))))
    `(progn
       (defmethod ,(ch-util:make-intern (concatenate 'string "mat-abs-range" suffix))
	   ((m ,type-1) startr endr startc endc)
         (destructuring-bind (mr mc) (dim m)
           (let ((p (make-instance ',accumulator-type :rows mr :cols mc)))
             (with-matrix-vals (m ,element-type-1 a)
               (with-matrix-vals (p ,accumulator-element-type c)
                 (do ((i startr (1+ i)))
                     ((> i endr))
                   (declare (dynamic-extent i) (type fixnum i))
                   (do ((j startc (1+ j)))
                       ((> j endc))
                     (declare (dynamic-extent j) (type fixnum j))
                     (setf (aref c i j)
                           (abs (aref a i j)))))))
             p)))
       
       (defmethod ,(ch-util:make-intern (concatenate 'string "mat-abs" suffix))
	   ((m ,type-1))
	 (destructuring-bind (mr mc) (dim m)
	   (,(ch-util:make-intern (concatenate 'string "mat-abs-range" suffix))
             m 0 (1- mr) 0 (1- mc)))))))

(defmacro def-matrix-abs! (type-1 &key suffix)
  (let ((element-type-1 (element-type (find-class `,type-1))))
    `(progn
       (defmethod ,(ch-util:make-intern (concatenate 'string "mat-abs-range!" suffix))
	   ((m ,type-1) startr endr startc endc)
         (with-matrix-vals (m ,element-type-1 a)
           (do ((i startr (1+ i)))
               ((> i endr))
             (declare (dynamic-extent i) (type fixnum i))
             (do ((j startc (1+ j)))
                 ((> j endc))
               (declare (dynamic-extent j) (type fixnum j))
               (setf (aref a i j) (abs (aref a i j))))))
         m)
       
       (defmethod ,(ch-util:make-intern (concatenate 'string "mat-abs!" suffix))
	   ((m ,type-1))
	 (destructuring-bind (mr mc) (dim m)
	   (,(ch-util:make-intern (concatenate 'string "mat-abs-range!" suffix))
             m 0 (1- mr) 0 (1- mc)))))))

(macrolet ((frob (type-1 type-2 &key suffix)
	     `(progn
		(def-matrix-abs ,type-1 ,type-2 :suffix ,suffix)
		(def-matrix-abs! ,type-1 :suffix ,suffix))))
  (frob double-float-matrix double-float-matrix)
  (frob single-float-matrix single-float-matrix)
  (frob bit-matrix bit-matrix)
  (frob integer-matrix integer-matrix)
  (frob t-matrix t-matrix))

(macrolet ((frob (type-1 type-2 &key suffix)
	     `(progn
		(def-matrix-abs ,type-1 ,type-2 :suffix ,suffix))))
  (frob ub8-matrix ub8-matrix)
  (frob ub16-matrix ub16-matrix)
  (frob ub32-matrix ub32-matrix)
  (frob sb8-matrix sb8-matrix)
  (frob sb16-matrix sb16-matrix)
  (frob sb32-matrix sb32-matrix)
  (frob fixnum-matrix fixnum-matrix)
  (frob real-matrix real-matrix)
  (frob complex-matrix real-matrix)
  (frob number-matrix real-matrix)
  (frob t-matrix t-matrix))

