;;; logical-operations.lop
;;; mlogand, mlogior, mlogxor, mlognot and mbitnor
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

;;; mlogand
(def-binary-op "mlogand" logand bit-matrix bit-matrix bit-matrix)
(def-binary-op "mlogand" logand ub8-matrix ub8-matrix ub8-matrix)
(def-binary-op "mlogand" logand ub16-matrix ub16-matrix ub16-matrix)
(def-binary-op "mlogand" logand ub32-matrix ub32-matrix ub32-matrix)
(def-binary-op "mlogand" logand integer-matrix integer-matrix integer-matrix)

;;; mlogior
(def-binary-op "mlogior" logior bit-matrix bit-matrix bit-matrix)
(def-binary-op "mlogior" logior ub8-matrix ub8-matrix ub8-matrix)
(def-binary-op "mlogior" logior ub16-matrix ub16-matrix ub16-matrix)
(def-binary-op "mlogior" logior ub32-matrix ub32-matrix ub32-matrix)
(def-binary-op "mlogior" logior integer-matrix integer-matrix integer-matrix)

;;; mlogxor
(def-binary-op "mlogxor" logxor bit-matrix bit-matrix bit-matrix)
(def-binary-op "mlogxor" logxor ub8-matrix ub8-matrix ub8-matrix)
(def-binary-op "mlogxor" logxor ub16-matrix ub16-matrix ub16-matrix)
(def-binary-op "mlogxor" logxor ub32-matrix ub32-matrix ub32-matrix)
(def-binary-op "mlogxor" logxor integer-matrix integer-matrix integer-matrix)

;;; mlognot
(def-unary-op "mlognot" lognot integer-matrix integer-matrix)
(def-unary-op "mlognot" lognot fixnum-matrix fixnum-matrix)
(def-unary-op "mlognot" lognot sb8-matrix sb8-matrix)
(def-unary-op "mlognot" lognot sb16-matrix sb16-matrix)
(def-unary-op "mlognot" lognot sb32-matrix sb32-matrix)

(defmacro defmbitnor (name type-1 type-2 accumulator-type &key suffix)
  (let ((class-1 (find-class `,type-1))
        (class-2 (find-class `,type-2)))
    (let ((element-type-1 (element-type class-1))
          (element-type-2 (element-type class-2))
          (accumulator-element-type (element-type (find-class `,accumulator-type))))
      (let ((max (max (maxval class-1) (maxval class-2))))
        `(progn
           (defmethod ,(ch-util:make-intern (concatenate 'string name "-range" suffix))
               ((m ,type-1) (n ,type-2) startr endr startc endc)
             (destructuring-bind (mr mc) (dim m)
               (let ((p (make-instance ',accumulator-type :rows mr :cols mc)))
                 (with-typed-mref (m ,element-type-1)
                   (with-typed-mref (n ,element-type-2)
                     (with-typed-mref (p ,accumulator-element-type)
                       (do ((i startr (1+ i)))
                           ((> i endr))
                         (declare (dynamic-extent i) (type fixnum i))
                         (do ((j startc (1+ j)))
                             ((> j endc))
                           (declare (dynamic-extent j) (type fixnum j))
                           (setf (mref p i j)
                                 (logand ,max (lognor (mref m i j) (mref n i j)))))))))
                 p)))
       
           (defmethod ,(ch-util:make-intern (concatenate 'string name suffix))
               ((m ,type-1) (n ,type-2))
             (destructuring-bind (mr mc) (dim m)
               (,(ch-util:make-intern (concatenate 'string name "-range" suffix)) m n 0 (1- mr) 0 (1- mc)))))))))
       
(defmacro defmbitnor! (name type-1 type-2 accumulator-type &key suffix)
  (declare (ignore accumulator-type))
  (let ((class-1 (find-class `,type-1))
        (class-2 (find-class `,type-2)))
    (let ((element-type-1 (element-type class-1))
          (element-type-2 (element-type class-2)))
      (let ((max (max (maxval class-1) (maxval class-2))))
        `(progn
           (defmethod ,(ch-util:make-intern (concatenate 'string name "!-range" suffix))
               ((m ,type-1) (n ,type-2) startr endr startc endc)
             (with-typed-mref (m ,element-type-1)
               (with-typed-mref (n ,element-type-2)
                 (do ((i startr (1+ i)))
                     ((> i endr))
                   (declare (dynamic-extent i) (type fixnum i))
                   (do ((j startc (1+ j)))
                       ((> j endc))
                     (declare (dynamic-extent j) (type fixnum j))
                     (setf (mref m i j)
                           (logand ,max (lognor (mref m i j) (mref n i j)))))))
               m))
       
           (defmethod ,(ch-util:make-intern (concatenate 'string name "!" suffix))
               ((m ,type-1) (n ,type-2))
             (destructuring-bind (mr mc) (dim m)
               (,(ch-util:make-intern (concatenate 'string name "!-range" suffix)) m n 0 (1- mr) 0 (1- mc))))
         
           )))))


(defun bitnor (integer1 integer2 andmask)
  (logand andmask (lognor integer1 integer2)))

(macrolet ((frob (name type-1 type-2 type-3 &key suffix)
	     `(progn
		(defmbitnor ,name ,type-1 ,type-2 ,type-3 :suffix ,suffix)
		(defmbitnor! ,name ,type-1 ,type-2 ,type-3 :suffix ,suffix))))
  ;; mbitnor
  (frob "mbitnor" bit-matrix bit-matrix bit-matrix)
  (frob "mbitnor" ub8-matrix ub8-matrix ub8-matrix)
  (frob "mbitnor" ub16-matrix ub16-matrix ub16-matrix)
  (frob "mbitnor" ub32-matrix ub32-matrix ub32-matrix))

