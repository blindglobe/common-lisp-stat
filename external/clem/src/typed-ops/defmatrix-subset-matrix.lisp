;;; -*- Mode: lisp; outline-regexp: ";;;;;*"; indent-tabs-mode: nil -*-;;;
;;;
;;; file: defmatrix-subset-matrix.cl
;;; author: cyrus harmon
;;;

(in-package :clem)

(defmacro def-matrix-subset-matrix (matrix-type)
  (let ((element-type (element-type (find-class `,matrix-type))))
    `(progn
       (defmethod subset-matrix ((u matrix) startr endr startc endc)
         (destructuring-bind (ur uc) (dim u)
           (cond
             ((and (<= startr endr ur) (<= startc endc uc))
              (let* ((m (1+ (- endr startr)))
                     (n (1+ (- endc startc)))
                     (c (mat-copy-proto-dim u m n)))
                (with-matrix-vals (u ,element-type a)
                  (with-matrix-vals (c ,element-type b)
                    (dotimes (i m)
                      (dotimes (j n)
                        (setf (aref b i j) (aref a (+ i startr) (+ j startc)))))))
                c))
             (t nil)))))))


(macrolet ((frob (type-1)
	     `(def-matrix-subset-matrix ,type-1)))
  (frob double-float-matrix))
