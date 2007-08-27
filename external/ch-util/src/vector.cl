;;;
;;; vector.cl -- various lisp utilities for vectors
;;;
;;; Author: Cyrus Harmon <ch-lisp@bobobeach.com>
;;;

(in-package :ch-util)

;;; Miscellaneous vector utilities

(defun map-vector (fn v
                   &key
                   (adjustable t)
                   (fill-pointer 0))
  (let* ((n (length v))
         (a (make-array n
                        :adjustable adjustable
                        :fill-pointer fill-pointer)))
    (map-into a fn v)))

