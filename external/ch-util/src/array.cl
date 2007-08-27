;;;
;;; array.cl -- various lisp utilities for vectors
;;;
;;; Author: Cyrus Harmon <ch-lisp@bobobeach.com>
;;;

(in-package :ch-util)

(defun copy-array (original-array)
  "returns a copy of original-array where each element contains
the value in original-arary"
  (let ((dim (array-dimensions original-array)))
    (let ((flat-array (make-array (reduce #'* dim) :displaced-to original-array)))
      (let ((flat-copy (copy-seq flat-array)))
        (make-array dim :displaced-to flat-copy)))))

