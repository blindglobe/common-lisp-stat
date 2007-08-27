;;;
;;; vector.cl -- various lisp utilities for sequences
;;;
;;; Author: Cyrus Harmon <ch-lisp@bobobeach.com>
;;;

(in-package :ch-util)

(defun max-length (&rest seqs)
  (apply #'max (mapcar #'length seqs)))

(defun seqmin (seq &key key)
  (car (apply #'sort (copy-seq seq) #'<
              (when key `(:key ,key)))))

(defun seqmax (seq &key key)
  (car (apply #'sort (copy-seq seq) #'>
              (when key `(:key ,key)))))
