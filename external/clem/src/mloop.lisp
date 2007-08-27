;;; -*- Mode: lisp; outline-regexp: ";;;;;*"; indent-tabs-mode: nil -*-;;;
;;;
;;; file: mloop.cl
;;; author: cyrus harmon
;;;

;;; macros for looping over matrices and doing operations with the benefit
;;; of type declarations

(in-package :clem)

(defun parse-mloop-vars (vars)
  (apply #'mapcar (cons #'list vars)))

(defmacro mloop ((mspecs m n i j) &body body)
  (destructuring-bind (matrices types vars)
      (parse-mloop-vars mspecs)
    `(destructuring-bind (,m ,n) (clem:dim ,(car matrices))
       (declare (type fixnum ,m ,n))
       (let (,@(loop for var in vars and matrix in matrices
                  collect
                    `(,var (clem::matrix-vals ,matrix))))
         ,@(loop for type in types and var in vars
              collect
                `(declare (type (simple-array ,type *) ,var)))
         (dotimes (,i ,m)
           (declare (type fixnum ,i))
           (dotimes (,j ,n)
             (declare (type fixnum ,j))
             ,@body))))))

(defmacro mloop-range ((mspecs startr endr startc endc i j) &body body)
  (destructuring-bind (matrices types vars)
      (parse-mloop-vars mspecs)
    `(let (,@(loop for var in vars and matrix in matrices
                collect
                  `(,var (clem::matrix-vals ,matrix))))
       ,@(loop for type in types and var in vars
            collect
              `(declare (type (simple-array ,type *) ,var)))
       (do ((,i ,startr (1+ ,i)))
           ((> ,i ,endr))
         (declare (type fixnum ,i))
         (do ((,j ,startc (1+ ,j)))
             ((> ,j ,endc))
           (declare (type fixnum ,j))
           ,@body)))))


