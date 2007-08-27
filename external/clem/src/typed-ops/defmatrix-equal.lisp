;;; -*- Mode: lisp; outline-regexp: ";;;;;*"; indent-tabs-mode: nil -*-;;;
;;;
;;; file: defmatrix-equal.cl
;;; author: cyrus harmon
;;;

(in-package :clem)

(defmacro def-matrix-equal (type-1 type-2 &key suffix)
  (let ((element-type-1 (element-type (find-class `,type-1)))
	(element-type-2 (element-type (find-class `,type-2))))
    `(progn
       (defmethod ,(ch-util:make-intern (concatenate 'string "mat-equal-range" suffix))
	   ((m ,type-1) (n ,type-2) startr endr startc endc)
         (let ((equal t))
           (clem::mloop-range (((m ,element-type-1 a)
                                (n ,element-type-2 b))
                               startr endr startc endc i j)
             (setf equal (and equal (= (aref a i j) (aref b i j)))))
           equal))
       
       (defmethod ,(ch-util:make-intern (concatenate 'string "mat-equal" suffix))
	   ((m ,type-1) (n ,type-2))
	 (destructuring-bind (mr mc) (dim m)
	   (,(ch-util:make-intern (concatenate 'string "mat-equal-range" suffix)) m n 0 (1- mr) 0 (1- mc)))))))

(macrolet ((frob (type-1 type-2 &key suffix)
	     `(progn
		(def-matrix-equal ,type-1 ,type-2 :suffix ,suffix))))
  
  (frob double-float-matrix double-float-matrix)
  (frob single-float-matrix single-float-matrix)
  
  (frob ub8-matrix ub8-matrix)
  (frob ub16-matrix ub16-matrix)
  (frob ub32-matrix ub32-matrix)
  
  (frob sb8-matrix sb8-matrix)
  (frob sb16-matrix sb16-matrix)
  (frob sb32-matrix sb32-matrix)
  
  (frob bit-matrix bit-matrix)
  
  (frob real-matrix real-matrix)
  (frob integer-matrix integer-matrix)
   
  (frob complex-matrix complex-matrix))

