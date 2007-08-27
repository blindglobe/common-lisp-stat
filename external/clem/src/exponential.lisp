
(in-package :clem)

(defmethod mat-square ((u matrix))
  (map-set-val-copy u #'(lambda (x) (* x x))))

(defmethod mat-square! ((u matrix))
  (map-set-val u #'(lambda (x) (* x x))))

(defmethod mat-sqrt ((u matrix))
  (map-set-val-copy u #'(lambda (x) (sqrt x))))

(defmethod mat-sqrt! ((u matrix))
  (map-set-val u #'(lambda (x) (sqrt x))))

(defmacro def-matrix-square (matrix-type)
  (let ((element-type (element-type (find-class `,matrix-type))))
    `(progn
      (defmethod mat-square! ((u ,matrix-type))
        (declare (optimize (speed 3)
                           (safety 0)))
        (destructuring-bind (rows cols) (mapcar #'1- (dim u))
          (declare (type fixnum rows cols))
          (with-typed-map-range u ,element-type 0 rows 0 cols (a i j)
            (let ((val (aref a i j)))
              (declare (type ,element-type val))
              (setf (aref a i j) (* val val)))))
        u)
      
      (defmethod mat-sqrt! ((u ,matrix-type))
        (destructuring-bind (rows cols) (mapcar #'1- (dim u))
          (declare (type fixnum rows cols))
          (with-map-range u ,element-type 0 rows 0 cols (a i j)
            (setf (aref a i j) (sqrt (aref a i j)))))
        u))))

(macrolet ((frob (type-1)
	     `(def-matrix-square ,type-1)))
  (frob double-float-matrix)
  (frob single-float-matrix))
