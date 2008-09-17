(asdf:oos 'asdf:load-op 'lispstat)

(in-package :ls-user)

#|
(trace lisp-stat-linalg-data::la-allocate)
(trace lisp-stat-linalg-data::la-put-double)
(trace lisp-stat-linalg-data::la-data-to-matrix)
(trace lisp-stat-linalg-data::la-matrix)
(trace lisp-stat-linalg::lu-solve)
|#

(lu-solve 
 (lu-decomp #2A((2 3 4) (1 2 4) (2 4 5))) 
 #(2 3 4))
;; #(-2.333333333333333 1.3333333333333335 0.6666666666666666)


#|
(dotimes (i 3) 
 (declare (fixnum i))
 (let ((vec (lisp-stat-linalg-data::la-get-pointer mat i)))
   (format t "vec => ~A~%" vec)
   (dotimes (j 3)
     (format t "LA-PUT-DOUBLE ~A <- (~A ~A)~%" vec j (aref data i j))
     (lisp-stat-linalg-data::la-put-double vec j (aref data i j)))))
|#
