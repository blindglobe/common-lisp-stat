
(in-package :clem)

(defmethod normalize ((u matrix) &key (normin) (normax) (truncate nil) (copy nil))
  (let ((min (min-val u))
	(max (max-val u))
	(nmin (if normin normin 0))
	(nmax (if normax normax 255))
        (u (if copy (mat-copy u) u)))
    (let ((slope (if (= max min)
                     0
                     (/ (- nmax nmin) (- max min)))))
      (map-set-val-fit u #'(lambda (x) (+ nmin (* slope (- x min))))
		       :truncate truncate))
    u))

(defmethod norm-0-255 ((u matrix) &key copy)
  (normalize u :normin 0 :normax 255 :copy copy))

(defmethod norm-0-1 ((u matrix) &key copy)
  (normalize u :normin 0 :normax 1 :copy copy))

