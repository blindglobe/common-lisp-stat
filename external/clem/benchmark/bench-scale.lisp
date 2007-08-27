
(in-package :clem-benchmark)

;;; mat-scale double float benchmarks
(let ((m (make-instance 'double-float-matrix :dimensions '(1024 1024) :initial-element 1d0)))
  (with-matrix-benchmark (:scale-1024-1024/double-float)
    (mat-scale m 2.0d0)))

(let ((m (make-instance 'double-float-matrix :dimensions '(1024 1024) :initial-element 1d0)))
  (with-matrix-benchmark (:scale-in-place-1024-1024/double-float)
    (mat-scale m 2.0d0 :in-place t)))

;;; mat-scale single float benchmarks
(let ((m (make-instance 'single-float-matrix :dimensions '(1024 1024) :initial-element 1s0)))
  (with-matrix-benchmark (:scale-1024-1024/single-float)
    (mat-scale m 2.0s0)))

(let ((m (make-instance 'single-float-matrix :dimensions '(1024 1024) :initial-element 1s0)))
  (with-matrix-benchmark (:scale-in-place-1024-1024/single-float)
    (mat-scale m 2.0s0 :in-place t)))

;;; mat-scale sb8 benchmarks
(let ((m (make-instance 'sb8-matrix :dimensions '(1024 1024) :initial-element 1)))
  (with-matrix-benchmark (:scale-1024-1024/sb8)
    (mat-scale m 2)))

(let ((m (make-instance 'sb8-matrix :dimensions '(1024 1024) :initial-element 1)))
  (with-matrix-benchmark (:scale-in-place-1024-1024/sb8)
    (mat-scale m 2 :in-place t)))

;;; mat-scale sb16 benchmarks
(let ((m (make-instance 'sb16-matrix :dimensions '(1024 1024) :initial-element 1)))
  (with-matrix-benchmark (:scale-1024-1024/sb16)
    (mat-scale m 2)))

(let ((m (make-instance 'sb16-matrix :dimensions '(1024 1024) :initial-element 1)))
  (with-matrix-benchmark (:scale-in-place-1024-1024/sb16)
    (mat-scale m 2 :in-place t)))


;;; mat-scale sb32 benchmarks
(let ((m (make-instance 'sb32-matrix :dimensions '(1024 1024) :initial-element 1)))
  (with-matrix-benchmark (:scale-1024-1024/sb32)
    (mat-scale m 2)))

(let ((m (make-instance 'sb32-matrix :dimensions '(1024 1024) :initial-element 1)))
  (with-matrix-benchmark (:scale-in-place-1024-1024/sb32)
    (mat-scale m 2 :in-place t)))


;;; mat-scale ub8 benchmarks
(let ((m (make-instance 'ub8-matrix :dimensions '(1024 1024) :initial-element 1)))
  (with-matrix-benchmark (:scale-1024-1024/ub8)
    (mat-scale m 2)))

(let ((m (make-instance 'ub8-matrix :dimensions '(1024 1024) :initial-element 1)))
  (with-matrix-benchmark (:scale-in-place-1024-1024/ub8)
    (clem::mat-scale m 2 :in-place t)))


;;; mat-scale ub16 benchmarks
(let ((m (make-instance 'ub16-matrix :dimensions '(1024 1024) :initial-element 1)))
  (with-matrix-benchmark (:scale-1024-1024/ub16)
    (mat-scale m 2)))

(let ((m (make-instance 'ub16-matrix :dimensions '(1024 1024) :initial-element 1)))
  (with-matrix-benchmark (:scale-in-place-1024-1024/ub16)
    (clem::mat-scale m 2 :in-place t)))


;;; mat-scale ub32 benchmarks
(let ((m (make-instance 'ub32-matrix :dimensions '(1024 1024) :initial-element 1)))
  (with-matrix-benchmark (:scale-1024-1024/ub32)
    (mat-scale m 2)))

(let ((m (make-instance 'ub32-matrix :dimensions '(1024 1024) :initial-element 1)))
  (with-matrix-benchmark (:scale-in-place-1024-1024/ub32)
    (clem::mat-scale m 2 :in-place t)))
