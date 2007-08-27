
(in-package :clem-test)

(defun convolve-test ()
  (let* ((h (clem:mat-scale (clem:gaussian-kernel 2 1) 1d0))
	 (g (make-instance 'double-float-matrix :rows 512 :cols 512
                           :initial-element 2d0))
	 (m (time (clem:discrete-convolve g h :truncate t))))
    m))

(defun convolve-test-2 ()
  (let* ((g (clem:array->double-float-matrix
             #2A((1 2 3 4 5)
                 (6 7 8 9 10)
                 (11 12 13 14 15))))
         (h (clem:array->double-float-matrix
             #2A((1 2 1)(2 4 2) (1 2 1)))))
    (clem::mat-scale! h (/ (clem:sum h)))
    (print h)
    (let ((m (clem:print-matrix (clem:discrete-convolve g h :truncate t :norm-v nil))))
      m)))

(defun convolve-test-3 ()
  (let* ((g (clem:array->double-float-matrix
             #2A((1 2 3 4 5)
                 (6 7 8 9 10)
                 (11 12 13 14 15))))
         (h1 (clem:array->double-float-matrix
              #2A((1 2 1))))
         (h2 (clem:transpose h1))
         (m (clem:print-matrix
             (clem:discrete-convolve 
              (clem:discrete-convolve g h1 :truncate t :norm-v nil)
              h2 :truncate t :norm-v nil))))
    m))

(defun convolve-test-4 ()
  (let* ((g (clem:array->double-float-matrix
             #2A((1 2 3 4 5)
                 (6 7 8 9 10)
                 (11 12 13 14 15))))
         (hrow (clem:array->double-float-matrix
              #2A((0.1 0.8 0.1))))
         (hcol (clem:transpose hrow))
         (m (clem:print-matrix (clem:separable-discrete-convolve g hcol hrow :truncate t :norm-v nil))))
    m))

(defun convolve-test-5 ()
  (let* ((g (clem:array->ub8-matrix
             #2A((1 2 3 4 5)
                 (6 7 8 9 10)
                 (11 12 13 14 15))))
         (h1 (clem:array->ub8-matrix
              #2A((1 2 1))))
         (h2 (clem:transpose h1))
         (m (clem:print-matrix
             (clem:discrete-convolve 
              (clem:discrete-convolve g h1 :truncate t :norm-v nil)
              h2 :truncate t :norm-v nil))))
    m))

