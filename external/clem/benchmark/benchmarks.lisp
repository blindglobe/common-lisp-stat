
(in-package :clem-benchmark)

(defmacro with-benchmark (&body body)
  (let ((start-var (gensym))
        (end-var (gensym)))
    `(let ((,start-var (get-internal-run-time)))
       (values
        (multiple-value-list
         (progn ,@body))
        (let ((,end-var (get-internal-run-time)))
          (coerce (/ (- ,end-var ,start-var) internal-time-units-per-second)
                  'double-float))))))

(defmacro benchmark-time (benchmark-results)
  `(nth-value 1 ,benchmark-results))

(defparameter *matrix-benchmark-times* (make-hash-table :test 'eql))

(defmacro with-matrix-benchmark ((key) &body body)
  (let ((time-sym (gensym))
        (results-sym (gensym)))
    `(multiple-value-bind (,results-sym ,time-sym)
         (with-benchmark
           (progn ,@body))
       (setf (gethash ,key *matrix-benchmark-times*)
             ,time-sym)
       (values-list ,results-sym))))

(defun get-benchmark-time (key)
  (gethash key *matrix-benchmark-times*))

(defun list-benchmarks ()
  (let ((l))
    (maphash #'(lambda (k v)
                 (push (cons k v) l))
             *matrix-benchmark-times*)
    (nreverse l)))

(defun clear-benchmarks ()
  (clrhash *matrix-benchmark-times*))

