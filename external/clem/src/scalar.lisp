
(in-package :clem)

(defclass scalar (matrix) ())

(defun scalar (val &key (matrix-class 'scalar))
  (let ((m (make-instance matrix-class)))
    (scalar-set-val m val)
    m))

(defgeneric scalar-val (s))
(defmethod scalar-val ((s scalar)) (aref (matrix-vals s) 0 0))

(defgeneric scalar-set-val (s v))
(defmethod scalar-set-val ((s scalar) v) (setf (matrix-vals s) v))

(defmethod dim ((s scalar)) '(1 1))
(defmethod val ((s scalar) i j) (declare (ignore i) (ignore j)) (scalar-val s))
(defmethod set-val ((s scalar) i j v &key (coerce t))
  (declare (ignore i) (ignore j))
  (scalar-set-val s (if coerce (coerce v (element-type (class-of s))) v)))


(defclass bit-scalar (bit-matrix scalar)
  ((initial-element :accessor initial-element
                    :initarg :initial-element :initform 0))
  (:metaclass standard-matrix-class)
  (:element-type (unsigned-byte 1))
  (:accumulator-type (unsigned-byte 32))
  (:minval 0)
  (:maxval 1))

(defclass ub8-scalar (ub8-matrix scalar)
  ((initial-element :accessor initial-element
                    :initarg :initial-element :initform 0))
  (:metaclass standard-matrix-class)
  (:element-type (unsigned-byte 8))
  (:accumulator-type (unsigned-byte 32))
  (:minval 0)
  (:maxval #.(- (expt 2 8) 1)))

(defclass sb8-scalar (sb8-matrix scalar)
  ((initial-element :accessor initial-element
                    :initarg :initial-element :initform 0))
  (:metaclass standard-matrix-class)
  (:element-type (signed-byte 8))
  (:accumulator-type (signed-byte 32))
  (:minval #.(- (expt 2 7)))
  (:maxval #.(- (expt 2 7) 1)))

