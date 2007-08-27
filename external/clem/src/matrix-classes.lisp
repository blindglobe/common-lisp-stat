
(in-package :clem)

;;; taken from KMR's clsql package
(declaim (inline delistify))
(defun delistify (list)
  "Some MOPs, like openmcl 0.14.2, cons attribute values in a list."
  (if (and (listp list) (null (cdr list)))
      (car list)
      list))

(defmacro defmatrixclass (type direct-superclasses &key 
			 (element-type)
			 (accumulator-type)
			 (initial-element)
			 minval maxval
			 (val-format))
  (unless direct-superclasses (setf direct-superclasses '(matrix)))
  `(progn
     (defclass ,type ,direct-superclasses
       ((initial-element :accessor initial-element
                         :initarg :initial-element :initform ,initial-element))
       (:metaclass standard-matrix-class)
       ,@(when element-type `((:element-type ,(delistify element-type))))
       ,@(when accumulator-type `((:accumulator-type ,(delistify accumulator-type))))
       ,@(when val-format `((:val-format ,(delistify val-format))))
       ,@(when minval `((:minval ,(if (symbolp minval) (symbol-value minval) minval))))
       ,@(when maxval `((:maxval ,(if (symbolp maxval) (symbol-value minval) maxval)))))))

(defmatrixclass t-matrix ()
  :element-type t
  :accumulator-type t)

(defmatrixclass number-matrix (t-matrix)
  :element-type number
  :accumulator-type number)

(defmatrixclass real-matrix (number-matrix)
  :element-type real
  :accumulator-type real)

(defmatrixclass complex-matrix (number-matrix)
  :element-type complex
  :accumulator-type complex)

(defmatrixclass float-matrix (real-matrix)
  :element-type float
  :accumulator-type float
  :val-format "~4,9F")

(defmatrixclass integer-matrix (real-matrix)
  :element-type integer
  :accumulator-type integer
  :val-format "~d")

(defmatrixclass unsigned-byte-matrix (integer-matrix)
  :element-type (unsigned-byte *)
  :accumulator-type (unsigned-byte *)
  :val-format "~d")

(defmatrixclass bit-matrix (integer-matrix) :element-type (unsigned-byte 1)
               :accumulator-type (signed-byte 32)
               :minval 0
               :maxval 1
               :val-format "~b")

(defmatrixclass ub8-matrix (unsigned-byte-matrix)
  :element-type (unsigned-byte 8)
  :accumulator-type (unsigned-byte 32)
  :minval 0
  :maxval #.(- (expt 2 8) 1))

(defmatrixclass ub16-matrix (unsigned-byte-matrix)
  :element-type (unsigned-byte 16)
  :accumulator-type (unsigned-byte 32)
  :minval 0
  :maxval #.(- (expt 2 16) 1))

(defmatrixclass ub32-matrix (unsigned-byte-matrix)
  :element-type (unsigned-byte 32)
  :accumulator-type (unsigned-byte 32)
  :minval 0
  :maxval #.(- (expt 2 32) 1))

(defmatrixclass sb8-matrix (integer-matrix)
  :element-type (signed-byte 8)
  :accumulator-type (signed-byte 32)
  :minval #.(- (expt 2 7))
  :maxval #.(- (expt 2 7) 1))

(defmatrixclass sb16-matrix (integer-matrix)
  :element-type (signed-byte 16)
  :accumulator-type (signed-byte 32)
  :minval #.(- (expt 2 15))
  :maxval #.(- (expt 2 15) 1))

(defmatrixclass sb32-matrix (integer-matrix)
  :element-type (signed-byte 32)
  :accumulator-type (signed-byte 32)
  :minval #.(- (expt 2 31))
  :maxval #.(- (expt 2 31) 1))

(defmatrixclass fixnum-matrix (integer-matrix)
  :element-type fixnum
  :accumulator-type (unsigned-byte 32)
  :minval most-negative-fixnum
  :maxval most-positive-fixnum)

(defmatrixclass single-float-matrix (float-matrix)
  :element-type single-float
  :accumulator-type single-float
  :initial-element 0f0
  :minval most-negative-single-float
  :maxval most-positive-single-float)

(defmatrixclass double-float-matrix (float-matrix)
  :element-type double-float
  :accumulator-type double-float
  :initial-element 0d0
  :minval most-negative-double-float
  :maxval most-positive-double-float)
