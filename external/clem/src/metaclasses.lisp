
(in-package :clem)

;;; Taken from KMR's clsql package
(defun remove-keyword-arg (arglist akey)
  (let ((mylist arglist)
	(newlist ()))
    (labels ((pop-arg (alist)
	       (let ((arg (pop alist))
		     (val (pop alist)))
		 (unless (equal arg akey)
		   (setf newlist (append (list arg val) newlist)))
		 (when alist (pop-arg alist)))))
      (pop-arg mylist))
    newlist))

;;; Also taken from KMR's clsql package
(declaim (inline delistify-dsd))
(defun delistify-dsd (list)
  "Some MOPs, like openmcl 0.14.2, cons attribute values in a list."
  (if (and (listp list) (null (cdr list)))
      (car list)
      list))

;;; Taken from util so I don't need to include it here
(defun insert-before (new old list)
  (labels ((build-list (old c &optional newlist)
	     (if c
		 (if (eq old (car c))
		     (append (reverse (cdr c)) (cons (car c) (cons new newlist)))
		     (build-list old (cdr c) (cons (car c) newlist)))
		 (cons new newlist))))
    (reverse (build-list old list))))


(defun fill-slot-from-ancestor (slot class)
  (let ((ancestor (find-if #'(lambda (anc)
			       (when (slot-exists-p anc slot)
				 (slot-boundp anc slot)))
			   (cdr (compute-class-precedence-list class)))))
    (when ancestor
      (setf (slot-value class slot) (slot-value ancestor slot)))))

(defun fill-standard-matrix-class-slots-from-ancestors (class &rest all-keys)
  (mapcar #'(lambda (x)
              (let ((name (slot-definition-name x))
                    (initargs (slot-definition-initargs x)))
                (unless (getf (car all-keys) (car initargs))
                  (fill-slot-from-ancestor name class))))
          (standard-matrix-class-slots class)))


;;; NOTE: don't use accessors here as they will return a list!
;;;       at least on SBCL
(defclass standard-matrix-class (standard-class)
  ((element-type :initarg :element-type)
   (accumulator-type :initarg :accumulator-type)
   (specialized-array :initarg :specialized-array :initform nil)
   (val-format :initarg :val-format :initform nil)
   (minval :initarg :minval)
   (maxval :initarg :maxval)))

(let ((smc (find-class 'standard-matrix-class)))
  (defun standard-matrix-class-p (class)
    (subtypep (class-of class) smc)))

(defun standard-matrix-class-precedence-list (class)
  (remove-if-not
   #'(lambda (x) (standard-matrix-class-p x))
   (class-precedence-list class)))

(defun standard-matrix-class-slots (class)
  (let ((slots) (slot-names))
    (mapcar #'(lambda (x)
                (mapcar #'(lambda (y)
                            (unless (member (slot-definition-name y)
                                            slot-names)
                              (push y slots)
                              (push (slot-definition-name y)
                                    slot-names)))
                        (class-direct-slots (class-of x))))
            (standard-matrix-class-precedence-list class))
    slots))

(defgeneric element-type (smc)
  (:documentation "the type of the elements of instances
of this matrix class"))
(defmethod element-type ((smc standard-matrix-class))
  (car (slot-value smc 'element-type)))

(defgeneric accumulator-type (smc)
  (:documentation "the type of the result of various mathematical
opreations on instances of this matrix class. needs work."))
(defmethod accumulator-type ((smc standard-matrix-class))
  (car (slot-value smc 'accumulator-type)))

;;; FIXME! This is a hack to get around the fact that
;;; if we have a say, integer-matrix class, we can't
;;; make certain declarations. this needs to be fixed
;;; and hopefully removed
(defgeneric specialized-array-p (smc))
(defmethod specialized-array-p ((smc standard-matrix-class))
  (car (slot-value smc 'specialized-array)))

(defgeneric val-format (smc)
  (:documentation "the format string used to print out
element values of instances of this matrix class"))
(defmethod val-format ((smc standard-matrix-class))
  (car (slot-value smc 'val-format)))

;;; FIXME this name is _way_ too close to min-val. Should
;;; be something like min-allowed-value or some such.
;;; also should be enforced more places if we're going to
;;; really use this!
(defgeneric minval (smc)
  (:documentation "the minimum value allowed by instances
of this matrix class."))
(defmethod minval ((smc standard-matrix-class))
  (car (slot-value smc 'minval)))

;;; FIXME this name is _way_ too close to max-val. Should
;;; be something like max-allowed-value or some such.
;;; also should be enforced more places if we're going to
;;; really use this!
(defgeneric maxval (smc)
  (:documentation "the maximum value allowed by instances
of this matrix class."))
(defmethod maxval ((smc standard-matrix-class))
  (car (slot-value smc 'maxval)))


;;;
;;; Need validate-superclass for some reason. Read AMOP and fix this note
;;;
(defmethod validate-superclass ((c1 standard-matrix-class) (c2 standard-class))
  t)

(defmethod validate-superclass ((c1 standard-class) (c2 standard-matrix-class))
  t)

(defun add-root-class (root-class direct-superclasses)
  (if (member root-class direct-superclasses)
      direct-superclasses
      (insert-before root-class
		     (car (class-direct-superclasses root-class))
		     direct-superclasses)))

(defclass typed-mixin ()
  ((specialzied-array :allocation :class :accessor specialized-array-p :initform nil)))

;;; FIXME this needs work
(defgeneric set-val-fit (m i j v &key truncate))
(defmethod set-val-fit ((m typed-mixin) i j v &key (truncate nil))
  (set-val m i j (if truncate (truncate v) v)))

(defgeneric map-matrix-fit (f a))
(defmethod map-matrix-fit (f (a typed-mixin))
  (destructuring-bind (m n) (dim a)
    (dotimes (i m)
      (dotimes (j n)
	(set-val-fit a i j (funcall f a i j)))))
  a)

(defmethod initialize-instance :around
    ((class standard-matrix-class) &rest all-keys &key direct-superclasses &allow-other-keys)
  (let ((root-class (find-class 'typed-mixin))
	(mc (find-class 'standard-matrix-class)))
    (if (and root-class (not (equal class root-class)))
	(if (member-if #'(lambda (super)
			   (eq (class-of super) mc)) direct-superclasses)
	    (call-next-method)
            (apply #'call-next-method class
                   :direct-superclasses
                   (add-root-class root-class direct-superclasses)
                   (remove-keyword-arg all-keys :direct-superclasses)))
	(call-next-method)))
  (finalize-inheritance class)
  (fill-standard-matrix-class-slots-from-ancestors class all-keys))

(defmethod reinitialize-instance :around
    ((class standard-matrix-class) &rest all-keys &key direct-superclasses &allow-other-keys)
  (let ((root-class (find-class 'typed-mixin))
	(mc (find-class 'standard-matrix-class)))
    (if (and root-class (not (equal class root-class)))
	(if (member-if #'(lambda (super)
			   (eq (class-of super) mc)) direct-superclasses)
	    (call-next-method)
	    (apply #'call-next-method class
		   :direct-superclasses
		   (add-root-class root-class direct-superclasses)
		   (remove-keyword-arg all-keys :direct-superclasses)))
	(call-next-method)))
  (finalize-inheritance class)
  (fill-standard-matrix-class-slots-from-ancestors class all-keys))

