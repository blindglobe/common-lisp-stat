
(in-package :clem)

(defmacro def-unary-op (name op type-1 accumulator-type &key suffix)
  (let ((element-type-1 (element-type (find-class `,type-1)))
	(accumulator-element-type (element-type (find-class `,accumulator-type))))
    `(progn
       (defmethod ,(ch-util:make-intern (concatenate 'string name "-range" suffix))
	   ((m ,type-1)  startr endr startc endc &key in-place)
	 (destructuring-bind (mr mc) (dim m)
           (if in-place
               (with-typed-mref (m ,element-type-1)
                 (do ((i startr (1+ i)))
                     ((> i endr))
                   (declare (dynamic-extent i) (type fixnum i))
                   (do ((j startc (1+ j)))
                       ((> j endc))
                     (declare (dynamic-extent j) (type fixnum j))
                     (setf (mref m i j)
                           (,op (mref m i j)))))
                 m)
               (let ((p (make-instance ',accumulator-type :rows mr :cols mc)))
                 (with-typed-mref (m ,element-type-1)
                   (with-typed-mref (p ,accumulator-element-type)
                     (do ((i startr (1+ i)))
                         ((> i endr))
                       (declare (dynamic-extent i) (type fixnum i))
                       (do ((j startc (1+ j)))
                           ((> j endc))
                         (declare (dynamic-extent j) (type fixnum j))
                         (setf (mref p i j)
                               (,op (mref m i j)))))))
                 p))))
       
       (defmethod ,(ch-util:make-intern (concatenate 'string name suffix))
	   ((m ,type-1) &key in-place)
         (if in-place
             (with-typed-mref (m ,element-type-1)
               (loop for i from 0 below (matrix-total-size m)
                  do (setf (row-major-mref m i)
                           (,op (row-major-mref m i))))
               m)
             (let ((p (make-instance ',accumulator-type :dimensions (matrix-dimensions m))))
               (with-typed-mref (m ,element-type-1)
                 (with-typed-mref (p ,accumulator-element-type)
                   (loop for i from 0 below (matrix-total-size m)
                      do (setf (row-major-mref p i)
                               (,op (row-major-mref m i))))))
               p))))))

(defmacro def-binary-op (name op type-1 type-2 accumulator-type &key suffix (allow-in-place t))
  (let ((element-type-1 (element-type (find-class `,type-1)))
	(element-type-2 (element-type (find-class `,type-2)))
	(accumulator-element-type (element-type (find-class `,accumulator-type))))
    `(progn
       (defmethod ,(ch-util:make-intern (concatenate 'string name "-range" suffix))
	   ((m ,type-1) (n ,type-2) startr endr startc endc &key in-place)
	 (destructuring-bind (mr mc) (dim m)
	   (if in-place
               ,(if allow-in-place
                    `(with-typed-mref (m ,element-type-1)
                       (with-typed-mref (n ,element-type-2)
                         (do ((i startr (1+ i)))
                             ((> i endr))
                           (declare (dynamic-extent i) (type fixnum i))
                           (do ((j startc (1+ j)))
                               ((> j endc))
                             (declare (dynamic-extent j) (type fixnum j))
                             (setf (mref m i j)
                                   (,op (mref m i j) (mref n i j))))))
                       m)
                    `(error 'matrix-argument-error
                            :format-control
                            "in-place operation not allowed (~S of ~S and ~S"
                            :format-arguments (list '+ ',type-1 ',type-2)))
               (let ((p (make-instance ',accumulator-type :rows mr :cols mc)))
                 (with-typed-mref (m ,element-type-1)
                   (with-typed-mref (p ,accumulator-element-type)
                     (with-typed-mref (n ,element-type-2)
                       (do ((i startr (1+ i)))
                           ((> i endr))
                         (declare (dynamic-extent i) (type fixnum i))
                         (do ((j startc (1+ j)))
                             ((> j endc))
                           (declare (dynamic-extent j) (type fixnum j))
                           (setf (mref p i j)
                                 (,op (mref m i j) (mref n i j))))))))
                 p))))

       (defmethod ,(ch-util:make-intern (concatenate 'string name suffix))
	   ((m ,type-1) (n ,type-2) &key in-place)
	 (if in-place
             ,(if allow-in-place
                  `(with-typed-mref (m ,element-type-1)
                     (with-typed-mref (n ,element-type-2)
                       (loop for i from 0 below (matrix-total-size m)
                          do (setf (row-major-mref m i)
                                   (,op (row-major-mref m i) (row-major-mref n i)))))
                     m)
                  `(error 'matrix-argument-error
                            :format-control
                            "in-place operation not allowed (~S of ~S and ~S"
                            :format-arguments (list '+ ',type-1 ',type-2)))
             (let ((p (make-instance ',accumulator-type :dimensions (matrix-dimensions m))))
               (with-typed-mref (m ,element-type-1)
                 (with-typed-mref (n ,element-type-2)
                   (with-typed-mref (p ,accumulator-element-type)
                     (loop for i from 0 below (matrix-total-size m)
                        do (setf (row-major-mref p i)
                                 (,op (row-major-mref m i) (row-major-mref n i)))))))
               p))))))
       


