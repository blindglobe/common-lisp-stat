

(in-package :clem)

(defparameter *max-block-size* 32)
(declaim (type fixnum *max-block-size*))

(defmacro fixnum1+ (place)
  `(the fixnum (1+ ,place)))

(defgeneric %mat-mult-block (m n p
                               mstartr mendr mstartc mendc
                               nstartr nendr nstartc nendc))


(defgeneric %mat-mult-with-blocks (m n p
                                     mstartr mendr mstartc mendc
                                     nstartr nendr nstartc nendc))

(defmacro def-matrix-mult-block (type-1 type-2 accumulator-type &key suffix)
  (declare (ignore suffix))
  (let ((element-type-1 (element-type (find-class `,type-1)))
	(element-type-2 (element-type (find-class `,type-2)))
	(accumulator-element-type (element-type (find-class `,accumulator-type))))
    `(progn
       (defmethod %mat-mult-block
	   ((m ,type-1) (n ,type-2) (p ,accumulator-type)
	    mstartr mendr mstartc mendc
	    nstartr nendr nstartc nendc)
	 (declare (type fixnum mstartr mendr mstartc mendc
			nstartr nendr nstartc nendc)
		  (optimize (speed 3) (safety 0) (space 0)))
	 (let ((a (clem::matrix-vals m))
	       (b (clem::matrix-vals n))
	       (c (clem::matrix-vals p))
	       (atemp (coerce 0 ',accumulator-element-type)))
	   (declare (type (simple-array ,element-type-1 *) a)
		    (type (simple-array ,element-type-2 *) b)
		    (type (simple-array ,accumulator-element-type *) c)
		    (type ,accumulator-element-type atemp))
	   (when (eql (- mendc mstartc) (- nendr nstartr))
	     (let ((ci 0))
	       (declare (type fixnum ci))
	       (do ((k mstartc (fixnum1+ k)))
		   ((> k mendc))
		 (declare (type fixnum k))
		 (setf ci 0)
		 (do ((i mstartr (fixnum1+ i)))
		     ((> i mendr))
		   (declare (type fixnum i))
		   (setf atemp (aref a i k))
		   (do ((j nstartc (fixnum1+ j)))
		       ((> j nendc))
		     (declare (type fixnum j))
		     (incf (row-major-aref c ci) (* (aref b k j) atemp))
		     (incf ci))))))))
       
       (defmethod %mat-mult-with-blocks
	   ((m ,type-1) (n ,type-2) (p ,accumulator-type)
	    (mstartr fixnum) (mendr fixnum) (mstartc fixnum) (mendc fixnum)
	    (nstartr fixnum) (nendr fixnum) (nstartc fixnum) (nendc fixnum))
	 (declare (type fixnum mstartr mendr mstartc mendc
			nstartr nendr nstartc nendc)
		  (optimize (speed 3) (safety 0)))
	 (if (and (> (- mendr mstartr) *max-block-size*)
		  (> (- mendc mstartc) *max-block-size*)
		  (> (- nendr nstartr) *max-block-size*)
		  (> (- nendc nstartc) *max-block-size*))
	     (let ((mblock-row-end (the fixnum
				     (+ (the fixnum mstartr)
					(the fixnum (ash (the fixnum (- mendr mstartr (the fixnum -1))) (the fixnum -1)))
					(the fixnum -1))))
		   (mblock-col-end (the fixnum (+ mstartc (ash (- mendc mstartc -1) -1) -1)))
		   (nblock-row-end (the fixnum (+ nstartr (ash (- nendr nstartr -1) -1) -1)))
		   (nblock-col-end (the fixnum (+ nstartc (the fixnum (ash (- nendc nstartc -1) -1)) -1))))
	       (declare (type fixnum mblock-row-end mblock-col-end nblock-row-end nblock-col-end))
	       ;; m_11 * n_11
	       (%mat-mult-with-blocks m n p
				      mstartr mblock-row-end
				      mstartc mblock-col-end
				      nstartr nblock-row-end
				      nstartc nblock-col-end)
	       
	       ;; m_11 * n_12
	       (%mat-mult-with-blocks m n p
				      mstartr mblock-row-end
				      mstartc mblock-col-end
				      nstartr nblock-row-end
				      (the fixnum (1+ nblock-col-end)) nendc)
	       
	       ;; m_21 * n_11
	       (%mat-mult-with-blocks m n p
				      (the fixnum (1+ mblock-row-end)) mendr
				      mstartc mblock-col-end
				      nstartr nblock-row-end
				      nstartc nblock-col-end)
	       
	       ;; m_21 * n_12
	       (%mat-mult-with-blocks m n p
				      (the fixnum (1+ mblock-row-end)) mendr
				      mstartc mblock-col-end
				      nstartr nblock-row-end
				      (fixnum1+ nblock-col-end) nendc)
	       
	       ;; m_12 * n_21
	       (%mat-mult-with-blocks m n p
				      mstartr mblock-row-end
				      (fixnum1+ mblock-col-end) mendc
				      (fixnum1+ nblock-row-end) nendr
				      nstartc nblock-col-end)
	       
	       ;; m_12 * n_22
	       (%mat-mult-with-blocks m n p
				      mstartr mblock-row-end
				      (fixnum1+ mblock-col-end) mendc
				      (fixnum1+ nblock-row-end) nendr
				      (fixnum1+ nblock-col-end) nendc)
	       
	       ;; m_22 * n_21
	       (%mat-mult-with-blocks m n p
				      (fixnum1+ mblock-row-end) mendr
				      (fixnum1+ mblock-col-end) mendc
				      (fixnum1+ nblock-row-end) nendr
				      nstartc nblock-col-end)
	       
	       ;; m_22 * n_22
	       (%mat-mult-with-blocks m n p
				      (fixnum1+ mblock-row-end) mendr
				      (fixnum1+ mblock-col-end) mendc
				      (fixnum1+ nblock-row-end) nendr
				      (fixnum1+ nblock-col-end) nendc)
	       
	       )
	     (%mat-mult-block m n p
			      mstartr mendr
			      mstartc mendc
			      nstartr nendr
			      nstartc nendc)))
       
       (defmethod mat-mult-3-block ((m ,type-1) (n ,type-2) (p ,accumulator-type))
	 (destructuring-bind (mr mc) (dim m)
	   (destructuring-bind (nr nc) (dim n)
	     (%mat-mult-with-blocks m n p
				    0 (1- mr) 0 (1- mc)
				    0 (1- nr) 0 (1- nc))))
	 p))))

(macrolet ((frob (type-1 type-2 type-3 &key suffix)
	     `(def-matrix-mult-block ,type-1 ,type-2 ,type-3 :suffix ,suffix)))
  (frob double-float-matrix double-float-matrix double-float-matrix)
  (frob single-float-matrix single-float-matrix single-float-matrix))

