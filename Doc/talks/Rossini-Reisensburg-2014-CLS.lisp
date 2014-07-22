;;;; Demo file

(in-package :cl-user)
(ql:quickload :rho)


(in-package :rho-user)

;; Simple data-frame

(defparameter df-1 (make-data-frame '(foo #(1 2 3)) 
                         '(bar ("a" "s" "d") string) 
                         '(baz (100 102 97) (integer 90 110)))) 

(pprint-data-frame df-1) 
(data-frame-column-types df-1)
(data-frame-column-names df-1)
(data-frame-as-lisp-array df-1)

;;; Error, 42 is not a string:  we need to catch this condition, and continue
(ignore-errors
  (setf (ref$ df-1 'bar 2) 42))

;;; Error, 42 is not a string:  we need to catch this condition, and continue
(ignore-errors
  (setf (ref$ df-1 'baz 1) 42))

;; works, since that is a string
(setf (ref$ df-1 'bar 2) "Works!")

(pprint-data-frame df-1) 

(typep (ref$ df-1 2 1) (ref$ (data-frame-column-types df-1) 2))

df-1



;;; simplee but needed example of making a variable of a particular
;;; user-spec'd type.

(defstruct pointSTR (x 0.0 :type float) (y 0.0 :type float))

(defparameter p1 (make-pointSTR :x 2.2 :y 2.3))

p1



(defclass pointCLOS ()
  ((x :type float :initarg :x :initform 0.1)
   (y :type float :initarg :y :initform 0.2))
  (:documentation "silly point class for illustration"))

(defparameter p2 (make-instance 'pointCLOS))
(defparameter p2a (make-instance 'pointCLOS :x 1.0 :y 2.0))

p2
p2a
(list (slot-value p2 'x) (slot-value p2 'y))
(list (slot-value p2a 'x) (slot-value p2a 'y))


(defparameter df-2a
  (make-data-frame '(foo #(1 2 3)) 
		   '(bar ("a" "s" "d") string) 
		   '(baz (100 102 97) (integer 90 110))
		   (make-strand 'bzr
				(vector (make-pointSTR :x 1.0 :y 2.0)
					(make-pointSTR :x 2.0 :y 2.0)
					(make-pointSTR :x 3.0 :y 2.0))
				'pointSTR)))



df-2a
(pprint-data-frame df-2a)

(data-frame-column-types df-2a)

;; and we want 

(ref$ df-2a 2 1) ; => 102
(ref$ df-2a 2 2) ; => 97
(ref$ df-2a 3 2) ; => #S(POINTSTR :X 3.0 :Y 2.0)



(pprint-data-frame df-1)

;; Get vector of columns
(data-frame-columns df-1)
;; get second column -- type strand
(aref  (data-frame-columns df-1) 1)
;; get data from the strand, type vector
(strand-data (aref  (data-frame-columns df-1) 1))
;; get the desired element in the strand
(aref (strand-data (aref  (data-frame-columns df-1) 1)) 1)
(ref$ (aref (data-frame-columns df-1) 1) 1)


;; (ref$ (symbol (nth 1 (data-frame-column-names df-1))) 1) ;;FIXME


;;; To get a row vector, from case 2

(let ((case 2))
  (map 'list
       (lambda (x) (ref$ df-1 x case))
       (data-frame-column-names df-1)))

(let ((case 2))
  (map 'list
       (lambda (x) (ref$ df-1 x case))
       (list 0 1 2 )))

(let ((case 2))
  (map 'list
       (lambda (x) (ref$ df-1 x case))
       (list 1 2 0)))



;;;;;; Matrix stuff

(in-package :cl-user)
(ql:quickload :cls)


(in-package :lisp-matrix-user)

;;  There are two initial forms for most datasets which are in
;;  cases-by-variables format -- listoflist structure and lisp
;;  arrays.

(defparameter *ex-lol* '((11d0 12d0 13d0 14d0)
			 (21d0 22d0 23d0 24d0)
			 (31d0 32d0 33d0 34d0)))

(defparameter *ex-array* #2A((11d0 12d0 13d0 14d0)
			   (21d0 22d0 23d0 24d0)
			   (31d0 32d0 33d0 34d0)))


(defparameter *ex-lol-array* (listoflist:listoflist->array  *ex-lol*))

;;; Matrices

(defparameter *ex-lol-mat*
  (lisp-matrix:make-matrix 3 4 :initial-contents *ex-lol*))
(defparameter *ex-array-mat*
  (make-matrix 3 4 :initial-contents *ex-array*))

(M+ *ex-lol-mat* *ex-array-mat*)
(M- *ex-lol-mat* *ex-array-mat*)
(defparameter M-0 (M- *ex-lol-mat* *ex-array-mat*))


(M* *ex-lol-mat* (transpose-matrix *ex-array-mat*))

(defparameter *ex1data2-array* (make-array '(4 3)
					   :initial-element 12.d0
					   :adjustable t))
(defparameter *ex1data2-matrix*
  (make-matrix 4 3
	       :implementation  :foreign-array
	       :element-type 'double-float
	       :initial-contents *ex1data2-array*))


;;; 


