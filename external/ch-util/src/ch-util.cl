;;;
;;; ch-util.cl -- various lisp utilities that make my life easier
;;;
;;; Author: Cyrus Harmon <ch-lisp@bobobeach.com>
;;;

(in-package :ch-util)

;;; Miscellaneous list utilities

(flet ((cca (l1 l2)
	 (dolist (x l1)
	   (let ((y (member x l2)))
	     (if y (return y))))))
  (defun closest-common-ancestor (itm &rest lis)
    (if (null lis)
	itm
	(cca itm (apply #'closest-common-ancestor lis)))))

;;; Miscellaneous class utilities
    
(defun subclassp (c1 c2)
  (subtypep (class-name c1) (class-name c2)))

;;; Miscellaneous string utilities
 
(defun strcat (&rest strs)
  (apply #'concatenate 'string strs))

(defun trim (seq suffix)
  (subseq seq 0 (search suffix seq)))

;;; This is a hack so that one day we might run under a case-sensitive lisp
;;; like Allegro mlisp (modern lisp). For now we encapsulate the uppercasing
;;; here so we can do the right thing later.
(defun interncase (x)
  (string-upcase x))

;;; simple wrapper for intern to allow us 
(defun make-intern (x &optional (package *package*))
  (intern (interncase x) package))

(defun make-keyword (x)
  (make-intern x 'keyword))

(defun keywordicate (x)
  (cond ((keywordp x) x)
        (t (make-keyword x))))

(defun keyword-list-names (k)
  (mapcar #'(lambda (x)
	    (symbol-name x))
	k))

(defun double-float-divide (&rest args)
  (apply #'/ (mapcar #'(lambda (x) (coerce x 'double-float)) args)))

(defun single-float-divide (&rest args)
  (apply #'/ (mapcar #'(lambda (x) (coerce x 'single-float)) args)))

(defmacro mapv (function &rest vals)
  `(values-list (mapcar ,function (multiple-value-list ,@vals))))

;;
;; Silly little macro to do a postincrement, that is
;; return the value of the place prior to incrementing
;; it. Like incf, this only works on valid places.
;;
(defmacro postincf (x &optional (step 1))
  (let ((pre (gensym)))
    `(let ((,pre ,x))
       (incf ,x ,step)
       ,pre)))

;; another silly little function.
;; this one to sum a 2d array.
;; undoubtedly a better way to do this.
(defun array-sum (a)
  (destructuring-bind (height width) (array-dimensions a)
    (let ((acc 0))
      (dotimes (h height)
	(dotimes (w width)
	  (incf acc (aref a h w))))
      acc)))

(defun array-from-string (str)
  (let ((a (make-array (length str) :element-type '(unsigned-byte 8))))
    (dotimes (i (length str))
      (setf (aref a i) (char-code (elt str i))))
    a))

(defun str-to-int (str)
  (let ((intval 0))
    (map nil #'(lambda (c) (setf intval (+ (ash intval 8) (char-code c)))) str)
    intval))

(defun int-to-str (i &optional s)
  (if (> i 0)
      (let ((r (mod i 256)))
	(int-to-str (ash i -8) (cons (code-char r) s)))
      (coerce s 'string)))


(defparameter *months*
  '((1 . "January")
    (2 . "February")
    (3 . "March")
    (4 . "April")
    (5 . "May")
    (6 . "June")
    (7 . "July")
    (8 . "August")
    (9 . "September")
    (10 . "October")
    (11 . "November")
    (12 . "December")))
     
(defun get-current-date ()
  (multiple-value-bind (sec min hour date mon year dow dst tz)
      (get-decoded-time)
    (declare (ignore sec min hour dow dst tz))
    (format nil
            "~A ~A, ~A"
            (cdr (assoc mon *months*))
            date
            year)))

(defun find-nth-zero (vector n)
  "finds the nth zero in a vector."
  (loop for i below (length vector)
     with count = 0
     with rem = n
     while (<= count n)
     do (when (zerop (aref vector i))
          (incf count)
          (decf rem))
     finally (return (when (minusp rem)
                       (1- i)))))

(defun generate-random-permutation (n &key (length n))
  "returns a random permutation of length length of the
integers from 0 to n-1."
  (let ((a (make-array n)))
    (let ((randlist
           (loop for i from n downto (1+ (- n length))
              collect
              (let ((k (random i)))
                (let ((n (find-nth-zero a k)))
                  (setf (aref a n) 1)
                  n)))))
      randlist)))

;;; this doesn't work yet!
#+nil 
(defun robust-mean (vector-or-list)
  (flet ((square (a) (* a a)))
    (let ((v (cond ((vectorp vector-or-list) vector-or-list)
                   ((listp vector-or-list (coerce vector-or-list 'vector))))))
      (when v
        (let ((n (length v))
              (b 1)
              (s1 (elt v 0))
              (s2 (square (elt v 0))))
          (loop for a below n
             do ))))))

(defun median (seq)
  (let ((v (cond ((vectorp seq) (copy-seq seq))
                 ((listp seq) (coerce seq 'vector)))))
    (when (and v (plusp (length v)))
      (sort v #'<)
      (let ((f (floor (length v) 2)))
        (cond ((oddp (length v))
               (elt v f))
              (t
               (/ (+ (elt v (1- f))
                     (elt v f))
                  2)))))))

(defun mean (seq)
  (let ((v (cond ((vectorp seq) (copy-seq seq))
                 ((listp seq) (coerce seq 'vector)))))
    (when (and v (plusp (length v)))
      (/  (reduce #'+ v)
          (length v)))))

(defun variance (seq)
  (flet ((square (x)
           (* x x)))
    (let* ((v (cond ((vectorp seq) (copy-seq seq))
                    ((listp seq) (coerce seq 'vector))))
           (n (length v)))
      (when (and v (plusp n))
        (let ((msq (square (mean seq))))
          (/ (reduce #'+ (mapcar #'(lambda (x)
                                     (- (square x) msq))
                                 seq))
             (1- n)))))))

(defun square-seq (seq)
  (flet ((square (x)
           (* x x)))
    (let ((v (cond ((vectorp seq) (copy-seq seq))
                   ((listp seq) (coerce seq 'vector)))))
      (when (and v (plusp (length v)))
        (mapcar #'square seq)))))

(defun remove-keywordish-args (keywords &rest args)
  (let ((keys))
    (let ((non-keys
           (loop for (x y) on args
              with skip = nil
              append (if skip
                         (setf skip nil)
                         (if (member x keywords)
                             (progn
                               (setf skip t)
                               (pushnew (cons x y) keys)
                               nil)
                             (list x))))))
      (list (mapcan #'(lambda (x)
                        (list (car x) (cdr x)))
                    (nreverse keys))
            non-keys))))

(defun keyword-arg-name (key)
  (cond ((atom key)  key)
        ((listp key) (car key))))

(defmacro with-keyword-args (((&rest args) rest) list &body body)
  `(destructuring-bind ((&key ,@args) (&rest ,rest))
       (apply #'remove-keywordish-args
              (mapcar #'keywordicate
                      (mapcar #'keyword-arg-name
                              ',args))
              ,list)
     ,@body))

(defun sort-copy (seq pred &key key)
  (apply #'sort (copy-seq seq) pred
         (when key `(:key ,key))))

(defun iota (n)
  (labels ((%iota (n)
             (when (plusp n)
               (cons n (%iota (1- n))))))
    (nreverse (%iota n))))

