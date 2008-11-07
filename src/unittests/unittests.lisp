;;; -*- mode: lisp -*-
;;; Copyright (c) 2007, by A.J. Rossini <blindglobe@gmail.com>
;;; See COPYRIGHT file for any additional restrictions (BSD license).
;;; Since 1991, ANSI was finally finished.  Edited for ANSI Common Lisp. 

;;; This is semi-external to lispstat core packages.  The dependency
;;; should be that lispstat packages are dependencies for the unit
;;; tests.  However, where they will end up is still to be
;;; determined. 

(in-package :lisp-stat-unittests)

;;; This file contains overall test support infrastructure.

(defun run-lisp-stat-tests ()
  (run-tests :suite 'lisp-stat-ut))

;; (run-lisp-stat-tests)

(deftestsuite lisp-stat-ut () ())

;;; Support for fine-grained numerical equivalence 

(defun almost= (a b &key (tol 0.000001)) 
  "Numerically compares 2 values to a tolerance."
   (< (abs (- a b)) tol))

(defun almost=lists (a b &key (tol 0.000001)) 
  "Numerically compare 2 lists using almost=."
  (if (and (null a) (null b))
      t
      (and (almost= (car a) (car b) :tol tol)
	   (almost=lists (cdr a) (cdr b) :tol tol))))

;; Need to consider a CLOSy approach for almost= to cover the range of
;; possible data structures that we would like to be equal to a
;; particular tolerance range.  For example, fill in a shell like:

(defgeneric numerical= (a b &key tol))

(defmethod numerical= ((a real) (b real) &key (tol 0.00001)) ;; real))
  ;;(print (format nil " equality pred for real a=~w real b=~w" a b))
  (< (abs (- a b)) tol))

;; can we just worry about reals if integers are a subclass?  
(defmethod numerical= ((a integer) (b integer) &key (tol 0.1)) ;; real))
  ;;(print (format nil " equality pred for int a=~w int b=~w" a b))
  (< (abs (- a b)) tol))

(defmethod numerical= ((a complex) (b complex) &key (tol 0.00001))
  ;;(print (format nil " equality pred for cmplx a=~w cmplx b=~w" a b))
  (< (abs (- a b)) tol))

(defmethod numerical= ((a sequence) (b sequence) &key (tol 0.00001))
  ;; (print (format nil "checking equality for list a ~w list b=~w" a b))
  ;; using sequence for lists and vectors, but not arrays.
  ;; FIXME++++  This is too slow, too many comparisons!
  (if (and (null a) (null b)) 
      t
      (if (and (= (length a) (length b))
	       (> (length a) 0)
	       (numerical= (car a) (car b) :tol tol))
	  (progn
	    (if (= (length (cdr a)) 0)
		t
		(numerical= (cdr a) (cdr b) :tol tol)))
	  nil)))

;; To do.

(defmethod numerical= ((a array) (b array) &key (tol 0.00001))
;;   (print (format nil
;; 		 "checking equality for array a ~w and array b=~w"
;; 		 a b))
  ;;; FIXME Warning!  Need to generalize past  2-d array!!
  (if (/= (array-dimensions a) (array-dimensions b))
      nil
      (let* ((a-dim (array-dimensions a))
	     (a-b-elt-eq (loop for i from 0 to (nth 0 a-dim)
			       for j from 0 to (nth 1 a-dim)
			       collect (numerical= (apply #'aref a (list i j))
						   (apply #'aref b (list i j))
						   :tol tol))))
	(every #'(lambda (x) x) a-b-elt-eq))))

(deftestsuite lisp-stat-ut-testsupport (lisp-stat-ut)
  ()
  (:tests
   (almost=1 (ensure (almost= 3 3.001 :tol 0.01)))
   (almost=2 (ensure (almost= 3 3.01 :tol 0.01)))
   (almost=3 (ensure (not (almost= 3 3.1 :tol 0.01))))
   (almost=lists1 (ensure (almost=lists nil nil :tol 0.01)))
   (almost=lists2 (ensure (almost=lists (list ) (list ) :tol 0.01)))
   (almost=lists3 (ensure (almost=lists (list 1.0) (list 1.0) :tol 0.01)))
   (almost=lists4 (ensure (almost=lists (list 1.0 1.0) (list 1.0 1.0) :tol 0.01)))
   (almost=lists5 (ensure (not (almost=lists (list 1.0 1.0)
					     (list 1.0 1.1) :tol 0.01))))))

(deftestsuite lisp-stat-ut-testsupport2 (lisp-stat-ut)
  ()
  (:tests
   (numerical=1 (ensure (numerical= 3 3.001 :tol 0.01)))
   (numerical=1.1 (ensure    (numerical= 2 2)))
   (numerical=1.2 (ensure (not (numerical= 2 3))))
   (numerical=2 (ensure (numerical= 3 3.01 :tol 0.01)))
   (numerical=3 (ensure (not (numerical= 3 3.1 :tol 0.01))))
   (numerical=4 (ensure (numerical= nil nil :tol 0.01)))
   (numerical=5 (ensure (numerical= (list ) (list ) :tol 0.01)))
   (numerical=6 (ensure (numerical= (list 1.0) (list 1.0) :tol 0.01)))
   (numerical=7 (ensure (numerical= (list 1.0 1.0) (list 1.0 1.0) :tol 0.01)))
   (numerical=7.5 (ensure-error (numerical= 1.0 (list 1.0 1.0) :tol 0.01)))
   (numerical=8 (ensure (not (numerical= (list 2.0 2.0 2.2) (list 2.1 2.0 2.2)))))
   (numerical=9 (ensure    (numerical= (list 2.1 2.0 2.2) (list 2.1 2.0 2.2)) ))
   (numerical=10 (ensure    (numerical= (list 2.1 2.0 2.2 4.2) (list 2.1 2.0 2.2 4.2))))
   (numerical=11 (ensure (not (numerical= (list 2.1 2.0 2.3 4.0) (list 2.1 2.0 2.2 4.0)))))
   (numerical=12 (ensure (not (numerical= (list 1.0 1.0)
					 (list 1.0 1.1) :tol 0.01))))
   (numerical=C1 (ensure (numerical= #C(2 3) #C(2 3))))
   (numerical=C2 (ensure (not(numerical= #C(2 3) #C(2 4)))))
   (numerical=C3 (ensure (numerical= #C(2 3) #C(3 4) :tol 2)))
   (numerical=C4 (ensure (not(numerical= #C(2 3) #C(3 4) :tol 1))))

   ;;;; Tests to fix

   (numerical=A1 (ensure (numerical= #1A(2 3 4) 
				     #1A(2 3 4))))

   (numerical=A2 (ensure (numerical= #2A((2 3 4) (1 2 4) (2 4 5))
				     #2A((2 3 4) (1 2 4) (2 4 5)))))

   (numerical=A3 (ensure (not (numerical= #2A((2 3 4) (1 2 4) (2 5 4))
					  #2A((2 3 4) (1 2 4) (2 4 5))))))

   (numerical=A4 (ensure (not (numerical= #1A(2 2 4) 
					  #1A(2 3 4)))))

   ))

;; (describe (run-tests :suite 'lisp-stat-ut-testsupport2))

#|
(describe 
 (run-test
  :test-case 'numerical=a2
  :suite 'lisp-stat-ut-testsupport2 ))
|#


#|
(describe 
 (run-test
  :test-case 'numerical=a1
  :suite 'lisp-stat-ut-testsupport2 ))
|#

