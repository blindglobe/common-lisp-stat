;;; -*- mode: lisp -*-

;;; Time-stamp: <2009-04-15 08:53:25 tony>
;;; Creation:   <2009-04-15 08:43:02 tony>
;;; File:       unittests-listoflist.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2009--, AJ Rossini.  BSD, LLGPL, or GPLv2, depending
;;;             on how it arrives.  
;;; Purpose:    unittests for the listoflist handling.


;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

(in-package :lisp-stat-unittests)

(deftestsuite lisp-stat-ut-listoflist (lisp-stat-ut)
  ((my-df-1
    (make-instance 'dataframe-array
		   :storage #2A((1d0 2d0 3d0 4d0)
				(10d0 20d0 30d0 40d0))
		   :doc "This is an interesting legal dataframe-array"
		   :case-labels (list "x" "y")
		   :var-labels  (list "a" "b" "c" "d" "e")))
   (my-matlike-1
    (make-matrix   2 4
		   :initial-element 0d0))
   (my-lol-1 '((0d0  1d0  2d0  3d0)
	       (10d0 11d0 12d0 13d0)))))


;;; Listoflist tests

(addtest (lisp-stat-ut-listoflist) lol-equalp
	 (ensure
	  (equalp (dataset (make-instance 'dataframe-array
					  :storage #2A(('a 'b)
						       ('c 'd))))
		  #2A(('a 'b)
		      ('c 'd)))))

(addtest (lisp-stat-ut-listoflist) lol-consdata
	 (ensure 
	  (consistent-dataframe-p my-df-1)))


;;;
;; (run-tests)
;; (describe (run-tests))
