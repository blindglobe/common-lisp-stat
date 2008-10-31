;;; -*- mode: lisp -*-
;;; Copyright (c) 2007, by A.J. Rossini <blindglobe@gmail.com>
;;; See COPYRIGHT file for any additional restrictions (BSD license).
;;; Since 1991, ANSI was finally finished.  Edited for ANSI Common Lisp. 

;;; This is semi-external to lispstat core packages.  The dependency
;;; should be that lispstat packages are dependencies for the unit
;;; tests.  However, where they will end up is still to be
;;; determined. 

(in-package :lisp-stat-unittests)

;;; TESTS

(defun run-lisp-stat-tests ()
  (run-tests :suite 'lisp-stat-ut))

;; (run-lisp-stat-tests)


(defun run-lisp-stat-test (&rest x)
  (run-test x))

(deftestsuite lisp-stat-ut () ())

;; others should be defined in another place...:
;;(deftestsuite lisp-stat-ut-lin-alg (lisp-stat-ut) ())
;;(deftestsuite lisp-stat-ut-spec-fns (lisp-stat-ut) ())
;;(deftestsuite lisp-stat-ut-probdistn (lisp-stat-ut) ())

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
  (print (format nil "checking equality for array a ~w and array b=~w" a b))
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



;;;; Log-gamma function

(addtest (lisp-stat-ut-spec-fns) log-gamma-fn
	 (ensure-same 
	  (log-gamma 3.4)
	  1.0923280596789584
	  :test 'almost=))


;;; Probability distributions

;; This macro should be generalized, but it's a good start now.
;;(defmacro ProbDistnTests (prefixName
;;			  quant-params quant-answer
;;			  cdf-params cdf-answer
;;			  pmf-params pmf-answer
;;			  rand-params rand-answer)
;;  (deftestsuite lisp-stat-ut-probdist-,prefixName (lisp-stat-ut-probdistn)
;;    ;;  ((  ))
;;    (:documentation "testing for ,testName distribution results")
;;    (:test (ensure-same
;;	    (lisp-stat-ut-basics:,testName-quant ,quant-params) ,quant-answer))
;;    (:test (ensure-same
;;	    (lisp-stat-ut-basics:,testName-cdf ,cdf-params) ,cdf-answer))
;;    (:test (ensure-same
;;	    (lisp-stat-ut-basics:,testName-pmf ,pmf-params) ,pmf-answer))
;;    (:test (progn
;;	     (set-seed 234)
;;	     (ensure-same
;;	      (lisp-stat-ut-basics:,testName-rand ,rand-params) ,rand-answer)))))

;;; Normal distribution

(deftestsuite lisp-stat-ut-probdist-f (lisp-stat-ut-probdistn)
  ()
  (:documentation "testing for Gaussian distn results")
  (:test (ensure-same
	  (normal-quant 0.95)
	  1.6448536279366268))
  (:test (ensure-same
	  (normal-cdf 1.3)
	  0.9031995154143897))
  (:test (ensure-same
	  (normal-dens 1.3)
	  0.17136859204780736))
  (:test (ensure-same
	  (normal-rand 2)
	  (list -0.40502015f0 -0.8091404f0)))
  (:test (ensure-same
	  (bivnorm-cdf 0.2 0.4 0.6)
	  0.4736873734160288)))

;;;; Cauchy distribution

(deftestsuite lisp-stat-ut-probdist-cauchy (lisp-stat-ut-probdistn)
  ()
  (:documentation "testing for Cachy-distn results")
  (:test (ensure-same
	  (cauchy-quant 0.95)
	  6.313751514675031))
  (:test (ensure-same
	  (cauchy-cdf 1.3)
	  0.7912855998398473))
  (:test (ensure-same
	  (cauchy-dens 1.3)
	  0.1183308127104695 ))
  (:test (ensure-same
	  (cauchy-rand 2)
	  (list -1.06224644160405 -0.4524695943939537))))

;;;; Gamma distribution

(deftestsuite lisp-stat-ut-probdist-gamma (lisp-stat-ut-probdistn)
  ()
  (:documentation "testing for gamma distn results")
  (:test (ensure-same
	  (gamma-quant 0.95 4.3)
	  8.178692439291645))
  (:test (ensure-same
	  (gamma-cdf 1.3 4.3)
	  0.028895150986674906))
  (:test (ensure-same
	  (gamma-dens 1.3 4.3)
	  0.0731517686447374))
  (:test (ensure-same
	  (gamma-rand 2 4.3)
	  (list 2.454918912880936 4.081365384357454))))

;;;; Chi-square distribution

(deftestsuite lisp-stat-ut-probdist-chisq (lisp-stat-ut-probdistn)
  ()
  (:documentation "testing for Chi-square distn results")
  (:test (ensure-same
	  (chisq-quant 0.95 3)
	  7.814727903379012))
  (:test (ensure-same
	  (chisq-cdf 1 5)
	  0.03743422675631789))
  (:test (ensure-same
	  (chisq-dens 1 5)
	  0.08065690818083521))
  (:test (progn
	   (set-seed 353)
	   (ensure-same
	    (chisq-rand 2 4)
	    (list 1.968535826180572 2.9988646156942997)))))

;;;; Beta distribution

(deftestsuite lisp-stat-ut-probdist-beta (lisp-stat-ut-probdistn)
  ()
  (:documentation "testing for beta distn results")
  (:test (ensure-same
	  (beta-quant 0.95 3 2)
	  0.9023885371149876))
  (:test (ensure-same
	  (beta-cdf 0.4 2 2.4)
	  0.4247997418541529 ))
  (:test (ensure-same
	  (beta-dens 0.4 2 2.4)
	  1.5964741858913518 ))
  (:test (ensure-same
	  (beta-rand 2 2 2.4)
	  (list 0.8014897077282279 0.6516371997922659))))

;;;; t distribution

(deftestsuite lisp-stat-ut-probdist-t (lisp-stat-ut-probdistn)
  ()
  (:documentation "testing for t-distn results")
  (:test (ensure-same
	  (t-quant 0.95 3)
	  2.35336343484194))
  (:test (ensure-same
	  (t-cdf 1 2.3)
	  0.794733624298342))
  (:test (ensure-same
	  (t-dens 1 2.3)
	  0.1978163816318102))
  (:test (ensure-same
	  (t-rand 2 2.3)
	  (list -0.34303672776089306 -1.142505872436518))))

;;;; F distribution

(deftestsuite lisp-stat-ut-probdist-f (lisp-stat-ut-probdistn)
  ()
  (:documentation "testing for f-distn results")
  (:test (ensure-same
	  (f-quant 0.95 3 5) 5.409451318117459))
  (:test (ensure-same
	  (f-cdf 1 3.2 5.4)
	  0.5347130905510765))
  (:test (ensure-same
	  (f-dens 1 3.2 5.4)
	  0.37551128864591415))
  (:test (progn
	   (set-seed 234)
	   (ensure-same
	    (f-rand 2 3 2)
	    (list 0.7939093442091963 0.07442694152491144)))))

;;;; Poisson distribution

(deftestsuite lisp-stat-ut-probdist-poisson (lisp-stat-ut-probdistn)
  ()
  (:documentation "testing for poisson distribution results")
  (:test (ensure-same
	  (poisson-quant 0.95 3.2) 6))
  (:test (ensure-same
	  (poisson-cdf 1 3.2)
	  0.17120125672252395))
  (:test (ensure-same
	  (poisson-pmf 1 3.2)
	  0.13043905274097067))
  (:test (progn
	   (set-seed 234)
	   (ensure-same
	    (poisson-rand 5 3.2)
	    (list 2 1 2 0 3)))))

;; Binomial distribution

(deftestsuite lisp-stat-ut-probdist-binomial (lisp-stat-ut-probdistn)
  ()
  (:documentation "testing for binomial distribution results")

  (:test (ensure-same
	  (binomial-quant 0.95 3 0.4) ;;; DOESN'T RETURN
	  ))
  (:test (ensure-same 
	  (binomial-quant 0 3 0.4)
	  ;; -2147483648
	  ))
  (:test (ensure-same
	  (binomial-cdf 1 3 0.4)
	  0.6479999999965776))
  
  (:test (ensure-same
	  (binomial-pmf 1 3 0.4)
	  0.4320000000226171))
  (:test (progn
	   (set-seed 526)
	   (ensure-same 
	    (binomial-rand 5 3 0.4)
	    (list 2 2 0 1 2)))))

