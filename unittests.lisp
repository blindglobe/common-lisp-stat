;;; -*- mode: lisp -*-
;;; Copyright (c) 2007, by A.J. Rossini <blindglobe@gmail.com>
;;; See COPYRIGHT file for any additional restrictions (BSD license).
;;; Since 1991, ANSI was finally finished.  Edited for ANSI Common Lisp. 

;;; This is semi-external to lispstat core packages.  The dependency
;;; should be that lispstat packages are dependencies for the unit
;;; tests.  However, where they will end up is still to be
;;; determined. 

(in-package :cl-user)

(defpackage :lisp-stat-unittests
  (:use :common-lisp :lift :lisp-stat)
  (:shadowing-import-from :lisp-stat
	slot-value call-method call-next-method ;; objects
	expt + - * / ** mod rem abs 1+ 1- log exp sqrt sin cos tan ;; lsmath
	asin acos atan sinh cosh tanh asinh acosh atanh float random
	truncate floor ceiling round minusp zerop plusp evenp oddp 
	< <= = /= >= > complex conjugate realpart imagpart phase
	min max logand logior logxor lognot ffloor fceiling
	ftruncate fround signum cis)
  (:export run-lisp-stat-tests run-lisp-stat-test scoreboard))

(in-package :lisp-stat-unittests)

;;; TESTS

;;; Need a waz to minimize the possibilities

(defun run-lisp-stat-tests ()
  (run-tests :suite 'lisp-stat))

(defun run-lisp-stat-test (&rest x)
  (run-test x))

(defun almost= (a b &key (tol 0.000001)) 
   (< (abs (- a b)) tol))

(deftestsuite lisp-stat () ())
(deftestsuite lisp-stat-lin-alg (lisp-stat) ())
(deftestsuite lisp-stat-spec-fns (lisp-stat) ())
(deftestsuite lisp-stat-probdistn (lisp-stat) ())

;; ;;; and add a test to it
;; (addtest (lisp-stat)
;;   (ensure-same (+ 1 1) 2))
;; ;; => #<Test passed>

;; ;;; add another test using ensure-error
;; (addtest (lisp-stat-lin-alg)
;;   (ensure-error (let ((x 0)) (/ x))))
;; ;; => #<Test passed>

;; ;;; add another, slightly more specific test
;; (addtest (lisp-stat)
;;   (ensure-condition division-by-zero (let ((x 0)) (/ x))))
;; ;; => #<Test passed>

(addtest (lisp-stat-lin-alg) cholesky-decomposition
	 (ensure-same
	  (chol-decomp  #2A((2 3 4) (1 2 4) (2 4 5)))
	  (values #2A((1.7888543819998317 0.0 0.0)
		      (1.6770509831248424 0.11180339887498929 0.0)
		      (2.23606797749979 2.23606797749979 3.332000937312528e-8))
		  5.000000000000003)))

(addtest (lisp-stat-lin-alg) lu-decomposition
	 (ensure-same
	  (lu-decomp  #2A((2 3 4) (1 2 4) (2 4 5)))
	  (list #2A((2.0 3.0 4.0) (1.0 1.0 1.0) (0.5 0.5 1.5)) #(0 2 2) -1.0 NIL)))

(addtest (lisp-stat-lin-alg) rcondest
	 (ensure-same 
	  (rcondest #2A((2 3 4) (1 2 4) (2 4 5))) 
	  6.8157451e7 ))

(addtest (lisp-stat-lin-alg) lu-solve
	 (ensure-same 
	  (lu-solve 
	   (lu-decomp
	    #2A((2 3 4) (1 2 4) (2 4 5)))
	   #(2 3 4))
	  #(-2.333333333333333 1.3333333333333335 0.6666666666666666)))

(addtest (lisp-stat-lin-alg) inverse
	 (ensure-same 
	  (inverse #2A((2 3 4) (1 2 4) (2 4 5)))
	  #2A((2.0 -0.33333333333333326 -1.3333333333333335)
	      (-1.0 -0.6666666666666666 1.3333333333333333)
	      (0.0 0.6666666666666666 -0.3333333333333333))))

(addtest (lisp-stat-lin-alg)  sv-decomp
	 (ensure-same 
	  (sv-decomp  #2A((2 3 4) (1 2 4) (2 4 5)))
	  (list #2A((-0.5536537653489974 0.34181191712789266 -0.7593629708013371)
		    (-0.4653437312661058 -0.8832095891230851 -0.05827549615722014)
		    (-0.6905959164998124 0.3211003503429828 0.6480523475178517))
		#(9.699290438141343 0.8971681569301373 0.3447525123483081)
		#2A((-0.30454218417339873 0.49334669582252344 -0.8147779426198863)
		    (-0.5520024849987308 0.6057035911404464 0.5730762743603965)
		    (-0.7762392122368734 -0.6242853493399995 -0.08786630745236332))
		T)))

(addtest (lisp-stat-lin-alg) qr-decomp
	 (ensure-same 
	  (qr-decomp  #2A((2 3 4) (1 2 4) (2 4 5)))
	  (list #2A((-0.6666666666666665 0.7453559924999298 5.551115123125783e-17)
		    (-0.3333333333333333 -0.2981423969999719 -0.894427190999916)
		    (-0.6666666666666666 -0.5962847939999439 0.44721359549995787))
		#2A((-3.0 -5.333333333333334 -7.333333333333332)
		    (0.0 -0.7453559924999292 -1.1925695879998877)
		    (0.0 0.0 -1.3416407864998738)))))

(addtest (lisp-stat-lin-alg) eigen
	 (ensure-same 
	  (eigen #2A((2 3 4) (1 2 4) (2 4 5)))
	  (list #(10.656854249492381 -0.6568542494923802 -0.9999999999999996)
		(list #(0.4999999999999998 0.4999999999999997 0.7071067811865475)
		      #(-0.49999999999999856 -0.5000000000000011 0.7071067811865474)
		      #(0.7071067811865483 -0.7071067811865466 -1.2560739669470215e-15))
		NIL)))

(addtest (lisp-stat-lin-alg) spline
	 (ensure-same 
	  (spline #(1.0 1.2 1.3 1.8 2.1 2.5)  
		  #(1.2 2.0 2.1 2.0 1.1 2.8)
		  :xvals 6)
	  (list (list 1.0 1.3 1.6 1.9 2.2 2.5)
		(list 1.2 2.1 2.2750696543866313 1.6465231041904045 1.2186576148879609 2.8))))

(addtest (lisp-stat-lin-alg) kernel-smooth
	 (ensure-same 
	  ;; using KERNEL-SMOOTH-FRONT, not KERNEL-SMOOTH-CPORT
	  (kernel-smooth
	   #(1.0 1.2 1.3 1.8 2.1 2.5)  
	   #(1.2 2.0 2.1 2.0 1.1 2.8)
	   :xvals 5)
	  (list (list 1.0 1.375 1.75 2.125 2.5)
		(list 1.6603277642110226 1.9471748095239771 1.7938127405752287 
		      1.5871511322219498 2.518194783156392))))

(addtest (lisp-stat-lin-alg) kernel-dens
	 (ensure-same 
	  (kernel-dens
	   #(1.0 1.2 2.5 2.1 1.8 1.2)
	   :xvals 5)
	  (list (list 1.0 1.375 1.75 2.125 2.5)
		(list 0.7224150453621405 0.5820045548233707 0.38216411702854214 
		      0.4829822708587095 0.3485939156929503))))


(addtest (lisp-stat-lin-alg) fft
	 (ensure-same 
	  (fft #(1.0 1.2 2.5 2.1 1.8))
	  (list #(#C(1.0 0.0) #C(1.2 0.0) #C(2.5 0.0) #C(2.1 0.0) #C(1.8 0.0)))))

(addtest (lisp-stat-lin-alg) lowess
	 (ensure-same 
	  (lowess #(1.0 1.2 2.5 2.1 1.8 1.2)
		  #(1.2 2.0 2.1 2.0 1.1 2.8))
	   #(1.0 1.2 1.2 1.8 2.1 2.5)))



;;;; Log-gamma function

(addtest (lisp-stat-spec-fns) log-gamma-fn
	 (ensure-same 
	  (log-gamma 3.4)
	  1.0923280596789584))


#+nil(progn

;;; Probability distributions

;; This macro should be generalized, but it's a good start now.
;;(defmacro ProbDistnTests (prefixName
;;			  quant-params quant-answer
;;			  cdf-params cdf-answer
;;			  pmf-params pmf-answer
;;			  rand-params rand-answer)
;;  (deftestsuite lisp-stat-probdist-,prefixName (lisp-stat-probdistn)
;;    ;;  ((  ))
;;    (:documentation "testing for ,testName distribution results")
;;    (:test (ensure-same
;;	    (lisp-stat-basics:,testName-quant ,quant-params) ,quant-answer))
;;    (:test (ensure-same
;;	    (lisp-stat-basics:,testName-cdf ,cdf-params) ,cdf-answer))
;;    (:test (ensure-same
;;	    (lisp-stat-basics:,testName-pmf ,pmf-params) ,pmf-answer))
;;    (:test (progn
;;	     (set-seed 234)
;;	     (ensure-same
;;	      (lisp-stat-basics:,testName-rand ,rand-params) ,rand-answer)))))

;;; Normal distribution

(deftestsuite lisp-stat-probdist-f (lisp-stat-probdistn)
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

(deftestsuite lisp-stat-probdist-cauchy (lisp-stat-probdistn)
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
	  (-1.06224644160405 -0.4524695943939537))))

;;;; Gamma distribution

(deftestsuite lisp-stat-probdist-gamma (lisp-stat-probdistn)
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
	  (2.454918912880936 4.081365384357454))))

;;;; Chi-square distribution

(deftestsuite lisp-stat-probdist-chisq (lisp-stat-probdistn)
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

(deftestsuite lisp-stat-probdist-beta (lisp-stat-probdistn)
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

(deftestsuite lisp-stat-probdist-t (lisp-stat-probdistn)
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

(deftestsuite lisp-stat-probdist-f (lisp-stat-probdistn)
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

(deftestsuite lisp-stat-probdist-poisson (lisp-stat-probdistn)
;;  ((  ))
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

(deftestsuite lisp-stat-probdist-binomial (lisp-stat-probdistn)
;;  ((  ))
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

)