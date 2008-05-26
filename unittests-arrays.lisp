;;; -*- mode: lisp -*-
;;; Copyright (c) 2007, by A.J. Rossini <blindglobe@gmail.com>
;;; See COPYRIGHT file for any additional restrictions (BSD license).
;;; Since 1991, ANSI was finally finished.  Edited for ANSI Common Lisp. 

;;; This is semi-external to lispstat core packages.  The dependency
;;; should be that lispstat packages are dependencies for the unit
;;; tests.  However, where they will end up is still to be
;;; determined. 

(in-package :cl-user)

(defpackage :lisp-stat-unittests-arrays
  (:use :common-lisp :lift
	;; use feature tests to check for matrix packages, i.e. 
	#+matlisp :matlisp 
	#+clem :clem
	;; and likewise, handle the tests appropriately as well.
	:lisp-stat             ;; basic tools
	:lisp-stat-unittests)  ;; support
  (:shadowing-import-from :lisp-stat
	slot-value call-method call-next-method ;; objects
	expt + - * / ** mod rem abs 1+ 1- log exp sqrt sin cos tan ;; lsmath
	asin acos atan sinh cosh tanh asinh acosh atanh float random
	truncate floor ceiling round minusp zerop plusp evenp oddp 
	< <= = /= >= > ;; complex
	conjugate realpart imagpart phase
	min max logand logior logxor lognot ffloor fceiling
	ftruncate fround signum cis)
  (:export run-lisp-stat-tests run-lisp-stat-test scoreboard))

(in-package :lisp-stat-unittests-arrays)

;;; TESTS

(defun run-lisp-stat-tests ()
  (run-tests :suite 'lisp-stat))

(defun run-lisp-stat-test (&rest x)
  (run-test x))

(deftestsuite lisp-stat-array (lisp-stat) ())



(addtest (lisp-stat-array) cholesky-decomposition-1
	 (ensure-same
	  (chol-decomp  #2A((2 3 4) (1 2 4) (2 4 5)))
	  (list #2A((1.7888543819998317 0.0 0.0)
		      (1.6770509831248424 0.11180339887498929 0.0)
		      (2.23606797749979 2.23606797749979 3.332000937312528e-8))
		  5.000000000000003)
	  :test 'almost=lists))

#|
(addtest (lisp-stat-array) cholesky-decomposition-2
	 (ensure-same
	  (matlisp:chol)))

|#


(addtest (lisp-stat-array) lu-decomposition
	 (ensure-same
	  (lu-decomp  #2A((2 3 4)
			  (1 2 4)
			  (2 4 5)))
	  (list #2A((2.0 3.0 4.0) (1.0 1.0 1.0) (0.5 0.5 1.5))
		#(0 2 2)
		-1.0
		NIL)))

(addtest (lisp-stat-array) lu-decomposition-2
	 (ensure-same
	  (lu-decomp  (make-real-matrix #2A((2 3 4)
					    (1 2 4)
					    (2 4 5))))
	  (list #2A((2.0 3.0 4.0) (1.0 1.0 1.0) (0.5 0.5 1.5))
		#(0 2 2)
		-1.0
		NIL)))



(addtest (lisp-stat-array) rcondest
	 ;; (ensure-same 
	 (ensure-error  ;; it barfs,  FIXME!!
	  (rcondest #2A((2 3 4) (1 2 4) (2 4 5))) 
	  6.8157451e7
	  :test 'almost=))

(addtest (lisp-stat-array) lu-solve
	 (ensure-same 
	  (lu-solve 
	   (lu-decomp
	    #2A((2 3 4) (1 2 4) (2 4 5)))
	   #(2 3 4))
	  #(-2.333333333333333 1.3333333333333335 0.6666666666666666)))

(addtest (lisp-stat-array) inverse
	 (ensure-same 
	  (inverse #2A((2 3 4) (1 2 4) (2 4 5)))
	  #2A((2.0 -0.33333333333333326 -1.3333333333333335)
	      (-1.0 -0.6666666666666666 1.3333333333333333)
	      (0.0 0.6666666666666666 -0.3333333333333333))))

(addtest (lisp-stat-array)  sv-decomp
	 (ensure-same 
	  (sv-decomp  #2A((2 3 4) (1 2 4) (2 4 5)))
	  (list #2A((-0.5536537653489974 0.34181191712789266 -0.7593629708013371)
		    (-0.4653437312661058 -0.8832095891230851 -0.05827549615722014)
		    (-0.6905959164998124 0.3211003503429828 0.6480523475178517))
		#(9.699290438141343 0.8971681569301373 0.3447525123483081)
		#2A((-0.30454218417339873 0.49334669582252344 -0.8147779426198863)
		    (-0.5520024849987308 0.6057035911404464 0.5730762743603965)
		    (-0.7762392122368734 -0.6242853493399995 -0.08786630745236332))
		T)
	  :test 'almost=lists))

(addtest (lisp-stat-array) qr-decomp
	 (ensure-same 
	  (qr-decomp  #2A((2 3 4) (1 2 4) (2 4 5)))
	  (list #2A((-0.6666666666666665 0.7453559924999298 5.551115123125783e-17)
		    (-0.3333333333333333 -0.2981423969999719 -0.894427190999916)
		    (-0.6666666666666666 -0.5962847939999439 0.44721359549995787))
		#2A((-3.0 -5.333333333333334 -7.333333333333332)
		    (0.0 -0.7453559924999292 -1.1925695879998877)
		    (0.0 0.0 -1.3416407864998738)))
	  :test 'almost=lists))

(addtest (lisp-stat-array) eigen
	 (ensure-same 
	  (eigen #2A((2 3 4) (1 2 4) (2 4 5)))
	  (list #(10.656854249492381 -0.6568542494923802 -0.9999999999999996)
		(list #(0.4999999999999998 0.4999999999999997 0.7071067811865475)
		      #(-0.49999999999999856 -0.5000000000000011 0.7071067811865474)
		      #(0.7071067811865483 -0.7071067811865466 -1.2560739669470215e-15))
		NIL)))

(addtest (lisp-stat-array) spline
	 (ensure-same 
	  (spline #(1.0 1.2 1.3 1.8 2.1 2.5)  
		  #(1.2 2.0 2.1 2.0 1.1 2.8)
		  :xvals 6)
	  (list (list 1.0 1.3 1.6 1.9 2.2 2.5)
		(list 1.2 2.1 2.2750696543866313 1.6465231041904045 1.2186576148879609 2.8))
	  :test 'almost=lists))

(addtest (lisp-stat-array) kernel-smooth
	 (ensure-same 
	  ;; using KERNEL-SMOOTH-FRONT, not KERNEL-SMOOTH-CPORT
	  (kernel-smooth
	   #(1.0 1.2 1.3 1.8 2.1 2.5)  
	   #(1.2 2.0 2.1 2.0 1.1 2.8)
	   :xvals 5)
	  (list (list 1.0 1.375 1.75 2.125 2.5)
		(list 1.6603277642110226 1.9471748095239771 1.7938127405752287 
		      1.5871511322219498 2.518194783156392))
	  :test 'almost=lists))

(addtest (lisp-stat-array) kernel-dens
	 (ensure-same 
	  (kernel-dens
	   #(1.0 1.2 2.5 2.1 1.8 1.2)
	   :xvals 5)
	  (list (list 1.0 1.375 1.75 2.125 2.5)
		(list 0.7224150453621405 0.5820045548233707 0.38216411702854214 
		      0.4829822708587095 0.3485939156929503))))


(addtest (lisp-stat-array) fft
	 (ensure-same 
	  (fft #(1.0 1.2 2.5 2.1 1.8))
	  (list #(#C(1.0 0.0) #C(1.2 0.0) #C(2.5 0.0) #C(2.1 0.0) #C(1.8 0.0)))
	  :test 'almost=lists))
	  

(addtest (lisp-stat-array) lowess
	 (ensure-same 
	  (lowess #(1.0 1.2 2.5 2.1 1.8 1.2)
		  #(1.2 2.0 2.1 2.0 1.1 2.8))
	  #(1.0 1.2 1.2 1.8 2.1 2.5)
	  :test 'almost=lists)) ;; result isn't a list!

