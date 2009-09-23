;;; -*- mode: lisp -*-
;;; Copyright (c) 2006-2008, by A.J. Rossini <blindglobe@gmail.com>
;;; See COPYRIGHT file for any additional restrictions (BSD license).
;;; Since 1991, ANSI was finally finished.  Edited for ANSI Common Lisp. 

;;; Time-stamp: <2009-09-22 23:42:06 tony>
;;; Creation:   <2009-09-17 22:19:31 tony> (sometime earlier, but serious now)
;;; File:       ls-demo.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c) 2007, AJ Rossini.  BSD.
;;; Purpose:    demonstrations of how one might use CLS.

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.


;; Start in the usual user-package for loading
(in-package :cl-user)

;; You've configured CLS, right?
(asdf:oos 'asdf:load-op 'cls)

;; Go somewhere so that you have the functions available.
(in-package :ls-user)

;; we'll be loading from directories in the CLS homedir, so we want to
;; make it easier to reach.  
(defparameter *my-cls-homedir* 
  "/media/disk/Desktop/sandbox/CLS.git/"    ; <- value with trailing
					    ;    directory separator
  "documentation: change this to localize") ; <- doc
;; so
(concatenate 'string *my-cls-homedir* "Data/example.csv")
;; implies
(defun localized-pathto (x)
  "return a string denoting the complete path.
FIXME: UNIX-centric (though might work on Mac OSX).  Might want to
return a pathspec, not a string/namespec"
  (check-type x string)
  (concatenate 'string *my-cls-homedir* x))


;;; DataFrames

(defparameter *df-test*
  (make-instance 'dataframe-array
		 :storage #2A (('a "test0" 0 0d0)
			       ('b "test1" 1 1d0)
			       ('c "test2" 2 2d0)
			       ('d "test3" 3 3d0)
			       ('e "test4" 4 4d0))
		 :doc "test reality"
		 :case-labels (list "0" "1" "2" "3" "4")
		 :var-labels (list "symb-var" "strng-var" "int-var" "dfloat-var")
		 :var-types (list 'symbol 'string 'integer 'double-float)))

*df-test* ; but with SBCL, ints become floats? 
(caselabels *df-test*)
(varlabels *df-test*)
(vartypes *df-test*)

(setf (xref *df-test* 0 0) -1d0) ; for dataframes, we might want to do
				 ; some type checking to prevent what
				 ; I just did!

(setf (xref *df-test* 0 0) (quote 'a)) ; so that we can restore the
				       ; quoted value.
*df-test*

;; Making from arrays and matrix-likes
(make-dataframe  #2A((1 2 3 4 5)
		     (10 20 30 40 50)))
(make-dataframe (rand 4 3))

;;; HERE#1
;;; == READ DATA

;;; read in a CSV dataframe...

;; a better approach is:
(asdf:oos 'asdf:load-op 'rsm-string)
(listoflist->array
 (rsm.string:file->string-table
  (localized-pathto "Data/example-mixed.csv")
  :delims ","))

(rsm.string:file->number-table
 (localized-pathto "Data/example-numeric.csv")
 :delims ",")






(rsm.string:file->number-table
 (localized-pathto "Data/R-chickwts.csv")
 :delims ",")
(rsm.string:file->string-table
 (localized-pathto "Data/R-chickwts.csv")
 :delims ",")

(defparameter *my-df-2*
  (make-instance 'dataframe-array
		 :storage
		 (listoflist->array
		   (rsm.string:file->string-table
		    (localized-pathto "Data/example-mixed.csv")))
		 :doc "This is an interesting dataframe-array"))
;; *my-df-2*

(defparameter *my-df-3*
  (make-instance 'dataframe-array
		 :storage
		 (listoflist->array
		  (transpose-listoflist 
		   (rsm.string:file->number-table
		    (localized-pathto "Data/example-numeric.csv"))))
		 :doc "This is an interesting dataframe-array"))
;; *my-df-3*


(defparameter *my-df-4*
  (make-instance 'dataframe-array
		 :storage
		 (listoflist->array
		   (rsm.string:file->number-table
		    (localized-pathto "Data/R-chickwts.csv")
		    :delims ","))
		 :doc "This is an interesting dataframe-array that currently fails"))
;; *my-df-4*

(aref  (dataset *my-df-4*) 0 1)


(defparameter *my-df-5*
  (make-instance 'dataframe-array
		 :storage
		 (listoflist->array
		  (transpose-listoflist 
		   (rsm.string:file->number-table
		    (localized-pathto "Data/R-swiss.csv"))))
		 :doc "This is an interesting dataframe-array that currently fails"))
;; *my-df-5*



(defparameter *mat-1*
  (make-matrix 3 3
	       :initial-contents #2A((2d0 3d0 4d0) (3d0 2d0 4d0) (4d0 4d0 5d0))))

(defparameter *mat-1*
  (make-matrix 3 3
     :initial-contents #2A((2d0 3d0 -4d0)
                           (3d0 2d0 -4d0)
                           (4d0 4d0 -5d0))))
(mref *mat-1* 2 0)

(defparameter *mat-2*
  (let ((m (rand 3 3)))
    (m* m (transpose m))))

(axpy 100.0d0 *mat-2* (eye 3 3))

(potrf (copy *mat-2*)) ;; factor
(potri (copy *mat-2*)) ;; invert 
(minv-cholesky (copy *mat-2*))
(m*  (minv-cholesky (copy *mat-2*)) *mat-2*)

(defparameter *mat-3*
  (make-matrix
   3 3
   :initial-contents '((16d0 13d0 12d0)
		       (13d0 22d0 7d0)
		       (12d0 7d0  17d0))))

(potrf (copy *mat-3*)) ;; factor

#|
 *mat-3* =>
 #<LA-SIMPLE-MATRIX-DOUBLE  3 x 3
  16.0 13.0 12.0
  13.0 22.0 7.0
  12.0 7.0 17.0>

 (potrf (copy *mat-3*)) =>
 (#<LA-SIMPLE-MATRIX-DOUBLE  3 x 3
  4.0 3.25 3.0
  13.0 3.3819373146171707 -0.8131433980500301
  12.0 7.0 2.7090215603069034>
  "U" NIL)

 ;; and compare with...

 > testm <- matrix(data=c(16,13,12,13,22,7,12,7,17),nrow=3)
 > chol(testm)
      [,1]     [,2]       [,3]
 [1,]    4 3.250000  3.0000000
 [2,]    0 3.381937 -0.8131434
 [3,]    0 0.000000  2.7090216
 > 

 ;; which suggests that the major difference is that R zero's out the
 ;; appropriate terms, and that CLS does not.

|#

(potri (copy *mat-2*)) ;; invert 
(minv-cholesky (copy *mat-2*))
(m*  (minv-cholesky (copy *mat-2*)) *mat-2*)


#|
 (lu-decomp  #2A((2 3 4) (1 2 4) (2 4 5)))
 ;; => (#2A((2.0 3.0 4.0) (1.0 1.0 1.0) (0.5 0.5 1.5)) #(0 2 2) -1.0 NIL)
 (lu-solve 
  (lu-decomp #2A((2 3 4) (1 2 4) (2 4 5))) 
  #(2 3 4))
 ;; => #(-2.333333333333333 1.3333333333333335 0.6666666666666666)
|#
(getrf
 (make-matrix 3 3
	      :initial-contents #2A((2d0 3d0 4d0) (1d0 2d0 4d0) (2d0 4d0 5d0))))

#| => ; so not so good for the vector, but matrix matches.
 (#<LA-SIMPLE-MATRIX-DOUBLE  3 x 3
  2.0 3.0 4.0
  1.0 1.0 1.0
  0.5 0.5 1.5>
  #<FNV-INT32 (3) 1 3 3> NIL)
|#

(msolve-lu 
 (make-matrix 3 3
	      :initial-contents #2A((2d0 3d0 4d0)
				    (1d0 2d0 4d0)
				    (2d0 4d0 5d0)))
 (make-vector 3 :type :column
	      :initial-contents '((2d0)
				  (3d0)
				  (4d0))))

#| =>
  #<LA-SIMPLE-VECTOR-DOUBLE (3 x 1)
   -2.3333333333333335
   1.3333333333333335
   0.6666666666666666>
|#



;;; LU common applications

(defun minv-lu (a)
  "invert A using LU Factorization"
  (let ((a-fac (getrf (copy a))))
    (first (getri (first a-fac) (second a-fac)))))

#+nil (progn
	(let ((m1 (rand 3 3)))
	  (m* m1 (minv-lu m1))))

(defun msolve-lu (a b)
  "Compute `x1' solving `A x = b', with LU factorization."
  (let ((a-fac (getrf (copy a))))
    (first (getrs (first a-fac) b (second a-fac)))))



;; (inverse #2A((2 3 4) (1 2 4) (2 4 5)))
;; #2A((2.0 -0.33333333333333326 -1.3333333333333335)
;;     (-1.0 -0.6666666666666666 1.3333333333333333)
;;     (0.0 0.6666666666666666 -0.3333333333333333))

(minv-lu
   (make-matrix
    3 3
    :initial-contents #2A((2d0 3d0 4d0)
			  (1d0 2d0 4d0)
			  (2d0 4d0 5d0))))

#|

 #<LA-SIMPLE-MATRIX-DOUBLE  3 x 3
  2.0 -0.3333333333333333 -1.3333333333333333
  -1.0 -0.6666666666666666 1.3333333333333333
  0.0 0.6666666666666666 -0.3333333333333333>

 ;; so is correct.

|#

;;;;;HERE#2

(factorize 
 (make-matrix 3 3
	      :initial-contents #2A((2d0 3d0 4d0)
				    (1d0 2d0 4d0)
				    (2d0 4d0 5d0)))
 :by :svd)

;; (sv-decomp  #2A((2 3 4) (1 2 4) (2 4 5)))
;; (#2A((-0.5536537653489974 0.34181191712789266 -0.7593629708013371)
;;      (-0.4653437312661058 -0.8832095891230851 -0.05827549615722014)
;;      (-0.6905959164998124 0.3211003503429828 0.6480523475178517))
;;  #(9.699290438141343 0.8971681569301373 0.3447525123483081)
;;  #2A((-0.30454218417339873 0.49334669582252344 -0.8147779426198863)
;;      (-0.5520024849987308 0.6057035911404464 0.5730762743603965)
;;      (-0.7762392122368734 -0.6242853493399995 -0.08786630745236332))
;;  T)

()

(qr-decomp  #2A((2 3 4) (1 2 4) (2 4 5)))
;; (#2A((-0.6666666666666665 0.7453559924999298 5.551115123125783e-17)
;;      (-0.3333333333333333 -0.2981423969999719 -0.894427190999916)
;;      (-0.6666666666666666 -0.5962847939999439 0.44721359549995787))
;;  #2A((-3.0 -5.333333333333334 -7.333333333333332)
;;      (0.0 -0.7453559924999292 -1.1925695879998877)
;;      (0.0 0.0 -1.3416407864998738)))

(rcondest #2A((2 3 4) (1 2 4) (2 4 5)))
;; 6.8157451e7
;;; CURRENTLY FAILS!!

(eigen #2A((2 3 4) (1 2 4) (2 4 5)))
;; (#(10.656854249492381 -0.6568542494923802 -0.9999999999999996)
;;  (#(0.4999999999999998 0.4999999999999997 0.7071067811865475)
;;   #(-0.49999999999999856 -0.5000000000000011 0.7071067811865474)
;;   #(0.7071067811865483 -0.7071067811865466 -1.2560739669470215e-15))
;;  NIL)

(spline #(1.0 1.2 1.3 1.8 2.1 2.5)  
	#(1.2 2.0 2.1 2.0 1.1 2.8) :xvals 6)
;; ((1.0 1.3 1.6 1.9 2.2 2.5)
;;  (1.2 2.1 2.2750696543866313 1.6465231041904045 1.2186576148879609 2.8))

;;; using KERNEL-SMOOTH-FRONT, not KERNEL-SMOOTH-CPORT
(kernel-smooth #(1.0 1.2 1.3 1.8 2.1 2.5)  
	       #(1.2 2.0 2.1 2.0 1.1 2.8) :xvals 5)
;; ((1.0 1.375 1.75 2.125 2.5)
;;  (1.6603277642110226 1.9471748095239771 1.7938127405752287 
;;   1.5871511322219498 2.518194783156392))

(kernel-dens #(1.0 1.2 2.5 2.1 1.8 1.2) :xvals 5)
;; ((1.0 1.375 1.75 2.125 2.5)
;;  (0.7224150453621405 0.5820045548233707 0.38216411702854214 
;;   0.4829822708587095 0.3485939156929503))

(fft #(1.0 1.2 2.5 2.1 1.8))
;; #(#C(1.0 0.0) #C(1.2 0.0) #C(2.5 0.0) #C(2.1 0.0) #C(1.8 0.0))

(lowess #(1.0 1.2 2.5 2.1 1.8 1.2)  #(1.2 2.0 2.1 2.0 1.1 2.8))
;; (#(1.0 1.2 1.2 1.8 2.1 2.5))



;;;; Special functions

;; Log-gamma function

(log-gamma 3.4) ;;1.0923280596789584



;;;; Probability functions

;; looking at these a bit more, perhaps a more CLOSy style is needed, i.e. 
;; (quantile :list-or-cons loc :type type (one of 'empirical 'normal 'cauchy, etc...))
;; similar for the cdf, density, and rand.
;; Probably worth figuring out how to add a new distribution
;; efficiently, i.e. by keeping some kind of list.

;; Normal distribution

(normal-quant 0.95) ;;1.6448536279366268
(normal-cdf 1.3) ;;0.9031995154143897
(normal-dens 1.3) ;;0.17136859204780736
(normal-rand 2) ;;(-0.40502015f0 -0.8091404f0)

(bivnorm-cdf 0.2 0.4 0.6) ;;0.4736873734160288

;; Cauchy distribution

(cauchy-quant 0.95) ;;6.313751514675031 
(cauchy-cdf 1.3) ;;0.7912855998398473
(cauchy-dens 1.3) ;;0.1183308127104695 
(cauchy-rand 2) ;;(-1.06224644160405 -0.4524695943939537)

;; Gamma distribution

(gamma-quant 0.95 4.3) ;;8.178692439291645
(gamma-cdf 1.3 4.3) ;;0.028895150986674906
(gamma-dens 1.3 4.3) ;;0.0731517686447374
(gamma-rand 2 4.3) ;;(2.454918912880936 4.081365384357454)

;; Chi-square distribution

(chisq-quant 0.95 3) ;;7.814727903379012
(chisq-cdf 1 5) ;;0.03743422675631789
(chisq-dens 1 5) ;;0.08065690818083521
(chisq-rand 2 4) ;;(1.968535826180572 2.9988646156942997)

;; Beta distribution

(beta-quant 0.95 3 2) ;;0.9023885371149876
(beta-cdf 0.4 2 2.4) ;;0.4247997418541529 
(beta-dens 0.4 2 2.4) ;;1.5964741858913518 
(beta-rand 2 2 2.4) ;;(0.8014897077282279 0.6516371997922659) 

;; t distribution

(t-quant 0.95 3) ;;2.35336343484194
(t-cdf 1 2.3) ;;0.794733624298342
(t-dens 1 2.3) ;;0.1978163816318102
(t-rand 2 2.3) ;;(-0.34303672776089306 -1.142505872436518)

;; F distribution

(f-quant 0.95 3 5) ;;5.409451318117459
(f-cdf 1 3.2 5.4) ;;0.5347130905510765
(f-dens 1 3.2 5.4) ;;0.37551128864591415
(f-rand 2 3 2) ;;(0.7939093442091963 0.07442694152491144)

;; Poisson distribution

(poisson-quant 0.95 3.2) ;;6
(poisson-cdf 1 3.2) ;;0.17120125672252395
(poisson-pmf 1 3.2) ;;0.13043905274097067
(poisson-rand 5 3.2) ;;(2 1 2 0 3)

;; Binomial distribution

(binomial-quant 0.95 3 0.4) ;;; DOESN'T RETURN
(binomial-quant 0 3 0.4) ;;; -2147483648
(binomial-cdf 1 3 0.4) ;;0.6479999999965776
(binomial-pmf 1 3 0.4) ;;0.4320000000226171
(binomial-rand 5 3 0.4) ;;(2 2 0 1 2)

;;;; OBJECT SYSTEM

(in-package :ls-user)
(defproto *test-proto*)
*test-proto*
(defmeth *test-proto* :make-data (&rest args) nil)

(defparameter my-proto-instance nil)
(setf my-proto-instance (send *test-proto* :new))
(send *test-proto* :own-slots)
(lsos::ls-object-slots *test-proto*)
(lsos::ls-object-methods *test-proto*)
(lsos::ls-object-parents *test-proto*)
(lsos::ls-object-preclist *test-proto*)
;;; The following fail and I do not know why?
(send *test-proto* :has-slot 'proto-name)
(send *test-proto* :has-slot 'PROTO-NAME)
(send *test-proto* :has-slot 'make-data)
(send *test-proto* :has-slot 'MAKE-DATA)
(send *test-proto* :has-method 'make-data)
(send *test-proto* :has-method 'MAKE-DATA)


(defproto2 *test-proto3* (list) (list) (list) "test doc" t)
(defproto2 *test-proto4*)
*test-proto2*
(defmeth *test-proto* :make-data (&rest args) nil)

(defparameter my-proto-instance nil)
(setf my-proto-instance (send *test-proto* :new))
(send *test-proto* :own-slots)
(send *test-proto* :has-slot 'proto-name)
(send *test-proto* :has-slot 'PROTO-NAME)


;;;; Testing 

(in-package :lisp-stat-unittests)
(testsuites)
(print-tests)
(run-tests)
(last-test-status)
;;(failures)

(describe (run-tests :suite 'lisp-stat-ut-testsupport))
(describe (run-tests :suite 'lisp-stat-ut-testsupport2))

(testsuite-tests 'lisp-stat-ut)
(run-tests :suite 'lisp-stat-ut)
(describe (run-tests :suite 'lisp-stat-ut))

(run-tests :suite 'lisp-stat-ut-probdistn)
(describe (run-tests :suite 'lisp-stat-ut-probdistn))
(run-tests :suite 'lisp-stat-ut-spec-fns)
(describe (run-tests :suite 'lisp-stat-ut-spec-fns))

(find-testsuite 'lisp-stat-ut-lin-alg)
(testsuite-tests 'lisp-stat-ut-lin-alg)
(run-tests :suite 'lisp-stat-ut-lin-alg)
(describe (run-tests :suite 'lisp-stat-ut-lin-alg))

;;;; Data Analysis test

(in-package :ls-user)

;; LispStat 1 approach to variables

(progn
  (def iron  (list 61 175 111 124 130 173 169 169 160 224 257 333 199))
  iron
  (def aluminum (list 13 21 24 23 64 38 33 61 39 71 112 88 54))
  aluminum
  (def absorbtion (list 4 18 14 18 26 26 21 30 28 36 65 62 40))
  absorbtion

  ;; LispStat 1 approach to data frames... (list of lists).

  (DEF DIABETES
      (QUOTE ((80 97 105 90 90 86 100 85 97 97 91 87 78 90 86 80 90 99 85 90 90 88 95 90 92 74 98 100 86 98 70 99 75 90 85 99 100 78 106 98 102 90 94 80 93 86 85 96 88 87 94 93 86 86 96 86 89 83 98 100 110 88 100 80 89 91 96 95 82 84 90 100 86 93 107 112 94 93 93 90 99 93 85 89 96 111 107 114 101 108 112 105 103 99 102 110 102 96 95 112 110 92 104 75 92 92 92 93 112 88 114 103 300 303 125 280 216 190 151 303 173 203 195 140 151 275 260 149 233 146 124 213 330 123 130 120 138 188 339 265 353 180 213 328 346)
	      (356 289 319 356 323 381 350 301 379 296 353 306 290 371 312 393 364 359 296 345 378 304 347 327 386 365 365 352 325 321 360 336 352 353 373 376 367 335 396 277 378 360 291 269 318 328 334 356 291 360 313 306 319 349 332 323 323 351 478 398 426 439 429 333 472 436 418 391 390 416 413 385 393 376 403 414 426 364 391 356 398 393 425 318 465 558 503 540 469 486 568 527 537 466 599 477 472 456 517 503 522 476 472 455 442 541 580 472 562 423 643 533 1468 1487 714 1470 1113 972 854 1364 832 967 920 613 857 1373 1133 849 1183 847 538 1001 1520 557 670 636 741 958 1354 1263 1428 923 1025 1246 1568)
	      (124 117 143 199 240 157 221 186 142 131 221 178 136 200 208 202 152 185 116 123 136 134 184 192 279 228 145 172 179 222 134 143 169 263 174 134 182 241 128 222 165 282 94 121 73 106 118 112 157 292 200 220 144 109 151 158 73 81 151 122 117 208 201 131 162 148 130 137 375 146 344 192 115 195 267 281 213 156 221 199 76 490 143 73 237 748 320 188 607 297 232 480 622 287 266 124 297 326 564 408 325 433 180 392 109 313 132 285 139 212 155 120 28 23 232 54 81 87 76 42 102 138 160 131 145 45 118 159 73 103 460 42 13 130 44 314 219 100 10 83 41 77 29 124 15) 
	      (3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 2 3 3 2 2 3 2 2 3 3 3 3 2 3 3 3 3 3 2 3 3 3 3 3 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))))
  
  
  (DEF DLABS (QUOTE ("GLUFAST" "GLUTEST" "INSTEST" "CCLASS"))) 
  (format t "loaded data.~%")
  )  ;; eval at this point.

;; Simple univariate variable-specific descriptions.
(fivnum absorbtion)
(median absorbtion)
(sort-data absorbtion)
(rank absorbtion)
(standard-deviation absorbtion)
(interquartile-range absorbtion)

(lisp-stat-matrix::bind-columns aluminum iron)
(bind-columns aluminum iron)
(apply #'bind-columns (list aluminum iron))
(lisp-stat-matrix::bind-columns  #2a((1 2)(3 4)) #(5 6))
(bind-columns #2a((1 2)(3 4)) #(5 6))


(defparameter fit1 nil)
(setf fit1 (regression-model absorbtion iron))
(send fit1 :display)
(send fit1 :residuals)

iron
(defparameter fit1a nil)
(setf fit1a (regression-model absorbtion iron :print nil))
(send fit1a :doc)
;;  (setf (send fit1a :doc) "this") ;; FIXME: this error...
(send fit1a :doc "this") ;; FIXME: this is a more natural
(send fit1a :doc)
(send fit1a :x)
(send fit1a :y)
(send fit1a :compute)
(send fit1a :sweep-matrix)
(send fit1a :basis)
(send fit1a :residuals)
(send fit1a :display)

#+nil(progn
       ;; syntax example
       (array-dimension #2A ((1)) 0)
       )

;;; FIXME: need to get multiple-linear regression working -- clearly
;;; simple linear is working above!
(defvar m nil "holding variable.")
(def m (regression-model (list iron aluminum) absorbtion :print nil))
(send m :compute)
(send m :sweep-matrix)
(format t "~%~A~%" (send m :sweep-matrix))

 ;; ERROR... FIX-ME!!
(send m :basis) ;; this should be positive?
(send m :coef-estimates)

(send m :display)
(def m (regression-model (bind-columns iron aluminum) absorbtion))
(send m :help)
(send m :help :display)
(send m :help :basis)
;; No graphics!  But handle the error gracefully...
(send m :plot-residuals)


(typep aluminum 'sequence)
(typep iron 'sequence)
(matrixp iron)

*variables*

(variables)
(undef 'iron)
(variables)

;;; Plotting!

(asdf:oos 'asdf:compile-op 'cl-cairo2 :force t)
(asdf:oos 'asdf:load-op 'cl-cairo2)

;; The above can be used to generate PDF, PS, PNG, and X11/Microsoft
;; displays (the latter being a proof of concept, of limited use for
;; "real work".

;; and this below, as well.
(asdf:oos 'asdf:load-op 'cl-plplot)

;;; Using R!

(asdf:oos 'asdf:compile-op 'rclg :force t)
(asdf:oos 'asdf:load-op 'rclg)


(in-package :rclg-user)

;; rclg-init::*r-started*

;;;#3 Start R within Lisp

(start-rclg)
;; rclg-init::*r-started*
(rclg-init::check-stack)
(r "Cstack_info")
(defparameter *x* (r seq 1 10))
(defparameter *y* (r rnorm 10))
*y*
(r plot *x* *y*)
*y*

(defparameter *r-version* (r "version"))

;; This is for illustrative purposes only.  It is not a "good" use of rnbi.
;; Really, you'll want rnbi to hold anonymous intermeditae results, like:
(r plot *x* (rnbi rnorm 10))

(r "Sys.getenv" "LD_LIBRARY_PATH")
(r "Sys.getenv" "LD_PRELOAD")

(r "ls")
(r ls)
(r "search")

(r "geterrmessage")

(r "library" "stats") 
(r library "MASS")
(r "library" "Biobase")

(setf my.lib "Biobase")
my.lib
(r library my.lib)

(r "ls")

(r "print.default" 3)
(r "rnorm" 10)

;; Working in the R space

(r assign "x" 5)
(r assign "x2" (list 1 2 3 5))

(r assign "x2" #(1 2 3 5 3 4 5))
(r assign "z" "y") ;; unlike the above, this assigns character data
(r "ls")
(r ls)

(setf my.r.x2 (r get "x2"))  ;; moving data from R to CL
(r assign "x2" my.r.x2)  ;; moving data from CL to R

;; The following is not the smartest thing to do!
;;(r q)



;;; How might we do statistics with Common Lisp? 
;;; How might we work with a data.frame?
;;; What could the structures be?  
;;; How much hinting, and of what type, should drive the data
;;; analysis?  

(defpackage :my-data-analysis-example
  (:documentation "Example work-package for a data analysis")
  (:use :common-lisp :lisp-stat)
  (:export results figures report))

(in-package :my-data-analysis-example)

(defvar my-dataset1 (read-file "data/test1.lisp"))
;; or
(defvar my-dataset2 (read-file "data/test1.csv" :type 'csv))

;;; manipulate

(setf my-dataset2 (set-description my-datasets2
				   :dependent-variables (list of symbols)))
(setf my-dataset2 (set-description my-datasets2
				   :independent-variables (list of symbols)))

;; the following could be true in many cases.
(assert 
 (list-intersection (get-description my-datasets2 :independent-variables)
		    (get-description my-datasets2 :dependent-variables)))
;;
;; but we could phrase better,i.e.
;;
(get-description
 my-datasets2
 :predicate-list-on-variable-metadata (list (and 'independent-variables
						 'dependent-variables)))


;; statistical relations re: input/output, as done above, is one
;; issue, another one is getting the right approach for statistical
;; typing, i.e. 
(get-description
 my-datasets2
 :predicate-list-on-variable-metadata (list 'ordinal-variables))


;; so we could use a set of logical ops to selection from variable
;; metadata, i.e.
;;    and, or, not
;; do we really need the simplifying extensions?


;;; output to REPL

(report my-dataset1 :style 'five-num)
(report my-dataset1 :style 'univariate)
(report my-dataset1 :style 'bivariate)
(report my-dataset1 :style 'metadata)

;;; to file?

(report my-dataset1
	:style 'five-num
	:format 'pdf
	:stream (filename-as-stream "my-dataset1-5num.pdf"))
(report my-dataset1 :style 'univariate)
(report my-dataset1 :style 'bivariate)
(report my-dataset1 :style 'metadata)

;;; so report could handle datasets... and models?

(report my-model :style 'formula)
(report my-model :style 'simulate
	(list :parameters (:eta 5 :mu 4 :sigma (list 2 1 0.5))
	      :number-of-reps 10))
;; should return a list of parameters along with range information,
;; useful for auto-building the above.   Note that there are 3 types
;; of parameters that can be considered -- we can have values which
;; define ddata, we can have values which define fixed values and some
;; could be things tht we estimate.  


(defgeneric report (object &optional style format stream)
  (:documentation "method for reporting on data"))

(defmethod report ((object dataset)
		   (style report-dataset-style-type)
		   (format output-format-type)
		   ((stream *repl*) output-stream-type))
  "dataset reporting")


(defmethod report ((object model)
		   (style report-model-style-type)
		   (format output-format-type)
		   ((stream *repl*) output-stream-type))
  "model reporting")

(defmethod report ((object analysis-instance)
		   (style report-analysis-style-type)
		   (format output-format-type)
		   ((stream *repl*) output-stream-type))
  "model + dataset reporting")


;; parameters are just things which get filled with values, repeatedly
;; with data, or by considering to need estimation.
(parameters my-model)
(parameters my-model :type 'data)
(parameters my-model :type 'fixed)
(parameters my-model :type 'estimate)
(parameters my-model :type '(estimate fixed))
(parameters my-model :list-types) ;; useful for list-based extraction
;; of particular types

(setf my-model-data-instance
      (compute model data :specification (list :spec 'linear-model
					       :depvar y
					       :indepvar (list x1 x2))))
(report my-model-data-instance)


;;; So how might we use this?  Probably need to consider the
;;; serialization of any lisp objects generated, perhaps via some form
;;; of memoization...?
(in-package :cl-user)

(my-data-analysis-example:report :type 'full)
(my-data-analysis-example:report :type 'summary)
(my-data-analysis-example:figures :type 'pdf :file "results-figs.pdf")

(my-data-analysis-example:report)

;;; more stuff

(send m :display)
(def m (regression-model (bind-columns iron aluminum) absorbtion))
(send m :help)
(send m :help :display)
(send m :help :basis)

(send m :plot-residuals)

(progn 
  ;; General Lisp, there is also a need to add, remove symbols from the
  ;; workspace/namespace.  This is a fundamental skill, similar to
  ;; stopping, which is critical.
  
  ;; boundp, fboundp
  ;; makunbound, fmakunbound
  )


(progn 
  ;;; A study in array vs list access
  (defparameter *x* (list 1 2 3))
  (defparameter *y* #(1 2 3))
  (defparameter *z* (list 1 (list 2 3) (list 4 5 (list 6 7)) ))
  (length *x*)
  (length *y*)
  (length *z*) ; => need a means to make this 7.
  (length  (reduce #'cons *z*)) ; => not quite -- missing iterative 

  (nelts *x*)
  (nth 1 *x*)
  (aref *y* 1)
  (setf (nth 1 *x*) 6)
  *x*
  (setf (aref *y* 1) 6)
  *y*
  )

(in-package :ls-user)

(progn
  (defparameter *x* (make-vector 5 :initial-contents '((1d0 2d0 3d0 4d0 5d0))))
  ;; estimating a mean, simple way.
  (/ (loop for i from 0 to (- (nelts *x*) 1)
	summing (vref *x* i))
     (nelts *x*))

  (defun mean (x)
    (checktype x 'vector-like)
    (/ (loop for i from 0 to (- (nelts *x*) 1)
	  summing (vref *x* i))
       (nelts *x*)))

  ;; estimating variance, Moments
  (let ((meanx (mean *x*))
	(n (nelts *x*)))
    (/ (loop for i from 0 to (1-  n)
	  summing (* (- (vref *x* i) meanx)
		     (- (vref *x* i) meanx)))
       n))

  ;; estimating variance, Moments
  (let ((meanx (mean *x*))
	(nm1 (1- (nelts *x*))))
    (/ (loop for i from 0 to nm1
	  summing (* (- (vref *x* i) meanx)
		     (- (vref *x* i) meanx) ))
       nm1))

 )

;;;;;;;;;;;;;;; Data stuff

(progn ;; Data setup

  ;; Making data-frames (i.e. cases (rows) by variables (columns))
  ;; takes a bit of getting used to.  For this, it is important to
  ;; realize that we can do the following:
  ;; #1 - consider the possibility of having a row, and transposing
  ;; it, so the list-of-lists is:  ((1 2 3 4 5))     (1 row, 5 columns)
  ;; #2 - naturally list-of-lists: ((1)(2)(3)(4)(5)) (5 rows, 1 column)
  ;; see src/data/listoflist.lisp for code to process this particular
  ;; data structure.
  (defparameter *indep-vars-1-matrix*
    (transpose  (make-matrix 1 (length iron)
		 :initial-contents
		 (list (mapcar #'(lambda (x) (coerce x 'double-float))
			       iron))))
    "creating iron into double float, straightforward")

  (documentation '*indep-vars-1-matrix* 'variable)
  ;; *indep-vars-1-matrix*

  ;; or directly:
  (defparameter *indep-vars-1a-matrix*
    (make-matrix (length iron)  1 
		 :initial-contents
		 (mapcar #'(lambda (x) (list  (coerce x 'double-float)))
			       iron)))
  ;; *indep-vars-1a-matrix*

  ;; and mathematically, they seem equal:
  (m= *indep-vars-1-matrix* *indep-vars-1a-matrix*) ; => T
  ;; but of course not completely... 
  (eql *indep-vars-1-matrix* *indep-vars-1a-matrix*) ; => NIL
  (eq *indep-vars-1-matrix* *indep-vars-1a-matrix*) ; => NIL

  ;; and verify...
  (print *indep-vars-1-matrix*)
  (print *indep-vars-1a-matrix*)

  (documentation 'lisp-matrix:bind2 'function) ; by which we mean:
  (documentation 'bind2 'function)
  (bind2 *indep-vars-1-matrix* *indep-vars-1a-matrix* :by :column) ; 2 col
  (bind2 *indep-vars-1-matrix* *indep-vars-1a-matrix* :by :row) ; 1 long col
  
  ;; the weird way  
  (defparameter *indep-vars-2-matrix*
    (transpose (make-matrix  2 (length iron)
			     :initial-contents
			     (list
			      (mapcar #'(lambda (x) (coerce x 'double-float))
				      iron)
			      (mapcar #'(lambda (x) (coerce x 'double-float))
				      aluminum)))))
  ;; *indep-vars-2-matrix*
  
  ;; the "right"? way  
  (defparameter *indep-vars-2-matrix*
    (make-matrix (length iron) 2
		 :initial-contents
		 (mapcar #'(lambda (x y) 
			     (list (coerce x 'double-float)
				   (coerce y 'double-float)))
			 iron aluminum)))
  ;; *indep-vars-2-matrix*
    

  ;; The below FAILS due to coercion issues; it just isn't lispy, it's R'y.
#|
  (defparameter *dep-var* (make-vector (length absorbtion)
				       :initial-contents (list absorbtion)))
|#
  ;; BUT below, this should be the right type.
  (defparameter *dep-var*
    (make-vector (length absorbtion)
		 :type :row
		 :initial-contents
		 (list 
		  (mapcar #'(lambda (x) (coerce x 'double-float))
			  absorbtion))))
  ;; *dep-var*

  
  (defparameter *dep-var-int*
    (make-vector (length absorbtion)
		 :type :row
		 :element-type 'integer
		 :initial-contents (list absorbtion)))
  
  (typep *dep-var* 'matrix-like)	; => T
  (typep *dep-var* 'vector-like)	; => T
  
  (typep *indep-vars-1-matrix* 'matrix-like) ; => T
  (typep *indep-vars-1-matrix* 'vector-like) ; => T
  (typep *indep-vars-2-matrix* 'matrix-like) ; => T
  (typep *indep-vars-2-matrix* 'vector-like) ; => F

  iron
  ;; following fails, need to ensure that we work on list elts, not just
  ;; elts within a list:
  ;;
  ;;     (coerce iron 'real) 
  ;;
  ;; the following is a general list-conversion coercion approach -- is
  ;; there a more efficient way?
  ;;     (coerce 1 'real)
  ;;     (mapcar #'(lambda (x) (coerce x 'double-float)) iron)
  
  (princ "Data Set up"))




(progn ;; Data setup

  (describe 'make-matrix)

  (defparameter *indep-vars-2-matrix*
    (make-matrix (length iron) 2
		 :initial-contents
		 (mapcar #'(lambda (x y) 
			     (list (coerce x 'double-float)
				   (coerce y 'double-float)))
			 iron aluminum)))

    
  (defparameter *dep-var*
    (make-vector (length absorbtion)
		 :type :row
		 :initial-contents
		 (list 
		  (mapcar #'(lambda (x) (coerce x 'double-float))
			  absorbtion))))

  (make-dataframe *dep-var*)
  (make-dataframe (transpose *dep-var*))

  (defparameter *dep-var-int*
    (make-vector (length absorbtion)
		 :type :row
		 :element-type 'integer
		 :initial-contents (list absorbtion)))


  (defparameter *xv+1a*
    (make-matrix
     8 2
     :initial-contents #2A((1d0 1d0)
			   (1d0 3d0)
			   (1d0 2d0)
			   (1d0 4d0)
			   (1d0 3d0)
			   (1d0 5d0)
			   (1d0 4d0)
			   (1d0 6d0))))

  (defparameter *xv+1b*
    (bind2
     (ones 8 1)
     (make-matrix
      8 1
      :initial-contents '((1d0)
			  (3d0)
			  (2d0)
			  (4d0)
			  (3d0)
			  (5d0)
			  (4d0)
			  (6d0)))
     :by :column))

  (m= *xv+1a* *xv+1b*) ; => T
  
  (princ "Data Set up"))



;;;; LM

(progn 

  (defparameter *y*
    (make-vector
     8
     :type :row
     :initial-contents '((1d0 2d0 3d0 4d0 5d0 6d0 7d0 8d0))))


  (defparameter *xv+1*
    (make-matrix
     8 2
     :initial-contents '((1d0 1d0)
			 (1d0 3d0)
			 (1d0 2d0)
			 (1d0 4d0)
			 (1d0 3d0)
			 (1d0 5d0)
			 (1d0 4d0)
			 (1d0 6d0))))


  ;; so something like (NOTE: matrices are transposed to begin with, hence the incongruety)
  (defparameter *xtx-2* (m* (transpose *xv+1*) *xv+1*))
  ;; #<LA-SIMPLE-MATRIX-DOUBLE  2 x 2
  ;;  8.0d0 28.0d0
  ;;  28.0d0 116.0d0>

  (defparameter *xty-2* (m* (transpose *xv+1*)  (transpose *y*)))
  ;; #<LA-SIMPLE-VECTOR-DOUBLE (2 x 1)
  ;;  36.0d0
  ;;  150.0d0>

  (defparameter *rcond-2* 0.000001)
  (defparameter *betahat-2*  (gelsy *xtx-2* *xty-2* *rcond-2*))
  ;; *xtx-2* => "details of complete orthogonal factorization"
  ;; according to man page:
  ;; #<LA-SIMPLE-MATRIX-DOUBLE  2 x 2
  ;;  -119.33147112141039d0 -29.095426104883202d0
  ;;  0.7873402682880205d0 -1.20672274167718d0>

  ;; *xty-2* => output becomes solution:
  ;; #<LA-SIMPLE-VECTOR-DOUBLE (2 x 1)
  ;;  -0.16666666666668312d0
  ;;  1.333333333333337d0>

  *betahat-2* ; which matches R, see below

  (documentation 'gelsy 'function)


;;   (#<LA-SIMPLE-VECTOR-DOUBLE (2 x 1)
;;    -0.16666666666668312 1.333333333333337>
;;    2)

;;   ## Test case in R:
;;   x <- c( 1.0, 3.0, 2.0, 4.0, 3.0, 5.0, 4.0, 6.0)
;;   y <- c( 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0)
;;   lm(y~x)
;;   ## => Call:  lm(formula = y ~ x)

;;   Coefficients:  (Intercept)            x  
;;                      -0.1667       1.3333  

;;   summary(lm(y~x))
;;   ## =>

;;   Call:
;;   lm(formula = y ~ x)

;;   Residuals:
;;          Min         1Q     Median         3Q        Max 
;;   -1.833e+00 -6.667e-01 -3.886e-16  6.667e-01  1.833e+00 

;;   Coefficients:
;;               Estimate Std. Error t value Pr(>|t|)   
;;   (Intercept)  -0.1667     1.1587  -0.144  0.89034   
;;   x             1.3333     0.3043   4.382  0.00466 **
;;   ---
;;   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

;;   Residual standard error: 1.291 on 6 degrees of freedom
;;   Multiple R-squared: 0.7619,	Adjusted R-squared: 0.7222 
;;   F-statistic:  19.2 on 1 and 6 DF,  p-value: 0.004659 



  ;; which suggests one might do (modulo ensuring correct
  ;; orientations).  When this is finalized, it should migrate to
  ;; CLS.
  ;;


  (defparameter *n* 20) ; # rows = # obsns
  (defparameter *p* 10) ; # cols = # vars 
  (defparameter *x-temp*  (rand *n* *p*))
  (defparameter *b-temp*  (rand *p* 1))
  (defparameter *y-temp*  (m* *x-temp* *b-temp*))
  ;; so Y=Xb + \eps
  (defparameter *rcond* (* (coerce (expt 2 -52) 'double-float)
		   (max (nrows *x-temp*) (ncols *y-temp*))))
  (defparameter *orig-x* (copy *x-temp*))
  (defparameter *orig-b* (copy *b-temp*))
  (defparameter *orig-y* (copy *y-temp*))

  (defparameter *lm-result* (lm *x-temp* *y-temp*))
  (princ (first *lm-result*))
  (princ (second *lm-result*))
  (princ (third *lm-result*))
  (v= (third *lm-result*)
      (v- (first (first *lm-result*))
	  (first  (second *lm-result*))))



  
  ;; Some issues exist in the LAPACK vs. LINPACK variants, hence R
  ;; uses LINPACK primarily, rather than LAPACK.  See comments in R
  ;; source for issues.  


  ;; Goal is to start from X, Y and then realize that if
  ;; Y = X \beta, then,   i.e. 8x1 = 8xp px1  + 8x1
  ;;      XtX \hat\beta = Xt Y
  ;; so that we can solve the equation  W \beta = Z   where W and Z
  ;; are known, to estimate \beta.

  ;; the above is known to be numerically instable -- some processing
  ;; of X is preferred and should be done prior.  And most of the
  ;; transformation-based work does precisely that.

  ;; recall:  Var[Y] = E[(Y - E[Y])(Y-E[Y])t]
  ;;   = E[Y Yt] - 2 \mu \mut + \mu \mut
  ;;   = E[Y Yt] - \mu \mut

  ;; Var Y = E[Y^2] - \mu^2


  ;; For initial estimates of covariance of \hat\beta:

  ;; \hat\beta = (Xt X)^-1 Xt Y
  ;; with E[ \hat\beta ] 
  ;;        = E[ (Xt X)^-1 Xt Y ]
  ;;        = E[(Xt X)^-1 Xt (X\beta)]
  ;;        = \beta 
  ;;        
  ;; So Var[\hat\beta] = ...
  ;;     (Xt X)
  ;; and this gives SE(\beta_i) = (* (sqrt (mref Var i i)) adjustment)


  ;; from docs:

  (setf *temp-result* 
	(let ((*default-implementation* :foreign-array))
	  (let* ((m 10)
		 (n 10)
		 (a (rand m n))
		 (x (rand n 1))
		 (b (m* a x))
		 (rcond (* (coerce (expt 2 -52) 'double-float)
			   (max (nrows a) (ncols a))))
		 (orig-a (copy a))
		 (orig-b (copy b))
		 (orig-x (copy x)))
	    (list x (gelsy a b rcond))
	    ;; no applicable conversion?
	    ;; (m-   (#<FA-SIMPLE-VECTOR-DOUBLE (10 x 1)) 
	    ;;       (#<FA-SIMPLE-VECTOR-DOUBLE (10 x 1)) )
	    (v- x (first (gelsy a b rcond))))))

  
  (princ *temp-result*)
  
  (setf *temp-result* 
	(let ((*default-implementation* :lisp-array))
	  (let* ((m 10)
		 (n 10)
		 (a (rand m n))
		 (x (rand n 1))
		 (b (m* a x))
		 (rcond (* (coerce (expt 2 -52) 'double-float)
			   (max (nrows a) (ncols a))))
		 (orig-a (copy a))
		 (orig-b (copy b))
		 (orig-x (copy x)))
	    (list x (gelsy a b rcond))
	    (m- x (first  (gelsy a b rcond)))
	    )))
  (princ *temp-result*)


  (defparameter *xv*
    (make-vector
     8
     :type :row ;; default, not usually needed!
     :initial-contents '((1d0 3d0 2d0 4d0 3d0 5d0 4d0 6d0))))

  (defparameter *y*
    (make-vector
     8
     :type :row
     :initial-contents '((1d0 2d0 3d0 4d0 5d0 6d0 7d0 8d0))))

  ;; so something like (NOTE: matrices are transposed to begin with, hence the incongruety)
  (defparameter *xtx-1* (m* *xv* (transpose *xv*)))
  (defparameter *xty-1* (m* *xv* (transpose  *y*)))
  (defparameter *rcond-in* (* (coerce (expt 2 -52) 'double-float)
			      (max (nrows *xtx-1*)
				   (ncols *xty-1*))))

  (defparameter *betahat*  (gelsy *xtx-1* *xty-1* *rcond-in*))

  ;;  (#<LA-SIMPLE-VECTOR-DOUBLE (1 x 1)
  ;;  1.293103448275862>
  ;;  1)

  ;;   ## Test case in R:
  ;;   x <- c( 1.0, 3.0, 2.0, 4.0, 3.0, 5.0, 4.0, 6.0)
  ;;   y <- c( 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0)
  ;;   lm(y~x-1)
  ;;   ## => 
  ;;   Call:
  ;;   lm(formula = y ~ x - 1)

  ;;   Coefficients:
  ;;       x  
  ;;   1.293  

  (first  *betahat*))



#|
  (type-of #2A((1 2 3 4 5)
               (10 20 30 40 50)))

  (type-of (rand 10 20))

  (typep #2A((1 2 3 4 5)
	     (10 20 30 40 50))
	 'matrix-like)

  (typep (rand 10 20) 'matrix-like)

  (typep #2A((1 2 3 4 5)
	     (10 20 30 40 50))
	 'array)

  (typep (rand 10 20) 'array)
|#

;;;;;;;;;;;;;;;;; ===========

(defparameter *my-df-trees*
  (make-instance 'dataframe-array
		 :storage
		 (listoflist->array
		  (transpose-listoflist 
		   (rsm.string:file->number-table
		    (localized-pathto "Data/trees.csv"))))
		 :doc "This is an interesting dataframe-array that currently fails"))

;; *my-df-trees*

(defparameter *my-df-trees2*
  (make-instance 'dataframe-array
		 :storage
		 (listoflist->array
		  (transpose-listoflist 
		   (rsm.string:file->number-table
		    (localized-pathto "Data/trees.csv")
		    :delims ",")))
		 :doc "This is an interesting dataframe-array that currently fails"))
;; *my-df-trees2*
;; (dataset *my-df-trees2*)

(defparameter *my-df-trees2a*
  (make-instance 'dataframe-array
		 :storage
		 (listoflist->array
		   (rsm.string:file->number-table
		    (localized-pathto "Data/trees.csv")
		    :delims ","))
		 :doc "This is an interesting dataframe-array that currently fails"))

;; *my-df-trees2a*
;; (dataset *my-df-trees2a*)
