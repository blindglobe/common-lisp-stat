;;; -*- mode: lisp -*-
;;; Copyright (c) 2007, by A.J. Rossini <blindglobe@gmail.com>
;;; See COPYRIGHT file for any additional restrictions (BSD license).
;;; Since 1991, ANSI was finally finished.  Edited for ANSI Common Lisp. 

;;; File:       ls-demo.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c) 2007, AJ Rossini.  BSD.
;;; Purpose:    demonstrations of how one might use CLS.
;;; Time-stamp: <>
;;; Creation:   

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

(load "init.lisp")  ;; init needs to be more like the asdf-loader for lisp-stat, though it is pretty close. 

;;; checking exports.
;;; This is generally not how I expect it to be used.

(in-package :cl-user)
(lisp-stat:binomial-quant 0.95 3 0.4) ;;; 3
(lisp-stat:binomial-quant 0 3 0.4) ;;; 0
(lisp-stat:normal-rand 20) ;;; DOESN'T RETURN


;;; This is how I expect it to be used, either with work in ls-user,
;;; or a cloned package similar to ls-user.

(in-package :ls-user)

;; Probability

(binomial-quant 0.95 3 0.4) ; 3
(binomial-quant 0 3 0.4) ; 0
(normal-rand 20)

;; Matrix algebra.

(chol-decomp  #2A((2 3 4) (1 2 4) (2 4 5)))
;; (#2A((1.7888543819998317 0.0 0.0)
;;      (1.6770509831248424 0.11180339887498929 0.0)
;;      (2.23606797749979 2.23606797749979 3.332000937312528e-8))
;;  5.000000000000003)

(defvar my-chol-decomp-test (chol-decomp  #2A((2 3 4) (1 2 4) (2 4 5))))
my-chol-decomp-test
(nth 0 my-chol-decomp-test)
(nth 1 my-chol-decomp-test)




(lu-decomp  #2A((2 3 4) (1 2 4) (2 4 5)))
;; (#2A((2.0 3.0 4.0) (1.0 1.0 1.0) (0.5 0.5 1.5)) #(0 2 2) -1.0 NIL)

(lu-solve 
 (lu-decomp #2A((2 3 4) (1 2 4) (2 4 5))) 
 #(2 3 4))
;; #(-2.333333333333333 1.3333333333333335 0.6666666666666666)

(inverse #2A((2 3 4) (1 2 4) (2 4 5)))
;; #2A((2.0 -0.33333333333333326 -1.3333333333333335)
;;     (-1.0 -0.6666666666666666 1.3333333333333333)
;;     (0.0 0.6666666666666666 -0.3333333333333333))

(sv-decomp  #2A((2 3 4) (1 2 4) (2 4 5)))
;; (#2A((-0.5536537653489974 0.34181191712789266 -0.7593629708013371)
;;      (-0.4653437312661058 -0.8832095891230851 -0.05827549615722014)
;;      (-0.6905959164998124 0.3211003503429828 0.6480523475178517))
;;  #(9.699290438141343 0.8971681569301373 0.3447525123483081)
;;  #2A((-0.30454218417339873 0.49334669582252344 -0.8147779426198863)
;;      (-0.5520024849987308 0.6057035911404464 0.5730762743603965)
;;      (-0.7762392122368734 -0.6242853493399995 -0.08786630745236332))
;;  T)

(qr-decomp  #2A((2 3 4) (1 2 4) (2 4 5)))
;; (#2A((-0.6666666666666665 0.7453559924999298 5.551115123125783e-17)
;;      (-0.3333333333333333 -0.2981423969999719 -0.894427190999916)
;;      (-0.6666666666666666 -0.5962847939999439 0.44721359549995787))
;;  #2A((-3.0 -5.333333333333334 -7.333333333333332)
;;      (0.0 -0.7453559924999292 -1.1925695879998877)
;;      (0.0 0.0 -1.3416407864998738)))

(rcondest #2A((2 3 4) (1 2 4) (2 4 5)))
;; 6.8157451e7

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



;;; Log-gamma function

(log-gamma 3.4) ;;1.0923280596789584

;;; Probability functions

;;; looking at these a bit more, perhaps a more CLOSy style is needed, i.e. 
;;; (quantile :list-or-cons loc :type type (one of 'empirical 'normal 'cauchy, etc...))
;;; similar for the cdf, density, and rand.
;;; Probably worth figuring out how to add a new distribution
;;; efficiently, i.e. by keeping some kind of list.

;;; Normal distribution

(normal-quant 0.95) ;;1.6448536279366268
(normal-cdf 1.3) ;;0.9031995154143897
(normal-dens 1.3) ;;0.17136859204780736
(normal-rand 2) ;;(-0.40502015f0 -0.8091404f0)

(bivnorm-cdf 0.2 0.4 0.6) ;;0.4736873734160288

;;; Cauchy distribution

(cauchy-quant 0.95) ;;6.313751514675031 
(cauchy-cdf 1.3) ;;0.7912855998398473
(cauchy-dens 1.3) ;;0.1183308127104695 
(cauchy-rand 2) ;;(-1.06224644160405 -0.4524695943939537)

;;; Gamma distribution

(gamma-quant 0.95 4.3) ;;8.178692439291645
(gamma-cdf 1.3 4.3) ;;0.028895150986674906
(gamma-dens 1.3 4.3) ;;0.0731517686447374
(gamma-rand 2 4.3) ;;(2.454918912880936 4.081365384357454)

;;; Chi-square distribution

(chisq-quant 0.95 3) ;;7.814727903379012
(chisq-cdf 1 5) ;;0.03743422675631789
(chisq-dens 1 5) ;;0.08065690818083521
(chisq-rand 2 4) ;;(1.968535826180572 2.9988646156942997)

;;; Beta distribution

(beta-quant 0.95 3 2) ;;0.9023885371149876
(beta-cdf 0.4 2 2.4) ;;0.4247997418541529 
(beta-dens 0.4 2 2.4) ;;1.5964741858913518 
(beta-rand 2 2 2.4) ;;(0.8014897077282279 0.6516371997922659) 

;;; t distribution

(t-quant 0.95 3) ;;2.35336343484194
(t-cdf 1 2.3) ;;0.794733624298342
(t-dens 1 2.3) ;;0.1978163816318102
(t-rand 2 2.3) ;;(-0.34303672776089306 -1.142505872436518)

;;; F distribution

(f-quant 0.95 3 5) ;;5.409451318117459
(f-cdf 1 3.2 5.4) ;;0.5347130905510765
(f-dens 1 3.2 5.4) ;;0.37551128864591415
(f-rand 2 3 2) ;;(0.7939093442091963 0.07442694152491144)

;;; Poisson distribution

(poisson-quant 0.95 3.2) ;;6
(poisson-cdf 1 3.2) ;;0.17120125672252395
(poisson-pmf 1 3.2) ;;0.13043905274097067
(poisson-rand 5 3.2) ;;(2 1 2 0 3)

;;; Binomial distribution

(binomial-quant 0.95 3 0.4) ;;; DOESN'T RETURN
(binomial-quant 0 3 0.4) ;;; -2147483648
(binomial-cdf 1 3 0.4) ;;0.6479999999965776
(binomial-pmf 1 3 0.4) ;;0.4320000000226171
(binomial-rand 5 3 0.4) ;;(2 2 0 1 2)

;;; OBJECT SYSTEM

(in-package :ls-user)
(defproto *test-proto*)
*test-proto*
(defmeth *test-proto* :make-data (&rest args) nil)
(send *test-proto* :make-data)


;;; Testing 

(in-package :lisp-stat-unittests)
(testsuites)
(print-tests)
(run-tests)
(last-test-status)
;;(failures)

(describe (run-tests :suite 'lisp-stat-testsupport))

(testsuite-tests 'lisp-stat)
(run-tests :suite 'lisp-stat)
(describe (run-tests :suite 'lisp-stat))

(run-tests :suite 'lisp-stat-probdistn)
(describe (run-tests :suite 'lisp-stat-probdistn))
(run-tests :suite 'lisp-stat-spec-fns)
(describe (run-tests :suite 'lisp-stat-spec-fns))

(find-testsuite 'lisp-stat-lin-alg)
(testsuite-tests 'lisp-stat-lin-alg)
(run-tests :suite 'lisp-stat-lin-alg)
(describe (run-tests :suite 'lisp-stat-lin-alg))

;;; Data Analysis test

(in-package :ls-user)


;; LispStat 1 approach to variables

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


;; Simple univariate variable-specific descriptions.
(fivnum absorbtion)
(median absorbtion) (sort absorbtion) (rank absorbtion)
(standard-deviation absorbtion)
(interquartile-range absorbtion)

;;; How do I make these work?
(bind-columns aluminum iron)
(apply #'bind-columns (list aluminum iron))
(bind-columns #2a((1 2)(3 4)) #(5 6))

(sequencep aluminum)
(sequencep iron)
(matrixp iron)

*variables*

(variables)
(undef 'iron)
(variables)


(defvar fit1 nil)
(setf fit1 (regression-model absorbtion iron))
(send fit1 :display)
(send fit1 :residuals)

(def m (regression-model (list iron aluminum) absorbtion))
(send m :help) (send m :plot-residuals)

