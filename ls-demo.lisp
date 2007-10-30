
(asdf:operate 'asdf:compile-op 'cffi)

(load "init.lisp")
(asdf:operate 'asdf:compile-op 'lispstat :force t)
(asdf:operate 'asdf:compile-op 'lispstat)

;; (asdf:operate 'asdf:load-op 'cffi)
;; (asdf:operate 'asdf:load-op 'rclg)

(asdf:oos 'asdf:load-op :lispstat)

(setf *my-base-directory*
      #p"/home/tony/sandbox/CLS.git/"
      #p"/Users/ungil/lisp/CommonLispStat/"
      )


;;; To make it easier for Carlos...
(load "/Users/ungil/lisp/CommonLispStat/init.lisp")
(load "/Users/ungil/lisp/CommonLispStat/cffiglue.lsp")

(in-package :cl-user)

;; Can we get from both the subpackage as well as the "basic
;; configuration"?

(lisp-stat-basics:binomial-quant 0.95 3 0.4) ;;; DOESN'T RETURN
(lisp-stat-basics:binomial-quant 0 3 0.4) ;;; -2147483648

(lisp-stat:binomial-quant 0.95 3 0.4) ;;; DOESN'T RETURN
(lisp-stat:binomial-quant 0 3 0.4) ;;; -2147483648


(in-package :ls-user)

(binomial-quant 0.95 3 0.4) ;;; DOESN'T RETURN
(binomial-quant 0 3 0.4) ;;; -2147483648

;; Once the above seem to work, we are on our way!








;; optimization-related functions not tested
;;(lisp-stat-optimize:numgrad
;;(lisp-stat-optimize:numhess
;;(lisp-stat-optimize::minfo-maximize

;; the rest return reasonable values, except BINOMIAL-QUANT:
;; (lisp-stat-basics:binomial-quant 0.95 3 0.4) ;;; DOESN'T RETURN
;; (lisp-stat-basics:binomial-quant 0 3 0.4) ;;; -2147483648

(lisp-stat-linalg:chol-decomp  #2A((2 3 4) (1 2 4) (2 4 5)))
;; (#2A((1.7888543819998317 0.0 0.0)
;;      (1.6770509831248424 0.11180339887498929 0.0)
;;      (2.23606797749979 2.23606797749979 3.332000937312528e-8))
;;  5.000000000000003)

(lisp-stat-linalg:lu-decomp  #2A((2 3 4) (1 2 4) (2 4 5)))
;; (#2A((2.0 3.0 4.0) (1.0 1.0 1.0) (0.5 0.5 1.5)) #(0 2 2) -1.0 NIL)

(lisp-stat-linalg:lu-solve 
 (lisp-stat-linalg:lu-decomp #2A((2 3 4) (1 2 4) (2 4 5))) 
 #(2 3 4)) 
;; #(-2.333333333333333 1.3333333333333335 0.6666666666666666)

(lisp-stat-linalg:inverse #2A((2 3 4) (1 2 4) (2 4 5)))
;; #2A((2.0 -0.33333333333333326 -1.3333333333333335)
;;     (-1.0 -0.6666666666666666 1.3333333333333333)
;;     (0.0 0.6666666666666666 -0.3333333333333333))

(lisp-stat-linalg:sv-decomp  #2A((2 3 4) (1 2 4) (2 4 5)))
;; (#2A((-0.5536537653489974 0.34181191712789266 -0.7593629708013371)
;;      (-0.4653437312661058 -0.8832095891230851 -0.05827549615722014)
;;      (-0.6905959164998124 0.3211003503429828 0.6480523475178517))
;;  #(9.699290438141343 0.8971681569301373 0.3447525123483081)
;;  #2A((-0.30454218417339873 0.49334669582252344 -0.8147779426198863)
;;      (-0.5520024849987308 0.6057035911404464 0.5730762743603965)
;;      (-0.7762392122368734 -0.6242853493399995 -0.08786630745236332))
;;  T)

(lisp-stat-linalg:qr-decomp  #2A((2 3 4) (1 2 4) (2 4 5)))
;; (#2A((-0.6666666666666665 0.7453559924999298 5.551115123125783e-17)
;;      (-0.3333333333333333 -0.2981423969999719 -0.894427190999916)
;;      (-0.6666666666666666 -0.5962847939999439 0.44721359549995787))
;;  #2A((-3.0 -5.333333333333334 -7.333333333333332)
;;      (0.0 -0.7453559924999292 -1.1925695879998877)
;;      (0.0 0.0 -1.3416407864998738)))

(lisp-stat-linalg:rcondest #2A((2 3 4) (1 2 4) (2 4 5))) 
;; 6.8157451e7

(lisp-stat-linalg:eigen #2A((2 3 4) (1 2 4) (2 4 5)))
;; (#(10.656854249492381 -0.6568542494923802 -0.9999999999999996)
;;  (#(0.4999999999999998 0.4999999999999997 0.7071067811865475)
;;   #(-0.49999999999999856 -0.5000000000000011 0.7071067811865474)
;;   #(0.7071067811865483 -0.7071067811865466 -1.2560739669470215e-15))
;;  NIL)

(lisp-stat-linalg:spline #(1.0 1.2 1.3 1.8 2.1 2.5)  
 #(1.2 2.0 2.1 2.0 1.1 2.8) :xvals 6)
;; ((1.0 1.3 1.6 1.9 2.2 2.5)
;;  (1.2 2.1 2.2750696543866313 1.6465231041904045 1.2186576148879609 2.8))

;;; using KERNEL-SMOOTH-FRONT, not KERNEL-SMOOTH-CPORT
(lisp-stat-linalg:kernel-smooth #(1.0 1.2 1.3 1.8 2.1 2.5)  
#(1.2 2.0 2.1 2.0 1.1 2.8) :xvals 5)
;; ((1.0 1.375 1.75 2.125 2.5)
;;  (1.6603277642110226 1.9471748095239771 1.7938127405752287 
;;   1.5871511322219498 2.518194783156392))

(lisp-stat-linalg:kernel-dens #(1.0 1.2 2.5 2.1 1.8 1.2) :xvals 5)
;; ((1.0 1.375 1.75 2.125 2.5)
;;  (0.7224150453621405 0.5820045548233707 0.38216411702854214 
;;   0.4829822708587095 0.3485939156929503))

(lisp-stat-linalg:fft #(1.0 1.2 2.5 2.1 1.8))
;; #(#C(1.0 0.0) #C(1.2 0.0) #C(2.5 0.0) #C(2.1 0.0) #C(1.8 0.0))

(lisp-stat-descriptive-statistics:lowess 
 #(1.0 1.2 2.5 2.1 1.8 1.2)  #(1.2 2.0 2.1 2.0 1.1 2.8))
;; (#(1.0 1.2 1.2 1.8 2.1 2.5))

;;;; Log-gamma function

(lisp-stat-basics:log-gamma 3.4) ;;1.0923280596789584

;;;; Normal distribution

(lisp-stat-basics:normal-quant 0.95) ;;1.6448536279366268
(lisp-stat-basics:normal-cdf 1.3) ;;0.9031995154143897
(lisp-stat-basics:normal-dens 1.3) ;;0.17136859204780736
(lisp-stat-basics:normal-rand 2) ;;(-0.40502015f0 -0.8091404f0)

(lisp-stat-basics:bivnorm-cdf 0.2 0.4 0.6) ;;0.4736873734160288

;;;; Cauchy distribution

(lisp-stat-basics:cauchy-quant 0.95) ;;6.313751514675031 
(lisp-stat-basics:cauchy-cdf 1.3) ;;0.7912855998398473
(lisp-stat-basics:cauchy-dens 1.3) ;;0.1183308127104695 
(lisp-stat-basics:cauchy-rand 2) ;;(-1.06224644160405 -0.4524695943939537)

;;;; Gamma distribution

(lisp-stat-basics:gamma-quant 0.95 4.3) ;;8.178692439291645
(lisp-stat-basics:gamma-cdf 1.3 4.3) ;;0.028895150986674906
(lisp-stat-basics:gamma-dens 1.3 4.3) ;;0.0731517686447374
(lisp-stat-basics:gamma-rand 2 4.3) ;;(2.454918912880936 4.081365384357454)

;;;; Chi-square distribution

(lisp-stat-basics:chisq-quant 0.95 3) ;;7.814727903379012
(lisp-stat-basics:chisq-cdf 1 5) ;;0.03743422675631789
(lisp-stat-basics:chisq-dens 1 5) ;;0.08065690818083521
(lisp-stat-basics:chisq-rand 2 4) ;;(1.968535826180572 2.9988646156942997)

;;;; Beta distribution

(lisp-stat-basics:beta-quant 0.95 3 2) ;;0.9023885371149876
(lisp-stat-basics:beta-cdf 0.4 2 2.4) ;;0.4247997418541529 
(lisp-stat-basics:beta-dens 0.4 2 2.4) ;;1.5964741858913518 
(lisp-stat-basics:beta-rand 2 2 2.4) ;;(0.8014897077282279 0.6516371997922659) 

;;;; t distribution

(lisp-stat-basics:t-quant 0.95 3) ;;2.35336343484194
(lisp-stat-basics:t-cdf 1 2.3) ;;0.794733624298342
(lisp-stat-basics:t-dens 1 2.3) ;;0.1978163816318102
(lisp-stat-basics:t-rand 2 2.3) ;;(-0.34303672776089306 -1.142505872436518)

;;;; F distribution

(lisp-stat-basics:f-quant 0.95 3 5) ;;5.409451318117459
(lisp-stat-basics:f-cdf 1 3.2 5.4) ;;0.5347130905510765
(lisp-stat-basics:f-dens 1 3.2 5.4) ;;0.37551128864591415
(lisp-stat-basics:f-rand 2 3 2) ;;(0.7939093442091963 0.07442694152491144)

;;;; Poisson distribution

(lisp-stat-basics:poisson-quant 0.95 3.2) ;;6
(lisp-stat-basics:poisson-cdf 1 3.2) ;;0.17120125672252395
(lisp-stat-basics:poisson-pmf 1 3.2) ;;0.13043905274097067
(lisp-stat-basics:poisson-rand 5 3.2) ;;(2 1 2 0 3)

;;;; Binomial distribution

(lisp-stat-basics:binomial-quant 0.95 3 0.4) ;;; DOESN'T RETURN
(lisp-stat-basics:binomial-quant 0 3 0.4) ;;; -2147483648
(lisp-stat-basics:binomial-cdf 1 3 0.4) ;;0.6479999999965776
(lisp-stat-basics:binomial-pmf 1 3 0.4) ;;0.4320000000226171
(lisp-stat-basics:binomial-rand 5 3 0.4) ;;(2 2 0 1 2)
