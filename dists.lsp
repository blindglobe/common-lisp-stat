;;; -*- mode: lisp -*-
;;; Copyright (c) 2005--2007, by A.J. Rossini <blindglobe@gmail.com>
;;; See COPYRIGHT file for any additional restrictions (BSD license).
;;; Since 1991, ANSI was finally finished.  Edited for ANSI Common Lisp. 

;;;; dists -- Lisp-Stat interface to basic probability distribution routines
;;;; 
;;;; Copyright (c) 1991, by Luke Tierney. Permission is granted for
;;;; unrestricted use.

;;;;
;;;; Package Setup
;;;;

(defpackage :lisp-stat-probability
    (:use :common-lisp
	  :lisp-stat-ffi-int
	  :lisp-stat-macros)
    (:export
     log-gamma
     uniform-rand
     normal-cdf normal-quant normal-dens normal-rand bivnorm-cdf
     cauchy-cdf cauchy-quant cauchy-dens cauchy-rand
     gamma-cdf gamma-quant gamma-dens gamma-rand
     chisq-cdf chisq-quant chisq-dens chisq-rand
     beta-cdf beta-quant beta-dens beta-rand
     t-cdf t-quant t-dens t-rand
     f-cdf f-quant f-dens f-rand
     poisson-cdf poisson-quant poisson-pmf poisson-rand 
     binomial-cdf binomial-quant binomial-pmf binomial-rand))

(in-package :lisp-stat-probability)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  CFFI support for Probability Distributions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; C-callable uniform generator
;;;

(cffi:defcfun ("register_uni" register-uni)
    :void (f :pointer))
(cffi:defcallback ccl-uni :int () (ccl-store-double (random 1.0)) 0)
(register-uni (cffi:callback ccl-uni))

(defun one-uniform-rand () (random 1.0))

;;;;
;;;; Log-gamma function
;;;;

(cffi:defcfun ("ccl_gamma" ccl-base-log-gamma)
    :double (x :double))
(defun base-log-gamma (x) 
  (ccl-base-log-gamma (float x 1d0)))

;;;;
;;;; Normal distribution
;;;;

(cffi:defcfun ("ccl_normalcdf" ccl-base-normal-cdf) 
    :double (x :double))
(defun base-normal-cdf (x) 
  (ccl-base-normal-cdf (float x 1d0)))

(cffi:defcfun ("ccl_normalquant" ccl-base-normal-quant)
    :double (x :double))
(defun base-normal-quant (x) 
  (ccl-base-normal-quant (float x 1d0)))

(cffi:defcfun ("ccl_normaldens" ccl-base-normal-dens)
        :double (x :double))
(defun base-normal-dens (x) 
  (ccl-base-normal-dens (float x 1d0)))

(cffi:defcfun ("ccl_normalrand" one-normal-rand) 
    :float)

(cffi:defcfun ("ccl_bnormcdf" ccl-base-bivnorm-cdf) 
    :double (x :double) (y :double) (z :double))
(defun base-bivnorm-cdf (x y z)
  (ccl-base-bivnorm-cdf (float x 1d0) (float y 1d0) (float z 1d0)))

;;;;
;;;; Cauchy distribution
;;;;

(cffi:defcfun ("ccl_cauchycdf" ccl-base-cauchy-cdf) 
            :double (x :double))
(defun base-cauchy-cdf (x) 
  (ccl-base-cauchy-cdf (float x 1d0)))

(cffi:defcfun ("ccl_cauchyquant" ccl-base-cauchy-quant)
            :double (x :double))
(defun base-cauchy-quant (x) 
  (ccl-base-cauchy-quant (float x 1d0)))

(cffi:defcfun ("ccl_cauchydens" ccl-base-cauchy-dens)
            :double (x :double))
(defun base-cauchy-dens (x) 
  (ccl-base-cauchy-dens (float x 1d0)))

(cffi:defcfun ("ccl_cauchyrand" one-cauchy-rand)
    :double)

;;;;
;;;; Gamma distribution
;;;;

(cffi:defcfun ("ccl_gammacdf" ccl-base-gamma-cdf)
            :double (x :double) (y :double))
(defun base-gamma-cdf (x y) 
  (ccl-base-gamma-cdf (float x 1d0) (float y 1d0)))

(cffi:defcfun ("ccl_gammaquant" ccl-base-gamma-quant)
            :double (x :double) (y :double))
(defun base-gamma-quant (x y) 
  (ccl-base-gamma-quant (float x 1d0) (float y 1d0)))

(cffi:defcfun ("ccl_gammadens" ccl-base-gamma-dens)
            :double (x :double) (y :double))
(defun base-gamma-dens (x y) 
  (ccl-base-gamma-dens (float x 1d0) (float y 1d0)))

(cffi:defcfun ("ccl_gammarand" ccl-gamma-rand)
            :double (x :double))
(defun one-gamma-rand (x) 
  (ccl-gamma-rand (float x 1d0)))

;;;;
;;;; Chi-square distribution
;;;;

(cffi:defcfun ("ccl_chisqcdf" ccl-base-chisq-cdf)
            :double (x :double) (y :double))
(defun base-chisq-cdf (x y) 
  (ccl-base-chisq-cdf (float x 1d0) (float y 1d0)))

(cffi:defcfun ("ccl_chisqquant" ccl-base-chisq-quant)
            :double (x :double) (y :double))
(defun base-chisq-quant (x y) 
  (ccl-base-chisq-quant (float x 1d0) (float y 1d0)))

(cffi:defcfun ("ccl_chisqdens" ccl-base-chisq-dens)
            :double (x :double) (y :double))
(defun base-chisq-dens (x y) 
  (ccl-base-chisq-dens (float x 1d0) (float y 1d0)))

(cffi:defcfun ("ccl_chisqrand" ccl-chisq-rand)
            :double (x :double))
(defun one-chisq-rand (x) 
  (ccl-chisq-rand (float x 1d0)))

;;;;
;;;; Beta distribution
;;;;

(cffi:defcfun ("ccl_betacdf" ccl-base-beta-cdf)
    :double (x :double) (y :double) (z :double))
(defun base-beta-cdf (x y z) 
  (ccl-base-beta-cdf (float x 1d0) (float y 1d0) (float z 1d0)))

(cffi:defcfun ("ccl_betaquant" ccl-base-beta-quant)
    :double (x :double) (y :double) (z :double))
(defun base-beta-quant (x y z) 
  (ccl-base-beta-quant (float x 1d0) (float y 1d0) (float z 1d0)))

(cffi:defcfun ("ccl_betadens" ccl-base-beta-dens) 
    :double (x :double) (y :double) (z :double))
(defun base-beta-dens (x y z) 
  (ccl-base-beta-dens (float x 1d0) (float y 1d0) (float z 1d0)))

(cffi:defcfun ("ccl_betarand" ccl-beta-rand)
    :double (x :double) (y :double))
(defun one-beta-rand (x y)
  (ccl-beta-rand (float x 1d0) (float y 1d0)))

;;;;
;;;; t distribution
;;;;

(cffi:defcfun ("ccl_tcdf" ccl-base-t-cdf)
    :double (x :double) (y :double))
(defun base-t-cdf (x y)
  (ccl-base-t-cdf (float x 1d0) (float y 1d0)))

(cffi:defcfun ("ccl_tquant" ccl-base-t-quant)
    :double (x :double) (y :double))
(defun base-t-quant (x y) 
  (ccl-base-t-quant (float x 1d0) (float y 1d0)))

(cffi:defcfun ("ccl_tdens" ccl-base-t-dens)
    :double (x :double) (y :double))
(defun base-t-dens (x y) 
  (ccl-base-t-dens (float x 1d0) (float y 1d0)))

(cffi:defcfun ("ccl_trand" ccl-t-rand)
    :double (x :double))
(defun one-t-rand (x) 
  (ccl-t-rand (float x 1d0)))

;;;;
;;;; F distribution
;;;;

(cffi:defcfun ("ccl_fcdf" ccl-base-f-cdf)
    :double (x :double) (y :double) (z :double))
(defun base-f-cdf (x y z) 
  (ccl-base-f-cdf (float x 1d0) (float y 1d0) (float z 1d0)))

(cffi:defcfun ("ccl_fquant" ccl-base-f-quant)
    :double (x :double) (y :double) (z :double))
(defun base-f-quant (x y z) 
  (ccl-base-f-quant (float x 1d0) (float y 1d0) (float z 1d0)))

(cffi:defcfun ("ccl_fdens" ccl-base-f-dens)
    :double (x :double) (y :double) (z :double))
(defun base-f-dens (x y z) 
  (ccl-base-f-dens (float x 1d0) (float y 1d0) (float z 1d0)))

(cffi:defcfun ("ccl_frand" ccl-f-rand)
    :double (x :double) (y :double))
(defun one-f-rand (x y) (ccl-f-rand (float x 1d0) (float y 1d0)))

;;;;
;;;; Poisson distribution
;;;;

(cffi:defcfun ("ccl_poissoncdf" ccl-base-poisson-cdf)
    :double (x :double) (y :double))
(defun base-poisson-cdf (x y)
  (ccl-base-poisson-cdf (float x 1d0) (float y 1d0)))

(cffi:defcfun ("ccl_poissonquant" ccl-base-poisson-quant)
    :int (x :double) (y :double))
(defun base-poisson-quant (x y) 
  (ccl-base-poisson-quant (float x 1d0) (float y 1d0)))

(cffi:defcfun ("ccl_poissonpmf" ccl-base-poisson-pmf)
    :double (x :int) (y :double))
(defun base-poisson-pmf (x y) 
  (ccl-base-poisson-pmf x (float y 1d0)))

(cffi:defcfun ("ccl_poissonrand" ccl-poisson-rand)
    :int (x :double))
(defun one-poisson-rand (x) 
  (ccl-poisson-rand (float x 1d0)))

;;;;
;;;; Binomial distribution
;;;;

(cffi:defcfun ("ccl_binomialcdf" ccl-base-binomial-cdf) 
    :double (x :double) (y :int) (z :double))
(defun base-binomial-cdf (x y z) 
  (ccl-base-binomial-cdf (float x 1d0) y (float z 1d0)))

(cffi:defcfun ("ccl_binomialquant" ccl-base-binomial-quant) 
    :int (x :double) (y :int) (z :double))
(defun base-binomial-quant (x y z) 
  (ccl-base-binomial-quant (float x 1d0) y (float z 1d0)))

(cffi:defcfun ("ccl_binomialpmf" ccl-base-binomial-pmf)
    :double (x :int) (y :int) (z :double))
(defun base-binomial-pmf (x y z) 
  (ccl-base-binomial-pmf x y (float z 1d0)))

(cffi:defcfun ("ccl_binomialrand" ccl-binomial-rand) 
    :int (x :int) (y :double))
(defun one-binomial-rand (x y) 
  (ccl-binomial-rand x (float y 1d0)))





;;; definitions though macros

(defmacro defbaserand (name onefun &rest args)
  `(defun ,name (n ,@args)
     (let ((result nil))
       (dotimes (i n result)
		(declare (fixnum i) (inline ,onefun))
		(setf result (cons (,onefun ,@args) result))))))

(defbaserand base-uniform-rand one-uniform-rand)
(defbaserand base-normal-rand one-normal-rand)
(defbaserand base-cauchy-rand one-cauchy-rand)
(defbaserand base-gamma-rand one-gamma-rand a)
(defbaserand base-chisq-rand one-chisq-rand df)
(defbaserand base-beta-rand one-beta-rand a b)
(defbaserand base-t-rand one-t-rand df)
(defbaserand base-f-rand one-f-rand ndf ddf)
(defbaserand base-poisson-rand one-poisson-rand a)
(defbaserand base-binomial-rand one-binomial-rand a b)

(make-rv-function log-gamma base-log-gamma x)

(make-rv-function uniform-rand base-uniform-rand n)

(make-rv-function normal-cdf base-normal-cdf x)
(make-rv-function normal-quant base-normal-quant p)
(make-rv-function normal-dens base-normal-dens x)
(make-rv-function normal-rand base-normal-rand n)
(make-rv-function bivnorm-cdf base-bivnorm-cdf x y r)

(make-rv-function cauchy-cdf base-cauchy-cdf x)
(make-rv-function cauchy-quant base-cauchy-quant p)
(make-rv-function cauchy-dens base-cauchy-dens x)
(make-rv-function cauchy-rand base-cauchy-rand n)

(make-rv-function gamma-cdf base-gamma-cdf x a)
(make-rv-function gamma-quant base-gamma-quant p a)
(make-rv-function gamma-dens base-gamma-dens x a)
(make-rv-function gamma-rand base-gamma-rand n a)

(make-rv-function chisq-cdf base-chisq-cdf x df)
(make-rv-function chisq-quant base-chisq-quant p df)
(make-rv-function chisq-dens base-chisq-dens x df)
(make-rv-function chisq-rand base-chisq-rand n df)

(make-rv-function beta-cdf base-beta-cdf x a b)
(make-rv-function beta-quant base-beta-quant p a b)
(make-rv-function beta-dens base-beta-dens x a b)
(make-rv-function beta-rand base-beta-rand n a b)

(make-rv-function t-cdf base-t-cdf x df)
(make-rv-function t-quant base-t-quant p df)
(make-rv-function t-dens base-t-dens x df)
(make-rv-function t-rand base-t-rand n df)

(make-rv-function f-cdf base-f-cdf x ndf ddf)
(make-rv-function f-quant base-f-quant p ndf ddf)
(make-rv-function f-dens base-f-dens x ndf ddf)
(make-rv-function f-rand base-f-rand n ndf ddf)

(make-rv-function poisson-cdf base-poisson-cdf x a)
(make-rv-function poisson-quant base-poisson-quant p a)
(make-rv-function poisson-pmf base-poisson-pmf x a)
(make-rv-function poisson-rand base-poisson-rand n a)

(make-rv-function binomial-cdf base-binomial-cdf x a b)
(make-rv-function binomial-quant base-binomial-quant p a b)
(make-rv-function binomial-pmf base-binomial-pmf x a b)
(make-rv-function binomial-rand base-binomial-rand n a b)

;;;;
;;;; Documentation
;;;;

(setf (documentation 'bivnorm-cdf 'function)
"Args: (x y r)
Returns the value of the standard bivariate normal distribution function 
with correlation R at (X, Y). Vectorized.")

(setf (documentation 'normal-cdf 'function)
"Args: (x)
Returns the value of the standard normal distribution function at X.
Vectorized.")

(setf (documentation 'beta-cdf 'function)
"Args: (x alpha beta)
Returns the value of the Beta(ALPHA, BETA) distribution function at X.
Vectorized.")

(setf (documentation 'gamma-cdf 'function)
"Args: (x alpha)
Returns the value of the Gamma(alpha, 1) distribution function at X.
Vectorized.")

(setf (documentation 'chisq-cdf 'function)
"Args: (x df)
Returns the value of the Chi-Square(DF) distribution function at X. Vectorized.")

(setf (documentation 't-cdf 'function)
"Args: (x df)
Returns the value of the T(DF) distribution function at X. Vectorized.")

(setf (documentation 'f-cdf 'function)
"Args: (x ndf ddf)
Returns the value of the F(NDF, DDF) distribution function at X. Vectorized.")

(setf (documentation 'cauchy-cdf 'function)
"Args: (x)
Returns the value of the standard Cauchy distribution function at X.
Vectorized.")

(setf (documentation 'log-gamma 'function)
"Args: (x)
Returns the log gamma function of X. Vectorized.")

(setf (documentation 'normal-quant 'function)
"Args (p)
Returns the P-th quantile of the standard normal distribution. Vectorized.")

(setf (documentation 'cauchy-quant 'function)
"Args (p)
Returns the P-th quantile(s) of the standard Cauchy distribution. Vectorized.")

(setf (documentation 'beta-quant 'function)
"Args: (p alpha beta)
Returns the P-th quantile of the Beta(ALPHA, BETA) distribution. Vectorized.")

(setf (documentation 'gamma-quant 'function)
"Args: (p alpha)
Returns the P-th quantile of the Gamma(ALPHA, 1) distribution. Vectorized.")

(setf (documentation 'chisq-quant 'function)
"Args: (p df)
Returns the P-th quantile of the Chi-Square(DF) distribution. Vectorized.")

(setf (documentation 't-quant 'function)
"Args: (p df)
Returns the P-th quantile of the T(DF) distribution. Vectorized.")

(setf (documentation 'f-quant 'function)
"Args: (p ndf ddf)
Returns the P-th quantile of the F(NDF, DDF) distribution. Vectorized.")

(setf (documentation 'normal-dens 'function)
"Args: (x)
Returns the density at X of the standard normal distribution. Vectorized.")

(setf (documentation 'cauchy-dens 'function)
"Args: (x)
Returns the density at X of the standard Cauchy distribution. Vectorized.")

(setf (documentation 'beta-dens 'function)
"Args: (x alpha beta)
Returns the density at X of the Beta(ALPHA, BETA) distribution. Vectorized.")

(setf (documentation 'gamma-dens 'function)
"Args: (x alpha)
Returns the density at X of the Gamma(ALPHA, 1) distribution. Vectorized.")

(setf (documentation 'chisq-dens 'function)
"Args: (x alpha)
Returns the density at X of the Chi-Square(DF) distribution. Vectorized.")

(setf (documentation 't-dens 'function)
"Args: (x alpha)
Returns the density at X of the T(DF) distribution. Vectorized.")

(setf (documentation 'f-dens 'function)
"Args: (x ndf ddf)
Returns the density at X of the F(NDF, DDF) distribution. Vectorized.")

(setf (documentation 'uniform-rand 'function)
"Args: (n)
Returns a list of N uniform random variables from the range (0, 1).
Vectorized.")

(setf (documentation 'normal-rand 'function)
"Args: (n)
Returns a list of N standard normal random numbers. Vectorized.")

(setf (documentation 'cauchy-rand 'function)
"Args: (n)
Returns a list of N standard Cauchy random numbers. Vectorized.")

(setf (documentation 't-rand 'function)
"Args: (n df)
Returns a list of N T(DF) random variables. Vectorized.")

(setf (documentation 'f-rand 'function)
"Args: (n ndf ddf)
Returns a list of N F(NDF, DDF) random variables. Vectorized.")

(setf (documentation 'gamma-rand 'function)
"Args: (n a)
Returns a list of N Gamma(A, 1) random variables. Vectorized.")

(setf (documentation 'chisq-rand 'function)
"Args: (n df)
Returns a list of N Chi-Square(DF) random variables. Vectorized.")

(setf (documentation 'beta-rand 'function)
"Args: (n a b)
Returns a list of N beta(A, B) random variables. Vectorized.")

(setf (documentation 'binomial-cdf 'function)
"Args (x n p)
Returns value of the Binomial(N, P) distribution function at X. Vectorized.")

(setf (documentation 'poisson-cdf 'function)
"Args (x mu)
Returns value of the Poisson(MU) distribution function at X. Vectorized.")

(setf (documentation 'binomial-pmf 'function)
"Args (k n p)
Returns value of the Binomial(N, P) pmf function at integer K. Vectorized.")

(setf (documentation 'poisson-pmf 'function)
"Args (k mu)
Returns value of the Poisson(MU) pmf function at integer K. Vectorized.")

(setf (documentation 'binomial-quant 'function)
"Args: (x n p)
Returns x-th quantile (left continuous inverse) of Binomial(N, P) cdf.
Vectorized.")

(setf (documentation 'poisson-quant 'function)
"Args: (x mu)
Returns x-th quantile (left continuous inverse) of Poisson(MU) cdf.
Vectorized.")

(setf (documentation 'binomial-rand 'function)
"Args: (k n p)
Returns list of K draws from the Binomial(N, P) distribution. Vectorized.")

(setf (documentation 'poisson-rand 'function)
"Args: (k mu)
Returns list of K draws from the Poisson(MU) distribution. Vectorized.")
