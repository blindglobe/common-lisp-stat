;;; -*- mode: lisp -*-

;;; Time-stamp: <2010-01-15 07:56:57 tony>
;;; Creation:   <2009-03-12 17:14:56 tony>
;;; File:       template.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2009--, AJ Rossini.  BSD, LLGPL, or GPLv2, depending
;;;             on how it arrives.  
;;; Purpose:    Template header file

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

;; start within the common-lisp-stat-user 
(in-package :cls-user)

;; we'll be loading from directories in the CLS homedir, so we want to
;; make it easier to reach.  
(defun localized-pathto (x)
  "return a string denoting the complete path.
FIXME: UNIX-centric (though might work on Mac OSX).  Might want to
return a pathspec, not a string/namespec"
  (check-type x string)
  (concatenate 'string *cls-home-dir* x))



(progn
  ;; LISP-STAT COMPATIBILITY MODE:
  ;;
  ;; FIXME: Need to clean up data examples, licenses, attributions, etc.
  ;; The following breaks because we should use a package to hold
  ;; configuration details, and this would be the only package outside
  ;; of packages.lisp, as it holds the overall defsystem structure.
  (load-data "iris.lsp")  ;; (the above partially fixed).
  diabetes
  (variables))




(progn
  ;; COMMON LISP STATISTICS
  ;;
  ;; Importing data from DSV text files.

#|

  We use as an example a simple R datasert, chickwts, which has one
  nominal categorical variable and one continuous categorical
  variable.   This dataset can be accessed in R as follows:

> data(chickwts)
> dim(chickwts)
[1] 71  2
> summary(chickwts)
     weight             feed   
 Min.   :108.0   casein   :12  
 1st Qu.:204.5   horsebean:10  
 Median :258.0   linseed  :12  
 Mean   :261.3   meatmeal :11  
 3rd Qu.:323.5   soybean  :14  
 Max.   :423.0   sunflower:12  
> 
|#

  (defparameter *chickwts-df* (filename.dsv->dataframe (localized-pathto "Data/R-chickwts.csv")))
  ;; *chickwts-df*
  (xref *chickwts-df* 1 1) ; => 160
  (xref *chickwts-df* 40 2) ; => "sunflower"
  )

