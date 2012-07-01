;; example of possible usage.


;; Load system
(ql:quickload "cls")

;; use the example package...
(in-package :cls-user)


;; or better yet, create a package/namespace for the particular problem being attacked.
(defpackage :my-package-user
  (:documentation "demo of how to put serious work should be placed in
    a similar package elsewhere for reproducibility.  This hints as to
    what needs to be done for a user- or analysis-package.")
  (:nicknames :my-clswork-user)
  (:use :common-lisp ; always needed for user playgrounds!
	:lisp-matrix
	:common-lisp-statistics
	:lisp-stat-data-examples) ;; this ensures access to a data package
  (:shadowing-import-from :lisp-stat call-method call-next-method

      expt + - * / ** mod rem abs 1+ 1- log exp sqrt sin cos tan
      asin acos atan sinh cosh tanh asinh acosh atanh float random
      truncate floor ceiling round minusp zerop plusp evenp oddp 
      < <= = /= >= > > ;; complex
      conjugate realpart imagpart phase
      min max logand logior logxor lognot ffloor fceiling
      ftruncate fround signum cis

      <= float imagpart)) 

(in-package :my-clswork-user)

;; create some data by hand

(setf testdata)