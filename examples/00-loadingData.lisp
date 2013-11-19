;;; -*- mode: lisp -*-    

;;; Time-stamp: <2013-11-18 10:56:10 tony>
;;; Creation:   <2009-03-12 17:14:56 tony>
;;; File:       00-loadingData.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2009--2013, AJ Rossini.  MIT license.
;;; Purpose:    Example of loading and QA/QC'ing data

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

(in-package :cl-user) ; start in the CL user namespace (i.e. package)

;;; load the systems we need
(ql:quickload :data-format-validation)
(ql:quickload :antik) ;; for all the wrong reasons
(ql:quickload :cls)

;;; create the package that we will be working in.
;; The CLS-EXAMPLES package is similar setup to CLS-USER package.
(cl:defpackage :cls-examples  
  (:use :common-lisp
	:xarray
	:lisp-matrix
	:data-format-validation
	:common-lisp-statistics)
  (:shadowing-import-from :lisp-stat
      call-method call-next-method

      expt + - * / ** mod rem abs 1+ 1- log exp sqrt sin cos tan
      asin acos atan sinh cosh tanh asinh acosh atanh float random
      truncate floor ceiling round minusp zerop plusp evenp oddp 
      < <= = /= >= > > ;; complex
      conjugate realpart imagpart phase
      min max logand logior logxor lognot ffloor fceiling
      ftruncate fround signum cis

      <= float imagpart)
  (:export localized-pathto
	   *chkwgts-df*))

;; and we will now work within this package
(in-package :cls-examples)

;; we'll be loading from directories in the CLS homedir, so we want to
;; make it easier to reach.  
(defun localized-pathto (x)
  "Given a string representing a relative path from the CLS home
directory, return a string denoting the complete path.  

FIXME: UNIX-centric (though might work on Mac OSX).  We really want to
return a pathspec, not a string/namespec"
  (check-type x string)
  (concatenate 'string *cls-installation-home-dir* x))



(progn
  ;; LISP-STAT COMPATIBILITY MODE:
  ;;
  ;; FIXME: Need to clean up data examples, licenses, attributions, etc.
  (load-data "iris.lisp")  ;; (the above partially fixed).
  (load-data "diabetes.lisp")
  lisp-stat-data-examples:diabetes
  lisp-stat-data-examples:dlabs
  (variables))

;; COMMON LISP STATISTICS
;; Importing data from DSV text files.

#| We use as an example a simple R datasert, chickwts, which has one
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


;;; Experiments with CSV file to dataframe conversion

(defparameter *test1*
  (let ((fare-csv:*separator* #\, )) ;; default, but we are making a general example
    (let ((csv-file-data (fare-csv:read-csv-file  (localized-pathto "Data/R-chickwts.csv")))
	  (varnameheader T))
      (let (
#|
	    (var-name-list (if varnameheader  ;; why did we need var-name-list? completely irrelevant
			       (car csv-file-data)
			       (make-labels "V"  (length (car csv-file-data)))))
|#
	    (data-list (if varnameheader
			   (listoflist:listoflist->array (cdr csv-file-data))
			   (listoflist:listoflist->array csv-file-data))))
	data-list))))

*test1*

;; confirming type
(typep *test1* 'array) ; => T
(typep *test1* 'list) ; => NIL
(typep *test1* 'sequence) ; => NIL
(typep *test1* 'vector) ; => NIL



;; both of the following are the same:
(= 
 (+ (data-format-validation:parse-input 'integer (aref *test1* 4 1))
    (data-format-validation:parse-input 'integer (aref *test1* 4 1)))

 (+ (data-format-validation:parse-input 'integer (xref *test1* 4 1))
    (data-format-validation:parse-input 'integer (xref *test1* 4 1)))) ; => T

(+ (data-format-validation:parse-input 'integer (aref *test1* 4 0))
   (data-format-validation:parse-input 'integer (aref *test1* 5 0)))


(data-format-validation:parse-input 'string (aref *test1* 5 2)) ;; correct, returns a string

;; we need to try-catch-continue with the error here so we can load
;; this file directly in without an error.  Or ensure that if we let
;; the user know what to do when the error occurs (to just continue
;; along merrily...).  Otherwise simple loading will not work!
;;
;; (data-format-validation:parse-input 'number (aref *test1* 5 2))
;; ;; => error, but restarts are not quite right...
;;

;; inline conversion of array data

(defparameter *test1-types* (list 'integer 'number 'string)) ; this works in the sequel
(defparameter *test2-types* '('integer 'number 'string)) ; this used to fail but no longer?

(nth 1 *test1-types*)

(array-dimensions *test1*)

(data-format-validation:parse-input (nth 1 *test1-types*) (aref *test1* 2 1))
(data-format-validation:parse-input 'integer (aref *test1* 2 1))



(defparameter *test2* (make-array (array-dimensions *test1*)))

;;; The next bit of code is an example for how to start column-typing
;;; arrays, i.e. for use in making objects which start to resemble R's
;;; dataframes.
;;;
;;; Is there a cleaner approach using map?  For example, could we work
;;; on column-at-a-time?  and of course: is there a faster or
;;; less-consing version?
(dotimes (i (array-dimension *test1* 0))
  (dotimes (j (array-dimension *test1* 1))
    (setf (aref *test2* i j)
	  (data-format-validation:parse-input (nth j *test1-types*) (aref *test1* i j)))))

*test1*
*test2*


;;; This next bit is the preliminary loader.  It is all about
;;; examples.  When we like it, it will get moved into the core.  More
;;; importantly, it should just do the "file contents to lisp array +
;;; extra metadata" reading.  FIXME: we don't have an "extra metadata"
;;; concept defined yet, so it is not clear exactly what we will have
;;; here.  That would be the statistical and typing information.
;;; Which might be auto-generated, and might be user-spec'd.

(defun filename.dsv->array3 (filepath list-of-vartypes)
  "Take a filepath referencing a DSV file, and return a Lisp array
with appropriate column(variable) typing.

list-of-vartypes should look like:
   (list 'integer 'number 'string)
but it would be nice if it could look like:
   (list :integer :number :string)
also, i.e. a list of types or classes."

  (let ((fare-csv:*separator* #\, )) ;; default, but we are making a general example
    (let ((csv-file-data-as-listoflist (fare-csv:read-csv-file  filepath))
	  (varnameheader T))
      (let (;; (var-name-list (if varnameheader
	    ;;		       (car csv-file-data-as-listoflist)
 	    ;;		       (make-labels "V"  (length (car csv-file-data-as-listoflist)))))
	    ;; the above used to auto-create variable names in case there was no info about them.
	    (data-array (if varnameheader
			   (listoflist:listoflist->array (cdr csv-file-data-as-listoflist))
			   (listoflist:listoflist->array csv-file-data-as-listoflist))))
	(dotimes (i (array-dimension data-array 0))
	  (dotimes (j (array-dimension data-array 1))
	    (setf (aref data-array i j)
		  (data-format-validation:parse-input (nth j list-of-vartypes) (aref data-array i j)))))
	data-array))))

 (progn
   (defparameter *chickwts-array*
     (filename.dsv->array3 (localized-pathto "Data/R-chickwts.csv")
			   (list 'integer 'number 'string)))
   ;; *chickwts-df*
   (xref *chickwts-array* 1 1) ; => 160
   (xref *chickwts-array* 40 2) ; => "sunflower"
   *chickwts-array*)

(+ (xref *chickwts-array* 1 1)
   (xref *chickwts-array* 63 1))

;; now, off to the next part!  more loading examples, and we show how
;; to make a dataframe from an array.
