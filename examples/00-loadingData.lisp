;;; -*- mode: lisp -*-    

;;; Time-stamp: <2013-03-17 12:07:51 tony>
;;; Creation:   <2009-03-12 17:14:56 tony>
;;; File:       00-loadingData.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2009--2012, AJ Rossini.  MIT license.
;;; Purpose:    Example of loading data

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

(in-package :cl-user)

(ql:quickload :data-format-validation)
(ql:quickload :cls)

(cl:defpackage :cls-examples  ;; similar setup to cls-user, without test data
  (:use :common-lisp
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

;; start within the cls-examples package, which is similar to cls-user
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
  ;; The following breaks because we should use a package to hold
  ;; configuration details, and this would be the only package outside
  ;; of packages.lisp, as it holds the overall defsystem structure.
  (load-data "iris.lsp")  ;; (the above partially fixed).
  (load-data "diabetes.lsp")
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
(typep *test1* 'array)
(typep *test1* 'list)
(typep *test1* 'sequence)
(typep *test1* 'vector)



;; both of the following are the same:
(= 
 (+ (data-format-validation:parse-input 'integer (aref *test1* 4 1))
    (data-format-validation:parse-input 'integer (aref *test1* 4 1)))

 (+ (data-format-validation:parse-input 'integer (xref *test1* 4 1))
    (data-format-validation:parse-input 'integer (xref *test1* 4 1))))

(+ (data-format-validation:parse-input 'integer (aref *test1* 4 0))
   (data-format-validation:parse-input 'integer (aref *test1* 5 0)))


(data-format-validation:parse-input 'string (aref *test1* 5 2)) ;; correct, returns a string
(data-format-validation:parse-input 'number (aref *test1* 5 2)) ;; error, but restarts are not quite right...

;; inline conversion of array data

(setf *test1-types* (list 'integer 'number 'string)) ;; this works in the sequel
(setf *test2-types* '('integer 'number 'string)) ;; this fails!

(nth 1 *test1-types*)

(array-dimensions *test1*)

(data-format-validation:parse-input (nth 1 *test1-types*) (aref *test1* 2 1))
(data-format-validation:parse-input 'integer (aref *test1* 2 1))


(setf *test2* (make-array (array-dimensions *test1*)))

;;; Is there a cleaner approach using map?  For example, could we work
;;; on column-at-a-time?  and of course: is there a faster or
;;; less-consing version?
(dotimes (i (array-dimension *test1* 0))
  (dotimes (j (array-dimension *test1* 1))
    (setf (aref *test2* i j)
	  (data-format-validation:parse-input (nth j *test1-types*) (aref *test1* i j)))))


*test1*
*test2*

(defun filename.dsv->array3 (filepath list-of-vartypes) 
  "Take a filepath referencing a DSV file, and return an array with
appropriate column(variable) typing.

list-of-vartypes should look like:
   (list 'integer 'number 'string)
"
  (let ((fare-csv:*separator* #\, )) ;; default, but we are making a general example
    (let ((csv-file-data-as-listoflist (fare-csv:read-csv-file  filepath))
	  (varnameheader T))
      (let ((var-name-list (if varnameheader
			       (car csv-file-data-as-listoflist)
			       (make-labels "V"  (length (car csv-file-data-as-listoflist)))))
	    (data-array (if varnameheader
			   (listoflist:listoflist->array (cdr csv-file-data-as-listoflist))
			   (listoflist:listoflist->array csv-file-data-as-listoflist))))
	(dotimes (i (array-dimension data-array 0))
	  (dotimes (j (array-dimension data-array 1))
	    (setf (aref data-array i j)
		  (data-format-validation:parse-input (nth j list-of-vartypes) (aref data-array i j)))))
	data-array))))

 (progn 
   (setf *chickwts-array* (filename.dsv->array3 (localized-pathto "Data/R-chickwts.csv")
						(list 'integer 'number 'string)))
   ;; *chickwts-df*
   (xref *chickwts-array* 1 1) ; => 160
   (xref *chickwts-array* 40 2) ; => "sunflower"
   *chickwts-array*)


(+ (xref *chickwts-array* 1 1)
   (xref *chickwts-array* 63 1))

;; now, off to the next part!  more loading examples, and we show how
;; to make a dataframe from an array.
