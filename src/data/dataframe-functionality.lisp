;;; -*- mode: lisp -*-

;;; Time-stamp: <2013-10-18 14:13:36 tony>
;;; Creation:   <2009 tony>
;;; File:       dataframe-functionality
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2012--, AJ Rossini.  Currently licensed under MIT
;;;             license.  See file LICENSE.mit in top-level directory
;;;             for information.
;;; Purpose:    Functions (not methods) for dataframes.

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

;; This file contains functions.  Most functionality is properly done
;; via generics, because Tony doesn't care about speed (and he is
;; waiting for hostile remarks from people reading this code), but
;; when we need to have functions, then we need to ensure that all
;; components are preloaded.

(defun make-dataframe (newdata
		       &key  (vartypes nil)
		       (caselabels nil) (varlabels nil)
		       (doc "no docs"))
  "Helper function to use instead of make-instance to assure
construction of proper DF-array."
  (check-type newdata (or matrix-like array list))
  (check-type caselabels sequence)
  (check-type varlabels sequence)
  (check-type vartypes sequence)
  (check-type doc string)
  (let ((ncases (ncases newdata))
	(nvars (nvars newdata)))
    
    (if caselabels (assert (= ncases (length caselabels))))
    (if varlabels (assert (= nvars (length varlabels))))
    (let ((newcaselabels (if caselabels
			     caselabels
			     (make-labels "C" ncases)))
	  (newvarlabels (if varlabels
			    varlabels
			    (make-labels "V" nvars))))
    
      (etypecase newdata 
	(list
	 (make-instance 'dataframe-listoflist
			:storage newdata
			:nrows (length newcaselabels)
			:ncols (length newvarlabels)
			:case-labels newcaselabels
			:var-labels newvarlabels
			:var-types vartypes))
	(array
	 (make-instance 'dataframe-array
			:storage newdata
			:nrows (length newcaselabels)
			:ncols (length newvarlabels)
			:case-labels newcaselabels
			:var-labels newvarlabels
			:var-types vartypes))
	
	(matrix-like
	 (make-instance 'dataframe-matrixlike
			:storage newdata
			:nrows (length newcaselabels)
			:ncols (length newvarlabels)
			:case-labels newcaselabels
			:var-labels newvarlabels
			:var-types vartypes))))))

