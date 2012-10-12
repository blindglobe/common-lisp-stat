;;; -*- mode: lisp -*-

;;; Time-stamp: <2012-10-12 10:44:00 tony>
;;; Creation:   <2009-04-19 09:41:09 tony>
;;; File:       t-test-example.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2009--, AJ Rossini.  MIT
;;; Purpose:    Example of basic exploratory data analysis in CLS. 

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.


;;; If we can't do a t-test, we can't do anything

;;;; BROKEN!!!

(in-package :cls-user)

(defparameter *my-df*
  (make-dataframe
   (rsm.string::filestream->string-table
    "/path/to/file.dsv"))
  "Initial read-in of data.")


;;;; possible APIs...

#|

summarize what how send-to

what: df, stat-proc
how:  numerically, visually, all, default
send-to: output, file, visual, all, default
 

compare in-model parameters framework philosophy assumptions

in-model: regression two-group 
parameters: means, "explicit choices", variances, means-and-variances, rates, proportions, hazards
framework: fisherian, neyman-pearson, empirical-bayes
philosophy: frequentist, bayesian, fiducial, decision-theoretic,
assumptions: (list 'apply 'ignore 'explore) ; any or all of list entries


FIXME: where would empirical bayes go?  part is a model, part is a philosophy.  framework?
FIXME: decision-theoretic adds risk to the decision, making 

the in-model objects need to be able to reflect their own assumptions and preferences, 

|# 


;;; The traditional intro-stat variant

(summarize
 (compare :parameter 'mean
	  :comparison 'two-groups
	  :group1 datapackage:variable1
	  :group2 datapackage:variable2
	  :assumptions (list 'apply 'ignore 'explore) ; any or all of list entries
	  ) 
 :resulttype (list 'numerical 'visual) 
 )  ; any or all of list entries

;;; the "know-your-dataset" variant

(summarize
 (compare :parameter 'mean
	  :resulttype (list 'numerical 'visual)
	  :comparison 'two-groups
	  :variable datapackage:comparing-variable ; or response-variable
	  :group-by datapackage:grouping-variable))

