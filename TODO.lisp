;;; -*- mode: lisp -*-

;;; Time-stamp: <2009-04-28 13:10:21 tony>
;;; Creation:   <2008-09-08 08:06:30 tony>
;;; File:       TODO.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c) 2007-2008, AJ Rossini <blindglobe@gmail.com>.  BSD.
;;; Purpose: Stuff that needs to be made working sits inside the
;;;          progns... This file contains the current challenges to
;;;          solve, including a description of the setup and the work
;;;          to solve....
 
;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

;;; SET UP

(in-package :cl-user)
;;(asdf:oos 'asdf:load-op 'lisp-matrix)
;;(asdf:oos 'asdf:compile-op 'lispstat :force t)
;;(asdf:oos 'asdf:load-op 'lispstat)

(in-package :lisp-stat-unittests)

;; tests = 80, failures = 8, errors = 15
(run-tests :suite 'lisp-stat-ut)
(describe (run-tests :suite 'lisp-stat-ut))

;; FIXME: Example: currently not relevant, yet
;;   (describe (lift::run-test :test-case  'lisp-stat-unittests::create-proto
;;                             :suite 'lisp-stat-unittests::lisp-stat-ut-proto))

(describe (lift::run-tests :suite 'lisp-stat-ut-dataframe))
(lift::run-tests :suite 'lisp-stat-ut-dataframe)

(describe 
 (lift::run-test
  :test-case  'lisp-stat-unittests::create-proto
  :suite 'lisp-stat-unittests::lisp-stat-ut-proto))

(describe 'lisp-stat-ut)

(in-package :ls-user)

(progn
  ;; Plotting -- need to figure out the core-dump, or change libraries.
  ;; (asdf:oos 'asdf:load-op 'cl-plplot)

  (plot-ex)
  (defparameter *gdev* "xcairo")
  ;; (defparameter *gdev* "xwin")
  (cl-plplot::plsdev *gdev*)

  ;; there is currently a loose pointer floating around that causes
  ;; errors the 3rd time that we create a plot (and crashes SBCL the
  ;; 4th time).  Order independent.
  (contour-plot-ex)
  (fn-contour-plot-ex)
  (shade-plot-ex)
  (3D-plot-ex))


(progn
  ;; REVIEW: general Lisp use guidance

  (fdefinition 'make-matrix)
  (documentation 'make-matrix 'function)

#| Examples from CLHS, a bit of guidance.

  ;; This function assumes its callers have checked the types of the
  ;; arguments, and authorizes the compiler to build in that assumption.
  (defun discriminant (a b c)
   (declare (number a b c))
   "Compute the discriminant for a quadratic equation."
   (- (* b b) (* 4 a c))) =>  DISCRIMINANT
  (discriminant 1 2/3 -2) =>  76/9

  ;; This function assumes its callers have not checked the types of the
  ;; arguments, and performs explicit type checks before making any assumptions. 
 (defun careful-discriminant (a b c)
   "Compute the discriminant for a quadratic equation."
   (check-type a number)
   (check-type b number)
   (check-type c number)
   (locally (declare (number a b c))
     (- (* b b) (* 4 a c)))) =>  CAREFUL-DISCRIMINANT
 (careful-discriminant 1 2/3 -2) =>  76/9
|#
  )


#+nil
(progn ;; experiments with GSL and the Lisp interface.
  (asdf:oos 'asdf:load-op 'gsll)
  (asdf:oos 'asdf:load-op 'gsll-tests)

  ;; the following should be equivalent
  (defparameter *t1*  (LIST 6.18d0 6.647777777777779d0 6.18d0))
  (defparameter *t2*  (MULTIPLE-VALUE-LIST
	       (LET ((VEC
		      (gsll:make-marray 'DOUBLE-FLOAT
					:INITIAL-CONTENTS '(-3.21d0 1.0d0 12.8d0)))
		     (WEIGHTS
		      (gsll:MAKE-MARRAY 'DOUBLE-FLOAT
					:INITIAL-CONTENTS '(3.0d0 1.0d0 2.0d0))))
		 (LET ((MEAN (gsll:MEAN VEC)))
		   (LIST (gsll:ABSOLUTE-DEVIATION VEC)
			 (gsll:WEIGHTED-ABSOLUTE-DEVIATION VEC WEIGHTS)
			 (gsll:ABSOLUTE-DEVIATION VEC MEAN))))))
  (eql *t1* *t2*)

  ;; from (gsll:examples 'gsll::numerical-integration) ...
  (gsll:integration-qng gsll::one-sine 0.0d0 PI)

  (gsll:defun-single axpb (x) (+ (* 2 x) 3)) ;; a<-2, b<-3
  (gsll:integration-qng axpb 1d0 2d0)

  (let ((a 2)
	(b 3))
    (defun-single axpb2 (x) (+ (* a x) b)))
  (gsll:integration-qng axpb2 1d0 2d0)

  ;;   BAD
  ;;   (gsll:integration-qng 
  ;;    (let ((a 2)
  ;; 	 (b 3))
  ;;      (defun-single axpb2 (x) (+ (* a x) b)))
  ;;    1d0 2d0)

  ;; right, but weird expansion...
  (gsll:integration-qng
   (let ((a 2)
	 (b 3))
     (defun axpb2 (x) (+ (* a x) b))
     (gsll:def-single-function axpb2)
     axpb2)
   1d0 2d0)

  ;; Linear least squares

  (gsll:gsl-lookup "gsl_linalg_LU_decomp") ; => gsll:lu-decomposition
  (gsll:gsl-lookup "gsl_linalg_LU_solve") ; => gsll:lu-solve
  )



#+nil
(progn ;; philosophy time
  
  (setf my-model (model :name "ex1"
			:data-slots (list w x y z)
			:param-slots (list alpha beta gamma)
			:math-form (regression-model :formula '(= w (+ (* beta x)
								     (* alpha y)
								     (* gamma z)
								     normal-error))
						     :centrality 'median ; 'mean
						     )))

#| or:
  #R"W ~ x+  y + z "			
|#

  (setf my-dataset (statistical-table :table data-frame-contents
				      :metadata (list (:case-names (list ))
						      (:var-names (list ))
						      (:documentation "string of doc"))))

  (setf my-analysis (analysis
		     :model my-model
		     :data my-dataset
		     :parameter-map (pairing (model-param-slots my-model)
					     (data-var-names my-dataset))))

  ;; ontological implications -- the analysis is an abstract class of
  ;; data, model, and mapping between the model and data.  The fit is
  ;; the instantiation of such.  This provides a statistical object
  ;; computation theory which can be realized as "executable
  ;; statistics" or "computable statistics".
  (setf my-analysis (analyze my-fit
			     :estimation-method 'linear-least-squares-regression))

  ;; one of the tricks here is that one needs to provide the structure
  ;; from which to consider estimation, and more importantly, the
  ;; validity of the estimation.

  ;;
  (setf linear-least-squares-regression
	(estimation-method-definition
	 :variable-defintions ((list 
				;; from MachLearn: supervised,
				;; unsupervised
				:data-response-vars list-drv ; nil if unsup
				;; 
				:param-vars list-pv
				     :data-predictor-vars list-dpv 
				     ;; nil in this case.  these
				     ;; describe "out-of-box" specs
				     :hyper-vars list-hv))
	 :form '(regression-additive-error
		 :central-form (linear-form drv pv dpv)
		 :error-form 'normal-error)
	 :resulting-decision '(point-estimation interval-estimation)
	 :philosophy 'frequentist
	 :documentation "use least squares to fit a linear regression
                         model to data."))

  (defparameter *statistical-philosophies*
    '(frequentist bayesian fiducial decision-analysis)
    "can be combined to build decision-making approaches and
    characterizations")

  (defparameter *decisions*
    '(estimation selection testing)
    "possible results from a...")
  ;; is this really true?  One can embedded hypothesis testing within
  ;; estimation, as the hypothesis estimated to select.  And
  ;; categorical/continuous rear their ugly heads, but not really in
  ;; an essential way.

  (defparameter *ontology-of-decision-procedures*
    (list :decisions
	  (list :estimation
		(list :point
		      (list :maximum-likelihood
			    :minimum-entropy
			    :least-squares
			    :method-of-moments)
		      :interval
		      (list :maximum-likelihood
			    :))
		:testing
		(list :fisherian
		      :neyman-pearson
		      (list :traditional
			    :bioequivalence-inversion)
		      :selection
		      (list :ranking
			    :top-k-of-n-select))
		:parametric
		:partially-parametric))
    "start of ontology"))

