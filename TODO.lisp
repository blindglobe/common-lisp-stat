;;; -*- mode: lisp -*-

;;; Time-stamp: <2009-03-27 17:05:50 tony>
;;; Creation:   <2008-09-08 08:06:30 tony>
;;; File:       TODO.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c) 2007-2008, AJ Rossini <blindglobe@gmail.com>.  BSD.
;;; Purpose:    Stuff that needs to be made working sits inside the progns...

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

;;; This file contains the current challenges to solve, including a
;;; description of the setup and the work to solve....
 
;;; SET UP

(in-package :cl-user)
;;(asdf:oos 'asdf:compile-op 'lispstat)
;;(asdf:oos 'asdf:load-op 'lispstat)

(in-package :lisp-stat-unittests)

;; tests = 87, failures = 9, errors = 22

(describe (run-tests :suite 'lisp-stat-ut))
(run-tests :suite 'lisp-stat-ut)

#|
  ;; FIXME: Example: currently not relevant, yet
  (describe 
    (lift::run-test
      :test-case  'lisp-stat-unittests::create-proto
      :suite 'lisp-stat-unittests::lisp-stat-ut-proto))
|#

(describe 'lisp-stat-ut)
(in-package :ls-user)


(progn ;; dataframe

  (describe (lift::run-tests :suite 'lisp-stat-ut-dataclos))
  (lift::run-tests :suite 'lisp-stat-ut-dataclos)


  (describe 
    (lift::run-test
      :test-case  'lisp-stat-unittests::create-proto
      :suite 'lisp-stat-unittests::lisp-stat-ut-proto))

  (defparameter *my-df-1*
	(make-instance 'dataframe-array
		       :storage #2A((1 2 3 4 5)
				    (10 20 30 40 50))
		       :doc "This is an interesting dataframe-array"
		       :case-labels (list "x" "y")
		       :var-labels (list "a" "b" "c" "d" "e")))



  (type-of #2A((1 2 3 4 5)
		       (10 20 30 40 50)))

  (type-of (rand 10 20))

  (typep #2A((1 2 3 4 5)
	     (10 20 30 40 50))
	 'matrix-like)

  (typep (rand 10 20) 'matrix-like)

  (typep #2A((1 2 3 4 5)
	     (10 20 30 40 50))
	 'array)

  (typep (rand 10 20) 'array)


  (make-dataframe  #2A((1 2 3 4 5)
		       (10 20 30 40 50)))

  (make-dataframe (rand 6 3))


  (defparameter *my-df-1*
    (make-dataframe  #2A((1 2 3 4 5)
			 (10 20 30 40 50))
		     :caselabels (list "x" "y")
		     :varlabels (list "a" "b" "c" "d" "e")	
		     :doc "This is an interesting dataframe-array")
    )

  (defparameter *my-df-2*
	(make-instance 'dataframe-array
		       :storage
		       (make-array-from-listoflists
			(cybertiggyr-dsv::load-escaped
			 "/media/disk/Desktop/sandbox/CLS.git/Data/example-mixed.csv"))
		       :doc "This is an interesting dataframe-array"))
#|		       :case-labels (list "x" "y")
		       :var-labels (list "a" "b" "c" "d" "e")
|#  


  )

(progn ;; Data setup

  (describe 'make-matrix)

  (defparameter *indep-vars-2-matrix*
    (make-matrix (length iron) 2
		 :initial-contents
		 (mapcar #'(lambda (x y) 
			     (list (coerce x 'double-float)
				   (coerce y 'double-float)))
			 iron aluminum)))

    
  (defparameter *dep-var*
    (make-vector (length absorbtion)
		 :type :row
		 :initial-contents
		 (list 
		  (mapcar #'(lambda (x) (coerce x 'double-float))
			  absorbtion))))
  
  (defparameter *dep-var-int*
    (make-vector (length absorbtion)
		 :type :row
		 :element-type 'integer
		 :initial-contents (list absorbtion)))


  (defparameter *xv+1a*
    (make-matrix
     8 2
     :initial-contents #2A((1d0 1d0)
			   (1d0 3d0)
			   (1d0 2d0)
			   (1d0 4d0)
			   (1d0 3d0)
			   (1d0 5d0)
			   (1d0 4d0)
			   (1d0 6d0))))

  (defparameter *xv+1b*
    (bind2
     (ones 8 1)
     (make-matrix
      8 1
      :initial-contents '((1d0)
			  (3d0)
			  (2d0)
			  (4d0)
			  (3d0)
			  (5d0)
			  (4d0)
			  (6d0)))
     :by :column))

  (m= *xv+1a* *xv+1b*) ; => T
  
  (princ "Data Set up"))




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
(progn ;; FIXME: Regression modeling

  ;; data setup in previous FIXME
  (defparameter  *m* nil
    "holding variable.")
  ;; need to make vectors and matrices from the lists...

  ;; BROKEN
  (def *m* (regression-model (list->vector-like iron)
			     (list->vector-like absorbtion)))

  (def m (regression-model (list->vector-like iron)
			   (list->vector-like absorbtion) :print nil))
			   ;;Good
  (send m :print)
  (send m :own-slots)
  (send m :own-methods)
  ;; (lsos::ls-objects-methods m) ; bogus?
  (send m :show)
  
  (def m (regression-model (list->vector-like iron)
			   (list->vector-like absorbtion)))

  (def m (regression-model (listoflists->matrix-like  (list iron aluminum))
			   (list->vector-like  absorbtion) :print nil))


  (send m :compute)
  (send m :sweep-matrix)
  (format t "~%~A~%" (send m :sweep-matrix))

  ;; need to get multiple-linear regression working (simple linear regr
  ;; works)... to do this, we need to redo the whole numeric structure,
  ;; I'm keeping these in as example of brokenness...
  
  (send m :basis) ;; this should be positive?
  (send m :coef-estimates)  )

#+nil
(progn ;; FIXME: Need to clean up data examples, licenses, attributions, etc.
  ;; The following breaks because we should use a package to hold
  ;; configuration details, and this would be the only package outside
  ;; of packages.lisp, as it holds the overall defsystem structure.
  (load-data "iris.lsp")  ;; (the above partially fixed).
  (variables)
  diabetes )




(progn ;; FIXME: read data from CSV file.  To do.

  
  ;; challenge is to ensure that we get mixed arrays when we want them,
  ;; and single-type (simple) arrays in other cases.


  (defparameter *csv-num*
    (cybertiggyr-dsv::load-escaped
     #p"/media/disk/Desktop/sandbox/CLS.git/Data/example-numeric.csv"
     :field-separator #\,
     :trace T))

  (nth 0 (nth 0 *csv-num*))

  (defparameter *csv-num*
    (cybertiggyr-dsv::load-escaped
     #p"/media/disk/Desktop/sandbox/CLS.git/Data/example-numeric2.dsv"
     :field-separator #\:))

  (nth 0 (nth 0 *csv-num*))


  ;; The handling of these types should be compariable to what we do for
  ;; matrices, but without the numerical processing.  i.e. mref, bind2,
  ;; make-dataframe, and the class structure should be similar. 
  
  ;; With numerical data, there should be a straightforward mapping from
  ;; the data.frame to a matrix.   With categorical data (including
  ;; dense categories such as doc-strings, as well as sparse categories
  ;; such as binary data), we need to include metadata about ordering,
  ;; coding, and such.  So the structures should probably consider 

  ;; Using the CSV file:

  (defun parse-number (s)
    (let* ((*read-eval* nil)
	   (n (read-from-string s)))
      (if (numberp n) n)))

  (parse-number "34")
  (parse-number "34 ")
  (parse-number " 34")
  (parse-number " 34 ")

  (+  (parse-number "3.4") 3)
  (parse-number "3.4 ")
  (parse-number " 3.4")
  (+  (parse-number " 3.4 ") 3)

  (parse-number "a")

  ;; (coerce "2.3" 'number)  => ERROR
  ;; (coerce "2" 'float)  => ERROR
  
  (defparameter *csv-num*
    (cybertiggyr-dsv::load-escaped
     #p"/media/disk/Desktop/sandbox/CLS.git/Data/example-numeric.csv"
     :field-separator #\,
     :filter #'parse-number
     :trace T))

  (nth 0 (nth 0 *csv-num*))

  (defparameter *csv-num*
    (cybertiggyr-dsv::load-escaped
     #p"/media/disk/Desktop/sandbox/CLS.git/Data/example-numeric2.dsv"
     :field-separator #\:
     :filter #'parse-number))

  (nth 0 (nth 0 *csv-num*))
  
  ;; now we've got the DSV code in the codebase, auto-loaded I hope:
  cybertiggyr-dsv:*field-separator*
  (defparameter *example-numeric.csv* 
    (cybertiggyr-dsv:load-escaped "Data/example-numeric.csv"
				  :field-separator #\,))
  *example-numeric.csv*

  ;; the following fails because we've got a bit of string conversion
  ;; to do.   2 thoughts: #1 modify dsv package, but mucking with
  ;; encapsulation.  #2 add a coercion tool (better, but potentially
  ;; inefficient).
  #+nil(coerce  (nth 3 (nth 3 *example-numeric.csv*)) 'double-float)

  ;; cases, simple to not so
  (defparameter *test-string1* "1.2")
  (defparameter *test-string2* " 1.2")
  (defparameter *test-string3* " 1.2 ")
  )


#+nil
(progn ;; experiments with GSL and the Lisp interface.
  (asdf:oos 'asdf:load-op 'gsll)
  (asdf:oos 'asdf:load-op 'gsll-tests)

  ;; the following should be equivalent
  (setf *t1*  (LIST 6.18d0 6.647777777777779d0 6.18d0))
  (setf *t2*  (MULTIPLE-VALUE-LIST
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


;;;; LM

(progn 

  (defparameter *y*
    (make-vector
     8
     :type :row
     :initial-contents '((1d0 2d0 3d0 4d0 5d0 6d0 7d0 8d0))))


  (defparameter *xv+1*
    (make-matrix
     8 2
     :initial-contents '((1d0 1d0)
			 (1d0 3d0)
			 (1d0 2d0)
			 (1d0 4d0)
			 (1d0 3d0)
			 (1d0 5d0)
			 (1d0 4d0)
			 (1d0 6d0))))


  ;; so something like (NOTE: matrices are transposed to begin with, hence the incongruety)
  (defparameter *xtx-2* (m* (transpose *xv+1*) *xv+1*))
  ;; #<LA-SIMPLE-MATRIX-DOUBLE  2 x 2
  ;;  8.0d0 28.0d0
  ;;  28.0d0 116.0d0>

  (defparameter *xty-2* (m* (transpose *xv+1*)  (transpose *y*)))
  ;; #<LA-SIMPLE-VECTOR-DOUBLE (2 x 1)
  ;;  36.0d0
  ;;  150.0d0>

  (defparameter *rcond-2* 0.000001)
  (defparameter *betahat-2*  (gelsy *xtx-2* *xty-2* *rcond-2*))
  ;; *xtx-2* => "details of complete orthogonal factorization"
  ;; according to man page:
  ;; #<LA-SIMPLE-MATRIX-DOUBLE  2 x 2
  ;;  -119.33147112141039d0 -29.095426104883202d0
  ;;  0.7873402682880205d0 -1.20672274167718d0>

  ;; *xty-2* => output becomes solution:
  ;; #<LA-SIMPLE-VECTOR-DOUBLE (2 x 1)
  ;;  -0.16666666666668312d0
  ;;  1.333333333333337d0>

  *betahat-2* ; which matches R, see below

  (documentation 'gelsy 'function)


;;   (#<LA-SIMPLE-VECTOR-DOUBLE (2 x 1)
;;    -0.16666666666668312 1.333333333333337>
;;    2)

;;   ## Test case in R:
;;   x <- c( 1.0, 3.0, 2.0, 4.0, 3.0, 5.0, 4.0, 6.0)
;;   y <- c( 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0)
;;   lm(y~x)
;;   ## => Call:  lm(formula = y ~ x)

;;   Coefficients:  (Intercept)            x  
;;                      -0.1667       1.3333  

;;   summary(lm(y~x))
;;   ## =>

;;   Call:
;;   lm(formula = y ~ x)

;;   Residuals:
;;          Min         1Q     Median         3Q        Max 
;;   -1.833e+00 -6.667e-01 -3.886e-16  6.667e-01  1.833e+00 

;;   Coefficients:
;;               Estimate Std. Error t value Pr(>|t|)   
;;   (Intercept)  -0.1667     1.1587  -0.144  0.89034   
;;   x             1.3333     0.3043   4.382  0.00466 **
;;   ---
;;   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

;;   Residual standard error: 1.291 on 6 degrees of freedom
;;   Multiple R-squared: 0.7619,	Adjusted R-squared: 0.7222 
;;   F-statistic:  19.2 on 1 and 6 DF,  p-value: 0.004659 



  ;; which suggests one might do (modulo ensuring correct
  ;; orientations).  When this is finalized, it should migrate to
  ;; CLS.
  ;;


  (defparameter *n* 20) ; # rows = # obsns
  (defparameter *p* 10) ; # cols = # vars 
  (defparameter *x-temp*  (rand *n* *p*))
  (defparameter *b-temp*  (rand *p* 1))
  (defparameter *y-temp*  (m* *x-temp* *b-temp*))
  ;; so Y=Xb + \eps
  (defparameter *rcond* (* (coerce (expt 2 -52) 'double-float)
		   (max (nrows *x-temp*) (ncols *y-temp*))))
  (defparameter *orig-x* (copy *x-temp*))
  (defparameter *orig-b* (copy *b-temp*))
  (defparameter *orig-y* (copy *y-temp*))

  (defparameter *lm-result* (lm *x-temp* *y-temp*))
  (princ (first *lm-result*))
  (princ (second *lm-result*))
  (princ (third *lm-result*))
  (v= (third *lm-result*)
      (v- (first (first *lm-result*)) 
	  (first  (second *lm-result*))))



  
  ;; Some issues exist in the LAPACK vs. LINPACK variants, hence R
  ;; uses LINPACK primarily, rather than LAPACK.  See comments in R
  ;; source for issues.  


  ;; Goal is to start from X, Y and then realize that if
  ;; Y = X \beta, then,   i.e. 8x1 = 8xp px1  + 8x1
  ;;      XtX \hat\beta = Xt Y
  ;; so that we can solve the equation  W \beta = Z   where W and Z
  ;; are known, to estimate \beta.

  ;; the above is known to be numerically instable -- some processing
  ;; of X is preferred and should be done prior.  And most of the
  ;; transformation-based work does precisely that.

  ;; recall:  Var[Y] = E[(Y - E[Y])(Y-E[Y])t]
  ;;   = E[Y Yt] - 2 \mu \mut + \mu \mut
  ;;   = E[Y Yt] - \mu \mut

  ;; Var Y = E[Y^2] - \mu^2


  ;; For initial estimates of covariance of \hat\beta:

  ;; \hat\beta = (Xt X)^-1 Xt Y
  ;; with E[ \hat\beta ] 
  ;;        = E[ (Xt X)^-1 Xt Y ]
  ;;        = E[(Xt X)^-1 Xt (X\beta)]
  ;;        = \beta 
  ;;        
  ;; So Var[\hat\beta] = ...
  ;;     (Xt X)
  ;; and this gives SE(\beta_i) = (* (sqrt (mref Var i i)) adjustment)


  ;; from docs:

  (setf *temp-result* 
	(let ((*default-implementation* :foreign-array))
	  (let* ((m 10)
		 (n 10)
		 (a (rand m n))
		 (x (rand n 1))
		 (b (m* a x))
		 (rcond (* (coerce (expt 2 -52) 'double-float)
			   (max (nrows a) (ncols a))))
		 (orig-a (copy a))
		 (orig-b (copy b))
		 (orig-x (copy x)))
	    (list x (gelsy a b rcond))
	    ;; no applicable conversion?
	    ;; (m-   (#<FA-SIMPLE-VECTOR-DOUBLE (10 x 1)) 
	    ;;       (#<FA-SIMPLE-VECTOR-DOUBLE (10 x 1)) )
	    (v- x (first (gelsy a b rcond))))))

  
  (princ *temp-result*)
  
  (setf *temp-result* 
	(let ((*default-implementation* :lisp-array))
	  (let* ((m 10)
		 (n 10)
		 (a (rand m n))
		 (x (rand n 1))
		 (b (m* a x))
		 (rcond (* (coerce (expt 2 -52) 'double-float)
			   (max (nrows a) (ncols a))))
		 (orig-a (copy a))
		 (orig-b (copy b))
		 (orig-x (copy x)))
	    (list x (gelsy a b rcond))
	    (m- x (first  (gelsy a b rcond)))
	    )))
  (princ *temp-result*)


  (defparameter *xv*
    (make-vector
     8
     :type :row ;; default, not usually needed!
     :initial-contents '((1d0 3d0 2d0 4d0 3d0 5d0 4d0 6d0))))

  (defparameter *y*
    (make-vector
     8
     :type :row
     :initial-contents '((1d0 2d0 3d0 4d0 5d0 6d0 7d0 8d0))))

  ;; so something like (NOTE: matrices are transposed to begin with, hence the incongruety)
  (defparameter *xtx-1* (m* *xv* (transpose *xv*)))
  (defparameter *xty-1* (m* *xv* (transpose  *y*)))
  (defparameter *rcond-in* (* (coerce (expt 2 -52) 'double-float)
			      (max (nrows *xtx-1*)
				   (ncols *xty-1*))))

  (defparameter *betahat*  (gelsy *xtx-1* *xty-1* *rcond-in*))

  ;;  (#<LA-SIMPLE-VECTOR-DOUBLE (1 x 1)
  ;;  1.293103448275862>
  ;;  1)

  ;;   ## Test case in R:
  ;;   x <- c( 1.0, 3.0, 2.0, 4.0, 3.0, 5.0, 4.0, 6.0)
  ;;   y <- c( 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0)
  ;;   lm(y~x-1)
  ;;   ## => 
  ;;   Call:
  ;;   lm(formula = y ~ x - 1)

  ;;   Coefficients:
  ;;       x  
  ;;   1.293  

  (first  *betahat*))



#+nil
(progn

  (asdf:oos 'asdf:load-op 'cl-plplot)

  (plot-ex))
