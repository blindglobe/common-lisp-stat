;;; -*- mode: lisp -*-
;;;
;;; Copyright (c) 2008--, by A.J. Rossini <blindglobe@gmail.com>
;;; See COPYRIGHT file for any additional restrictions (BSD license).
;;; Since 1991, ANSI was finally finished.  Modified to match ANSI
;;; Common Lisp.  

;;;; Originally from:
;;;; regression.lsp XLISP-STAT regression model proto and methods
;;;; XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney
;;;; Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz
;;;; You may give out copies of this software; for conditions see the file
;;;; COPYING included with this distribution.
;;;;
;;;; Incorporates modifications suggested by Sandy Weisberg.

;;; This version uses lisp-matrix for underlying numerics.

(in-package :lisp-stat-regression-linear)

;;; Regresion Model Prototype

;; The general strategy behind the fitting of models using prototypes
;; is that we need to think about want the actual fits are, and then
;; the fits can be used to recompute as components are changes.  One
;; catch here is that we'd like some notion of trace-ability, in
;; particular, there is not necessarily a fixed way to take care of the
;; audit trail.  save-and-die might be a means of recording the final
;; approach, but we are challenged by the problem of using advice and
;; other such features to capture stages and steps that are considered
;; along the goals of estimating a model.

(defvar regression-model-proto nil "Prototype for all regression model instances.")
(defproto regression-model-proto
    '(x y intercept betahat
      basis weights included
      total-sum-of-squares residual-sum-of-squares
      predictor-names response-name case-labels doc)
  () *object*  "Normal Linear Regression Model")

(defclass regression-model-class () ; (statistical-model)
  (;; Data
   (y
    :initform nil
    :initarg  :y
    :accessor response-variable
    :type     vector-like
    :documentation "vector-like containing univariate response values."
    )
   (x
    :initform nil
    :initarg  :x
    :accessor design-matrix
    :type matrix-like
    :documentation "matrix-like containing design/data matrix.  If
  interceptp is T, assume first column is intercept (ones), and hence
  a bit special.")
   (interceptp
    :initform nil
    :initarg :interceptp
    :accessor interceptp
    :type boolean
    :documentation "When T, treat first column of design/data matrix
  special as intercept fit.")

   ;; Metadata
   (covariate-names
    :initform nil
    :initarg :covariate-names
    :accessor covariate-names
    :type list
    :documentation "List of strings representing covariate names.
   Might include symbols at some point")
   (response-name
    :initform nil
    :initarg :response-name
    :accessor response-name
    :type list)
   (case-labels
    :initform nil
    :initarg :case-labels
    :accessor case-labels
    :type list)
   (doc-string
    :initform ""
    :initarg :docs
    :accessor doc-string
    :type string))
  (:documentation "Normal Linear Regression Model with CLOS.
  Historical design based on LispStat."))


(defclass regression-model-fit-class ()
  (;; data/design/state
   (data-model 
    :initform nil
    :initarg  :data-model
    :accessor data-model
    :type     regression-model-class)


   (needs-computing
    :initform T
    :initarg  :needs-computing
    :accessor needs-computing)
   ;; above modified to TRUE when the following are changed
   (included
    :initform nil
    :initarg  :included
    :accessor included
    :type     matrix-like)
   (weights
    :initform nil
    :initarg  :weights
    :accessor weights
    :type     matrix-like)
   (weights-types
    :initform nil
    :initarg  :weight-types
    :accessor weight-types
    :type     matrix-like)

   ;; computational artifacts for storage; when NEEDS-COMPUTING is
   ;; NIL, these are up-to-date.
   (basis
    :initform nil
    :accessor basis
    :type     matrix-like)
   (estimates
    :initform nil 
    :initarg  :estimates
    :accessor estimates
    :type     vector-like)
   (estimates-covariance-matrix
    :initform nil
    :initarg  :estimates-covariance-matrix
    :accessor covariance-matrix
    :type     matrix-like)
   (total-sum-of-squares
    :initform 0d0
    :accessor tss
    :type number)
   (residual-sum-of-squares
    :initform 0d0
    :accessor rss
    :type number)

   (doc-string
    :initform "No info given."
    :initarg :doc
    :accessor doc-string
    :type string))
  (:documentation "Normal Linear Regression Model _FIT_ through CLOS."))

;;;;;;;; Helper functions

(defun xtxinv (x &key (intercept T))
  "In: X      Out: (XtX)^-1 

X is NxP, resulting in PxP.  Represents Var[\hat\beta], the varest for
\hat \beta from Y = X \beta + \eps.  Done by Cholesky decomposition,
with LAPACK's dpotri routine, factorizing with dpotrf.

<example>
  (let ((m1 (rand 7 5)))
     (xtxinv m1))
</example>"
  (check-type x matrix-like)
  (let ((myx (if intercept
		  (bind2 (ones (matrix-dimension x 0) 1)
			 x :by :column)
		  x)))
    (minv-cholesky (m* (transpose myx) myx))))

;; might add args: (method 'gelsy), or do we want to put a more
;; general front end, linear-least-square, across the range of
;; LAPACK solvers? 
(defun lm (x y &key (intercept T) rcond)
  "fit the linear model:
           y = x \beta + e 

and estimate \beta.  X,Y should be in cases-by-vars form, i.e. X
should be n x p, Y should be n x 1.  Returns estimates, n and p.
Probably should return a form providing the call, as well.

R's lm object returns: coefficients, residuals, effects, rank, fitted,
qr-results for numerical considerations, DF_resid.  Need to
encapsulate into a class or struct."
    (check-type x matrix-like)
    (check-type y vector-like) ; vector-like might be too strict?
    (assert
     (= (nrows y) (nrows x)) ; same number of observations/cases
     (x y) "Can not multiply x:~S by y:~S" x y)
    (let ((x1 (if intercept
		  (bind2 (ones (matrix-dimension x 0) 1)
			 x :by :column)
		  x)))
      (let ((betahat (gelsy (m* (transpose x1) x1)
			    (m* (transpose x1) y)
			    (if rcond rcond
				(* (coerce (expt 2 -52) 'double-float)
				   (max (nrows x1)
					(ncols y))))))
	    (betahat1 (gelsy x1
			     y
			     (if rcond rcond
				 (* (coerce (expt 2 -52) 'double-float)
				     (max (nrows x1)
					  (ncols y)))))))
	;; need computation for SEs, 
	(format t "")
	(list betahat  ; LA-SIMPLE-VECTOR-DOUBLE
	      betahat1 ; LA-SLICE-VECVIEW-DOUBLE
	      (xtxinv x1); (sebetahat betahat x y) ; TODO: write me!
	      (nrows x)  ; surrogate for n
	      (ncols x1) ; surrogate for p
	      (v- (first betahat) (first betahat1)) ))))


(defun regression-model (x y &key  (intercept T)
			 (predictor-names nil) (response-name nil) (case-labels nil)
			 (doc "Undocumented Regression Model Instance"))
  "Args: (x y &key (intercept T) (print T) (weights nil)
          included predictor-names response-name case-labels)
X           - list of independent variables or X matrix
Y           - dependent variable.
INTERCEPT   - T to include (default), NIL for no intercept
PRINT       - if not NIL print summary information
WEIGHTS     - if supplied should be the same length as Y; error
              variances are  
              assumed to be inversely proportional to WEIGHTS
PREDICTOR-NAMES, RESPONSE-NAME, CASE-LABELS
            - sequences of strings or symbols.
INCLUDED    - if supplied, should be length Y or 1, with
 	      elements nil to skip or T to include for computing estimates
              (always included in residual analysis).
Returns a regression model object."
  (check-type x matrix-like)
  (check-type y vector-like)
  (let ((newmodel (make-instance 'regression-model-class
				 :y y
				 :x x
				 :interceptp intercept
				 :case-labels case-labels
				 :covariate-names predictor-names
				 :response-name  response-name
				 :docs doc )))
    newmodel))

(defun fit-model (model &key (included T) (wgts nil) (docs "No Docs"))
  (let ((result (make-instance 'regression-model-fit-class
		 :data-model model
		 :needs-computing T
		 :included included
		 :weights wgts
		 :estimates (first (lm (design-matrix model)
				       (response-variable model)
				       :intercept (interceptp model)))
		 :estimates-covariance-matrix
		    (xtxinv (design-matrix model)
			    :intercept (interceptp model))
		 :doc docs)))
    result))

(defmethod print-object ((obj regression-model-fit-class) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "Estimates: ~%~A~%" (estimates obj))
    (format stream "Covariance: ~%~A~%" (covariance-matrix obj))
    (format stream "Doc: ~%~A~%" (doc-string obj))))

(defmethod print-object ((obj regression-model-class) stream)
  "Need better formatting and output -- clearly this is a WRONG EXAMPLE."
  (print-unreadable-object (obj stream :type t)
    (format stream "Response: ~%~A~%" (response-variable obj))
    (format stream "Design: ~%~A~%" (design-matrix obj))
    (format stream "Covariates: ~%~A~%" (covariate-names obj))
    (format stream "Doc: ~%~A~%" (doc-string obj))))


#|
 (defun regression-model-reconstruct  (regr-obj)
  "ALL WRONG.  But when right, returns an expression to reconstruct the regression model."
  `(regression-model ',(response-variable regr-obj)
                     ',(send self :y)
                     :intercept ',(send self :intercept)
                     :weights ',(send self :weights)
                     :included ',(send self :included)
                     :predictor-names ',(send self :predictor-names)
                     :response-name ',(send self :response-name)
                     :case-labels ',(send self :case-labels)))
|#
                       
;;; Computing and Display Methods

;; [X|Y]t [X|Y]
;; = XtX  XtY
;;   YtX  YtY
;; so with (= (dim X) (list n p))
;; we end up with p x p   p x 1
;;                1 x p   1 x 1
;;
;; and this can be implemented by
#|
  (setf XY (bind2 X Y :by :row))
  (setf XYtXY (m* (transpose XY) XY))
|#
;; which is too procedural.  Sigh, I meant
#|
  (setf XYtXY (let ((XY (bind2 X Y :by :row)))
                 (m*  (transpose XY) XY)))
|#
;; which at least looks lispy.

(defmeth regression-model-proto :compute ()
"Message args: ()
Recomputes the estimates. For internal use by other messages"
  (let* ((included (if-else (send self :included) 1d0 0d0))
         (x (send self :x))
         (y (send self :y))
         (intercept (send self :intercept)) ;; T/nil
         (weights (send self :weights)) ;; vector-like or nil
         (w (if weights (* included weights) included))
         (n (matrix-dimension x 0))
         (p (if intercept
		(1- (matrix-dimension x 1))
		(matrix-dimension x 1))) ;; remove intercept from # params -- right?
         (tss 0)
	 (res (make-vector (nrows x) :type :column :initial-element 0d0)) ;  (compute-residuals y yhat)
         (tol 0.000001 
	   ;;  (* 0.001 (reduce #'* (mapcar #'standard-deviation (list-of-columns x))))
	  ))
    (format t
	    "~%REMVME: regr-pr :compute~%x= ~A~%y= ~A~% tss= ~A~% tol= ~A~%  w= ~A~% n= ~A~%  res= ~A  p=~A ~% "
	   x y tss tol w n res p)

    (setf (proto-slot-value 'betahat)
	  (first  (lm (send self :x)
		      (send self :y)))) ;; FIXME!

    (setf (proto-slot-value 'total-sum-of-squares) tss)
    (setf (proto-slot-value 'residual-sum-of-squares) 
	  0d0
          ;; (m* (ones 1 n) (v* res res))
	  )))

(defmeth regression-model-proto :needs-computing (&optional set)
"Message args: ( &optional set )

If value given, sets the flag for whether (re)computation is needed to
update the model fits."	 
   (send self :nop)
   (if set (setf (proto-slot-value 'betahat) nil))
   (null (proto-slot-value 'betahat)))
  
(defmeth regression-model-proto :display ()
  "Message args: ()
Prints the least squares regression summary. Variables not used in the fit
are marked as aliased."
  (send self :x)
  (format nil "Computing Regression Proto :display"))

#|
  (let ((coefs (vector-like->list (send self :coef-estimates)))
        (se-s (send self :coef-standard-errors))
        (x (send self :x))
        (p-names (send self :predictor-names)))
    (if (send self :weights) 
        (format t "~%Weighted Least Squares Estimates:~2%")
        (format t "~%Least Squares Estimates:~2%"))
    (when (send self :intercept)
          (format t "Constant               ~10f   ~A~%"
                  (car coefs) (list (car se-s)))
	  (setf coefs (cdr coefs))
          (setf se-s (cdr se-s)))
    (dotimes (i (array-dimension x 1)) 
             (cond 
               ((member i (send self :basis))
                (format t "~22a ~10f   ~A~%"
                        (select p-names i) (car coefs) (list (car se-s)))
                (setf coefs (cdr coefs) se-s (cdr se-s)))
               (t (format t "~22a    aliased~%" (select p-names i)))))
    (format t "~%")
    (format t "R Squared:             ~10f~%" (send self :r-squared))
    (format t "Sigma hat:             ~10f~%" (send self :sigma-hat))
    (format t "Number of cases:       ~10d~%" (send self :num-cases))
    (if (/= (send self :num-cases) (send self :num-included))
        (format t "Number of cases used:  ~10d~%" (send self :num-included)))
    (format t "Degrees of freedom:    ~10d~%" (send self :df))
    (format t "~%")))
|#

;;; Slot accessors and mutators

(defmeth regression-model-proto :doc (&optional new-doc append)
"Message args: (&optional new-doc)

Returns the DOC-STRING as supplied to m.  
Additionally, with an argument NEW-DOC, sets the DOC-STRING to
NEW-DOC.  In this setting, when APPEND is T, don't replace and just
append NEW-DOC to DOC."
  (send self :nop)
  (when (and new-doc (stringp new-doc))
    (setf (proto-slot-value 'doc)
	  (if append
	      (concatenate 'string
			   (proto-slot-value 'doc)
			   new-doc)
	      new-doc)))
  (proto-slot-value 'doc))


(defmeth regression-model-proto :x (&optional new-x)
"Message args: (&optional new-x)

With no argument returns the x matrix-like as supplied to m. With an
argument, NEW-X sets the x matrix-like to NEW-X and recomputes the
estimates."
  (when (and new-x (typep new-x 'matrix-like))
    (setf (proto-slot-value 'x) new-x)
    (send self :needs-computing t))
  (proto-slot-value 'x))

(defmeth regression-model-proto :y (&optional new-y)
"Message args: (&optional new-y)

With no argument returns the y vector-like as supplied to m. With an
argument, NEW-Y sets the y vector-like to NEW-Y and recomputes the
estimates."
  (when (and new-y
	     (typep new-y 'vector-like))
    (setf (proto-slot-value 'y) new-y) ;; fixme -- pls set slot value to a vector-like!
    (send self :needs-computing t))
  (proto-slot-value 'y))

(defmeth regression-model-proto :intercept (&optional (val nil set))
"Message args: (&optional new-intercept)

With no argument returns T if the model includes an intercept term,
nil if not. With an argument NEW-INTERCEPT the model is changed to
include or exclude an intercept, according to the value of
NEW-INTERCEPT."
  (when set 
    (setf (proto-slot-value 'intercept) val)
    (send self :needs-computing t))
  (proto-slot-value 'intercept))

(defmeth regression-model-proto :weights (&optional (new-w nil set))
"Message args: (&optional new-w)

With no argument returns the weight vector-like as supplied to m; NIL
means an unweighted model. NEW-W sets the weights vector-like to NEW-W
and recomputes the estimates."
  (when set
#|    ;; probably need to use "check-type" or similar?
      (and set nil
	   (or (= new-w nil)
	       (typep new-w 'vector-like)))
|#
    (setf (proto-slot-value 'weights) new-w) 
    (send self :needs-computing t))
  (proto-slot-value 'weights))

(defmeth regression-model-proto :total-sum-of-squares ()
"Message args: ()

Returns the total sum of squares around the mean.
This is recomputed if an update is needed."
  (if (send self :needs-computing)
      (send self :compute))
  (proto-slot-value 'total-sum-of-squares))

(defmeth regression-model-proto :residual-sum-of-squares () 
"Message args: ()

Returns the residual sum of squares for the model.
This is recomputed if an update is needed."
  (if (send self :needs-computing)
      (send self :compute))
  (proto-slot-value 'residual-sum-of-squares))

(defmeth regression-model-proto :basis ()
"Message args: ()

Returns the indices of the variables used in fitting the model, in a
sequence.
This is recomputed if an update is needed."
  (if (send self :needs-computing)
      (send self :compute))
  (proto-slot-value 'basis))
  
(defmeth regression-model-proto :included (&optional new-included)
"Message args: (&optional new-included)

With no argument, NIL means a case is not used in calculating
estimates, and non-nil means it is used.  NEW-INCLUDED is a sequence
of length of y of nil and t to select cases.  Estimates are
recomputed."
  (when new-included
#|
    (and new-included 
         (= (length new-included) (send self :num-cases)))
|#
        (setf (proto-slot-value 'included) (copy-seq new-included)) 
        (send self :needs-computing t))
  (if (proto-slot-value 'included)
      (proto-slot-value 'included)
      (repeat t (send self :num-cases))))

(defmeth regression-model-proto :predictor-names (&optional (names nil set))
"Message args: (&optional (names nil set))

With no argument returns the predictor names. NAMES sets the names."
  (if set (setf (proto-slot-value 'predictor-names) (mapcar #'string names)))
  (let ((p (matrix-dimension (send self :x) 1))
        (p-names (proto-slot-value 'predictor-names)))
    (if (not (and p-names (= (length p-names) p)))
        (setf (proto-slot-value 'predictor-names)
              (mapcar #'(lambda (a) (format nil "Variable ~a" a)) 
                      (iseq 0 (- p 1))))))
  (proto-slot-value 'predictor-names))

(defmeth regression-model-proto :response-name (&optional (name "Y" set))
   "Message args: (&optional name)

With no argument returns the response name. NAME sets the name."
   (send self :nop)
   (if set (setf (proto-slot-value 'response-name) (if name (string name) "Y")))
   (proto-slot-value 'response-name))

(defmeth regression-model-proto :case-labels (&optional (labels nil set))
"Message args: (&optional labels)
With no argument returns the case-labels. LABELS sets the labels."
  (if set (setf (proto-slot-value 'case-labels) 
                (if labels 
                    (mapcar #'string labels)
                    (mapcar #'(lambda (x) (format nil "~d" x)) 
                            (iseq 0 (- (send self :num-cases) 1))))))
  (proto-slot-value 'case-labels))

;;;
;;; Other Methods
;;; None of these methods access any slots directly.
;;;

(defmeth regression-model-proto :num-cases ()
"Message args: ()
Returns the number of cases in the model."
  (nelts (send self :y))) ; # cases in data, must accomodate weights or masking!

(defmeth regression-model-proto :num-included ()
"Message args: ()
Returns the number of cases used in the computations."
  (sum (if-else (send self :included) 1 0)))

(defmeth regression-model-proto :num-coefs ()
"Message args: ()
Returns the number of coefficients in the fit model (including the
intercept if the model includes one)."
  (if (send self :intercept)
      (+ 1 (ncols (send self :x)))
      (ncols  (send self :x))))

(defmeth regression-model-proto :df ()
"Message args: ()
Returns the number of degrees of freedom in the model."
  (- (send self :num-included) (send self :num-coefs)))
  
(defmeth regression-model-proto :x-matrix ()
"Message args: ()
Returns the X matrix for the model, including a column of 1's, if
appropriate. Columns of X matrix correspond to entries in basis."
  (let ((m (select (send self :x) 
                   (iseq 0 (- (send self :num-cases) 1)) 
                   (send self :basis))))
    (if (send self :intercept)
        (bind2 (repeat 1 (send self :num-cases)) m)
        m)))

(defmeth regression-model-proto :leverages ()
"Message args: ()
Returns the diagonal elements of the hat matrix."
  (let* ((x (send self :x-matrix))
         (raw-levs 
          (m* (m* (m* x
		      (send self :xtxinv))
		  x)
	      (repeat 1 (send self :num-coefs)))))
    (if (send self :weights)
	(m* (send self :weights) raw-levs)
	raw-levs)))

(defmeth regression-model-proto :fit-values ()
"Message args: ()
Returns the fitted values for the model."
  (m* (send self :x-matrix)
      (send self :coef-estimates)))

(defmeth regression-model-proto :raw-residuals () 
"Message args: ()
Returns the raw residuals for a model."
  (v- (send self :y) (send self :fit-values)))

(defmeth regression-model-proto :residuals () 
"Message args: ()
Returns the raw residuals for a model without weights. If the model
includes weights the raw residuals times the square roots of the weights
are returned."
  (let ((raw-residuals (send self :raw-residuals))
        (weights (send self :weights)))
    (if weights (* (sqrt weights) raw-residuals) raw-residuals)))

(defmeth regression-model-proto :sum-of-squares () 
"Message args: ()
Returns the error sum of squares for the model."
  (send self :residual-sum-of-squares))

(defmeth regression-model-proto :sigma-hat ()
"Message args: ()
Returns the estimated standard deviation of the deviations about the 
regression line."
  (let ((ss (send self :sum-of-squares))
        (df (send self :df)))
    (if (/= df 0) (sqrt (/ ss df)))))

;; for models without an intercept the 'usual' formula for R^2 can give
;; negative results; hence the max.
(defmeth regression-model-proto :r-squared ()
"Message args: ()
Returns the sample squared multiple correlation coefficient, R squared, for
the regression."
  (max (- 1 (/ (send self :sum-of-squares) (send self :total-sum-of-squares)))
       0))

(defmeth regression-model-proto :coef-estimates ()
"Message args: ()

Returns the OLS (ordinary least squares) estimates of the regression
coefficients. Entries beyond the intercept correspond to entries in
basis."
  (let ((x (send self :x)))
    (princ x)))
#|
  (let ((n (matrix-dimension (send self :x) 1))
        (indices (flatten-list
		  (if (send self :intercept) 
                     (cons 0 (+ 1 (send self :basis)))
                     (list (+ 1 (send self :basis))))))
        (x (send self :x)))
    (format t "~%REMOVEME2: Coef-ests: ~% Sweep Matrix: ~A ~% array dim 1: ~A ~% Swept indices:  ~A ~% basis: ~A"
	    x n indices (send self :basis))
    (coerce (compound-data-seq (select m (1+ n) indices)) 'list))) ;; ERROR
|#


(defmeth regression-model-proto :coef-standard-errors ()
"Message args: ()
Returns estimated standard errors of coefficients. Entries beyond the
intercept correspond to entries in basis."
  (let ((s (send self :sigma-hat))
	(v (map-vec #'sqrt (diagonalf (send self :xtxinv)))))
    (if s
	(etypecase s
	  (double (axpy s v (make-vector (nelts v) :type :column :initial-element 0d0)))
	  (vector-like (v* (send self :sigma-hat) v)))
	v)))

(defmeth regression-model-proto :studentized-residuals ()
"Message args:  ()
Computes the internally studentized residuals for included cases and externally studentized residuals for excluded cases."
  (let ((res (send self :residuals))
        (lev (send self :leverages))
        (sig (send self :sigma-hat))
        (inc (send self :included)))
    (if-else inc
             (/ res (* sig (sqrt (max .00001 (- 1 lev))))) ; vectorize max
             (/ res (* sig (sqrt (+ 1 lev)))))))

(defmeth regression-model-proto :externally-studentized-residuals ()
"Message args:  ()
Computes the externally studentized residuals."
  (let* ((res (send self :studentized-residuals))
         (df (send self :df)))
    (if-else (send self :included)
             (* res (sqrt (/ (- df 1) (- df (v* res res)))))
             res)))

(defmeth regression-model-proto :cooks-distances ()
"Message args: ()
Computes Cook's distances."
  (let ((lev (send self :leverages))
        (res (/ (v* (send self :studentized-residuals)
		    (send self :studentized-residuals))
                (send self :num-coefs))))
    (if-else (send self :included) (* res (/ lev (- 1 lev) )) (* res lev))))

#|
 (defun plot-points (x y &rest args)
  "need to fix."
  (error "Graphics not implemented yet."))
|#


#|
;; Can not plot points yet!!
 (defmeth regression-model-proto :plot-residuals (&optional x-values)
"Message args: (&optional x-values)
Opens a window with a plot of the residuals. If X-VALUES are not supplied 
the fitted values are used. The plot can be linked to other plots with the 
link-views function. Returns a plot object."
  (plot-points (if x-values x-values (send self :fit-values))
               (send self :residuals)
               :title "Residual Plot"
               :point-labels (send self :case-labels)))
|#

#|
 (defmeth regression-model-proto :plot-bayes-residuals 
  (&optional x-values)
  "Message args: (&optional x-values)

Opens a window with a plot of the standardized residuals and two
standard error bars for the posterior distribution of the actual
deviations from the line. See Chaloner and Brant. If X-VALUES are not
supplied the fitted values are used. The plot can be linked to other
plots with the link-views function.  Returns a plot object."

  (let* ((r (/ (send self :residuals)
	       (send self :sigma-hat)))
         (d (* 2 (sqrt (send self :leverages))))
         (low (- r d))
         (high (+ r d))
         (x-values (if x-values x-values (send self :fit-values)))
         (p (plot-points x-values r
			 :title "Bayes Residual Plot"
                         :point-labels (send self :case-labels))))
    (map 'list #'(lambda (a b c d) (send p :plotline a b c d nil))
	 x-values low x-values high)
    (send p :adjust-to-data)
    p))
|#

;;;; Other code

#|
  (defun print-lm (lm-obj)
    "transcribed from R"
    (p (rank lm-obj)
    (when (= p 0) 
      ;; EVIL LOGIC!  Just to store for now.
      (let ()
	    (n (length (residuals lm-obj)))
	    (w (if (weights lm-obj)
		   (weights lm-obj)
		   (ones n 1)))
	    (r  (if (weights lm-obj)
		      (residuals lm-obj)
		      (v.* (residuals lm-obj)
			   (mapcar #'sqrt (weights lm-obj)))))
	    (rss (sum (v.* r r)))
	    (resvar (/ rss (- n p)))
	    ;; then answer, to be encapsulated in a struct/class
	    ;; instance, 
	    (aliased (is.na (coef lm-obj)))
	    (residuals r)
	    (df (list 0 n (length aliased)))
	    (coefficients (list 'NA 0d0 4d0))o
	    (sigma (sqrt resvar))
	    (r.squared 0d0)
	    (adj.r.squared 0d0)))
      )
    ;;otherwise...
    (when (not (= p 0))
      (let ((n (nrows (qr lm-obj)))
	    (rdf  (- n p))
	    ))))

  (lm *xv+1* *y2*)
  (lm (transpose *xv*) *y2*)

  (princ "Linear Models Code setup")

|#
