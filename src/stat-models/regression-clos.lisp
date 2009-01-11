;;; -*- mode: lisp -*-

;;; Time-stamp: <2009-01-09 12:06:16 tony>
;;; Creation:   <2008-10-03 02:07:10 tony>
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2007, AJ Rossini.  BSD, LLGPL, or GPLv2, depending
;;;             on how it arrives.  
;;; Purpose:    redoing regression in a CLOS based framework.  See
;;;             regression.lsp for basis of work.  

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

;;; This organization and structure is new to the 21st Century
;;; version.

(in-package :lisp-stat-regression-linear-clos)

;;; Regresion Model CLOS, data structures

(defclass regression-model-clos (statistical-model)
  ((x :initform nil :initarg :x :accessor x)
   (y :initform nil :initarg :y :accessor y)
   (included :initform nil :initarg :y :accessor y)
   (total-sum-of-squares :initform nil :initarg :y :accessor y)
   (residual-sum-of-squares :initform nil :initarg :y :accessor y)
   (predictor-names :initform nil :initarg :y :accessor y)
   (response-name :initform nil :initarg :y :accessor y)
   (case-labels :initform nil :initarg :y :accessor y)
   (needs-computing :initform T :initarg :compute? :accessor compute?))
  (:documentation "Normal Linear Regression Model through CLOS.
  Historical design based on what was done for LispStat, not modern."))

(defclass model-specification ()
  ((spec-string :initform nil
		:initarg :specification
		:accessor :specification)
   (spec-form :initform nil
	      :initarg :spec-form
	      :accessor :spec-form)
   (model-class :initform nil))
  (:documentation "container for mathematical structure"))

(defclass bayesian-model-specification (model-specification)
  ((prior-model-class)
   (spec-string :initform nil
		:initarg :specification
		:accessor :specification)
   (spec-form :initform nil
	      :initarg :spec-form
	      :accessor :spec-form))
  (:documentation "adds structure holding priors to the model"))

;;; The following should be self-created based on introspection of
;;; available:
;;; ## inferential technologies (bayesian, frequentist, etc),
;;; ## optimization criteria (likelihood, least-squares, min-entropy,
;;;    minimax, etc) 
;;; ## simplification macros, i.e. mapping directly to linear
;;;    regression and other applications. fast specialized
;;;    algorithms for edge cases and narrow conditions.
;;; ## 

(defparameter *model-class-list*
  '((linear-regression frequentist)
    (generalized-linear-regression  parametric)
    (linear-regression bayesian)
    ()))



;;; Regression model generics and methods

(defgeneric regression-model (model-spec data-pointer &key debug)
  (:documentation "CLOSy framework for regression, using numerics from "))

(defmethod regression-model
    ((regr-inst regression-model-clos)
     (data-ptr  data-pointer)
     &key debug)
  "Args: (regr-inst regressino-model-clos)

Returns a fitted regression model object. To examine the model further
assign the result to a variable and send it messages.  Example (data
are in file absorbtion.lsp in the sample data directory/folder):

 (def fit-m (fit (new 'regression-model-clos (list iron aluminum) absorbtion)))
 (print fit-m)
 (plot  fit-m :feature 'residuals)
"
  (let ((x (get-variable-matrix (x regr-inst) data-ptr))
	(y (get-variable-vector (y regr-inst) data-ptr)))


(defmeth regression-model-proto :compute ()
  "Message args: ()
Recomputes the estimates. For internal use by other messages"
  (let* ((included (if-else (send self :included) 1 0))
	 (x (send self :x))
	 (y (send self :y))
	 (intercept (send self :intercept))
	 (weights (send self :weights))
	 (w (if weights (* included weights) included))
	 (m (make-sweep-matrix x y w))
	 (n (array-dimension x 1))
	 (p (- (array-dimension m 0) 1))
	 (tss (aref m p p))
	 (tol (* 0.001 (reduce #'* (mapcar #'standard-deviation (column-list x)))))
	 ;; (tol (* 0.001 (apply #'* (mapcar #'standard-deviation (column-list x)))))
	 (sweep-result
	  (if intercept
	      (sweep-operator m (iseq 1 n) tol)
	    (sweep-operator m (iseq 0 n) (cons 0.0 tol)))))
    (setf (slot-value 'sweep-matrix) (first sweep-result))
    (setf (slot-value 'total-sum-of-squares) tss)
    (setf (slot-value 'residual-sum-of-squares) 
	  (aref (first sweep-result) p p))
    (setf (slot-value 'basis)
	  (let ((b (remove 0 (second sweep-result))))
	    (if b (- (reduce #'- (reverse b)) 1)
	      (error "no columns could be swept"))))))

))


(defmeth regression-model-proto :display ()
  "Message args: ()
Prints the least squares regression summary. Variables not used in the fit
are marked as aliased."
  (let ((coefs (coerce (send self :coef-estimates) 'list))
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

;;; Slot accessors and mutators


(defmeth regression-model-proto :included (&optional new-included)
  "Message args: (&optional new-included)
With no argument,  NIL means a case is not used in calculating estimates, and non-nil means it is used.  NEW-INCLUDED is a sequence of length of y of nil and t to select cases.  Estimates are recomputed."
  (when (and new-included 
	     (= (length new-included) (send self :num-cases)))
    (setf (slot-value 'included) (copy-seq new-included)) 
    (send self :needs-computing t))
  (if (slot-value 'included)
      (slot-value 'included)
    (repeat t (send self :num-cases))))



(defmeth regression-model-proto :leverages ()
	 "Message args: ()
Returns the diagonal elements of the hat matrix."
	 (let* ((weights (send self :weights))
		(x (send self :x-matrix))
		(raw-levs 
		 (matmult (* (matmult x (send self :xtxinv)) x)
			  (repeat 1 (send self :num-coefs)))))
	   (if weights (* weights raw-levs) raw-levs)))


(defmeth regression-model-proto :raw-residuals () 
	 "Message args: ()
Returns the raw residuals for a model."
	 (- (send self :y) (send self :fit-values)))

(defmeth regression-model-proto :residuals () 
	 "Message args: ()
Returns the raw residuals for a model without weights. If the model
includes weights the raw residuals times the square roots of the weights
are returned."
	 (let ((raw-residuals (send self :raw-residuals))
	       (weights (send self :weights)))
	   (if weights (* (sqrt weights) raw-residuals) raw-residuals)))


(defmeth regression-model-proto :sigma-hat ()
	 "Message args: ()
Returns the estimated standard deviation of the deviations about the 
regression line."
	 (let ((ss (send self :sum-of-squares))
	       (df (send self :df)))
	   (if (/= df 0) (sqrt (/ ss df)))))


(defmeth regression-model-proto :coef-estimates ()
	 "Message args: ()
Returns the OLS (ordinary least squares) estimates of the regression
coefficients. Entries beyond the intercept correspond to entries in basis."
	 (let ((n (array-dimension (send self :x) 1))
	       (indices (if (send self :intercept) 
			    (cons 0 (+ 1 (send self :basis)))
			    (+ 1 (send self :basis))))
	       (m (send self :sweep-matrix)))
	   (coerce (compound-data-seq (select m (+ 1 n) indices)) 'list)))

(defmeth regression-model-proto :xtxinv () 
	 "Message args: ()
Returns ((X^T) X)^(-1) or ((X^T) W X)^(-1)."
	 (let ((indices (if (send self :intercept) 
			    (cons 0 (1+ (send self :basis))) 
			    (1+ (send self :basis)))))
	   (select (send self :sweep-matrix) indices indices)))


(defmeth regression-model-proto :studentized-residuals ()
	 "Message args:  ()
Computes the internally studentized residuals for included cases and externally studentized residuals for excluded cases."
	 (let ((res (send self :residuals))
	       (lev (send self :leverages))
	       (sig (send self :sigma-hat))
	       (inc (send self :included)))
	   (if-else inc
		    (/ res (* sig (sqrt (pmax .00001 (- 1 lev)))))
		    (/ res (* sig (sqrt (+ 1 lev)))))))

(defmeth regression-model-proto :externally-studentized-residuals ()
	 "Message args:  ()
Computes the externally studentized residuals."
	 (let* ((res (send self :studentized-residuals))
		(df (send self :df)))
	   (if-else (send self :included)
		    (* res (sqrt (/ (- df 1) (- df (^ res 2)))))
		    res)))

(defmeth regression-model-proto :cooks-distances ()
	 "Message args: ()
Computes Cook's distances."
	 (let ((lev (send self :leverages))
	       (res (/ (^ (send self :studentized-residuals) 2)
		       (send self :num-coefs))))
	   (if-else (send self :included) (* res (/ lev (- 1 lev) )) (* res lev))))



(defmeth regression-model-proto :plot-bayes-residuals 
  (&optional x-values)
  "Message args: (&optional x-values)
Opens a window with a plot of the standardized residuals and two standard
error bars for the posterior distribution of the actual deviations from the
line. See Chaloner and Brant. If X-VALUES are not supplied  the fitted values
are used. The plot can be linked to other plots with the link-views function.
Returns a plot object."
  (let* ((r (/ (send self :residuals) (send self :sigma-hat)))
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


;;;;; More mischief from a different time


;; regression-model is the old API, but regression as a generic will
;; be the new API.  We need to distinguish between APIs which enable
;; the user to do clear activities, and APIs which enable developers
;; to do clear extensions and development, and underlying
;; infrastructure to keep everything straight and enabled.

;; There are conflicting theories for how to structure the
;; specification of mathematical models, along with the statistical
;; inference, along with the data which is instantiating the model.
;; 
;; i.e.:  mathematical model for the relationships between components,
;; between a component and a summarizing parameter, and between
;; parameters.
;; 
;; statistical inference describes the general approach for
;; aggregating into a decision and has impliciations for the scale up
;; from the model on a single instance to the generalization.
;;
;; The data represents the particular substantive context that is
;; driving the model/inference combination, and about which we hope to
;; generate knowledge.
;; 
;; numerical analysis selects appropriate algorithms/implementations
;; for combining the above 3.  
;; 
;; the end result is input on the decision being made (which could be
;; specific (decision analysis/testing), risk-analysis (interval
;; estimation) , most likely/appropriate selection (point estimation)
;; 

#|
(defclass model ()
  ((type structure)))

(defgeneric regression ;; assumes x/y from lisp-matrix -- start of a set of generics.
    (model dataset)
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
INCLUDED    - if supplied should be the same length as Y, with
 	      elements nil to skip a in computing estimates (but not
              in residual analysis).
Returns a regression model object. To examine the model further assign the
result to a variable and send it messages.
Example (data are in file absorbtion.lsp in the sample data directory): 
  (def m (regression-model (list iron aluminum) absorbtion))
  (send m :help) (send m :plot-residuals)"
  (let ((m (send regression-model-proto :new)))
    (format t "~%")
    (send m :doc doc)
    (send m :x x)
    (send m :y y)
    (send m :intercept intercept)
    (send m :weights weights)
    (send m :included included)
    (send m :predictor-names predictor-names)
    (send m :response-name response-name)
    (send m :case-labels case-labels)
    (if debug
	(progn
	  (format t "~%")
	  (format t "~S~%" (send m :doc))
	  (format t "X: ~S~%" (send m :x))
	  (format t "Y: ~S~%" (send m :y))))
    (if print (send m :display))
    m))
|#
