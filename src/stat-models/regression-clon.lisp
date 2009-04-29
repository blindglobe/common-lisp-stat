;;; -*- mode: lisp -*-

;;;;
;;;; regression.lsp XLISP-STAT regression model proto and methods
;;;; XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney
;;;; Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz
;;;; You may give out copies of this software; for conditions see the file
;;;; COPYING included with this distribution.
;;;;
;;;;
;;;; Incorporates modifications suggested by Sandy Weisberg.

(asdf:oos 'asdf:load-op 'clon)

(defpackage :regression-clon
  (:use common-lisp
	clon
	lisp-matrix
	lisp-stat-data-examples)
  (:export regression-model regression-model-proto x y intercept
	   sweep-matrix basis weights included total-sum-of-squares
	   residual-sum-of-squares predictor-names response-name
	   case-labels))

(in-package :regression-clon)

;;; Support functions

(defun flatten-list (lst)
  "Flattens a list of lists into a single list.  Only useful when
we've mucked up data.  Sign of usage means poor coding!"
  (cond ((null lst) ;; endp?
	 nil)
        ((listp lst)
         (append (flatten-list (car lst)) (flatten-list (cdr lst))))
        (t
	 (list lst))))

(defun repeat (a b)
  "Args: (vals times)
Repeats VALS. If TIMES is a number and VALS is a non-null, non-array atom,
a list of length TIMES with all elements eq to VALS is returned. If VALS
is a list and TIMES is a number then VALS is appended TIMES times. If
TIMES is a list of numbers then VALS must be a list of equal length and 
the simpler version of repeat is mapped down the two lists.
Examples: (repeat 2 5)                 returns (2 2 2 2 2)
          (repeat '(1 2) 3)            returns (1 2 1 2 1 2)
but not:
	  (repeat '(4 5 6) '(1 2 3))   returns (4 5 5 6 6 6)
	  (repeat '((4) (5 6)) '(2 3)) returns (4 4 5 6 5 6 5 6)"
  (if (> b 2) 
      (flatten-list  (append (repeat (list a) (- b 1)) (list a)))
      a))
#|
  (cond ((compound-data-p b)
	 (let* ((reps (coerce (compound-data-seq (map-elements #'repeat a b))
			      'list))
		(result (first reps))
		(tail (last (first reps))))
	   (dolist (next (rest reps) result)
		   (when next
			 (setf (rest tail) next)
			 (setf tail (last next))))))
	(t (let* ((a (if (compound-data-p a) 
			 (coerce (compound-data-seq a) 'list)
		         (list a)))
		  (result nil))
	     (dotimes (i b result)
		      (let ((next (copy-list a)))
			(if result (setf (rest (last next)) result))
			(setf result next)))))))

|#


;;;; Regresion Model Prototype

(define-prototype regression-model-proto
    (:documentation "port of proto to CLON for Normal Linear Regression Model.")
  x y intercept sweep-matrix basis weights 
  included total-sum-of-squares residual-sum-of-squares
  predictor-names response-name  case-labels)

;; The doc for this function string is at the limit of XLISP's string 
;; constant size - making it longer may cause problems
(defun regression-model (x y &key 
                           (intercept T) 
                           (print T) 
                           weights
                           (included (repeat t (length y)))
                           predictor-names
                           response-name
                           case-labels)
"Args: (x y &key (intercept T) (print T) weights 
          included predictor-names response-name case-labels)
X           - list of independent variables or X matrix
Y           - dependent variable.
INTERCEPT   - T to include (default), NIL for no intercept
PRINT       - if not NIL print summary information
WEIGHTS     - if supplied should be the same length as Y; error variances are
               assumed to be inversely proportional to WEIGHTS
PREDICTOR-NAMES
RESPONSE-NAME
CASE-LABELS - sequences of strings or symbols.
INCLUDED    - if supplied should be the same length as Y, with elements nil
              to skip a in computing estimates (but not in residual analysis). 
Returns a regression model object. To examine the model further assign the
result to a variable and send it messages.
Example (data are in file absorbtion.lsp in the sample data directory/folder):
  (def m (regression-model (list iron aluminum) absorbtion))
  (send m :help)
  (send m :plot-residuals)"
  (check-type x matrix-like)
  (let ((m (clone =regression-model-proto=)))
    (setf (field-value x m) x)
    (setf (field-value y m) y)
    (send nil :intercept m intercept)
    (send nil :weights m weights)
    (send nil :included m included)
    (send nil :predictor-names m predictor-names)
    (send nil :response-name m response-name)
    (send nil :case-labels m case-labels)
    (if print (send nil :display m))
    m))

(define-method
    is-new regression-model-proto ()
    (send nil self :needs-computing t))

(define-method
    save regression-model-proto ()
    "Message args: ()
Returns an expression that will reconstruct the regression model."
    `(regression-model ',(send nil :x self)
		       ',(send nil :y self)
		       :intercept ',(send nil :intercept self)
		       :weights ',(send nil :weights self)
		       :included ',(send nil :included self)
		       :predictor-names ',(send nil :predictor-names self)
		       :response-name ',(send nil :response-name self)
		       :case-labels ',(send nil :case-labels self)))
                       
;;;
;;; Computing and Display Methods
;;;

(define-method
    compute  regression-model-proto ()
    "Message args: ().  Recomputes the estimates. For internal use by other messages"
    (let* ((included (if-else (send nil :included) 1 0))
	   (x (send nil :x self))
	   (y (send nil :y self))
	   (intercept (send nil :intercept self))
	   (weights (send nil :weights self))
	   (w (if weights (* included weights) included))
	   ;;  (m (make-sweep-matrix x y w))
	   (n (array-dimension x 1))
	   (p (- (array-dimension m 0) 1))
	   (tss (aref m p p))
	   (tol (* .0001 (mapcar #'standard-deviation (column-list x))))
	   (sweep-result
	    (if intercept
		(sweep-operator m (iseq 1 n) tol)
		(sweep-operator m (iseq 0 n) (cons 0.0 tol)))))
      (setf (field-value 'sweep-matrix self) (first sweep-result))
      (setf (field-value 'total-sum-of-squares self) tss)
      (setf (field-value 'residual-sum-of-squares self) 
	    (aref (first sweep-result) p p))
      (setf (field-value 'basis self)
	    (let ((b (remove 0 (second sweep-result))))
	      (if b 
		  (- (reverse b) 1)
		  (error "no columns could be swept"))))))

(define-method
    needs-computing regression-model-proto (&optional set)
    "Toggle switch as needed to figure out if we recompute."
    (if set (setf (field-value 'sweep-matrix self) nil))
	       (null (field-value 'sweep-matrix self)))
  
(define-method
    display  regression-model-proto  ()
    "Message args: ()
    Prints the least squares regression summary. Variables not used in the fit
    are marked as aliased."
    (let ((coefs (coerce (send nil  :coef-estimates self) 'list))
        (se-s (send nil  :coef-standard-errors self))
        (x (send nil  :x self))
        (p-names (send nil  :predictor-names self)))
    (if (send nil :weights self) 
        (format t "~%Weighted Least Squares Estimates:~2%")
        (format t "~%Least Squares Estimates:~2%"))
    (when (send nil :intercept self)
          (format t "Constant               ~10f   ~A~%"
                  (car coefs) (list (car se-s)))
          (setf coefs (cdr coefs))
          (setf se-s (cdr se-s)))
    (dotimes (i (array-dimension x 1)) 
             (cond 
               ((member i (send nil :basis self))
                (format t "~22a ~10f   ~A~%"
                        (select p-names i) (car coefs) (list (car se-s)))
                (setf coefs (cdr coefs) se-s (cdr se-s)))
               (t (format t "~22a    aliased~%" (select p-names i)))))
    (format t "~%")
    (format t "R Squared:             ~10f~%" (send nil :r-squared self))
    (format t "Sigma hat:             ~10f~%" (send nil :sigma-hat self))
    (format t "Number of cases:       ~10d~%" (send nil :num-cases self))
    (if (/= (send nil :num-cases self) (send nil :num-included  self))
        (format t "Number of cases used:  ~10d~%" (send nil :num-included self)))
    (format t "Degrees of freedom:    ~10d~%" (send nil :df self))
    (format t "~%")))

;;;
;;; Slot accessors and mutators
;;;

(define-method
    x regression-model-proto (&optional new-x)
    "Message args: (&optional new-x)
With no argument returns the x matrix as supplied to m. With an argument
NEW-X sets the x matrix to NEW-X and recomputes the estimates."
    (when (and new-x (typep new-x 'matrix-like))
      (setf (field-value 'x self) new-x)
        (send nil :needs-computing self t))
  (field-value 'x self))

(define-method
    y regression-model-proto (&optional new-y)
    "Message args: (&optional new-y)
With no argument returns the y sequence as supplied to m. With an argument
NEW-Y sets the y sequence to NEW-Y and recomputes the estimates."
    (when (and new-y (or (typep new-y 'matrix-like)
			 (typep new-y 'sequence)))
      (setf (field-value 'y self) new-y)
      (send nil :needs-computing self t))
    (field-value 'y self))

(define-method 
    intercept regression-model-proto (&optional (val nil set))
"Message args: (&optional new-intercept)
With no argument returns T if the model includes an intercept term, nil if
not. With an argument NEW-INTERCEPT the model is changed to include or
exclude an intercept, according to the value of NEW-INTERCEPT."
  (when set 
        (setf (field-value 'intercept self ) val)
        (send nil :needs-computing self t))
  (field-value 'intercept self))

(define-method
    weights regression-model-proto (&optional (new-w nil set))
"Message args: (&optional new-w)
With no argument returns the weight sequence as supplied to m; NIL means
an unweighted model. NEW-W sets the weights sequence to NEW-W and
recomputes the estimates."
  (when set 
        (setf (field-value 'weights self) new-w) 
        (send nil :needs-computing self t))
  (field-value 'weights self))

(define-method
    total-sum-of-squares regression-model-proto ()
    "Message args: ()
Returns the total sum of squares around the mean."
    (if (send nil self :needs-computing) (send nil :compute self))
    (field-value 'total-sum-of-squares self))

(define-method
    residual-sum-of-squares regression-model-proto  () 
    "Message args: ()
Returns the residual sum of squares for the model."
    (if (send nil  :needs-computing self) (send nil self :compute self))
    (field-value 'residual-sum-of-squares self))

(define-method
    basis regression-model-proto ()
    "Message args: ()
Returns the indices of the variables used in fitting the model."
    (if (send nil :needs-computing self) (send nil :compute self))
  (field-value 'basis self))

(define-method
    sweep-matrix regression-model-proto ()
    "Message args: ()
Returns the swept sweep matrix. For internal use"
    (if (send nil :needs-computing self) (send nil :compute self))
    (field-value 'sweep-matrix self))

(define-method
    included  regression-model-proto (&optional new-included)
    "Message args: (&optional new-included)
With no argument, NIL means a case is not used in calculating
estimates, and non-nil means it is used.  NEW-INCLUDED is a sequence
of length of y of nil and t to select cases.  Estimates are
recomputed."
    (when (and new-included 
	       (= (length new-included) (send nil :num-cases self)))
      (setf (field-value 'included self) (copy-seq new-included)) 
      (send nil :needs-computing self t))
    (if (field-value 'included self)
	(field-value 'included self)
	(repeat t (send nil :num-cases self))))

(define-method
    predictor-names regression-model-proto (&optional (names nil set))
    "Message args: (&optional (names nil set))
With no argument returns the predictor names. NAMES sets the names."
    (if set (setf (field-value 'predictor-names self) (mapcar #'string names)))
    (let ((p (array-dimension (send nil :x self) 1))
        (p-names (field-value 'predictor-names self)))
    (if (not (and p-names (= (length p-names) p)))
        (setf (field-value 'predictor-names self)
              (mapcar #'(lambda (a) (format nil "Variable ~a" a)) 
                      (iseq 0 (- p 1))))))
  (field-value 'predictor-names self))

(define-method
    response-name regression-model-proto (&optional (name "Y" set))
    "Message args: (&optional name)
With no argument returns the response name. NAME sets the name."
    (if set (setf (field-value 'response-name self) (if name (string name) "Y")))
    (field-value 'response-name self))

(define-method
    case-labels regression-model-proto (&optional (labels nil set))
    "Message args: (&optional labels)
With no argument returns the case-labels. LABELS sets the labels."
    (if set (setf (field-value 'case-labels self) 
		  (if labels 
		      (mapcar #'string labels)
		      (mapcar #'(lambda (x) (format nil "~d" x)) 
			      (iseq 0 (- (send nil :num-cases self) 1))))))
    (field-value 'case-labels self))

;;;
;;; Other Methods
;;; None of these methods access any slots directly.
;;;

(define-method
    num-cases regression-model-proto ()
    "Message args: ()
Returns the number of cases in the model."
    (length (send nil :y self)))

(define-method
    num-included regression-model-proto ()
    "Message args: ()
Returns the number of cases used in the computations."
    (sum (if-else (send nil :included self) 1 0)))

(define-method
    num-coefs regression-model-proto ()
    "Message args: ()
Returns the number of coefficients in the fit model (including the
intercept if the model includes one)."
    (if (send nil :intercept self)
	(+ 1 (length (send nil :basis self)))
	(length (send nil :basis self))))

(define-method
    df regression-model-proto ()
    "Message args: ()
Returns the number of degrees of freedom in the model."
    (- (send nil :num-included self) (send nil :num-coefs self)))
  
(define-method
    x-matrix regression-model-proto ()
    "Message args: ()
Returns the X matrix for the model, including a column of 1's, if
appropriate. Columns of X matrix correspond to entries in basis."
    (field-value :x self))

(define-method
    leverages  regression-model-proto ()
    "Message args: ()
Returns the diagonal elements of the hat matrix."
    (let* ((weights (send nil :weights self))
	   (x (send nil  :x-matrix self))
	   (raw-levs (m* (* (m* x (send nil :xtxinv self)) x)
			 (repeat 1 (send nil :num-coefs self)))))
      (if weights (* weights raw-levs) raw-levs)))

(define-method
    fit-values regression-model-proto ()
    "Message args: ()
Returns the fitted values for the model."
    (m* (send nil :x-matrix self) (send nil :coef-estimates self)))

(define-method
    raw-residuals regression-model-proto () 
    "Message args: ()
Returns the raw residuals for a model."
    (- (send nil :y self) (send nil :fit-values self)))

(define-method
    residuals regression-model-proto ()
    "Message args: ()
Returns the raw residuals for a model without weights. If the model
includes weights the raw residuals times the square roots of the
weights are returned."
    (let ((raw-residuals (send nil :raw-residuals self))
	  (weights (send nil :weights self)))
      (if weights
	  (* (sqrt weights) raw-residuals)
	  raw-residuals)))

(define-method
    sum-of-squares  regression-model-proto () 
    "Message args: ()
Returns the error sum of squares for the model."
    (send nil :residual-sum-of-squares self))

(define-method
    sigma-hat regression-model-proto ()
    "Message args: ()
Returns the estimated standard deviation of the deviations about the
regression line."
    (let ((ss (send nil :sum-of-squares self))
	  (df (send nil :df self)))
      (if (/= df 0) (sqrt (/ ss df)))))

;; for models without an intercept the 'usual' formula for R^2 can
;; give negative results; hence the max.
(define-method
    r-squared  regression-model-proto ()
    "Message args: ()
Returns the sample squared multiple correlation coefficient, R
squared, for the regression."
  (max (- 1 (/ (send nil :sum-of-squares self)
	       (send nil :total-sum-of-squares self)))
       0))

(define-method
    coef-estimates regression-model-proto ()
    "Message args: ()
Returns the OLS (ordinary least squares) estimates of the regression
coefficients. Entries beyond the intercept correspond to entries in
basis."
    (let ((n (array-dimension (send nil :x self) 1))
	  (indices (if (send nil :intercept self)
		       (cons 0 (+ 1 (send nil :basis self)))
		       (+ 1 (send nil :basis  self))))
	  (m (send nil :sweep-matrix self)))
      (coerce (compound-data-seq (select m (+ 1 n) indices)) 'list)))

(define-method
    xtxinv regression-model-proto () 
    "Message args: ()
Returns ((X^T) X)^(-1) or ((X^T) W X)^(-1)."
  (let ((indices (if (send nil :intercept self)
                     (cons 0 (1+ (send nil :basis self))) 
                     (1+ (send nil :basis self)))))
    (select (send nil :sweep-matrix  self) indices indices)))

(define-method
    coef-standard-errors regression-model-proto ()
    "Message args: ()
Returns estimated standard errors of coefficients. Entries beyond the
intercept correspond to entries in basis."
    (let ((s (send nil :sigma-hat self)))
      (if s (* (send nil :sigma-hat self)
	       (sqrt (diagonal (send nil :xtxinv self)))))))

(define-method
    studentized-residuals regression-model-proto ()
    "Message args:  ()

Computes the internally studentized residuals for included cases and
externally studentized residuals for excluded cases."
    (let ((res (send nil :residuals self))
	  (lev (send nil :leverages self))
	  (sig (send nil :sigma-hat self))
	  (inc (send nil :included self)))
      (if-else inc
	       (/ res (* sig (sqrt (pmax .00001 (- 1 lev)))))
	       (/ res (* sig (sqrt (+ 1 lev)))))))

(define-method
    externally-studentized-residuals regression-model-proto ()
    "Message args:  ()
Computes the externally studentized residuals."
    (let* ((res (send nil  :studentized-residuals self))
	   (df  (send nil :df self)))
      (if-else (send nil :included self)
	       (* res (sqrt (/ (- df 1) (- df (^ res 2)))))
	       res)))

(define-method
    cooks-distances regression-model-proto ()
    "Message args: ()
Computes Cook's distances."
    (let ((lev (send nil :leverages self))
	  (res (/ (^ (send nil :studentized-residuals self) 2)
		  (send nil :num-coefs self))))
    (if-else (send nil :included self)
	     (* res (/ lev (- 1 lev)))
	     (* res lev))))

#|
 (define-method
    plot-residuals regression-model-proto (&optional x-values)
    "Message args: (&optional x-values)

Opens a window with a plot of the residuals. If X-VALUES are not
supplied the fitted values are used. The plot can be linked to other
plots with the link-views function. Returns a plot object."
    (plot-points (if x-values
		     x-values
		     (send nil :fit-values self))
		 (send nil :residuals self)
		 :title "Residual Plot"
		 :point-labels (send nil :case-labels self)))

 (define-method
    plot-bayes-residuals regression-model-proto (&optional x-values)
    "Message args: (&optional x-values)

Opens a window with a plot of the standardized residuals and two
standard error bars for the posterior distribution of the actual
deviations from the line. See Chaloner and Brant. If X-VALUES are not
supplied the fitted values are used. The plot can be linked to other
plots with the link-views function.  Returns a plot object."
    (let* ((r (/ (send nil :residuals self)
		 (send nil :sigma-hat self)))
	   (d (* 2 (sqrt (send nil :leverages self))))
	   (low  (- r d))
	   (high (+ r d))
	   (x-values (if x-values
			 x-values
			 (send nil :fit-values self)))
	   (p (plot-points x-values r
			   :title "Bayes Residual Plot"
			   :point-labels (send nil :case-labels self))))
      (map 'list
	   #'(lambda (a b c d)
	       (send nil :plotline p a b c d nil))
	   x-values low x-values high)
      (send nil :adjust-to-data p)
      p))
|#

;;; examples


(defparameter *lm-ex-obj*
  (regression-model absorbtion iron))


