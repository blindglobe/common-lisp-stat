;;; -*- mode: lisp -*-
;;;
;;; Copyright (c) 2005--2007, by A.J. Rossini <blindglobe@gmail.com>
;;; See COPYRIGHT file for any additional restrictions (BSD license).
;;; Since 1991, ANSI was finally finished.  Modified to match ANSI
;;; Common Lisp.  

;;;;
;;;; regression.lsp XLISP-STAT regression model proto and methods
;;;; XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney
;;;; Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz
;;;; You may give out copies of this software; for conditions see the file
;;;; COPYING included with this distribution.
;;;;
;;;;
;;;; Incorporates modifications suggested by Sandy Weisberg.
;;;;

(defpackage :lisp-stat-regression-linear
  (:use :common-lisp
	:lisp-stat-object-system
	:lisp-stat-basics
	:lisp-stat-compound-data
	:lisp-stat-matrix
	:lisp-stat-sequence)
  (:shadowing-import-from :lisp-stat-object-system
			  slot-value call-method call-next-method)

  (:export regression-model regression-model-proto x y intercept sweep-matrix
	   basis weights included total-sum-of-squares residual-sum-of-squares
	   predictor-names response-name case-labels))

(in-package :lisp-stat-regression-linear)

;;; Regresion Model Prototype

(defproto regression-model-proto
          '(x y intercept sweep-matrix basis weights 
              included
              total-sum-of-squares
              residual-sum-of-squares
              predictor-names
              response-name
              case-labels)
          ()
          *object*
          "Normal Linear Regression Model")

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
PREDICTOR-NAMES, RESPONSE-NAME, CASE-LABELS
            - sequences of strings or symbols.
INCLUDED    - if supplied should be the same length as Y, with elements nil
              to skip a in computing estimates (but not in residual analysis). 
Returns a regression model object. To examine the model further assign the
result to a variable and send it messages.
Example (data are in file absorbtion.lsp in the sample data directory/folder):
  (def m (regression-model (list iron aluminum) absorbtion))
  (send m :help) (send m :plot-residuals)"
  (let ((x (cond 
	    ((matrixp x) x)
	    ((vectorp x) (list x))
	    ((and (consp x) (numberp (car x))) (list x))
	    (t x)))
        (m (send regression-model-proto :new)))
    (send m :x (if (matrixp x) x (apply #'bind-columns x)))
    (send m :y y)
    (send m :intercept intercept)
    (send m :weights weights)
    (send m :included included)
    (send m :predictor-names predictor-names)
    (send m :response-name response-name)
    (send m :case-labels case-labels)
    (if print (send m :display))
    m))

(defmeth regression-model-proto :isnew ()
  (send self :needs-computing t))

(defmeth regression-model-proto :save ()
"Message args: ()
Returns an expression that will reconstruct the regression model."
  `(regression-model ',(send self :x)
                     ',(send self :y)
                     :intercept ',(send self :intercept)
                     :weights ',(send self :weights)
                     :included ',(send self :included)
                     :predictor-names ',(send self :predictor-names)
                     :response-name ',(send self :response-name)
                     :case-labels ',(send self :case-labels)))
                       
;;; Computing and Display Methods

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
         (tol (* .0001 (mapcar #'standard-deviation (column-list x))))
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
            (if b 
                (- (reverse b) 1)
                (error "no columns could be swept"))))))

(defmeth regression-model-proto :needs-computing (&optional set)
  (if set (setf (slot-value 'sweep-matrix) nil))
  (null (slot-value 'sweep-matrix)))
  
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

(defmeth regression-model-proto :x (&optional new-x)
"Message args: (&optional new-x)
With no argument returns the x matrix as supplied to m. With an argument
NEW-X sets the x matrix to NEW-X and recomputes the estimates."
  (when (and new-x (matrixp new-x))
        (setf (slot-value 'x) new-x)
        (send self :needs-computing t))
  (slot-value 'x))

(defmeth regression-model-proto :y (&optional new-y)
"Message args: (&optional new-y)
With no argument returns the y sequence as supplied to m. With an argument
NEW-Y sets the y sequence to NEW-Y and recomputes the estimates."
  (when (and new-y (or (matrixp new-y) (sequencep new-y)))
        (setf (slot-value 'y) new-y)
        (send self :needs-computing t))
  (slot-value 'y))

(defmeth regression-model-proto :intercept (&optional (val nil set))
"Message args: (&optional new-intercept)
With no argument returns T if the model includes an intercept term, nil if
not. With an argument NEW-INTERCEPT the model is changed to include or
exclude an intercept, according to the value of NEW-INTERCEPT."
  (when set 
        (setf (slot-value 'intercept) val)
        (send self :needs-computing t))
  (slot-value 'intercept))

(defmeth regression-model-proto :weights (&optional (new-w nil set))
"Message args: (&optional new-w)
With no argument returns the weight sequence as supplied to m; NIL means
an unweighted model. NEW-W sets the weights sequence to NEW-W and
recomputes the estimates."
  (when set 
        (setf (slot-value 'weights) new-w) 
        (send self :needs-computing t))
  (slot-value 'weights))

(defmeth regression-model-proto :total-sum-of-squares ()
"Message args: ()
Returns the total sum of squares around the mean."
  (if (send self :needs-computing) (send self :compute))
  (slot-value 'total-sum-of-squares))

(defmeth regression-model-proto :residual-sum-of-squares () 
"Message args: ()
Returns the residual sum of squares for the model."
  (if (send self :needs-computing) (send self :compute))
  (slot-value 'residual-sum-of-squares))

(defmeth regression-model-proto :basis ()
"Message args: ()
Returns the indices of the variables used in fitting the model."
  (if (send self :needs-computing) (send self :compute))
  (slot-value 'basis))

(defmeth regression-model-proto :sweep-matrix ()
"Message args: ()
Returns the swept sweep matrix. For internal use"
  (if (send self :needs-computing) (send self :compute))
  (slot-value 'sweep-matrix))

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

(defmeth regression-model-proto :predictor-names (&optional (names nil set))
"Message args: (&optional (names nil set))
With no argument returns the predictor names. NAMES sets the names."
  (if set (setf (slot-value 'predictor-names) (mapcar #'string names)))
  (let ((p (array-dimension (send self :x) 1))
        (p-names (slot-value 'predictor-names)))
    (if (not (and p-names (= (length p-names) p)))
        (setf (slot-value 'predictor-names)
              (mapcar #'(lambda (a) (format nil "Variable ~a" a)) 
                      (iseq 0 (- p 1))))))
  (slot-value 'predictor-names))

(defmeth regression-model-proto :response-name (&optional (name "Y" set))
"Message args: (&optional name)
With no argument returns the response name. NAME sets the name."
  (if set (setf (slot-value 'response-name) (if name (string name) "Y")))
  (slot-value 'response-name))

(defmeth regression-model-proto :case-labels (&optional (labels nil set))
"Message args: (&optional labels)
With no argument returns the case-labels. LABELS sets the labels."
  (if set (setf (slot-value 'case-labels) 
                (if labels 
                    (mapcar #'string labels)
                    (mapcar #'(lambda (x) (format nil "~d" x)) 
                            (iseq 0 (- (send self :num-cases) 1))))))
  (slot-value 'case-labels))

;;;
;;; Other Methods
;;; None of these methods access any slots directly.
;;;

(defmeth regression-model-proto :num-cases ()
"Message args: ()
Returns the number of cases in the model."
  (length (send self :y)))

(defmeth regression-model-proto :num-included ()
"Message args: ()
Returns the number of cases used in the computations."
  (sum (if-else (send self :included) 1 0)))

(defmeth regression-model-proto :num-coefs ()
"Message args: ()
Returns the number of coefficients in the fit model (including the
intercept if the model includes one)."
  (if (send self :intercept)
      (+ 1 (length (send self :basis)))
      (length (send self :basis))))

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
        (bind-columns (repeat 1 (send self :num-cases)) m)
        m)))

(defmeth regression-model-proto :leverages ()
"Message args: ()
Returns the diagonal elements of the hat matrix."
  (let* ((weights (send self :weights))
         (x (send self :x-matrix))
         (raw-levs 
          (matmult (* (matmult x (send self :xtxinv)) x)
                   (repeat 1 (send self :num-coefs)))))
    (if weights (* weights raw-levs) raw-levs)))

(defmeth regression-model-proto :fit-values ()
"Message args: ()
Returns the fitted values for the model."
  (matmult (send self :x-matrix) (send self :coef-estimates)))

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

(defmeth regression-model-proto :coef-standard-errors ()
"Message args: ()
Returns estimated standard errors of coefficients. Entries beyond the
intercept correspond to entries in basis."
  (let ((s (send self :sigma-hat)))
    (if s (* (send self :sigma-hat) (sqrt (diagonal (send self :xtxinv)))))))

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

(defmeth regression-model-proto :plot-residuals (&optional x-values)
"Message args: (&optional x-values)
Opens a window with a plot of the residuals. If X-VALUES are not supplied 
the fitted values are used. The plot can be linked to other plots with the 
link-views function. Returns a plot object."
  (plot-points (if x-values x-values (send self :fit-values))
               (send self :residuals)
               :title "Residual Plot"
               :point-labels (send self :case-labels)))

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
         (p (plot-points x-values r :title "Bayes Residual Plot"
                         :point-labels (send self :case-labels))))
    (map 'list #'(lambda (a b c d) (send p :plotline a b c d nil))
               x-values low x-values high)
    (send p :adjust-to-data)
    p))
