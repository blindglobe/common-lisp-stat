;;; -*- mode: lisp -*-
;;;
;;; Copyright (c) 2005--2007, by A.J. Rossini <blindglobe@gmail.com>
;;; See COPYRIGHT file for any additional restrictions (BSD license).
;;; Since 1991, ANSI was finally finished.  Modified to match ANSI
;;; Common Lisp.  


;;;; XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney
;;;; Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz
;;;; You may give out copies of this software; for conditions see the file
;;;; COPYING included with this distribution.

(defpackage :lisp-stat-regression-nonlin
 (:use :common-lisp
       :lisp-stat-object-system
       :lisp-stat-basics
       :lisp-stat-compound-data
       :lisp-stat-sequence
       :lisp-stat-matrix
       :lisp-stat-linalg
       :lisp-stat-regression-linear)
 (:shadowing-import-from :lisp-stat-object-system
			 slot-value call-method call-next-method)
 (:export nreg-model nreg-model-proto mean-function theta-hat epsilon
	  count-limit verbose))

(in-package :lisp-stat-regression-nonlin)

;;;;
;;;; Nonlinear Regression Model Prototype
;;;;

(defproto nreg-model-proto 
          '(mean-function theta-hat epsilon count-limit verbose)
          '()
          regression-model-proto)

(defun nreg-model (mean-function y theta 
                                 &key 
                                 (epsilon .0001) 
                                 (print t) 
                                 (count-limit 20)
                                 parameter-names
                                 response-name
                                 case-labels
                                 weights
                                 included 
                                 (verbose print))
"Args: (mean-function y theta &key (epsilon .0001) (count-limit 20) 
                      (print t) parameter-names response-name case-labels
                      weights included (vetbose print))
Fits nonlinear regression model with MEAN-FUNCTION and response Y using initial
parameter guess THETA. Returns model object."
  (let ((m (send nreg-model-proto :new)))
    (send m :mean-function mean-function)
    (send m :y y)
    (send m :new-initial-guess theta)
    (send m :epsilon epsilon)
    (send m :count-limit count-limit)
    (send m :parameter-names parameter-names)
    (send m :response-name response-name)
    (send m :case-labels case-labels)
    (send m :weights weights)
    (send m :included included)
    (send m :verbose verbose)
    (if print (send m :display))
    m))

(defmeth nreg-model-proto :save ()
"Message args: ()
Returns an expression that will reconstruct the regression model."
  `(nreg-model ',(send self :mean-function)
               ',(send self :y)
               ',(send self :coef-estimates)
               :epsilon ',(send self :epsilon)
               :count-limit ',(send self :count-limit)
               :predictor-names ',(send self :predictor-names)
               :response-name ',(send self :response-name)
               :case-labels ',(send self :case-labels)
               :weights ',(send self :weights)
               :included ',(send self :included)
               :verbose ',(send self :verbose)))

;;;
;;; Computing Method
;;;
              
(defmeth nreg-model-proto :compute ()
"Message args: ()
Recomputes the estimates. For internal use by other messages"
  (let* ((y (send self :y))
         (weights (send self :weights))
         (inc (if-else (send self :included) 1 0))
         (w (if weights (* inc weights) inc)))
    (setf (slot-value 'theta-hat)
          (nlreg (send self :mean-function) 
                 y 
                 (slot-value 'theta-hat) 
                 (send self :epsilon)
                 (send self :count-limit)
                 w
                 (send self :verbose)))
    (setf (slot-value 'x) 
          (funcall (make-jacobian (slot-value 'mean-function) 
                                  (length (slot-value 'theta-hat))) 
                   (slot-value 'theta-hat)))
    (setf (slot-value 'intercept) nil)
    (call-next-method)
    (let ((r (send self :residuals)))
      (setf (slot-value 'residual-sum-of-squares)
	    (sum (* inc r r))))))

;;;
;;; Slot Accessors and Mutators
;;;

(defmeth nreg-model-proto :new-initial-guess (guess)
"Message args: (guess)
Sets a new initial uess for parmeters."
  (setf (slot-value 'theta-hat) guess)
  (send self :needs-computing t))

(defmeth nreg-model-proto :theta-hat ()
"Message args: ()
Returns current parameter estimate."
  (if (send self :needs-computing) (send self :compute))
  (coerce (slot-value 'theta-hat) 'list))

(defmeth nreg-model-proto :mean-function (&optional f)
"Message args: (&optional f)
With no argument returns the mean function as supplied to m. With an 
argument F sets the mean function of m to F and recomputes the
estimates."
  (when (and f (functionp f))
        (setf (slot-value 'mean-function) f)
        (send self :needs-computing t))
  (slot-value 'mean-function))

(defmeth nreg-model-proto :epsilon (&optional eps)
"Message args: (&optional eps)
With no argument returns the tolerance as supplied to m. With an argument
EPS sets the tolerance of m to EPS and recomputes the estimates."
  (when (and eps (numberp eps))
        (setf (slot-value 'epsilon) eps)
        (send self :needs-computing t))
   (slot-value 'epsilon))

(defmeth nreg-model-proto :count-limit (&optional count)
"Message args: (&optional new-count)
With no argument returns the iteration count limit as supplied to m. With
an argument COUNT sets the limit to COUNT and recomputes the
estimates."
  (when (and count (numberp count))
        (setf (slot-value 'count-limit) count)
        (send self :needs-computing t))
  (slot-value 'count-limit))

(defmeth nreg-model-proto :parameter-names (&optional (names nil set))
"Method args: (&optional names)
Sets or returns parameter names."
  (if set (setf (slot-value 'predictor-names) names))
  (let ((p-names (slot-value 'predictor-names))
        (p (length (slot-value 'theta-hat))))
    (if (not (and p-names (= p (length p-names))))
        (setf (slot-value 'predictor-names)
              (mapcar #'(lambda (a) (format nil "Parameter ~a" a)) 
                      (iseq 0 (- p 1))))))
  (slot-value 'predictor-names))

(defmeth nreg-model-proto :verbose (&optional (val nil set))
"Method args: (&optional val)
Sets or retrieves verbose setting. If T iteration info is printed during
optimization."
  (if set (setf (slot-value 'verbose) val))
  (slot-value 'verbose))

;;;
;;; Overrides for Linear Regression Methods
;;;

(defmeth nreg-model-proto :x ()
"Message args: ()
Returns the Jacobian matrix at theta-hat."
   (call-next-method))

(defmeth nreg-model-proto :intercept (&rest args)
"Message args: ()
Always returns nil. (For compatibility with linear regression.)"
  nil)

(defmeth nreg-model-proto :fit-values () 
"Message args: ()
Returns the fitted values for the model."
  (coerce (funcall (send self :mean-function) (send self :theta-hat)) 
          'list))

(defmeth nreg-model-proto :coef-estimates (&optional guess)
"Message args: (&optional guess)
With no argument returns the current parameter estimate. With an 
argument GUESS takes it as a new initial guess and recomputes
the estimates."
  (if guess (send self :new-initial-guess guess)) 
  (send self :theta-hat))

(defmeth nreg-model-proto :predictor-names () (send self :parameter-names))

;;;;
;;;;
;;;; Linear Regression Coefficients
;;;;
;;;;

(defun regression-coefficients (x y &key (intercept T) weights)
"Args: (x y &key (intercept T) weights)
Returns the coefficients of the regression of the sequence Y on the columns of
the matrix X."
  (let* ((m (if weights
                (make-sweep-matrix x y weights)
                (make-sweep-matrix x y)))
         (n (array-dimension x 1)))
    (coerce (compound-data-seq
	     (if intercept
		 (select (car (sweep-operator m (iseq 1 n)))
			 (1+ n)
			 (iseq 0 n))
 	         (select (car (sweep-operator m (iseq 0 n)))
			 (1+ n)
			 (iseq 1 n))))
            'vector)))

;;;;
;;;;
;;;; Nonlinear Regression Functions
;;;;
;;;;
(defun nlreg1 (f j y initial-beta epsilon count-limit weights verbose)
"Args: (mean-function jacobian y initial-beta 
        epsilon count-limit weights verbose)
MEAN-FUNCTION returns the mean response vector for a given parameter vector.
JACOBIAN returns the jacobian of MEAN-FUNCTION at a given parameter vector.
Y is the observed response vector. Returns the estimated parameter vector 
obtained by a Gauss-Newton algorithm with backtracking that continues until
the COUNT-LIMIT is reached or no component of the parameter vector changes
by more than EPSILON."
  (labels ((rss (beta)                   ; residual sum of squares
                (let ((res (- y (funcall f beta)))) 
                  (sum (if weights (* res res weights) (* res res)))))
           (next-beta (beta delta rss)   ; next beta by backtracking
                      (do* ((lambda 1 (/ lambda 2))
                            (new-rss (rss (+ beta delta)) 
                                     (rss (+ beta (* lambda delta)))))
                           ((or (< new-rss rss) (< lambda .0001))
                            (if (>= lambda .0001)
                                (+ beta (* lambda delta))
                                beta)))))
    (do* ((delbeta (regression-coefficients
                    (funcall j initial-beta)
                    (- y (funcall f initial-beta))
                    :intercept nil
                    :weights weights)
                   (regression-coefficients
                    (funcall j beta)
                    (- y (funcall f beta))
                    :intercept nil
                    :weights weights))
          (beta initial-beta (next-beta beta delbeta rss))
          (rss (rss beta) (rss beta))
          (count 0 (1+ count)))
         ((or (> count count-limit) (> epsilon (max (abs delbeta))))
          (if (and verbose (> count count-limit))
              (format t "Iteration limit exceeded.~%"))
          beta)
         (if verbose (format t "Residual sum of squares: ~10g~%" rss)))))

(defun make-jacobian (f n)
"Args: (f n)
F is a function of an N-vector. Returns a function that approximates the
jacobian function iof F by a symmetric difference."
  (let* ((h .0001)
         (del (* h (column-list (identity-matrix n)))))
    #'(lambda (b) 
      (let ((b+ (mapcar #'(lambda (x) (+ b x)) del))
            (b- (mapcar #'(lambda (x) (- b x)) del)))
        (apply #'bind-columns (/ (- (mapcar f b+) (mapcar f b-)) (* 2 h)))))))

(defun nlreg (f y guess &optional 
                (epsilon .0001) (count-limit 20) weights verbose)
"Args: (mean-function y guess &optional 
        (epsilon .0001) (count-limit 20) weights verbose)
MEAN-FUNCTION returns the mean response vector for a given parameter vector.
Y is the observed response vector. Returns the estimated parameter vector 
obtained by a Gauss-Newton algorithm that continues until the ITERATION-LIMIT
is reached or no component of the parameter vector changes by more than
EPSILON. The jacobian of MEAN-FUNCTION is approximated by a symmetric difference."
  (nlreg1 f (make-jacobian f (length guess)) y guess 
          epsilon count-limit weights verbose))
