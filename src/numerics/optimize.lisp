;;; -*- mode: lisp -*-
;;; Copyright (c) 2005--2007, by A.J. Rossini <blindglobe@gmail.com>
;;; See COPYRIGHT file for any additional restrictions (BSD license).
;;; Since 1991, ANSI was finally finished.  Edited for ANSI Common Lisp.

(in-package :cl-user)

(defpackage :lisp-stat-optimize
 (:use :common-lisp
       :cffi
       :lisp-stat-ffi-int
       :lisp-stat-object-system
       :lisp-stat-types
       :lisp-stat-compound-data
       :lisp-stat-math
       :lisp-stat-float
       :lisp-stat-basics
       :lisp-stat-matrix
       :lisp-stat-linalg-data
       :lisp-stat-linalg)
 (:shadowing-import-from :lisp-stat-object-system
			 slot-value call-method call-next-method)
 (:shadowing-import-from :lisp-stat-math
	   expt + - * / ** mod rem abs 1+ 1- log exp sqrt sin cos tan
 	   asin acos atan sinh cosh tanh asinh acosh atanh float random
 	   truncate floor ceiling round minusp zerop plusp evenp oddp 
 	   < <= = /= >= > complex conjugate realpart imagpart phase
 	   min max logand logior logxor lognot ffloor fceiling
 	   ftruncate fround signum cis)
 (:export
     ;; derivatives
     numgrad numhess

     ;; optimization
     newtonmax nelmeadmax))
	  
;; matrix is in statistics, but should that be a predecessor?	

;;; FIXME:AJR: There is a need to figure out the proper symbols to
;;; export.  more importantly should there be any specialty package
;;; that are exported for maximization?

(in-package :lisp-stat-optimize)

#+openmcl
(defctype size-t :unsigned-long)
#+sbcl
(defctype size-t :unsigned-int)

(defvar *maximize-callback-function* nil
  "Used in generic optimization to determine function name -- symbol or string?")

(defvar *maximize-callback-arg* nil
  "args to function to maximize")


;;;
;;; CFFI support using library for optimization work.
;;;

;; There is a problem with this particular approach, in terms of
;; circular dependencies.  We can not have this out-of-object call
;; into optimize, at least not from here.  
(cffi:defcallback ccl-maximize-callback :void ((n :int)
 					       (px :pointer)
 					       (pfval :pointer)
 					       (pgrad :pointer)
 					       (phess :pointer)
 					       (pderivs :pointer))
   (lisp-stat-optimize::maximize-callback n px pfval pgrad phess pderivs))

(cffi:defcfun ("register_maximize_callback" register-maximize-callback)
    :void (x :pointer))
(register-maximize-callback (cffi:callback ccl-maximize-callback))

(cffi:defcfun ("ccl_numgrad_front" ccl-numgrad-front)
    :void (x size-t) (y :pointer) (z :pointer) (u :double) (v :pointer))
(defun numgrad-front (x y z u v)
  (ccl-numgrad-front x y z (float u 1d0) v))

(cffi:defcfun ("ccl_numhess_front" ccl-numhess-front)
    :void (x size-t) (y :pointer) (z :pointer) (u :pointer) (v :pointer) (w :double) (a :pointer))
(defun numhess-front (x y z u v w a) 
  (ccl-numhess-front x y z u v (float w 1d0) a))

(cffi:defcfun ("ccl_minfo_maximize" ccl-minfo-maximize)
    :void (x :pointer) (y :pointer) (z :pointer) (u :pointer) (v :pointer) (w :int))
(defun base-minfo-maximize (x y z u v w) 
  (ccl-minfo-maximize x y z u v w))



;;;;
;;;; minfo basics (internal??)
;;;;

(defun init-minfo-ipar-values (n ipars &key
			       (TRUE 1)
			       (FALSE 0)
			       (k 0)
			       (m 0)
			       (itnlimit -1)
			       (backtrack TRUE)
			       (verbose 0)
			       (vals_suppl FALSE)
			       (exptilt TRUE)
			       (count 0)
			       (termcode 0))
  "Initialize ipars (iteration parameters) by destructive modification."
  (setf (aref ipars 0) n)
  (setf (aref ipars 1) m)
  (setf (aref ipars 2) k)
  (setf (aref ipars 3) itnlimit)
  (setf (aref ipars 4) backtrack)
  (setf (aref ipars 5) verbose)
  (setf (aref ipars 6) vals_suppl)
  (setf (aref ipars 7) exptilt)
  (setf (aref ipars 8) count)
  (setf (aref ipars 9) termcode))

(defun init-minfo-dpar-values (h dpars &key
			       (typf 1.0)
			       (gradtol -1.0)
			       (steptol -1.0)
			       (maxstep -1.0)
			       (dflt 0.0)
			       (tilt 0.0)
			       (newtilt 0.0)
			       (hessadd 0.0))
  "Initialize dpars (derivative parameters) by destructive modification."
  (setf (aref dpars 0) typf)
  (setf (aref dpars 1) h)
  (setf (aref dpars 2) gradtol)
  (setf (aref dpars 3) steptol)
  (setf (aref dpars 4) maxstep)
  (setf (aref dpars 5) dflt)
  (setf (aref dpars 6) tilt)
  (setf (aref dpars 7) newtilt)
  (setf (aref dpars 8) hessadd))

(defun init-minfo-internals (n h internals)
  (let ((ipars (aref internals 8))
	(dpars (aref internals 9)))
    (init-minfo-ipar-values n ipars)
    (init-minfo-dpar-values h dpars)))

(defun new-minfo-internals (f x &key scale ((:derivstep h) -1.0))
  (check-sequence x)
  (check-real x)
  (check-one-real h)
  (let ((n (length x)))
    (when scale
	  (check-sequence scale)
	  (check-real scale)
	  (if (/= n (length scale)) (error "scale and x not the same length")))
    (let ((internals (make-array 12)))
      (setf (aref internals 0) f)
      (setf (aref internals 3) (if (consp x) (copy-list x) (coerce x 'list)))
      (setf (aref internals 4)
	    (if scale (copy-seq scale) (make-array n :initial-element 1.0)))
      (setf (aref internals 5) (make-list (+ 1 n (* n n))))
      (setf (aref internals 8) (make-array 10))
      (setf (aref internals 9) (make-array 9))
      (init-minfo-internals n h internals)
      internals)))

(defun minfo-maximize (internals &optional verbose)
  "This function does what?"
  (let* ((f (aref internals 0))
	 (x (aref internals 3))
	 (fvals (aref internals 5))
	 (n (length x))
	 (v (if verbose (if (integerp verbose) verbose 1) -1)))
    (setf (aref internals 3) (copy-list x))
    (setf (aref internals 5) (copy-list fvals))
    (let ((*maximize-callback-function* f)
	  (*maximize-callback-arg* (make-list n)))
      (let* ((x (aref internals 3))
	     (scale (aref internals 4))
	     (fvals (aref internals 5))
	     (ip (aref internals 8))
	     (dp (aref internals 9))
	     (px (la-data-to-vector x +mode-re+))
	     (pscale (la-data-to-vector scale +mode-re+))
	     (pfvals (la-vector (length fvals) +mode-re+))
	     (pip (la-data-to-vector ip +mode-in+))
	     (pdp (la-data-to-vector dp +mode-re+)))
	(unwind-protect
	    (progn
	      (base-minfo-maximize px pfvals pscale pip pdp v)) ;; access to C
	  (la-vector-to-data px n +mode-re+ x)
	  (la-vector-to-data pfvals (+ 1 n (* n n)) +mode-re+ fvals)
	  (la-vector-to-data pip (length ip) +mode-in+ ip)
	  (la-vector-to-data pdp (length dp) +mode-re+ dp))
	(get-buf)))))



;;;;
;;;; Mode Info Prototype
;;;;

(defvar minfo-proto)
(defproto minfo-proto '(internals))

#+xlisp (send minfo-proto :add-method :isnew #'|minfo-isnew|)
#+xlisp (send minfo-proto :add-method :maximize #'|minfo-maximize|)
#+xlisp (send minfo-proto :add-method :loglaplace #'|minfo-loglap|)
#-xlisp
(defmeth minfo-proto :isnew (&rest args)
  (setf (slot-value 'internals) (apply #'new-minfo-internals args)))
#-xlisp
(defmeth minfo-proto :maximize (&rest args)
  (apply #'minfo-maximize (slot-value 'internals) args))

(defmeth minfo-proto :x () (aref (slot-value 'internals) 3))
(defmeth minfo-proto :scale () (aref (slot-value 'internals) 4))
(defmeth minfo-proto :derivstep () (aref (aref (slot-value 'internals) 9) 1))
(defmeth minfo-proto :tilt () (aref (aref (slot-value 'internals) 9) 6))

(defmeth minfo-proto :f (&optional (val nil set))
  (when set
	(send self :set-no-vals-supplied)
	(setf (aref (slot-value 'internals) 0) val))
  (aref (slot-value 'internals) 0))

(defmeth minfo-proto :set-no-vals-supplied ()
  (setf (aref (aref (slot-value 'internals) 8) 6) 0))

(defmeth minfo-proto :exptilt (&optional (val nil set))
  (if set
      (let ((old (send self :exptilt)))
	(setf (aref (aref (slot-value 'internals) 8) 7) (if val 1 0))
	(if (and (not (or (and old val) (and (not old) (not val))))
		 (/= (send self :tilt) 0.0))
	    (send self :set-no-vals-supplied))))
  (= 1 (aref (aref (slot-value 'internals) 8) 7)))

(defmeth minfo-proto :newtilt (&optional (val nil set))
  (when set
	(setf (aref (aref (slot-value 'internals) 9) 7) (float val))
	(if (/= (send self :tilt) 0.0) (send self :set-no-vals-supplied)))
  (aref (aref (slot-value 'internals) 9) 7))

(defmeth minfo-proto :gfuns (&optional (val nil set))
  (when set
	(if (or (not (consp val))
		(not (every #'functionp val)))
	    (error "not all functions"))
	(setf (aref (slot-value 'internals) 1) val)
	(setf (aref (aref (slot-value 'internals) 8) 1) (length val))
	(setf (aref (slot-value 'internals) 10) (repeat 1.0 (length val)))
	(if (/= (send self :tilt) 0.0) (send self :set-no-vals-supplied)))
  (aref (slot-value 'internals) 1))

(defmeth minfo-proto :cfuns (&optional (val nil set))
  (when set
	(if (or (not (consp val))
                (not (every #'functionp val)))
            (error "not all functions"))
	(setf (aref (slot-value 'internals) 2) val)
	(setf (aref (aref (slot-value 'internals) 8) 2) (length val))
	(setf (aref (slot-value 'internals) 7) (repeat 0.0 (length val)))
	(setf (aref (slot-value 'internals) 11) (repeat 0.0 (length val)))
	(send self :set-no-vals-supplied))
  (aref (slot-value 'internals) 2))

(defmeth minfo-proto :ctarget (&optional (val nil set))
  (when set
	(if (/= (length val) (length (send self :ctarget)))
	    (error "bad target length"))
	(setf (aref (slot-value 'internals) 7) val))
  (aref (slot-value 'internals) 7))
	
(defmeth minfo-proto :fvals ()
  (let* ((fv (aref (slot-value 'internals) 5))
	 (n (length (send self :x)))
	 (val (select fv 0))
	 (grad (select fv (iseq 1 n)))
	 (hess (matrix (list n n) (select fv (iseq (+ 1 n) (+ n (* n n)))))))
    (list val grad hess)))

(defmeth minfo-proto :copy ()
"Method: ()

Make a copy of an minfo instance."
  (let ((obj (make-object minfo-proto))
	(internals (copy-seq (slot-value 'internals))))
    (dotimes (i (length internals))
	     (let ((x (aref internals i)))
	       (if (typep x 'sequence)
		   (setf (aref internals i) (copy-seq x)))))
    (send obj :add-slot 'internals internals)
    obj))

(defmeth minfo-proto :derivscale ()
  (let* ((step (^ machine-epsilon (/ 1 6)))
	 (hess (numhess (send self :f) (send self :x) (send self :scale) step))
	 (scale (pmax (abs (send self :x)) (sqrt (abs (/ (diagonal hess)))))))
    (setf hess (numhess (send self :f) (send self :x) scale step))
    (setf scale (pmax (abs (send self :x)) (sqrt (abs (/ (diagonal hess))))))
    (setf (aref (slot-value 'internals) 4) scale)
    (setf (aref (aref (slot-value 'internals) 9) 1) step)))

(defmeth minfo-proto :verbose (&optional (val nil set))
  (when set
	(setf (aref (aref (slot-value 'internals) 8) 5)
	      (cond ((integerp val) val)
		    ((null val) 0)
		    (t 1))))
  (aref (aref (slot-value 'internals) 8) 5))

(defmeth minfo-proto :backtrack (&optional (val nil set))
  (if set (setf (aref (aref (slot-value 'internals) 8) 4) (if val 1 0)))
  (aref (aref (slot-value 'internals) 8) 4))

(defmeth minfo-proto :maxiter (&optional (val nil set))
    (if set (setf (aref (aref (slot-value 'internals) 8) 3) 
		  (if (integerp val) val -1)))
    (aref (aref (slot-value 'internals) 8) 3))

(defmeth minfo-proto :tiltscale (&optional (val nil set))
  (when set
	(if (/= (length val) (length (send self :gfuns)))
	    (error "wrong size tilt scale sequence"))
	(setf (aref (slot-value 'internals) 10) val))
  (aref (slot-value 'internals) 10))

;;;;
;;;;
;;;; Newton's Method with Backtracking
;;;;
;;;;

(defun newtonmax (f start &key 
                    scale 
                    (derivstep -1.0)
                    (count-limit -1)
                    (verbose 1)
                    return-derivs)
"Args:(f start &key scale derivstep (verbose 1) return-derivs)
Maximizes F starting from START using Newton's method with backtracking.
If RETURN-DERIVS is NIL returns location of maximum; otherwise returns
list of location, unction value, gradient and hessian at maximum.
SCALE should be a list of the typical magnitudes of the parameters.
DERIVSTEP is used in numerical derivatives and VERBOSE controls printing
of iteration information. COUNT-LIMIT limits the number of iterations"
  (let ((verbose (if verbose (if (integerp verbose) verbose 1) 0))
        (minfo (send minfo-proto :new f start 
                     :scale scale :derivstep derivstep)))
    (send minfo :maxiter count-limit)
    (send minfo :derivscale)
    (send minfo :maximize verbose)
    (if return-derivs
        (cons (send minfo :x) (- (send minfo :fvals)))
        (send minfo :x))))

;;;
;;; Nelder-Mead Simplex Method
;;;

;;; Simplex Prototype

(defvar simplex-proto)
(defproto simplex-proto '(f simplex))

(defun nelmeadmax (f start &key 
                     (size 1)
                     (epsilon (sqrt machine-epsilon)) 
                     (count-limit 500)
                     (verbose t)
                     (alpha 1.0) 
                     (beta 0.5) 
                     (gamma 2.0)
                     (delta 0.5))
"Args: (f start &key (size 1) (epsilon (sqrt machine-epsilon)) 
          (count-limit 500) (verbose t) alpha beta gamma delta)
Maximizes F using the Nelder-Mead simplex method. START can be a
starting simplex - a list of N+1 points, with N=dimension of problem,
or a single point. If start is a single point you should give the
size of the initial simplex as SIZE, a sequence of length N. Default is
all 1's. EPSILON is the convergence tolerance. ALPHA-DELTA can be used to
control the behavior of simplex algorithm."
    (let ((s (send simplex-proto :new f start size)))
      (do ((best (send s :best-point) (send s :best-point))
           (count 0 (+ count 1))
           next)
          ((or (< (send s :relative-range) epsilon) (>= count count-limit))
           (if (and verbose (>= count count-limit))
               (format t "Iteration limit exceeded.~%"))
           (send s :point-location (send s :best-point)))
          (setf next (send s :extrapolate-from-worst (- alpha)))
          (if (send s :is-worse best next)
              (setf next (send s :extrapolate-from-worst gamma))
              (when (send s :is-worse next (send s :second-worst-point))
                    (setf next (send s :extrapolate-from-worst beta))
                    (if (send s :is-worse next (send s :worst-point))
                        (send s :shrink-to-best delta))))
          (if verbose 
              (format t "Value = ~10g~%" 
                      (send s :point-value (send s :best-point)))))))
          


;;;
;;; Simplex Points
;;;

(defmeth simplex-proto :make-point (x)
  (let ((f (send self :f)))
    (if f 
        (let ((val (funcall f x)))
          (cons (if (consp val) (car val) val) x))
        (cons nil x))))

(defmeth simplex-proto :point-value (x) (car x))

(defmeth simplex-proto :point-location (x) (cdr x))

(defmeth simplex-proto :is-worse (x y)
  (< (send self :point-value x) (send self :point-value y)))

;;;
;;; Making New Simplices
;;;

(defmeth simplex-proto :isnew (f start &optional size)
  (send self :simplex start size)
  (send self :f f))

;;;
;;; Slot Accessors and Mutators
;;;

(defmeth simplex-proto :simplex (&optional new size)
  (if new
      (let ((simplex 
             (if (and (consp new) (sequencep (car new)))
                 (if (/= (length new) (+ 1 (length (car new))))
                     (error "bad simplex data")
                     (copy-list new))
                 (let* ((n (length new))
                        (size (if size size (repeat 1 n)))
                     ;   (pts (- (* 2 (uniform-rand (repeat n (+ n 1)))) 1)))
                        (diag (* 2 size (- (random (repeat 2 n)) .5)))
                     	(pts (cons (repeat 0 n)	
                                   (mapcar #'(lambda (x) (coerce x 'list))
                                           (column-list (diagonal diag))))))
                   (mapcar #'(lambda (x) (reduce #'+ (list (* size x) new))) pts)))))
        (setf (slot-value 'simplex) 
              (mapcar #'(lambda (x) (send self :make-point x)) simplex))
        (send self :sort-simplex)))
  (slot-value 'simplex))

(defmeth simplex-proto :f (&optional f)
  (when f
        (setf (slot-value 'f) f)
        (let ((simplex 
               (mapcar #'(lambda (x) (send self :point-location x))
                       (send self :simplex))))
          (send self :simplex simplex)))
  (slot-value 'f))

(defmeth simplex-proto :sort-simplex ()
  (if (send self :f)
      (setf (slot-value 'simplex) 
            (sort (slot-value 'simplex)
                  #'(lambda (x y) (send self :is-worse x y))))))

;;;
;;; Other Methods Using List Representation of SImplex
;;;

(defmeth simplex-proto :best-point () (car (last (send self :simplex))))
(defmeth simplex-proto :worst-point () (first (send self :simplex)))
(defmeth simplex-proto :second-worst-point () (second (send self :simplex)))
(defmeth simplex-proto :replace-point (new old)
  (let* ((simplex (send self :simplex))
         (n (position old simplex)))
    (when n 
          (setf (nth n simplex) new)
          (send self :sort-simplex))))
(defmeth simplex-proto :mean-opposite-face (x)
  (let ((face (mapcar #'(lambda (x) (send self :point-location x))
                      (remove x (send self :simplex)))))
    (/ (reduce #'+ face) (length face))))

;;;
;;; Iteration Step Methods
;;;

(defmeth simplex-proto :extrapolate-from-worst (fac)
  (let* ((worst (send self :worst-point))
         (wloc (send self :point-location worst))
         (delta (- (send self :mean-opposite-face worst) wloc))
         (new (send self :make-point (+ wloc (* (- 1 fac) delta)))))
    (if (send self :is-worse worst new) (send self :replace-point new worst))
    new))

(defmeth simplex-proto :shrink-to-best (fac)
  (let* ((best (send self :best-point))
         (bloc (send self :point-location best)))
    (dolist (x (copy-list (send self :simplex)))
            (if (not (eq x best))
                (send self :replace-point 
                      (send self :make-point 
                            (+ bloc 
                               (* fac 
                                  (- (send self :point-location x) bloc))))
                      x)))))

(defmeth simplex-proto :relative-range ()
  (let ((best (send self :point-value (send self :best-point)))
        (worst (send self :point-value (send self :worst-point))))
    (* 2 (/ (abs (- best worst)) (+ 1 (abs best) (abs worst))))))




;;;;
;;;; Maximization and Numerical Derivatives
;;;;


(defun data2double (n data ptr)
  (declare (fixnum n))
  (let* ((seq (compound-data-seq data))
	 (elem (make-next-element seq)))
    (if (/= (length seq) n) (error "bad data size"))
    (dotimes (i n) 
      (declare (fixnum i))
      (la-put-double ptr i (get-next-element elem i)))))

(defun maximize-callback (n px pfval pgrad phess pderivs)
  (la-vector-to-data px n +mode-re+ *maximize-callback-arg*)
  (let* ((val (funcall *maximize-callback-function* *maximize-callback-arg*))
	 (derivs (if (consp val) (- (length val) 1) 0)))
    (la-put-integer pderivs 0 derivs)
    (la-put-double pfval 0 (if (consp val) (first val) val))
    (if (<= 1 derivs) (data2double n (second val) pgrad))
    (if (<= 2 derivs) (data2double (* n n) (third val) phess))))

(defun numgrad (f x &optional scale (h -1.0))
"Args: (f x &optional scale derivstep)
Computes the numerical gradient of F at X."
  (check-sequence x)
  (check-real x)
  (when scale
	(check-sequence scale)
	(check-real scale))
  (check-one-real h)
  (let* ((n (length x))
	 (result (make-list n)))
    (if (and scale (/= n (length scale)))
	(error "scale not the same length as x"))
    (let ((*maximize-callback-function* f)
	  (*maximize-callback-arg* (make-list n)))
      (let ((px (la-data-to-vector x +mode-re+))
	    (pgrad (la-vector n +mode-re+))
	    (pscale (la-data-to-vector
		     (if scale scale (make-list n :initial-element 1.0))
		     +mode-re+)))
	(unwind-protect
	    (progn
	      (numgrad-front n px pgrad h pscale)
	      (la-vector-to-data pgrad n +mode-re+ result))
	  (la-free-vector px)
	  (la-free-vector pgrad)
	  (la-free-vector pscale))))
      result))

(defun numhess (f x &optional scale (h -1.0) all)
"Args: (f x &optional scale derivstep)
Computes the numerical Hessian matrix of F at X."
  (check-sequence x)
  (check-real x)
  (when scale
	(check-sequence scale)
	(check-real scale))
  (check-one-real h)
  (let* ((n (length x))
	 (result (if all 
		     (list nil (make-list n) (make-array (list n n)))
		     (make-array (list n n)))))
    (if (and scale (/= n (length scale)))
	(error "scale not the same length as x"))
    (let ((*maximize-callback-function* f)
	  (*maximize-callback-arg* (make-list n)))
      (let ((hess-data (compound-data-seq (if all (third result) result)))
	    (px (la-data-to-vector x +mode-re+))
	    (pf (la-vector 1 +mode-re+))
	    (pgrad (la-vector n +mode-re+))
	    (phess (la-vector (* n n) +mode-re+))
	    (pscale (la-data-to-vector
		     (if scale scale (make-list n :initial-element 1.0))
		     +mode-re+)))
	(unwind-protect
	    (progn
	      (numhess-front n px pf pgrad phess h pscale)
	      (when all
		    (setf (first result) (la-get-double pf 0))
		    (la-vector-to-data pgrad n +mode-re+ (second result)))
	      (la-vector-to-data phess (* n n) +mode-re+ hess-data))
	  (la-free-vector pf)
	  (la-free-vector px)
	  (la-free-vector pgrad)
	  (la-free-vector phess)
	  (la-free-vector pscale))))
    result))

