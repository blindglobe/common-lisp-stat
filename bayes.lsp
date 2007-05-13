;;; -*- mode: lisp -*-
;;; Copyright (c) 2005--2007, by A.J. Rossini <blindglobe@gmail.com>
;;; See COPYRIGHT file for any additional restrictions (BSD license).
;;; Since 1991, ANSI was finally finished.  Edited for ANSI Common Lisp. 
;;; File moved from XLISP-STAT to CommonLispStat by Luke, note the following: 

;;;; XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney
;;;; Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz
;;;; You may give out copies of this software; for conditions see the file
;;;; COPYING included with this distribution.


(defpackage :lisp-stat-bayes
  (:use :common-lisp
	:lisp-stat-object-system
	:lisp-stat-basics)
  (:shadowing-import-from :lisp-stat-object-system
			  slot-value call-method call-next-method)
  ;;(:export .... )
  )


(in-package :lisp-stat-bayes)

;;; Objects Representing Functions

;; Generic C2 Functions

(defproto c2-function-proto '(f h num-derivs))

(defmeth c2-function-proto :isnew (f &optional (h .001) (num-derivs 0))
  (setf (slot-value 'f) f)
  (setf (slot-value 'h) (if (numberp h) (list h h) h))
  (setf (slot-value 'num-derivs) num-derivs))

(defmeth c2-function-proto :f (&optional f)
  (if f (setf (slot-value 'f) f))
  (slot-value 'f))

(defmeth c2-function-proto :grad-h () (first (slot-value 'h)))
(defmeth c2-function-proto :hess-h () (second (slot-value 'h)))
(defmeth c2-function-proto :num-derivs () (slot-value 'num-derivs))

(defmeth c2-function-proto :value (x)
  (let ((f (send self :f)))
    (if (objectp f) 
        (send f :value x) 
        (let ((v (funcall f x)))
          (if (consp v) (first v) v)))))

(defmeth c2-function-proto :gradient (x &optional (h (send self :grad-h)))
  (let ((f (send self :f)))
    (if (objectp f) (send f :gradient x h) (numgrad f x nil h))))

(defmeth c2-function-proto :hessian (x &optional (h (send self :hess-h)))
  (let ((f (send self :f)))
    (if (objectp f) (send f :hessian x h) (numhess f x nil h))))

(defmeth c2-function-proto :vals (x &optional (h (send self :hess-h)))
  (let ((f (send self :f)))
    (if (objectp f)
        (send f :vals x h)
        (let ((v (funcall f x)))
          (if (consp v)
              (if (= (length v) 3)
                  v  
                  (list (first v) (second v) (send self :hessian x h)))
              (list v (send self :gradient x h) (send self :hessian x h)))))))

(defmeth c2-function-proto :vals (x &optional (h (send self :hess-h)))
  (let ((f (send self :f)))
    (if (objectp f) (send f :vals x h) (numhess f x nil h t))))


;; Scaled C2 Functions

(defproto scaled-c2-function-proto '(scaling) () c2-function-proto)

;;**** allow function objects?
(defmeth scaled-c2-function-proto :isnew (f &optional 
                                            theta
                                            sigma
                                            (center 0)
                                            (scale 1)
                                            (h 0.001))
  (let* ((value (funcall f theta))
         (num-derivs (if (consp value) (- (length value) 1) -1))
         (sigma-t (if (< 0 num-derivs) (transpose sigma))))
    (labels ((scale (v)
               (if v
                   (case num-derivs
                     (-1 (/ (- v center) scale))
                     (0 (/ (- (first v) center) scale))
                     (1 (list (/ (- (first v) center) scale)
                              (matmult sigma-t (/ (second v) scale))))
                     (2 (list (/ (- (first v) center) scale)
                              (matmult sigma-t (/ (second v) scale))
                              (matmult sigma-t (/ (third v) scale) sigma))))))
             (sf (x) (scale (funcall f (ax+y sigma x theta t)))))
      (call-next-method #'sf h num-derivs))))

;; Tilted C2 Functions
;; **** allow nil values?
(defproto tilt-function-proto '(tilt exptilt) () c2-function-proto)

(defmeth tilt-function-proto :isnew (&optional f (tilt .1) (h .001))
  (call-next-method f h)
  (setf (slot-value 'exptilt) t)
  (setf (slot-value 'tilt) tilt))

(defmeth tilt-function-proto :tilt (&optional tilt)
  (if tilt (setf (slot-value 'tilt) tilt))
  (slot-value 'tilt))

(defmeth tilt-function-proto :exptilt (&optional (new nil set))
  (if set (setf (slot-value 'exptilt) new))
  (slot-value 'exptilt))

(defmeth tilt-function-proto :value (x)
  (let ((f (send self :f))
        (tilt (send self :tilt))
        (exptilt (send self :exptilt)))
    (flet ((value (f)
             (let ((v (send f :value x)))
               (if exptilt v (log v)))))
      (* tilt (if (consp f) (apply #'+ (mapcar #'value f)) (value f))))))

(defmeth tilt-function-proto :gradient (x &optional (h (send self :grad-h)))
  (let ((f (send self :f))
        (tilt (send self :tilt))
        (exptilt (send self :exptilt)))
    (flet ((gradient (f)
             (if exptilt
                 (send f :gradient x h)
                 (let ((v (send f :value x))
                       (grad (send f :gradient x h)))
                   (/ grad v)))))
      (* tilt 
         (if (consp f) (apply #'+ (mapcar #'gradient f)) (gradient f))))))

(defmeth tilt-function-proto :hessian (x &optional (h (send self :hess-h)))
  (let ((f (send self :f))
        (tilt (send self :tilt))
        (exptilt (send self :exptilt)))
    (flet ((hessian (f)
             (let* ((vals (send f :vals x h))
                    (v (first vals))
                    (grad (if exptilt (second vals) (/ (second vals) v)))
                    (hess (if exptilt (third vals) (/ (third vals) v))))
               (if exptilt hess (- hess (outer-product grad grad))))))
      (* tilt (if (consp f) (apply #'+ (mapcar #'hessian f)) (hessian f))))))

(defmeth tilt-function-proto :vals (x &optional (h (send self :hess-h)))
  (let ((f (send self :f))
        (tilt (send self :tilt))
        (exptilt (send self :exptilt)))
    (flet ((vals (f)
             (let ((vals (send f :vals x h)))
               (if exptilt
                   vals
                   (let* ((v (first vals))
                          (grad (/ (second vals) v))
                          (hess (- (/ (third vals) v) 
                                   (outer-product grad grad))))
                     (list (log v) grad hess))))))
      (let ((v (if (consp f) (mapcar #'vals f) (vals f))))
        (* tilt (if (consp f) (apply #'+ v) v))))))
        
;; scaled log posterior prototype

(defproto scaled-logpost-proto 
  '(tilt-object init-pars) () scaled-c2-function-proto)

(defmeth scaled-logpost-proto :isnew (f &optional 
                                        theta sigma
                                        (center 0) (scale 1) (h .001))
  (let* ((n (length theta))
         (m (repeat 0 n))
         (m-grad (repeat 0 n))
         (m-hess (- (identity-matrix n)))
         (pars (list m m-grad m-hess)))
    (call-next-method f theta sigma center scale h)
    (setf (slot-value 'init-pars) pars)
    (setf (slot-value 'tilt-object) (send tilt-function-proto :new))))

(defmeth scaled-logpost-proto :log-laplace (g &optional
                                              (count-limit 2) det-only (h .1))
  (let* ((x (send self :tilt-newton g count-limit))
         (vals (send self :vals x h))
         (gvals (if g (send g :vals x h)))
         (hess (if g (+ (third vals) (third gvals)) (third vals)))
         (det (- (sum (log (diagonal (first (chol-decomp (- hess)))))))))
    (if det-only 
        det 
        (if g (+ (first vals) (first gvals) det) (+ (first vals) det)))))

(defmeth scaled-logpost-proto :tilt-newton (tilt &optional (count-limit 2))
  (let* ((pars (slot-value 'init-pars))
         (mode (first pars))
         (mode-grad (second pars))
         (mode-hess (third pars)))
    (flet ((gradhess (x initial) 
             (let ((gh (if (and initial mode-grad mode-hess)
                                 (list mode-grad mode-hess)
                                 (rest (send self :vals x)))))
               (if tilt (+ gh (rest (send tilt :vals x))) gh)))
           (newton-step (x gh) (- x (solve (second gh) (first gh)))))
      (do* ((count 1 (+ count 1))
            (gradhess (gradhess mode t) (gradhess x nil))
            (x (newton-step mode gradhess) (newton-step x gradhess)))
           ((>= count count-limit) x)))))

(defmeth scaled-logpost-proto :tilt-laplace (g tilt &optional 
                                               (exptilt t) maxiter det-only h)
  (let ((tilt-object (slot-value 'tilt-object)))
    (send tilt-object :exptilt exptilt)
    (send tilt-object :f g)
    (send tilt-object :tilt tilt)
    (send self :log-laplace tilt-object maxiter det-only h)))

(defmeth scaled-logpost-proto :tilt-mode (g tilt &key (exptilt t) (maxiter 2))
  (let ((tilt-object (slot-value 'tilt-object)))
    (send tilt-object :exptilt exptilt)
    (send tilt-object :f g)
    (send tilt-object :tilt tilt)
    (send self :tilt-newton tilt-object maxiter)))

;;;;
;;;; Bayes Model Prototype
;;;;

(defproto bayes-model-proto '(bayes-internals))

;; initialization methods and constructor function

(defmeth bayes-model-proto :isnew (logpost mode &key
                                           scale 
                                           (derivstep .001)
                                           (verbose t)
                                           (maximize t)
                                           domain)
  (send self :set-bayes-internals 
        logpost mode scale derivstep nil nil t domain)
  (if maximize (send self :maximize verbose)))

(defun bayes-model (logpost mode &rest args &key (quick t) (print t))
"Args: (logpost mode &key scale derivstep (verbose t)
       (quick t) (print t)))
LOGPOST computes the logposterior density. It should return the
function, or a list of the function value and gradient, or a list of
the function value, gradient and Hessian. MODE is an initial guess for
the mode. SCALE and DERIVSTEP are used for numerical derivatives and
scaling. VERBOSE controls printing of iteration information during
optimization, PRINT controls printing of summary information. If QUICK
is T the summary is based on first order approximations."
  (let ((m (apply #'send bayes-model-proto :new logpost mode args)))
    (if print (send m :display :quick quick))
    m))

;; display method

(defmeth bayes-model-proto :display (&key (quick t))
  (let* ((moments (send self (if quick :1stmoments :moments)))
         (means (first moments))
         (stdevs (second moments))
         (p-names (send self :parameter-names)))
    (if quick
        (format t "~2%First Order Approximations to Posterior Moments:~2%")
        (format t "~2%Approximate Posterior Moments:~2%"))
    (mapcar #'(lambda (name mu sd)
                (format t "~22a  ~10g (~a)~%" name mu sd))
            p-names
            means
            stdevs)
    (format t "~%")))

(defmeth bayes-model-proto :parameter-names ()
  (let ((n (length (send self :mode))))
    (mapcar #'(lambda (x) (format nil "Parameter ~d" x)) (iseq 0 (- n 1)))))

;; implementation-dependent access methods

(defmeth bayes-model-proto :set-bayes-internals (lp m s h mval ch max dom)
  (setf (slot-value 'bayes-internals) 
        (vector lp m s h mval ch max dom)))

(defmeth bayes-model-proto :logpost (&optional new)
  (let ((internals (slot-value 'bayes-internals)))
    (when new
          (setf (select internals 0) new)
          (send self :needs-maximizing t))
    (select internals 0)))

(defmeth bayes-model-proto :domain (&optional new)
  (let ((internals (slot-value 'bayes-internals)))
    (if new (setf (select internals 7) new))
    (select internals 7)))

(defmeth bayes-model-proto :mode-values (&optional mode mval ch)
  (let ((internals (slot-value 'bayes-internals)))
    (when mode
          (setf (select internals 1) mode)
          (setf (select internals 4) mval)
          (setf (select internals 5) ch))
    (list (select internals 1)
          (select internals 4)
          (select internals 5))))

(defmeth bayes-model-proto :parameter-scale (&optional new)
  (let ((internals (slot-value 'bayes-internals)))
    (if new (setf (select internals 2) new))
    (select internals 2)))

(defmeth bayes-model-proto :parameter-dimension ()
  (length (select (slot-value 'bayes-internals) 1)))

(defmeth bayes-model-proto :derivstep ()
  (select (slot-value 'bayes-internals) 3))

(defmeth bayes-model-proto :needs-maximizing (&optional (new nil set))
  (let ((internals (slot-value 'bayes-internals)))
    (if set (setf (select internals 6) new))
    (select internals 6)))

;; Transformation-Related Methods
;; (These should be the only ones needing to be changed to handle 
;; an internal parameter transformation; perhaps also :logpost)

;; **** fix to be more careful about use of functionp
(defun function-list (g &optional n)
  (cond
    ((or (functionp g) (objectp g)) (list g))
    ((integerp g) 
     (if (null n)
         (list #'(lambda (x) (elt x g)))
         (let ((grad (make-array n :initial-element 0))
               (hess (make-array (list n n) :initial-element 0)))
           (setf (aref grad g) 1)
           (list #'(lambda (x) (list (elt x g) grad hess))))))
    (t (mapcar #'(lambda (x) (car (function-list x n))) g))))

(defmeth bayes-model-proto :mode ()
  (if (send self :needs-maximizing) (send self :maximize))
  (first (send self :mode-values)))

(defmeth bayes-model-proto :new-mode-guess (new)
  (send self :needs-maximizing t)
  (send self :mode-values new))

(defmeth bayes-model-proto :transformed-logpost ()
  (if (send self :needs-maximizing) (send self :maximize))
  (let* ((m-values (send self :mode-values))
         (mode (first m-values))
         (mval (second m-values))
         (ch (third m-values))
         (h (send self :derivstep))
         (f (send self :logpost)))
    (send scaled-logpost-proto :new f mode ch mval 1 h)))

;;**** need transformed domain here

(defmeth bayes-model-proto :transformed-functions (&optional g (c 0) (s 1))
  (if (send self :needs-maximizing) (send self :maximize))
  (let* ((m-values (send self :mode-values))
         (mode (first m-values))
         (mval (second m-values))
         (ch (third m-values))
         (h (send self :derivstep))
         (n (length mode))
         (g (function-list (if g g (iseq n)) n))
         (c (if (numberp c) (repeat c (length g)) c))
         (s (if (numberp s) (repeat s (length g)) s)))
    (mapcar #'(lambda (g c s) 
                (send scaled-c2-function-proto :new g mode ch c s h))
            g c s)))

;; computing methods

(defmeth bayes-model-proto :maximize (&optional (verbose 0))
  (let* ((lp (send self :logpost))
         (x (first (send self :mode-values)))
         (scale (send self :parameter-scale))
         (h (send self :derivstep))
         (minfo (newtonmax lp x 
                           :scale scale
                           :derivstep h
                           :verbose verbose
                           :return-derivs t))
         (mode (first minfo))
         (mval (second minfo))
         (ch (first (chol-decomp (inverse (- (fourth minfo)))))))
    (send self :mode-values mode mval ch)
    (send self :needs-maximizing nil)
    (send self :check-derivatives verbose)))

(defmeth bayes-model-proto :check-derivatives (&optional 
                                               (verbose 0) 
                                               (epsilon .00001))
  (let* ((verbose (if (numberp verbose) (< 0 verbose) verbose))
         (n (send self :parameter-dimension))
         (tlp (send self :transformed-logpost))
         (hess (send tlp :hessian (repeat 0 n)))
         (needs-max (send self :needs-maximizing)))
    (when (> (max (abs (+ hess (identity-matrix n)))) epsilon)
          (if verbose (format t "Adjusting derivatives...~%"))
          (let* ((ch (first (chol-decomp (- (inverse hess)))))
                 (mvals (send self :mode-values))
                 (m (matmult (third mvals) ch)))
            (send self :mode-values (first mvals) (second mvals) m)
            (if (not needs-max) (send self :needs-maximizing nil))
            (if verbose
                (let* ((tlp (send self :transformed-logpost))
                       (hess (send tlp :hessian (repeat 0 n))))
                  (if (> (max (abs (+ hess (identity-matrix n)))) epsilon)
                      (format t 
                              "Derivatives may not be well-behaved.~%"))))))))

;; moments

(defmeth bayes-model-proto :1stmoments (&optional gfuns &key covar)
"Args: (&optional gfuns &key covar) 
Computes first order approximations to posterior moments. GFUNS can be
a parameter index, list of indices, a function of the parameters or a
list of such functions. Returns a the list of first order approximate
means and standard deviations if COVAR is NIL. If COVAR is T the
covaraince is appended to the end of the result as well."
  (if (send self :needs-maximizing) (send self :maximize))
  (let* ((n (send self :parameter-dimension))
         (x (repeat 0 n))
         (g (send self :transformed-functions gfuns 0 1))
         (grads (apply #'bind-columns 
                       (mapcar #'(lambda (g) (send g :gradient x)) g)))
         (mean (mapcar #'(lambda (g) (send g :value x)) g))
         (cov (matmult  (transpose grads) grads)))
    (if covar
        (list mean (sqrt (diagonal cov)) cov)
        (list mean (sqrt (diagonal cov))))))

(defmeth bayes-model-proto :mgfmoments (&optional g &key covar 
                                                  (mgfdel .1)
                                                  ((:derivstep h) .1)
                                                  (maxiter 2))
  (let* ((moms1 (send self :1stmoments g :covar covar))
         (mean1 (first moms1))
         (stdev1 (second moms1))
         (cov1 (if covar (third moms1)))
         (l-object (send self :transformed-logpost))
         (g-objects (send self :transformed-functions g mean1 stdev1))
         (ldet0 (send l-object :log-laplace nil maxiter t h)))
    (labels ((lapdet (g tilt)
               (- (send l-object :tilt-laplace g tilt t maxiter t h) ldet0))
             (moms2 (m s g)
               (let ((ldet1 (lapdet g mgfdel))
                     (ldet2 (lapdet g (- mgfdel))))
                 (list (+ m (* s (/  (- ldet1 ldet2) (* 2 mgfdel))))
                       (* s (sqrt (+ 1 (/ (+ ldet1 ldet2) (^ mgfdel 2))))))))
             (covar (g mean-sd)
               (let* ((mu (first mean-sd))
                      (sd (second mean-sd))
                      (cov (diagonal (^ sd 2)))
                      (var1 (^ stdev1 2))
                      (var (^ sd 2))
                      (rvdiff (/ (- var var1) var))
                      (tilt mgfdel)
                      (2tilt2 (* 2 (^ tilt 2)))
                      (negtilt (- tilt)))
                 (dotimes (i (length g) cov)
                   (dotimes (j i)
                     (let* ((g (select g (list i j)))
                            (rvdi (select rvdiff i))
                            (rvdj (select rvdiff j))
                            (sdi (select sd i))
                            (sdj (select sd j))
                            (ldt1 (lapdet g tilt))
                            (ldt2 (lapdet g negtilt))
                            (del2 (/ (+ ldt1 ldt2) 2tilt2))
                            (d (- del2 (* 0.5 rvdi) (* 0.5 rvdj)))
                            (c (+ (aref cov1 i j) (* d sdi sdj))))
                       (setf (aref cov i j) c)
                       (setf (aref cov j i) c)))))))
      (let ((mean-sd (transpose (mapcar #'moms2 mean1 stdev1 g-objects))))
        (if covar 
            (append mean-sd (list (covar g-objects mean-sd)))
            mean-sd)))))

(defmeth bayes-model-proto :fullmoments (&optional g &key covar
                                                   ((:derivstep h) .1)
                                                   (maxiter 2))
  (let* ((moms1 (send self :1stmoments g))
         (mean1 (first moms1))
         (stdev1 (second moms1))
         (l-object (send self :transformed-logpost))
         (g-objects (send self :transformed-functions g 0 mean1))
         (loglap0 (send l-object :log-laplace nil maxiter nil h)))
    (labels ((loglap (g tilt)
               (- (send l-object :tilt-laplace g tilt nil maxiter nil h)
                  loglap0))
             (moms2 (g mu)
               (let ((mu1 (exp (loglap g 1.0)))
                     (mu2 (exp (loglap g 2.0))))
                 (* mu (list mu1 (sqrt (- mu2 (^ mu1 2)))))))
             (covar (g mean-sd)
               (let* ((mu (/ (first mean-sd) mean1))
                      (sd (second mean-sd))
                      (cov (diagonal (^ sd 2))))
                 (dotimes (i (length g) cov)
                   (dotimes (j i)
                     (let* ((g (select g (list i j)))
                            (muij (exp (loglap g 1.0)))
                            (mui (select mu i))
                            (muj (select mu j))
                            (mu1i (select mean1 i))
                            (mu1j (select mean1 j))
                            (c (* (- muij (* mui muj)) mu1i mu1j)))
                       (setf (aref cov i j) c)
                       (setf (aref cov j i) c)))))))
      (let ((mean-sd (transpose (mapcar #'moms2 g-objects mean1))))
        (if covar 
            (append mean-sd (list (covar g-objects mean-sd)))
            mean-sd)))))

(defmeth bayes-model-proto :2ndmoments (&rest args)
  (apply #'send self :mgfmoments args))

(defmeth bayes-model-proto :moments (&rest args)
"Args: (&optional gfuns &key covar) 
Computes second order approximations to posterior moments. GFUNS can be
a parameter index, list of indices, a function of the parameters or a
list of such functions. Returns a the list of second order approximate
means and standard deviations if COVAR is NIL. If COVAR is T the
covaraince is appended to the end of the result as well."
  (apply #'send self :2ndmoments args))

;; margins

(defproto laplace-margin-proto '(logpost g x val i j a grad gval lu h))

(defmeth laplace-margin-proto :isnew (logpost g n k h)
  (setf (slot-value 'logpost) logpost)
  (setf (slot-value 'g) g)
  (setf (slot-value 'x) (repeat 0 (+ n k)))
  (setf (slot-value 'i) (iseq n))
  (setf (slot-value 'j) (+ n (iseq k)))
  (setf (slot-value 'a)
        (make-array (list (+ n k) (+ n k)) :initial-element 0))
  (setf (slot-value 'h) h)
  (send self :adjust-internals t))

(defmeth laplace-margin-proto :adjust-internals (&optional initial)
  (let* ((logpost (slot-value 'logpost))
         (g (slot-value 'g))
         (i (slot-value 'i))
         (j (slot-value 'j))
         (x (slot-value 'x))
         (a (slot-value 'a))
         (h (slot-value 'h))
         (y (select x i))
         (lambda (select x j))
         (n (length y))
         (vals (if initial 
                   (list 0 (repeat 0 n) (- (identity-matrix n)))
                   (send logpost :vals y h)))
         (val (first vals))
         (grad (second vals))
         (hess (third vals))
         (gvals (mapcar #'(lambda (x) (send x :vals y h)) g))
         (gval (mapcar #'first gvals))
         (ggrad (mapcar #'second gvals))
         (ghess (mapcar #'third gvals))
         (ggradmat (apply #' bind-columns ggrad)))
    (setf (slot-value 'val) val)
    (setf (slot-value 'grad) (apply #'+ grad (* lambda ggrad)))
    (setf (slot-value 'gval) gval)
    (setf (select a i i) (apply #'+ hess (* lambda ghess)))
    (setf (select a i j) ggradmat)
    (setf (select a j i) (transpose ggradmat))
    (setf (slot-value 'lu) (lu-decomp a))))

;; **** test for nonsingularity?
(defmeth laplace-margin-proto :move-to (target)
  (let* ((x (slot-value 'x))
         (grad (slot-value 'grad))
         (gval (slot-value 'gval))
         (lu (slot-value 'lu))
         (next-x (- x (lu-solve lu (combine grad (- gval target))))))
    (setf (slot-value 'x) next-x)
    (send self :adjust-internals)))

(defmeth laplace-margin-proto :log-density (&optional profile)
  (let ((val (slot-value 'val)))
    (if profile 
        val
        (let* ((lu (slot-value 'lu))
               (nonsing (null (fourth lu))))
          (if nonsing
              (+ (* -0.5 (sum (log (abs (diagonal (first lu))))))
                 val))))))

;; ***** fix step choice
;; ***** Cut off at first nil?
(defmeth bayes-model-proto :log-margin1 (g x &key 
                                           ((:derivstep h) .05)
                                           (spline t)
                                           profile)
  (let* ((moms1 (send self :1stmoments g))
         (mean1 (select (first moms1) 0))
         (stdev1 (select (second moms1) 0))
         (n (send self :parameter-dimension))
         (l-ob (send self :transformed-logpost))
         (g-obs (send self :transformed-functions g mean1 stdev1))
         (xs (/ (- x mean1) stdev1))
         (xlow (coerce (sort-data (select xs (which (<= xs 0)))) 'list))
         (xhigh (coerce (sort-data (select xs (which (> xs 0)))) 'list)))
    (flet ((margin (x)
             (let ((margin (send laplace-margin-proto :new l-ob g-obs n 1 h)))
               (flet ((nextmargin (x)
                        (send margin :move-to x)
                        (send margin :log-density profile)))
                 (mapcar #'nextmargin x)))))
      (let* ((ylow (reverse (margin (reverse xlow))))
             (yhigh (margin xhigh))
             (x (append xlow xhigh))
             (y (append ylow yhigh))
             (i (which (mapcar #'numberp y)))
             (xi (select x i))
             (yi (select y i))
             (xy (if spline (spline xi yi) (list xi yi))))
        (list (+ mean1 (* stdev1 (first xy)))
              (- (second xy) (log stdev1) (* 0.5 (log (* 2 pi)))))))))

(defmeth bayes-model-proto :margin1 (g x &key 
                                       (derivstep .05)
                                       (spline t)
                                       profile)
"Args: (g x &key (:derivstep .05) (spline t) profile)
Computes Laplace approximation to marginal posterior density of G at
points X. G can be an index or a function of the parameter vector. X
is a sequence that should include the modal value of G. If SPLINE is
true the log density is splined. If PROFILE is true, a profile of the
posterior is returned."
  (let* ((logmar (send self :log-margin1 g x 
                       :derivstep derivstep 
                       :spline spline
                       :profile profile)))
    (list (first logmar) (exp (second logmar)))))
    
;;**** allow domain test function
(defmeth bayes-model-proto :impsample (&optional g &key (n 100) (df 2))
  (let* ((l-ob (send self :transformed-logpost))
         (g-obs (send self :transformed-functions g))
         (k (send self :parameter-dimension))
         (v (chisq-rand n df))
         (z (* (normal-rand (repeat k n)) (sqrt (/ df v))))
         (c (- (log-gamma (/ (+ k df) 2)) 
               (log-gamma (/ df 2)) 
               (* (/ k 2) (log (/ df 2))))))
    (flet ((w (z)
              (let ((lp (send l-ob :value z))
                    (lt (* -0.5 (+ k df) (log (+ 1 (/ (sum (* z z)) df))))))
                (if (realp lp) (exp (- lp lt c)) 0)))
           (gvals (z) (mapcar #'(lambda (g) (send g :value z)) g-obs)))
      (list (mapcar #'gvals z) (mapcar #'w z)))))

(defmeth bayes-model-proto :impmoments (&optional g &key (n 100) (df 2))
  (let* ((impsample (send self :impsample g :n n :df df))
         (means (/ (reduce #'+ (* (first impsample) (second impsample))) 
                   (reduce #'+ (second impsample))))
         (x (mapcar #'(lambda (z) (^ (- z means) 2)) (first impsample)))
         (vars (/ (reduce #'+ (* x (second impsample))) 
                  (reduce #'+ (second impsample)))))
    (list means (sqrt vars))))
