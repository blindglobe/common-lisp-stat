;last change 2009-03-16
;2009-02-28

;implementation of the mrg32k3a PRNG of Pierre L'Ecuyer,
;http://www.iro.umontreal.ca/~lecuyer/myftp/papers/streams00.pdf

;; From:    http://www.ccs.neu.edu/home/dorai/notes/mrg32k3a.html
;; D/L'd, 20.2.2014

#|

An mrg32k3a random-number generator for Common Lisp

mrg32k3a.lisp is a Common Lisp implementation of Pierre L’Ecuyer’s
mrg32k3a pseudorandom number generator. See that paper for why this
PRNG is great.

Scheme’s SRFI 27 by Sebastian Egner provides what look like truly
sophisticated implementations. PLT Scheme uses mrg32k3a for its native
PRNG.

mrg32k3a.lisp just encodes the four (!) lines of arithmetic given by
L’Ecuyer into simple Lisp, without thought for efficiencies, and gives
them the PRNG interface documented in the Common Lisp standard. The
package it provides is named mrg32k3a, and its exported symbols are
random, *random-state*, make-random-state, and random-state-p.

To make this PRNG a plugin-replacement for your CL’s PRNG, you could
load mrg32k3a.lisp, and then do


   (shadowing-import '(mrg32k3a:random
                       mrg32k3a:*random-state*
                       mrg32k3a:make-random-state
                       mrg32k3a:random-state-p))

|#


(defpackage :mrg32k3a
  (:use :cl)
  (:shadow :random :*random-state* :make-random-state :random-state-p)
  (:export :random :*random-state* :make-random-state :random-state-p))

(in-package :mrg32k3a)

(setq *read-default-float-format* 'long-float)

(defstruct (rstate (:predicate random-state-p))
  (x1.n-1 12345)
  (x1.n-2 23456)
  (x1.n-3 34567)
  (x2.n-1 45678)
  (x2.n-2 56789)
  (x2.n-3 67890))

(defvar *random-state* (make-rstate))

(defvar *m1* #.(- (expt 2 32) 209))

(defvar *m2* #.(- (expt 2 32) 22853))

(defun random-m1 (&optional (state *random-state*))
  (let* ((x1.n-1 (rstate-x1.n-1 state))
         (x1.n-2 (rstate-x1.n-2 state))
         (x1.n-3 (rstate-x1.n-3 state))
         (x2.n-1 (rstate-x2.n-1 state))
         (x2.n-2 (rstate-x2.n-2 state))
         (x2.n-3 (rstate-x2.n-3 state))
         ;
         (x1.n (mod (- (* 1403580 x1.n-2)
                       (* 810728 x1.n-3))
                    *m1*))
         (x2.n (mod (- (* 527612 x2.n-1)
                       (* 1370589 x2.n-3))
                    *m2*)))
    (setf (rstate-x1.n-1 state) x1.n
          (rstate-x1.n-2 state) x1.n-1
          (rstate-x1.n-3 state) x1.n-2
          (rstate-x2.n-1 state) x2.n
          (rstate-x2.n-2 state) x2.n-1
          (rstate-x2.n-3 state) x2.n-2)
    (mod (- x1.n x2.n) *m1*)))

#|
(defun random-real (r &optional (state *random-state*))
  ;returns real in [0,r)
  (* 1.0 r (/ (random-m1 state) *m1*)))

(defun random-int (i &optional (state *random-state*))
  ;returns int in [0,i)
  (let ((w (floor *m1* i))
        (r (random-m1 state)))
    (mod (floor r w) i)))

The above seem natural,
but Sebastian Egner's implementation of mrg32k3a for Scheme (SRFI 27) uses
the following extra arithmetic, and I'll go along.

Note that SRFI 27 decrees that (random-real 1) shouldn't return y = 0 or y = 1,
so that (log y) and (log (- 1 y)) don't generate an exception.
|#

(defun random-real (r &optional (state *random-state*))
  ;returns real in (0,r)
  (let ((z (random-m1 state)))
    (when (= z 0) (setq z *m1*))
    (* 1.0 r (/ z (1+ *m1*)))))

(defun random-int (i &optional (state *random-state*))
  (let* ((w (floor *m1* i))
         (w*i (* w i)))
    (loop
      (let ((r (random-m1 state)))
        (when (< r w*i) (return (values (floor r w))))))))

(defun random (n &optional (state *random-state*))
  (funcall (if (integerp n) #'random-int #'random-real)
           n state))

(defun make-random-state (&optional arg)
  (if (eql arg t)
      (let ((x (mod (get-universal-time) *m1*)))
        ;not all of x1.* should be 0; and not all of x2.* should be 0
        (make-rstate :x1.n-1 (1+ (mod (random-int x) (1- *m1*)))
                     :x1.n-2 (mod (random-int x) *m1*)
                     :x1.n-3 (mod (random-int x) *m1*)
                     :x2.n-1 (1+ (mod (random-int x) (1- *m2*)))
                     :x2.n-2 (mod (random-int x) *m2*)
                     :x2.n-3 (mod (random-int x) *m2*)))
    (copy-rstate (or arg *random-state*))))
