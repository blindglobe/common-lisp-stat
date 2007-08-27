;;; arithmetic.lisp
;;;
;;; Copyright (c) 2004-2006 Cyrus Harmon (ch-lisp@bobobeach.com)
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

(in-package :clem)

;;;;
;;;; level-1 arithmetic functions

(defmethod m+ (&rest matrices)
  (reduce #'mat-add matrices))

(defmethod m- (&rest matrices)
  (if (cdr matrices)
      (reduce #'mat-subtr matrices)
      (mat-scale (car matrices) -1)))

(defmethod m* (&rest matrices)
  (reduce
   #'(lambda (x y)
       (cond ((and (typep y 'matrix)
                   (typep x 'matrix))
              (mat-mult x y))
             ((and (typep x 'matrix)
                   (numberp y))
              (mat-scale x y))
             ((and (numberp x)
                   (typep y 'matrix))
              (mat-scale y x))
             (t (error 'matrix-argument-error
                       :format-control "At least one argument (~{~S~^ ~}) must be a MATRIX."
                       :format-arguments (list matrices)))))
   matrices))

(defmethod m.* (&rest matrices)
  (reduce #'mat-hprod matrices))

