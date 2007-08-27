;;; extrema.lisp
;;; macros, functions and methods for finding matrix extrema
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

;;; slow functions

(defmethod min-range ((m matrix) (startr fixnum) (endr fixnum) (startc fixnum) (endc fixnum))
  (declare (dynamic-extent startr endr startc endc)
	   (fixnum startr endr startc endc))
  (let ((retval (val m startr startc)))
    (map-range m startr endr startc endc
	       #'(lambda (v i j)
		   (declare (ignore i j))
		   (setf retval (min retval v))))
    retval))

(defmethod max-range ((m matrix) (startr fixnum) (endr fixnum) (startc fixnum) (endc fixnum))
  (let ((retval (val m startr startc)))
    (map-range m startr endr startc endc
	       #'(lambda (v i j)
		   (declare (ignore i j))
		   (setf retval (max retval v))))
    retval))

(defmethod min-val ((m matrix))
  (let ((minval (row-major-mref m 0)))
    (loop for i from 0 below (matrix-total-size m)
       do (setf minval (min minval (row-major-mref m i))))
    minval))

(defmethod max-val ((m matrix))
  (let ((maxval (row-major-mref m 0)))
    (loop for i from 0 below (matrix-total-size m)
       do (setf maxval (max maxval (row-major-mref m i))))
    maxval))

;;; fast functions

(defmacro def-matrix-min-max (type)
  (let ((element-type (element-type (find-class `,type)))
	(accumulator-element-type (element-type (find-class `,type))))
    `(progn
       (defmethod min-range ((m ,type) (startr fixnum) (endr fixnum) (startc fixnum) (endc fixnum))
         (let ((acc (coerce (aref (matrix-vals m) startr startc) ',accumulator-element-type)))
           (declare (type ,accumulator-element-type acc))
           (with-map-range m ,element-type startr endr startc endc (a i j)
             (when (< (aref a i j) acc)
               (setf acc (aref a i j))))
           acc))
       
       (defmethod max-range ((m ,type) (startr fixnum) (endr fixnum) (startc fixnum) (endc fixnum))
         (let ((acc (coerce (aref (matrix-vals m) startr startc) ',accumulator-element-type)))
           (declare (type ,accumulator-element-type acc))
           (with-map-range m ,element-type startr endr startc endc (a i j)
             (when (> (aref a i j) acc)
               (setf acc (aref a i j))))
           acc)))))

(macrolet ((frob (type)
	     `(def-matrix-min-max ,type)))
  (frob double-float-matrix)
  (frob single-float-matrix)
  (frob ub8-matrix)
  (frob ub16-matrix)
  (frob ub32-matrix)
  (frob sb8-matrix)
  (frob sb16-matrix)
  (frob sb32-matrix)
  (frob fixnum-matrix)
  (frob bit-matrix)
  (frob integer-matrix)
  (frob real-matrix))

