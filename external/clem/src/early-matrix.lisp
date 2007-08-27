;;; early-matrix.lisp
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

(define-condition matrix-condition () ())

(define-condition matrix-error (simple-error matrix-condition) ())

(define-condition matrix-argument-error (matrix-error)
  ()
  (:report (lambda (condition stream)
             (format stream "~?"
                     (simple-condition-format-control condition)
                     (simple-condition-format-arguments condition)))))

(deftype index-type ()
  '(integer 0 #.(1- array-dimension-limit)))

(defclass matrix ()
  ((m :accessor matrix-vals)
   (dimensions :initarg :dimensions :initform '(1) :type (or list null))
   (initial-element :accessor initial-element :initarg :initial-element :initform 0d0)
   (adjustable :accessor adjustable :initarg :adjustable :initform nil)
   (resizeable :accessor resizable :initform nil))
  (:metaclass standard-matrix-class)
  (:element-type double-float)
  (:val-format "~4,9F")
  (:minval nil)
  (:maxval nil))

(defmethod make-load-form ((matrix matrix) &optional env)
  "Creates and returns a creation form and an initialization form
that can be used to externalize matrices."
  (make-load-form-saving-slots matrix :environment env))

(defgeneric allocate-matrix-vals (object &key adjustable initial-element &allow-other-keys))

(defmethod allocate-matrix-vals ((object matrix)
                                 &key
                                 (dimensions '(1))
                                 adjustable
                                 (initial-element 0))
  (setf (slot-value object 'm)
	(make-array dimensions
		    :adjustable adjustable
		    :initial-element (coerce initial-element (element-type (class-of object)))
		    :element-type (element-type (class-of object)))))

(defmethod shared-initialize :before
    ((object matrix) slot-names &rest initargs &key dimensions rows cols adjustable initial-element)
  (declare (ignore slot-names initargs dimensions adjustable initial-element)
           (optimize (debug 3)))
  (when (and rows cols)
    (setf (slot-value object 'dimensions)
          (list rows cols))))

(defmethod shared-initialize :after
    ((object matrix) slot-names &rest initargs &key dimensions rows cols adjustable initial-element)
  (declare (ignore slot-names initargs dimensions adjustable initial-element)
           (optimize (debug 3)))
  (apply #'allocate-matrix-vals object
         (append
          (list :dimensions (slot-value object 'dimensions))
          (list :adjustable (slot-value object 'adjustable))
          (when (slot-value object 'initial-element)
            (list :initial-element (slot-value object 'initial-element))))))

