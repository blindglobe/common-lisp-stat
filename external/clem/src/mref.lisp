;;; mref.lisp
;;; macros, functions and methods for matrix element access
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mref and friends

(defmethod mref ((m matrix) &rest indices)
  (apply #'aref (matrix-vals m) indices))

(defmethod (setf mref) (v (m matrix) &rest indices)
  (setf (apply #'aref (matrix-vals m) indices) v))

(defmethod row-major-mref ((m matrix) index)
  (row-major-aref (matrix-vals m) index))

(defmethod (setf row-major-mref) (v (m matrix) index)
  (setf (row-major-aref (matrix-vals m) index) v))

;;; with-typed-mref establishes variables for the matrix-vals of the
;;; matrix and local macros that shadow mref and 
;;; thanks to nyef, kpreid, rahul and Riastradh from #lisp for
;;; suggestions on how to make this work properly.
;;;
;;; (NOW FIXED) BIG NASTY HACK ALERT!
;;;
;;; Unfortunately, a naive implementation of with-typed-mref would
;;; rely on implementation-specific behavior of defmacro. The problem
;;; is that the environment might have dynamic extent and might not be
;;; around when we call the local mref macro, so this isn't guaranteed
;;; to work. It seems that the extent of the environment allows this
;;; to work on SBCL, but other implmenatations (or future versions of
;;; SBCL for that matter) aren't guaranteed to provide this, therefore
;;; this might break. Hopefully if it breaks it will do so in a really
;;; nasty, obvious way.
;;;
;;; it would be nice if this worked:
;;;
;; (defmacro with-typed-mref ((z element-type) &body body &environment env)
;;   (let ((vals (gensym "MATRIX-")))
;;     `(let ((,vals (matrix-vals ,z)))
;;        (declare (type (simple-array ,element-type *) ,vals))
;;        (macrolet ((mref (&whole whole mat &rest args)
;;                     (if (eql ',z mat)
;;                         `(aref ,',vals ,@args)
;;                         (macroexpand whole ,env)))
;;                   (row-major-mref (&whole whole mat &rest args)
;;                     (if (eql ',z mat)
;;                         `(row-major-aref ,',vals ,@args)
;;                         (macroexpand whole ,env))))
;;          ,@body))))
;;
;;; but we may not have access to the environment when we want to
;;; expand mref, so we need to do the following. Thanks to cmm on
;;; #lisp for the revised macro.
;;;

;;; define a local symbol-macro that is initially bound to nil. This
;;; will hold an alist of matrices and their corresponding matrix-vals
(define-symbol-macro .mref-expanders. nil)

;;; 1. gensym a symbol to hold the result of (matrix-vals ,z)
;;; 2. declare the type of this array
;;; 3. (do a "shadowing") symbol-macrolet .mref-expanders. with the matrix and vals
;;;    consed on to the front of the list
;;; 4. 
(defmacro with-typed-mref ((z element-type) &body body &environment env)
  (let ((vals (gensym "MATRIX-VALS-")))
    `(let ((,vals (matrix-vals ,z)))
       (declare (type (simple-array ,element-type *) ,vals))
       (symbol-macrolet
           ((.mref-expanders. ,(acons z vals (macroexpand-1 '.mref-expanders. env))))
         (macrolet
             ((mref (mat &rest args &environment env)
                (let ((vals (cdr (assoc mat (macroexpand-1 '.mref-expanders. env)))))
                  (if vals
                      `(aref ,vals ,@args)
                      `(aref (matrix-vals ,mat) ,@args))))
              (row-major-mref (mat &rest args &environment env)
                (let ((vals (cdr (assoc mat (macroexpand-1 '.mref-expanders. env)))))
                  (if vals
                      `(row-major-aref ,vals ,@args)
                      `(row-major-aref (matrix-vals ,mat) ,@args)))))
           ,@body)))))

(defmacro matrix-total-size (matrix)
  `(array-total-size (matrix-vals ,matrix)))

(defmacro matrix-dimensions (matrix)
  `(array-dimensions (matrix-vals ,matrix)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Old approahes to getting fast performance from matrices


;;; old approach 1: use with-typed-matrix-vals whereby we bind a
;;; (user-specified) local-variable to the matrix-vals of the array
;;; and the user accesses the matrix data via aref of this symbol.
(defmacro with-typed-matrix-vals ((m element-type a) &body body)
  `(let ((,a (matrix-vals ,m)))
     (declare (type (simple-array ,element-type *) ,a))
     ,@body))

;;; as an analog to with-typed-matrix-vals, we'd like to be able to
;;; use the same code for both typed and untyped matrices, therefore
;;; we define with-untyped-matrix-vals.
(defmacro with-untyped-matrix-vals ((m element-type a) &body body)
  (declare (ignore element-type))
  `(let ((,a (matrix-vals ,m)))
     ,@body))

;;; with-matrix-vals is supposed to choose between
;;; with-typed-matrix-vals and with-untyped-matrix-vals if 1) we know
;;; the type of the array and 2) it matches the element type of the
;;; matrix class.
(defmacro with-matrix-vals ((m element-type a) &body body)
  `(if (equal ',element-type (element-type (class-of ,m)))
       (with-typed-matrix-vals (,m ,element-type ,a)
	 ,@body)
       (with-untyped-matrix-vals (,m ,element-type ,a)
	 ,@body)))
	 
;;; old approach 2: like with-typed-matrix-vals, but instead of
;;; specifying a symbol to hold the matrix-vals, we gensym that symbol
;;; and instead the users passes in the name of an accessor, which is
;;; then macrolet-ed to perform the aref of the gensym-ed symbol,
;;; which gets bound to the matrix-vals prior to exectuing the body.
;;;
(defmacro with-typed-matrix-accessor ((m element-type ref) &body body)
  (let ((vals (gensym)))
    `(let ((,vals (matrix-vals ,m)))
       (declare (type (simple-array ,element-type *) ,vals))
       (macrolet ((,ref (&rest args)
                    `(aref ,',vals ,@args)))
         ,@body))))
