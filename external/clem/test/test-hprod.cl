
(in-package :clem-test)

(defparameter *hprod-test-matrix-size* 256)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun symbolicate (&rest args)
    (intern (string-upcase (apply #'concatenate 'string args)))))

(defmacro def-hprod-test (type-1 val-1 type-2 val-2)
  (let ((funcname (symbolicate "test/mat-hprod/" (symbol-name type-1)
			       "/" (symbol-name type-2)))
	(m1 (symbolicate (symbol-name type-1) "-matrix"))
	(m2 (symbolicate (symbol-name type-2) "-matrix")))
    `(defun ,funcname (&key (size *hprod-test-matrix-size*))
       (let ((m (make-instance ',m1 :cols size :rows size
			       :initial-element ,val-1))
	     (n (make-instance ',m2 :cols size :rows size
			       :initial-element ,val-2)))
	 (let ((p (time (clem:mat-hprod m n))))
	   p)))))

(defmacro def-hprod!-test (type-1 val-1 type-2 val-2)
  (let ((funcname (symbolicate "test/mat-hprod!/" (symbol-name type-1)
			       "/" (symbol-name type-2)))
	(m1 (symbolicate (symbol-name type-1) "-matrix"))
	(m2 (symbolicate (symbol-name type-2) "-matrix")))
    `(defun ,funcname (&key (size *hprod-test-matrix-size*))
       (let ((m (make-instance ',m1 :cols size :rows size
			       :initial-element ,val-1))
	     (n (make-instance ',m2 :cols size :rows size
			       :initial-element ,val-2)))
	 (let ((p (time (clem:mat-hprod! m n))))
	   p)))))

(defmacro def-hprod-tests (type-1 val-1 type-2 val-2)
  `(progn
     (def-hprod-test ,type-1 ,val-1 ,type-2 ,val-2)
     (def-hprod!-test ,type-1 ,val-1 ,type-2 ,val-2)))

(def-hprod-tests double-float 1.25d0 double-float pi)
(def-hprod-tests double-float 1.25d0 single-float 2.81818s0)
(def-hprod-tests double-float 1.25d0 ub8 12)
(def-hprod-tests double-float 1.25d0 ub16 256)
(def-hprod-tests double-float 1.25d0 ub32 #x000f0000)
(def-hprod-tests double-float 1.25d0 sb8 12)
(def-hprod-tests double-float 1.25d0 sb16 256)
(def-hprod-tests double-float 1.25d0 sb32 #x000f0000)
(def-hprod-tests double-float 1.25d0 bit 0)

(def-hprod-tests single-float 1.25s0 single-float 2.81818s0)
(def-hprod-tests single-float 1.25s0 ub8 12)
(def-hprod-tests single-float 1.25s0 ub16 256)
(def-hprod-tests single-float 1.25s0 ub32 #x000f0000)
(def-hprod-tests single-float 1.25s0 sb8 12)
(def-hprod-tests single-float 1.25s0 sb16 256)
(def-hprod-tests single-float 1.25s0 sb32 #x000f0000)
(def-hprod-tests single-float 1.25s0 bit 0)

(def-hprod-tests ub8 2 ub8 2)
(def-hprod-tests ub8 2 bit 0)

(def-hprod-tests ub16 2 ub16 2)
(def-hprod-tests ub16 2 ub8 2)
(def-hprod-tests ub16 2 bit 0)

(def-hprod-tests ub32 2 ub32 2)
(def-hprod-tests ub32 2 ub16 2)
(def-hprod-tests ub32 2 ub8 2)
(def-hprod-tests ub32 2 bit 0)

(def-hprod-tests sb8 2 sb8 2)
(def-hprod-tests sb8 2 bit 0)

(def-hprod-tests sb16 2 sb16 2)
(def-hprod-tests sb16 2 sb8 2)
(def-hprod-tests sb16 2 bit 0)

(def-hprod-tests sb32 2 sb32 2)
(def-hprod-tests sb32 2 sb16 2)
(def-hprod-tests sb32 2 sb8 2)
(def-hprod-tests sb32 2 bit 0)

(def-hprod-tests fixnum 2 fixnum 0)
(def-hprod-tests fixnum 2 bit 0)

(def-hprod-tests bit 1 bit 0)

(defun run-hprod-tests ()

  (test/mat-hprod/double-float/double-float)
  (test/mat-hprod/double-float/single-float)
  (test/mat-hprod/double-float/ub8)
  (test/mat-hprod/double-float/ub16)
  (test/mat-hprod/double-float/ub32)
  (test/mat-hprod/double-float/sb8)
  (test/mat-hprod/double-float/sb16)
  (test/mat-hprod/double-float/sb32)
  (test/mat-hprod/double-float/bit)

  (test/mat-hprod/single-float/single-float)
  (test/mat-hprod/single-float/ub8)
  (test/mat-hprod/single-float/ub16)
  (test/mat-hprod/single-float/ub32)
  (test/mat-hprod/single-float/sb8)
  (test/mat-hprod/single-float/sb16)
  (test/mat-hprod/single-float/sb32)
  (test/mat-hprod/single-float/bit)

  (test/mat-hprod/ub8/ub8)
  (test/mat-hprod/ub8/bit)

  (test/mat-hprod/ub16/ub16)
  (test/mat-hprod/ub16/ub8)
  (test/mat-hprod/ub16/bit)

  (test/mat-hprod/ub32/ub32)
  (test/mat-hprod/ub32/ub16)
  (test/mat-hprod/ub32/ub8)
  (test/mat-hprod/ub32/bit)

  (test/mat-hprod/sb8/sb8)
  (test/mat-hprod/sb8/bit)

  (test/mat-hprod/sb16/sb16)
  (test/mat-hprod/sb16/sb8)
  (test/mat-hprod/sb16/bit)

  (test/mat-hprod/sb32/sb32)
  (test/mat-hprod/sb32/sb16)
  (test/mat-hprod/sb32/sb8)
  (test/mat-hprod/sb32/bit)

  (test/mat-hprod/fixnum/fixnum)
  (test/mat-hprod/fixnum/bit)

  (test/mat-hprod/bit/bit))

(defun run-hprod!-tests ()

  (test/mat-hprod!/double-float/double-float)
  (test/mat-hprod!/double-float/single-float)
  (test/mat-hprod!/double-float/ub8)
  (test/mat-hprod!/double-float/ub16)
  (test/mat-hprod!/double-float/ub32)
  (test/mat-hprod!/double-float/sb8)
  (test/mat-hprod!/double-float/sb16)
  (test/mat-hprod!/double-float/sb32)
  (test/mat-hprod!/double-float/bit)

  (test/mat-hprod!/single-float/single-float)
  (test/mat-hprod!/single-float/ub8)
  (test/mat-hprod!/single-float/ub16)
  (test/mat-hprod!/single-float/ub32)
  (test/mat-hprod!/single-float/sb8)
  (test/mat-hprod!/single-float/sb16)
  (test/mat-hprod!/single-float/sb32)
  (test/mat-hprod!/single-float/bit)

  (test/mat-hprod!/ub8/ub8)
  (test/mat-hprod!/ub8/bit)

  (test/mat-hprod!/ub16/ub16)
  (test/mat-hprod!/ub16/ub8)
  (test/mat-hprod!/ub16/bit)

  (test/mat-hprod!/ub32/ub32)
  (test/mat-hprod!/ub32/ub16)
  (test/mat-hprod!/ub32/ub8)
  (test/mat-hprod!/ub32/bit)

  (test/mat-hprod!/sb8/sb8)
  (test/mat-hprod!/sb8/bit)

  (test/mat-hprod!/sb16/sb16)
  (test/mat-hprod!/sb16/sb8)
  (test/mat-hprod!/sb16/bit)

  (test/mat-hprod!/sb32/sb32)
  (test/mat-hprod!/sb32/sb16)
  (test/mat-hprod!/sb32/sb8)
  (test/mat-hprod!/sb32/bit)

  (test/mat-hprod!/fixnum/fixnum)
  (test/mat-hprod!/fixnum/bit)

  (test/mat-hprod!/bit/bit))
