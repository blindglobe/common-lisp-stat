;;;
;;; File: interpolation.cl
;;; Description: interpolation for the clem matrix package
;;; Author: Cyrus Harmon
;;;

(in-package :clem)


(defmacro bilinear-interpolate
    (g00 g01 g10 g11 a b)
  (ch-util::once-only (g00 g01 g10 g11 a b)
    `(+ ,g00
        (* ,a (- ,g10 ,g00))
        (* ,b (- ,g01 ,g00))
        (* ,a ,b (- (+ ,g00 ,g11)
                    (+ ,g10 ,g01))))))

(defmacro quadratic-kernel (s type)
  (let ((minus-half (coerce -0.5 `,type))
        (half (coerce 0.5 `,type))
        (minus-one-point-five  (coerce -1.5 `,type))
        (one-point-five (coerce 1.5 `,type))
        (five (coerce 5 `,type))
        (minus-two (coerce -2 `,type))
        (two (coerce 2 `,type))
        (one (coerce 1 `,type))
        (zero (coerce 0 `,type)))
    `(cond ((<= ,minus-half ,s ,half)
            (+ (* ,minus-two (* ,s ,s)) ,one))
           ((<= ,minus-one-point-five ,s ,one-point-five)
            (+ (* ,s ,s) (- (/ (* ,five (abs ,s)) ,two)) ,one-point-five))
           (t ,zero))))

(defmacro quadratic-interpolate
    (g00 g01 g02
     g10 g11 g12 
     g20 g21 g22 a b
     type)
  `(let ((a0 (quadratic-kernel (- -1, a) ,type))
         (a1 (quadratic-kernel (- ,a) ,type))
         (a2 (quadratic-kernel (- 1 ,a) ,type))
         (b0 (quadratic-kernel (- -1 ,b) ,type))
         (b1 (quadratic-kernel (- ,b) ,type))
         (b2 (quadratic-kernel (- 1 ,b) ,type)))
     (+ (* a0 (+ (* b0 ,g00)
                 (* b1 ,g01)
                 (* b2 ,g02)))
        (* a1 (+ (* b0 ,g10)
                 (* b1 ,g11)
                 (* b2 ,g12)))
        (* a2 (+ (* b0 ,g20)
                 (* b1 ,g21)
                 (* b2 ,g22))))))

