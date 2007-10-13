(in-package #:lift)

(deftestsuite integer-math () ())

(defrandom-instance an-integer nil (- (random 200) 100))

(addtest (integer-math)
  commutivity
  (with-random-cases 10 ((a an-integer) (b an-integer))
    (format t "~&~a ~a" a b)
    (ensure-same (+ a b) (+ b a) :test =)))

(deftestsuite small-positive-integer-math (integer-math)
  ())

(addtest (small-positive-integer-math)
  commutivity
  (with-random-cases 10 ((a an-integer) (b an-integer))
    (ensure-same (+ a b) (+ b a) :test =)))

(addtest (small-positive-integer-math)
  closedness
  (with-random-cases 10 ((a an-integer) (b an-integer))
    (ensure (< (+ a b) 15))))

(defrandom-instance an-integer small-positive-integer-math
  (1+ (random 10)))

(deftestsuite small-positive-integer-math (integer-math)
  ()
  (:random-instance an-integer (1+ (random 10))))

