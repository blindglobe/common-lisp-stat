(defpackage #:neldermead
  (:use :cl)
  (:export :nm-optimize :grnm-optimize :initial-simplex
	   :rosenbrock :standard-quadratic :pp-volume-test
	   :burmen-et-al-convergence-test))