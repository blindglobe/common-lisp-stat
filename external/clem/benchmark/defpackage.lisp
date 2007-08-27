
(in-package #:cl-user)

(defpackage #:clem-benchmark
  (:use #:cl #:asdf #:clem)
  (:export #:list-benchmarks
	   #:clear-benchmarks))

