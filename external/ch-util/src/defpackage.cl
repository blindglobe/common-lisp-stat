
(in-package #:cl-user)

(defpackage #:ch-util
  (:use #:cl #:asdf)
  (:export #:get-fasl-directory

	   #:insert-before
	   #:insert-before-all
	   #:closest-common-ancestor

	   #:subclassp

	   #:strcat
	   #:trim
           
           #:str-to-int
           #:int-to-str

           #:find-nth-zero
           #:generate-random-permutation
           
           #:get-current-date

           #:time-to-string
           
	   #:double-float-divide
	   #:single-float-divide
	   
	   #:defun-export
	   #:defparameter-export
	   #:defclass-export
	   #:defmethod-export

	   #:postincf
	   #:array-sum

	   #:interncase
	   #:make-intern
	   #:make-keyword
	   #:keyword-list-names
	   #:byte-buffer
	   #:read-file-to-buffer
	   #:print-buffer
	   #:make-test-run
	   #:test-run-tests
	   #:test-run-passed
	   #:run-test

           #:pathname-as-directory
	   #:pwd
	   #:ls
	   #:with-open-file-preserving-case
           #:contents-of-file
           
           #:prefix
           #:subdirectories
           #:list-directory

           #:unix-name

	   #:make-dist
	   #:unregister-system

	   #:run-program
	   #:process-output-stream

           #:make-hash-table-from-plist
           #:make-hash-table-from-alist
           
           ;; array.cl functions
	   #:copy-array

           ;; sequence.cl
           #:max-length
           #:seqmin
           #:seqmax
           
           ;; vector.cl
           #:map-vector
	   
	   #:pdf-open
	   #:html-open))

