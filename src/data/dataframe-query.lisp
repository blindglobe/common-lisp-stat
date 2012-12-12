;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-

(in-package #:cls-dataframe)
;; this implments a few convenience functions for querying a dataframe
;; the general idea is to take a lisp expression and rewrite any keyword fields to call an approporate accessor
;; so (> :id 50) ==> (> (xref df row (translate-column :id) 50)
;; if a keyword is not translateable, we leave it alone.
;; this has the side effect of introducing spelling errors later when the ex



(defun maybe-rewrite-keyword (df keyword )
  "if we find the keyword is a column name translate it to the index value, otherwise leave it alone."
  (let ((column (translate-column df keyword  t)))
    (if column
	`(xref df the-row ,column) 
	keyword)))

(defun rewrite-column-keywords (df query)
  "look for keywords, if they are matched as part of the dataframe metdata, translate to an index number."
  (typecase query
    (cons (cons (rewrite-column-keywords df (first query))
		(rewrite-column-keywords df (rest query))))
    (keyword (maybe-rewrite-keyword df query))
    (t query)))

(defun make-query-lambda ( df query)
  (compile nil `(lambda (df the-row)
		  (declare (ignorable df the-row))
		  ,(rewrite-column-keywords df query))))

(defun copy-df-rows (df rows)
  (loop for obs in rows
	collect (loop for var below (nvars df)
		      collect (xref df obs var)) into result
	finally (return result)))


(defun dfquery (df query)
  (let (( query-function  (make-query-lambda df query)))
    (loop for row below (ncases df)
	  when (funcall query-function df row)
	    collect row into query-rows
	  finally (return (if query-rows
			      (make-dataframe
			       ( listoflist:listoflist->array (copy-df-rows df query-rows))
			       :varlabels (varlabels df)
			       :vartypes (vartypes df))
			      nil )))))

(defmacro test-query-lambda ( df &rest query)
  `(make-query-lambda ,df ,@query))
  
(defparameter query '(and (> :ID 50) (string= :feed "casein")))



                           