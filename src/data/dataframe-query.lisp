;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-

(in-package #:cls-dataframe)
;; this implments a few convenience functions for querying a dataframe
;; the general idea is to take a lisp expression and rewrite any keyword fields to call an approporate accessor
;; so (> :id 50) ==> (> (xref df row (translate-column :id) 50)
;; if a keyword is not translateable, we leave it alone.
;; this has the side effect of 



(defun maybe-rewrite-keyword (df keyword )
  "if we find the keyword is a column name translate it to the index value, otherwise leave it alone."
  (let ((column (translate-column df keyword  t)))
    (if column
	`(xref df the-row ,column) 
	keyword)))

(defun rewrite-column-keywords (df query)
  (typecase query
    (cons (cons (rewrite-column-keywords df (first query))
		(rewrite-column-keywords df (rest query))))
    (keyword (maybe-rewrite-keyword df query))
    (t query)))

(defun make-query-lambda ( df query)
  (compile nil   `(lambda (df the-row)
		    (declare (ignorable df the-row))
		    ,(rewrite-column-keywords df query))))

(defun dfquery (df query)
  (let (( query-function  (make-query-lambda df query)))
    (loop for row below (ncases df)
	  when (funcall query-function df row)
	    collect row)))

(defmacro test-query-lambda ( df &rest query)
  `(make-query-lambda ,df ,@query))
  
(defparameter query '(and (> :ID 50) (string= :feed "casein")))


(defmacro fn (expr) `#', (rbuild expr))

 (defun rbuild (expr)
       (if (or (atom expr) (eq (car expr) 'lambda))
            expr
            (if (eq (car expr) 'compose)
                 (build-compose (cdr expr))
                 (build-call (car expr) (cdr expr)))))

 (defun build-call (op fns)
       (let ((g (gensym)))
         `(lambda (,g)
             (,op ,@(mapcar #'(lambda (f)
                                      `(, (rbuild f) ,g))
                                 fns)))))

 (defun build-compose (fns)
       (let ((g (gensym)))
         `(lambda (,g)
             , (labels ((rec (fns)
                             (if fns
                                  `(, (rbuild (car fns))
                                     , (rec (cdr fns)))
                                  g)))
                 (rec fns)))))
