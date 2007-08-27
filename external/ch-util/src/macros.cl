;;;
;;; macros.cl -- macro writing macros
;;;
;;; Author: Cyrus Harmon <ch-lisp@bobobeach.com>
;;;

(in-package :ch-util)

;;; this is taken from Peter Seibel's Practical Common Lisp
;;; book, p. 102
(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
       `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
          ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
                ,@body)))))

;;
;; Reference implementation of with-unique-names from cliki
;;
(defmacro with-unique-names ((&rest bindings) &body body)
  `(let ,(mapcar #'(lambda (binding)
                     (destructuring-bind (var prefix)
			 (if (consp binding) binding (list binding binding))
                       `(,var (gensym ,(string prefix)))))
                 bindings)
    ,@body))

(defmacro time-to-string (&body body)
  (let ((strstr (gensym))
        (time-string (make-array '(0) :element-type 'character
                                 :fill-pointer 0 :adjustable t)))
    `(with-output-to-string (,strstr ,time-string)
       (let ((*trace-output* ,strstr))
         (time ,@body))
       ,time-string)))

