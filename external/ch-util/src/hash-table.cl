;;;
;;; hash-table.cl -- various lisp utilities for hash-tables
;;;
;;; Author: Cyrus Harmon <ch-lisp@bobobeach.com>
;;;

(in-package :ch-util)

;;; Miscellaneous hash-table utilities

(defun make-hash-table-from-plist (plist &key (test #'eql))
  (let ((h (make-hash-table :test test)))
    (loop for (x y) on plist by #'cddr
       do (setf (gethash x h) y))
    h))

(defun make-hash-table-from-alist (alist &key (test #'eql))
  (let ((h (make-hash-table :test test)))
    (loop for (x . y) in alist
       do (setf (gethash x h) y))
    h))

(defun hash-table-to-plist (h &aux l)
  (if (hash-table-p h)
      (progn (maphash
              #'(lambda (key val)
                  (setf l (cons (hash-table-to-plist val)
                                (cons key l)))) h)
             (nreverse l))
      h))

(defun hash-ref (h &rest keys)                                                                                  
  (reduce #'(lambda (h k) (gethash k h)) keys :initial-value h))                                                

(defun %put-hash-ref (new-value h key &rest more-keys)
  ;; not quite Perl-style autovivification, but we do create
  ;; appropriate list structure for intermediate keys that can't be found
  (unless (hash-table-p h) (setf h (make-hash-table :test 'equal)))
  (let* ((sub (gethash key h))
         (val (if more-keys
                  (apply #'%put-hash-ref new-value sub more-keys)
                  new-value)))
    (progn (setf (gethash key h) val) h)))

(define-setf-expander hash-ref (place &rest props
                                  &environment env)                                                             
  ;; %put-ref may cons new structure or mutate its argument.
  ;; all this magic is just so that we can
  ;; (let ((l nil)) (setf (ref l :foo :bar) t))
  (multiple-value-bind (temps values stores set get)
      (get-setf-expansion place env)
    (let ((newval (gensym))
          (ptemps (loop for i in props collect (gensym))))
      (values `(,@temps ,@ptemps )
              `(,@values ,@props )
              `(,newval)
              `(let ((,(car stores) (%put-hash-ref ,newval ,get ,@ptemps)))
                 ,set
                 ,newval)
              `(hash-ref ,get ,@ptemps)))))
