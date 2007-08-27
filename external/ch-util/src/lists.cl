;;;
;;; lists.cl -- various lisp list utilities that make my life easier
;;;
;;; Author: Cyrus Harmon <ch-lisp@bobobeach.com>
;;;

(in-package :ch-util)

;;; Miscellaneous list utilities

(defun insert-before (new old list)
  (labels ((build-list (old c &optional newlist)
	     (if c
		 (if (eq old (car c))
		     (append (reverse (cdr c)) (cons (car c) (cons new newlist)))
		     (build-list old (cdr c) (cons (car c) newlist)))
		 (cons new newlist))))
    (reverse (build-list old list))))

(defun insert-before-all (new old list)
  (labels ((build-list (old c &optional newlist)
	     (if c
		 (if (eq old (car c))
		     (build-list old (cdr c) (cons (car c) (cons new newlist)))
		     (build-list old (cdr c) (cons (car c) newlist)))
		 newlist)))
    (reverse (build-list old list))))

(defun flatten (l)
  (mapcan #'(lambda (x)
              (cond ((null x) nil)
                    ((atom x) (list x))
                    (t (flatten x))))
          l))

(defun properties (plist)
  "Returns a list of the names properties present in plist, but
not the value of the properties."
  (loop for prop in plist by #'cddr collect prop))

(defun remove-nulls (list)
  (loop for l in list append (when l (list l))))

(defun ensure-list (l)
  (cond ((listp l) l)
        ((atom l) (list l))))

