
(in-package #:smarkup)

;;; quote macro reader

(defun get-delimiter (char)
  (case char
    (#\{ #\})
    (#\( #\))
    (#\[ #\])
    (#\< #\>)
    (t char)))

(defun enable-quote-reader-macro ()
  (set-dispatch-macro-character
   #\# #\q 
   #'(lambda (in c n)
       (declare (ignore c n))
       (let ((delimiter (get-delimiter (read-char in))))
         (let ((string (make-array '(0) :element-type 'character
                                   :fill-pointer 0 :adjustable t)))
           (with-output-to-string (string-stream string)
             (loop for char = (read-char in nil)
                while (and char (not (char-equal char delimiter)))
                do
                  (princ char string-stream)))
           string)))))


