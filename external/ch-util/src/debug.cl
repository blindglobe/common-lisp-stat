

(in-package :ch-util)

(defun hex-dump-word (address)
  #+sbcl 
  (format nil "~8,'0X"
          (sb-alien:deref
           (sb-alien:sap-alien
            (sb-alien::int-sap address)
            (* (sb-alien:unsigned 32)))))
  #-sbcl 
  (format "not yet implemented!"))

(defun hex-dump-byte (address)
  #+sbcl 
  (format nil "~2,'0X"
          (sb-alien:deref
           (sb-alien:sap-alien
            (sb-alien::int-sap address)
            (* (sb-alien:unsigned 8)))))
  #-sbcl 
  (format "not yet implemented!"))

(defun hex-dump-memory (start-address length)
  (loop for i from start-address below (+ start-address length)
     collect (format nil (hex-dump-byte i))))

(defun hex-dump-words (start-address length)
  (loop for i from start-address below (+ start-address length) by 4
     collect (format nil (hex-dump-word i))))

(defun hex-dump-long (address)
  (hex-dump-memory address 4))

(defun char-dump-byte (address)
  #+sbcl
  (format nil "~A"
          (code-char
           (sb-alien:deref
            (sb-alien:sap-alien
             (sb-alien::int-sap address)
             (* (sb-alien:unsigned 8))))))
  #-sbcl
  (format nil "not yet implemented"))

(defun char-dump-memory (start-address length)
  (loop for i from start-address below (+ start-address length)
     collect (format nil (char-dump-byte i))))

(defun double-at-address (address)
  (sb-alien:deref
   (sb-alien:sap-alien
    (sb-alien::int-sap address)
    (* (sb-alien:double-float)))))

(defun double-dump-memory (start-address length)
  (let ((size (sb-alien:alien-size sb-alien:double-float :bytes)))
    (loop for i from start-address
       below (+ start-address (*  length size))
       by size
       collect (cons (format nil "~X" i) 
                     (double-at-address i)))))
