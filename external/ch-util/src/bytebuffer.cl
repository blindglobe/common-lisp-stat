
(in-package :ch-util)

(defclass byte-buffer ()
  ((storage :accessor storage
	    :initform (make-array '(256)
				  :element-type '(unsigned-byte 8)
				  :fill-pointer 0
				  :adjustable t))
   (chunk-size :accessor chunk-size :initform 256)))

(defun byte-buffer ()
  (make-instance 'byte-buffer))

(defgeneric append-byte (buf byte))
(defmethod append-byte ((buf byte-buffer) byte)
  (let* ((a (storage buf))
	 (l (first (array-dimensions a)))
	 (fp (fill-pointer a)))
    (when (= l fp)
      (let ((newlen (+ l (chunk-size buf))))
	(adjust-array a (list newlen))))
    (setf (aref a (1- (incf (fill-pointer a)))) byte)))

(defgeneric print-buffer (buf))
(defmethod print-buffer ((buf byte-buffer))
  (let* ((a (storage buf))
	 (fp (fill-pointer a)))
    (dotimes (i fp)
      (princ (code-char (aref a i))))))

(defun read-file-to-buffer (filename)
  (let ((buf (byte-buffer)))
    (with-open-file (f filename :element-type '(unsigned-byte 8))
      (do ((b (read-byte f) (read-byte f nil 'eof)))
	  ((eq b 'eof))
	(append-byte buf b)))
    buf))

(defun contents-of-stream (in)
  "Returns a string with the entire contents of the specified file."
  (with-output-to-string (contents)
			 (let* ((buffer-size 4096)
				(buffer (make-string buffer-size)))
			   (labels ((read-chunks ()
				      (let ((size (read-sequence buffer in)))
					(if (< size buffer-size)
					    (princ (subseq buffer 0 size) contents)
					    (progn
					      (princ buffer contents)
					      (read-chunks))))))
			     (read-chunks)))))

;;;
;;; From lemonodor.com
;;; John Wiseman's blog
;;;
(defun contents-of-file (pathname)
  (with-open-file (in pathname :direction :input)
    (contents-of-stream in)))
