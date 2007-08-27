;;
;; Copyright (c) 2007, Cyrus Harmon
;;
;; typesetting.cl - cl-typesettng/cl-pdf output for smarkup
;;

(in-package :smarkup)

(in-package :tt)

(defmacro with-paragraph (style-designator &body body)
  (with-gensyms (top-margin bottom-margin first-line-indent
                            style new-style restore-style first-indent)
    `(let* ((,style ,style-designator)
            (,top-margin (getf ,style :top-margin 0))
            (,bottom-margin (getf ,style :bottom-margin 0))
            (,first-line-indent (getf ,style :first-line-indent 0))
            (,new-style (typecase ,style
                          (text-style ,style)
                          (t (apply #'make-instance 'text-style ,style))))
            (,restore-style (make-restore-style ,new-style))
            (,first-indent ,first-line-indent))
       (add-box ,new-style)
       (use-style ,new-style)
       (add-box (make-instance 'v-spacing :dy ,top-margin))
       (unless (zerop ,first-indent)
         (add-box (make-instance 'h-spacing :dx ,first-indent)))
       ,@(mapcar 'insert-stuff body)
       (unless (eq (first (boxes-tail *content*)) :eol)
         (add-box :eol))
       (add-box (make-instance 'v-spacing :dy ,bottom-margin))
       (add-box ,restore-style)
       (use-style ,restore-style))))

(in-package :smarkup)

(defgeneric %render-elt (tag contents))

(defun render-elts (sexp)
  (loop for s in sexp
     do (render-elt s)))

(defun put-smarkup-string (string)
  (tt:put-string
   (macrolet ((match-second-char (c1 c2 out-char)
                `(let ((n (peek-char nil in nil nil)))
                  (if (eql n ,c2)
                      (progn
                        (read-char in)
                        (write-char ,out-char out))
                      (write-char ,c1 out)))))
     (with-output-to-string (out)
       (with-input-from-string (in string)
         (loop for c = (read-char in nil nil) while c
            do 
            (cond ((eql c #\~)
                   (write-char #\No-Break_Space out))
                  ((eql c #\.)
                   (match-second-char #\. #\\ #\Space))
                  ((eql c #\`)
                   (match-second-char #\` #\` (code-char #x93)))
                  ((eql c #\')
                   (match-second-char #\' #\' (code-char #x94)))
                  ((eql c #\-)
                   (match-second-char #\- #\- (code-char #x97)))
                  (t (write-char c out)))))))))

(defun render-elt (elt)
  #+nil (print (car elt))
  (cond ((stringp elt)
         (put-smarkup-string elt))
        ((stringp (car elt))
         (put-smarkup-string (car elt))
         (render-elt (cdr elt)))
        #+nil ((and elt (atom elt))
               elt)
        ((atom (car elt))
         (%render-elt (car elt)
                      (cdr elt)))
        ((listp (car elt))
         (render-elt (car elt))
         (render-elt (cdr elt)))
        (t nil)))

(defun collect-elements (contents)
  (apply #'concatenate 'string (mapcar #'render-elt contents)))

(defparameter *default-paragraph-font*
  '(:h-align :justified
    :font "Times-Roman"
    :font-size 9.8
    :first-line-indent 18
    :bottom-margin 3))

(defmethod %render-elt ((tag (eql :p)) contents)
  (tt::with-paragraph *default-paragraph-font*
    (render-elt contents)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *default-bold-font*
    '(:font "Times-Bold")))

(defmacro highlight (text)
  `(tt:with-style ,*default-bold-font*
     ,text))

(defmethod %render-elt ((tag (eql :b)) contents)
  (highlight
    (render-elt contents)))

(defparameter *default-h1-font*
  '(:h-align :center :font "Helvetica-Bold" :font-size 14 :bottom-margin 3))

(defparameter *default-h2-font*
  '(:font "Helvetica-Bold" :font-size 11 :bottom-margin 3 :top-margin 3))

(defmethod %render-elt ((tag (eql :h1)) contents)
  (tt::with-paragraph *default-h1-font*
    (render-elt contents)))

(defmethod %render-elt ((tag (eql :h2)) contents)
  (tt::with-paragraph *default-h2-font*
    (render-elt contents)))

(defmethod %render-elt ((tag (eql :span)) contents)
  (render-elts contents))

(defmethod %render-elt ((tag (eql :list)) contents)
  (render-elts contents))

(defparameter *item-decorator*
  (format nil "~A " (code-char #x81)))

(defparameter *default-item-font*
  (concatenate
   'list
   '(:left-margin 18)
   *default-paragraph-font*))

(defun get-item-decorator-width ()
  (let ((font-size (or (getf *default-item-font* :font-size)
                       tt::*font-size*)))
    (let* ((item-font (getf *default-item-font* :font))
           (font (if item-font
                     (pdf:get-font item-font)
                     tt::*font*)))
      (loop for char across *item-decorator*
         summing (pdf:get-char-width char font font-size)))))

(setf (getf *default-item-font* :first-line-indent)
      (- (get-item-decorator-width)))

(defmethod %render-elt ((tag (eql :item)) contents)
  (tt::with-paragraph *default-item-font*
    (tt:put-string *item-decorator*)
    (render-elt contents)))

(defmethod %render-elt (tag contents)
  (when contents (render-elt (cdr contents))))

(defmethod %render-elt ((tag (eql :eol)) contents)
  (tt:new-line)
  (when contents (render-elt contents)))

(defparameter *pdf-header-function* nil)

(defmethod render-as ((type (eql :cl-pdf)) sexp file)
  (setq nix::*left-hyphen-minimum* 999
        nix::*right-hyphen-minimum* 999)
  (tt:with-document ()
    (let ((content
           (tt::with-text-content ((make-instance 'tt::text-content) :dont-save-style t)
             (tt::add-box (tt::copy-style (tt::text-style tt::*content*)))
             (render-elts sexp)
             tt::*content*)))
      (apply #'tt:draw-pages
             content
             :size :letter
             :margins '(72 72 72 72)
             (when *pdf-header-function*
               `(:header ,(funcall *pdf-header-function*))))
      (when pdf:*page* (typeset:finalize-page pdf:*page*))
      (tt:write-document file))))
