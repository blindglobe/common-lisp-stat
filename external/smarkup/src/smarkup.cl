;;; -*- Mode: lisp; outline-regexp: ";;;;;*"; indent-tabs-mode: nil -*-;;;
;;;
;;; file: smarkup-asdf.cl
;;; author: cyrus harmon
;;;

;;; miscellaneous functions

(in-package #:smarkup)

(defgeneric render-as (type sexp file))

(defun remove-from-plist (plist &rest keys)
  (cond ((eql (length keys) 1)
         (loop for (x y) on plist by #'cddr
            append (unless (eql x (car keys))
                     (list x y))))
        ((> (length keys) 1)
         (reduce (lambda (&optional plist x)
                   (when x (remove-from-plist plist x)))
                 (cons plist keys)))))

(defun remove-pair-from-list (list key)
  (let ((pos (position key list)))
    (if pos
        (append (subseq list 0 pos)
                (subseq list (+ pos 2))))))

(defun find-file-for-types (default-file types)
  (loop for type in types
     do (let ((path (merge-pathnames (make-pathname :type type) default-file)))
          (when (probe-file path)
            (return path)))))

(defparameter *images-per-line* 5)
(defparameter *images-per-page* 30)

(defun multi-line-figure (image-sequence
                          caption
                          &key
                          label
                          (start 0)
                          (end)
                          (images-per-line *images-per-line*)
                          (width "1.1in"))
  (let* ((image-sequence
          (mapcan #'(lambda (x)
                      (when x (list x)))
                  image-sequence))
         (end (or end (1- (length image-sequence)))))
    (when (some #'identity image-sequence)
      `(:figure
        ,@(when label `(:label ,label))
        (:centering
         ,@(loop for i from start to end by images-per-line
              collect
              `(:subfigure
                ,@(loop
                     for j from i to (min (+ i images-per-line -1) end)
                     collect
                     (let ((img (elt image-sequence j)))
                       `(:image ,(namestring img)
                                :width ,width))))))
        ,(when caption `(:caption ,@(if (listp caption) caption (list caption))))))))


(defun multi-multi-line-figure (image-sequence
                                &key
                                caption
                                (first-caption caption)
                                (start 0)
                                (end)
                                (images-per-line *images-per-line*)
                                (images-per-page *images-per-page*)
                                (width "1in"))
  (let* ((image-sequence
          (mapcan #'(lambda (x)
                      (when x (list x)))
                  image-sequence))
         (end (or end (1- (length image-sequence)))))
    `(:span
      ,@(loop for i from start to end by images-per-page
           collect
           (multi-line-figure image-sequence (if (= i start)
                                                 first-caption
                                                 caption)
                              :start i :end (min (+ i images-per-page -1) end)
                              :images-per-line images-per-line
                              :width width)))))


#+nil
(defun multi-line-subfigure (image-sequence
                             &key
                             caption
                             (start 0)
                             (end (1- (length image-sequence)))
                             (images-per-line *images-per-line*)
                             (width "1in"))
  (when (some #'identity image-sequence)
    `(:subfigure
      ,@(loop for i from start to end by images-per-line
           append
           (append
            (loop
               for j from i to (min (+ i images-per-line -1) end)
               collect
               (let ((img (elt image-sequence j)))
                 `(:image ,(namestring img)
                          :width ,width)))
            `((:p)
              (:p))))
      ,@(when caption `(:caption ,caption)))))

(defun multi-line-subfigure (image-sequence
                             &key
                             caption
                             (start 0)
                             (end (1- (length image-sequence)))
                             (images-per-line *images-per-line*)
                             increment-counter
                             (width "1in"))
  (when (some #'identity image-sequence)
    (append
     (loop for i from start to end by images-per-line
        collect
          (cons
           :subfigure
           (append
            (loop
               for j from i to (min (+ i images-per-line -1) end)
               collect
               (let ((img (elt image-sequence j)))
                 `(:image ,(namestring img)
                          :width ,width)))
            (if (and caption (> (+ i images-per-line) end))
                `(:caption ,caption)
                (unless increment-counter
                  `(:increment-counter nil)))))))))