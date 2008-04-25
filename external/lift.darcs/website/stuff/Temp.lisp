(in-package #:few)

#+Ignore
;; suck site up and output as LML
(mapc 
 (lambda (file)
   (let ((html (net.html.parser:parse-html file)))
     (setf html (remove-if (lambda (x)
                             (and (consp x)
                                  (member (first x) '(:!doctype))))
                           html))
     (with-new-file (s (make-pathname :type "lml" :defaults file)
                       :print-right-margin 70)
       (format s "~S" html))))
 (directory "Billy-Pilgrim:Users:gwking:darcs:metabang.tinaa:website:*.html"))

(net.html.parser:parse-html 
 #P"Billy-Pilgrim:Users:gwking:darcs:metabang.tinaa:website:index.html")

(probe-file
 "Billy-Pilgrim:Users:gwking:darcs:cl-containers:website:index.shtml")

(eval `(html
        ((:html :xmlns "http://www.w3.org/1999/xhtml")
         ,@(rest ccl:!))))



#+No
(with-new-file (*html-stream* (spy (make-pathname :type "lml" :defaults file)))
  (dtd-prologue :xhtml11)
  (eval `(html
          ((:html :xmlns "http://www.w3.org/1999/xhtml")
           ,@html))))