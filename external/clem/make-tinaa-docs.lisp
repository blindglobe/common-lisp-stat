
(require 'asdf)

(asdf:operate 'asdf:load-op 'clem)
(asdf:operate 'asdf:load-op 'clem-doc)
(asdf:operate 'asdf:load-op 'tinaa)

(defun clem-doc-system::make-tinaa-docs ()
  (asdf:operate 'asdf:load-op 'tinaa)
  (let ((tinaa::*short-documentation-length* 512))
    (tinaa:document-system
     'package 'clem (asdf:component-pathname
                     (asdf:find-component
                      (asdf:find-component
                       (asdf:find-system 'clem-doc)
                       "doc")
                      "tinaa")))))
  
(clem-doc-system::make-tinaa-docs)
