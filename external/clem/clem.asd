
(in-package #:cl-user)

(defpackage #:clem-system (:use #:asdf #:cl))
(in-package #:clem-system)

(defsystem :clem
  :name "clem"
  :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
  :version #.(with-open-file
                 (vers (merge-pathnames "version.lisp-expr" *load-truename*))
               (read vers))
  :licence "BSD"
  :depends-on (ch-util)
  :components
  ((:static-file "version" :pathname #p"version.lisp-expr")
   (:module
    :src
    :components
    ((:cl-source-file "defpackage")
     (:cl-source-file "metaclasses" :depends-on ("defpackage"))
     (:cl-source-file "early-matrix" :depends-on ("defpackage" "metaclasses"))
     (:cl-source-file "mref" :depends-on ("early-matrix"))
     (:cl-source-file "macros" :depends-on ("defpackage" "metaclasses" "mref"))
     (:cl-source-file "matrix-classes" :depends-on ("defpackage"
                                                    "metaclasses"
                                                    "mref"
                                                    "macros"))
     (:cl-source-file "matrix" :depends-on ("matrix-classes"))
     (:cl-source-file "print" :depends-on ("matrix"))
     (:cl-source-file "typed-matrix" :depends-on ("defpackage" "matrix"))
     (:cl-source-file "mloop" :depends-on ("defpackage" "matrix"))
     (:cl-source-file "defmatrix" :depends-on ("typed-matrix" "mref"))
     (:cl-source-file "defmatrix-types" :depends-on ("defmatrix"))
     (:cl-source-file "scalar" :depends-on ("matrix"))
     (:cl-source-file "typed-matrix-utils" :depends-on ("typed-matrix"))
     (:cl-source-file "vector" :depends-on ("matrix"))
     (:cl-source-file "matrixops" :depends-on ("typed-matrix-utils"))
     (:cl-source-file "interpolation" :depends-on ("matrix" "defmatrix-types"))
     (:cl-source-file "transform" :depends-on ("matrix"
                                               "defmatrix-types"
                                               "interpolation"))
     (:cl-source-file "extrema" :depends-on ("matrix" "defmatrix-types"))
     (:cl-source-file "add" :depends-on ("matrix"
                                         "defmatrix-types"
                                         "scalar"
                                         "mloop"
                                         "mref" 
                                         "vector"
                                         "typed-matrix-utils"))
     (:cl-source-file "subtr" :depends-on ("matrix" "defmatrix-types"))
     (:cl-source-file "scale" :depends-on ("matrix" "defmatrix-types"))
     (:cl-source-file "move" :depends-on ("matrix" "defmatrix-types"))
     (:cl-source-file "sum" :depends-on ("matrix" "defmatrix-types"))
     (:cl-source-file "arithmetic" :depends-on ("matrix" "defmatrix-types"))
     (:cl-source-file "logical-operations" :depends-on ("matrix" "defmatrix-types"))
     (:cl-source-file "abs" :depends-on ("matrix" "defmatrix-types"))
     (:cl-source-file "normalize" :depends-on ("matrix" "defmatrix-types"))
     (:cl-source-file "statistics" :depends-on ("matrix" "defmatrix-types"))
     (:cl-source-file "exponential" :depends-on ("matrix" "defmatrix-types"))
     
     (:module
      :typed-ops
      :components
      ((:cl-source-file "defmatrix-equal")
       (:cl-source-file "defmatrix-hprod")
       (:cl-source-file "defmatrix-mult")
       (:cl-source-file "defmatrix-mult-block")
       (:cl-source-file "defmatrix-transform")
       (:cl-source-file "defmatrix-scale")
       (:cl-source-file "defmatrix-subset-matrix")
       (:cl-source-file "defmatrix-convolve"))
      :depends-on ("defmatrix-types"
                   "matrix"
                   "mloop"
                   "scalar"
                   "interpolation"
                   "matrixops"
                   "typed-matrix-utils"))))
   (:static-file "bootstrap" :pathname #p"bootstrap.lisp")
   (:static-file "COPYRIGHT")
   (:static-file "NEWS")
   (:static-file "ChangeLog")
   (:static-file "README")
   (:static-file "make-dist" :pathname #p"make-dist.sh")))

