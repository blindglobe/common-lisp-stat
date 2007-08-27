(in-package #:common-lisp-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package '#:lift)
    (defpackage #:lift
      (:use #:common-lisp)
      (:import-from		     
       #+allegro #:mop
       #+clisp #:clos
       #+lispworks #:clos
       #+mcl #:ccl
       #+cmu #:clos-mop
       #+sbcl #:sb-mop
       class-direct-subclasses
       class-direct-superclasses
       class-precedence-list)
      (:export
       #:with-profile-report))))

