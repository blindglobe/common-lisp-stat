;;; Basic ASDF stuff, was: basic initialization for LispStat
;;; Time-stamp: <2008-05-15 18:31:47 tony>
;;; Created: <2007-05-30 17:09:47 blindglobe>


(asdf:oos 'asdf:load-op :cffi)            ;; FFI
(asdf:oos 'asdf:load-op :lift)            ;; Unit Testing 
  
;; Finally...
(asdf:oos 'asdf:load-op :lispstat)

;; Things to never automatically do...


(asdf:oos 'asdf:load-op :lift-test)  

;; compiling
(asdf:oos 'asdf:compile-op :cffi)
;; (asdf:oos 'asdf:compile-op :lisp-unit)
(asdf:oos 'asdf:compile-op :lift)
(asdf:oos 'asdf:compile-op :rclg)
(asdf:oos 'asdf:compile-op :cl-cairo2)
;; (asdf:operate 'asdf:load-op 'cffi)
;; (asdf:operate 'asdf:load-op 'rclg)
(asdf:oos 'asdf:load-op :celtk)
(asdf:oos 'asdf:compile-op :lispstat :force t)
;; (asdf:operate 'asdf:compile-op 'lispstat)

;; Things to do later...

#+nil(asdf:oos 'asdf:load-op :clem) ;; matrix stuff
#+nil(asdf:oos 'asdf:load-op :clem-test)
#+nil(asdf:oos 'asdf:load-op :clem-benchmark)
#+nil(asdf:oos 'asdf:load-op :cells)
