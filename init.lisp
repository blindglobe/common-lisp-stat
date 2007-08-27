;;; Basic initialization for LispStat
;;; Time-stamp: <2007-06-25 19:58:14 ROSSIAN6>
;;; Created: <2007-05-30 17:09:47 ROSSIAN6>

;; Goal:
;; 
;; Associate ASDF directory for loading.
;; Make sure that we have initialized any locally defined global variables
;; ensure appropriate tools are linked in for loading.
;; i.e. if features don't exist, load from particular locations in this directory structure.


;; Need to replace this with some form of self-referential structure. 

(defvar *lispstat-home-dir*
  ;; #p"/cygdrive/c/local/sandbox/Lisp/CommonLispStat/"w
  #p"/home/tony/sandbox/CommonLispStat.git/"
  "Value considered \"home\" for our data")


(defmacro ls-dir (root-str)
  `(pathname (concatenate 'string (namestring *lispstat-home-dir*) ,root-str)))

(defmacro ls-defdir (target-dir-var  root-str)
  `(defvar ,target-dir-var (ls-dir ,root-str)))

;;(macroexpand '(ls-defdir *lispstat-asdf-dir* "ASDF"))
;;(macroexpand-1 '(ls-defdir *lispstat-asdf-dir* "ASDF"))
;;(macroexpand-1 '(ls-dir "ASDF"))

(ls-defdir *lispstat-asdf-dir* "ASDF/")
(ls-defdir *lispstat-data-dir* "data/")
(ls-defdir *lispstat-external-dir* "external/")

;; Load ASDF if it isn't loaded
#-asdf(load (pathname (concatenate 'string (namestring *lispstat-external-dir*) "asdf")))

;; Add the critical paths to the ASDF facility

(progn 
  ;; (pushnew #p"C:/Lisp/libs/" asdf-util:*source-dirs* :test #'equal)
  (pushnew  *lispstat-asdf-dir*  asdf:*central-registry*)

  ;; FFI
  (asdf:oos 'asdf:load-op :cffi)

  ;; Unit Testing 
  (asdf:oos 'asdf:load-op :lift)
  ;;(asdf:oos 'asdf:load-op :lisp-unit)

  ;; Constraint System
  ;;(asdf:oos 'asdf:load-op :cells)

  ;; Finally...
  #+nil(asdf:oos 'asdf:load-op :lispstat)
  )

#+nil(progn
       (asdf:oos 'asdf:compile-op :cffi)
       (asdf:oos 'asdf:compile-op :lisp-unit)
       (asdf:oos 'asdf:compile-op :lift)
       (asdf:oos 'asdf:compile-op :rclg)
       (asdf:oos 'asdf:compile-op :cl-cairo2)
       (asdf:oos 'asdf:load-op :celtk))
