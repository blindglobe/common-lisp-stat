;;; Basic initialization for LispStat
;;; Time-stamp: <2007-06-25 09:07:38 ROSSIAN6>
;;; Created: <2007-05-30 17:09:47 ROSSIAN6>

;; Goal:
;; 
;; Associate ASDF directory for loading.
;; Make sure that we have initialized any locally defined global variables
;; ensure appropriate tools are linked in for loading.
;; i.e. if features don't exist, load from particular locations in this directory structure.

(defvar *lispstat-homedir* #p"/cygdrive/c/local/sandbox/Lisp/CommonLispStat/"
	"Value considered \"home\" for our data")
(defvar *tony-local-lispdirstr* (namestring *tony-local-lispdir*)
	"Value considered \"home\" for our data")

#+clisp
(progn 
  (load (pathname (concatenate 'string (namestring *tony-local-lispdir*) "asdf")))
  (load (pathname (concatenate 'string (namestring *tony-local-lispdir*) "pcl-portable-files"))))

(progn 
  ;; (pushnew #p"C:/Lisp/libs/" asdf-util:*source-dirs* :test #'equal)
  (pushnew  (pathname (concatenate 'string (namestring *lispstat-homedir*) "ASDF"))
	    ;;"/cygdrive/c/local/sandbox/Lisp/asdf-packages/")
	    asdf:*central-registry*)
  (asdf:oos 'asdf:load-op :cffi)
  (asdf:oos 'asdf:load-op :lisp-unit)
  (asdf:oos 'asdf:load-op :cells)
  (asdf:oos 'asdf:load-op :lisp-stat))

#+nil(progn
       (asdf:oos 'asdf:compile-op :cffi)
       (asdf:oos 'asdf:compile-op :lisp-unit)
       (asdf:oos 'asdf:compile-op :rclg)
       (asdf:oos 'asdf:compile-op :cl-cairo2)
       (asdf:oos 'asdf:load-op :celtk))
