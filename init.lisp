;;; Basic initialization for LispStat
;;; Time-stamp: <2007-12-26 18:23:24 rossini>
;;; Created: <2007-05-30 17:09:47 ROSSIAN6>

;; Goal:
;; 
;; Associate ASDF directory for loading.
;; Make sure that we have initialized any locally defined global variables
;; ensure appropriate tools are linked in for loading.
;; i.e. if features don't exist, load from particular locations in
;; this directory structure.


;; (setf *my-base-directory*
;;       #p"/home/tony/sandbox/CLS.git/"
;;       #p"/Users/ungil/lisp/CommonLispStat/")



;; What package should we be in?  Contaminating cl-user is probably EVIL.

;; Need to replace this with some form of self-referential structure. 

;; Add the critical paths to the ASDF facility and set up other
;; things; currently this will not do the other setups.

(progn
  (defvar *lispstat-home-dir*
    ;; #p"/home/tony/sandbox/CommonLispStat.git/"
    ;; #p"/cygdrive/c/local/sandbox/Lisp/CommonLispStat/"
    ;; #p"/Users/ungil/lisp/CommonLispStat/")
    ;; #p"/home/rossini/public_html/GIT.repos/CommonLispStat/"
    #p"/home/tony/Desktop/sandbox/CLS.git/"
    "Value considered \"home\" for our data")

  (setf *lispstat-home-dir*
	;; #p"/cygdrive/c/local/sandbox/Lisp/CommonLispStat/"
	;; #p"/home/tony/sandbox/CommonLispStat.git/"
	;; #p"/Users/ungil/lisp/CommonLispStat/")
	;; #p"/home/rossini/public_html/GIT.repos/CommonLispStat/"
	#p"/home/tony/Desktop/sandbox/CLS.git/"
	)
  
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
  (ls-defdir *lispstat-examples-dir* "examples/")
  
  ;; Load ASDF if it isn't loaded
  #-asdf(load (pathname (concatenate 'string (namestring *lispstat-external-dir*) "asdf")))
  
  ;; (pushnew #p"C:/Lisp/libs/" asdf-util:*source-dirs* :test #'equal)
  (pushnew  *lispstat-asdf-dir*  asdf:*central-registry*))

(progn 
  ;; Load needed packages

  (asdf:oos 'asdf:load-op :cffi)            ;; FFI
  (asdf:oos 'asdf:load-op :lift)            ;; Unit Testing 
  #+nil(asdf:oos 'asdf:load-op :lift-test)  
  
  
  
  (asdf:oos 'asdf:load-op :clem)            ;; matrix stuff
  #+nil(asdf:oos 'asdf:load-op :clem-test)
  #+nil(asdf:oos 'asdf:load-op :clem-benchmark)
  #+nil(asdf:oos 'asdf:load-op :cells)

  ;; Finally...
  (asdf:oos 'asdf:load-op :lispstat)
  )

#+nil(progn
       (asdf:oos 'asdf:compile-op :cffi)
       (asdf:oos 'asdf:compile-op :lisp-unit)
       (asdf:oos 'asdf:compile-op :lift)
       (asdf:oos 'asdf:compile-op :rclg)
       (asdf:oos 'asdf:compile-op :cl-cairo2)
;; (asdf:operate 'asdf:load-op 'cffi)
;; (asdf:operate 'asdf:load-op 'rclg)
       (asdf:oos 'asdf:load-op :celtk)
       (asdf:oos 'asdf:compile-op :lispstat :force t)
;; (asdf:operate 'asdf:compile-op 'lispstat)
       )


