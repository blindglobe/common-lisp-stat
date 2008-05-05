(defpackage #:asdf-lift (:use #:asdf #:cl))
(in-package #:asdf-lift)

(defsystem lift
  :version "1.4.3"
  :author "Gary Warren King <gwking@metabang.com>"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style License; see file COPYING for details"
  :description "LIsp Framework for Testing"
  :long-description "LIFT is an SUnit variant and much much more."  
  :components ((:module
		"dev" 
		:components 
		((:static-file "notes.text")
             
		 (:file "packages")
		 (:file "lift"
			:depends-on ("packages" "measuring" "port"))
		 (:file "copy-file"
			:depends-on ("packages"))
		 (:file "random-testing" 
			:depends-on ("packages" "lift"))
		 (:file "port" 
			:depends-on ("packages"))
		 (:file "measuring" 
			:depends-on ("packages" "port"))
		 (:file "config" 
			:depends-on ("port" "lift"))
		 (:file "reports" 
			:depends-on ("port" "lift"))
		 #+Ignore
		 (:file "prototypes"
			:depends-on ("lift"))))
               
	       #+(or)
               (:module 
		"website"
		:components ((:module "source"
				      :components 
				      ((:static-file "index.md"))))))
  
  :in-order-to ((test-op (load-op lift-test)))
  :depends-on ()
  :perform (test-op :after (op c)
		    (funcall
		      (intern (symbol-name '#:run-tests) :lift)
		      :config :generic)))

(defmethod operation-done-p 
           ((o test-op) (c (eql (find-system 'lift))))
  (values nil))


(when (find-system 'asdf-system-connections nil)
  (asdf:operate 'asdf:load-op 'asdf-system-connections))

#+asdf-system-connections
(asdf:defsystem-connection lift-report-locations
  :requires (:lift :asdf-binary-locations)
  :components ((:module "dev"
			:components ((:file "report-locations")))))
