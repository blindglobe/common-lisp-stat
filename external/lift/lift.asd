(defpackage #:asdf-lift (:use #:asdf #:cl))
(in-package #:asdf-lift)

(defsystem lift
  :version "1.3.1"
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
			:depends-on ("packages" "measuring"))
		 (:file "copy-file"
			:depends-on ("packages"))
		 (:file "random-testing" 
			:depends-on ("packages" "lift"))
		 (:file "port" 
			:depends-on ("packages"))
		 (:file "measuring" 
			:depends-on ("packages"))
		 (:file "config" 
			:depends-on ("port"))
		 (:file "reports" 
			:depends-on ("port"))
		 #+Ignore
		 (:file "prototypes"
			:depends-on ("lift"))))
               
               (:module 
		"website"
		:components ((:module "source"
				      :components 
				      ((:static-file "index.lml"))))))
  
  :in-order-to ((test-op (load-op lift-test)))
  :perform (test-op :after (op c)
                    (describe 
		     (funcall (intern (symbol-name '#:run-tests) :lift) 
			      :suite '#:lift-test)))
  :depends-on ()) 

(defmethod operation-done-p 
           ((o test-op) (c (eql (find-system 'lift))))
  (values nil))
