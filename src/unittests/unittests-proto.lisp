;;; -*- mode: lisp -*-
;;; Copyright (c) 2007, by A.J. Rossini <blindglobe@gmail.com>
;;; See COPYRIGHT file for any additional restrictions (BSD license).
;;; Since 1991, ANSI was finally finished.  Edited for ANSI Common Lisp. 

;;; This is semi-external to lispstat core packages.  The dependency
;;; should be that lispstat packages are dependencies for the unit
;;; tests.  However, where they will end up is still to be
;;; determined. 

(in-package :lisp-stat-unittests)

;;; Object System tests

;; need to ensure stability of add, display, send, instantiate and
;; similar actions.

;;; FIXME: reorient this set of tests as a singleton.
(deftestsuite lisp-stat-ut-proto (lisp-stat-ut)
  ()
;;   (:tests
;;    (create-proto (ensure  (typep (defproto test-me)
;; 				 'lisp-stat-object-system::ls-object)))
;;    (instance1 (ensure (send test-me :isnew)))
;;    (instance2 (ensure (send test-me :has-slot 'new)))
;;    (instance5 (ensure (send test-me :own-slots 'new))))
  )


(addtest (lisp-stat-ut-proto)
  create-proto-type
  (let ((test-me nil))
    (ensure (typep (defproto test-me)
		   'lisp-stat-object-system::ls-object) )))

(addtest (lisp-stat-ut-proto)
  create-proto-type
  (let ((test-me nil))
    (ensure (lisp-stat-object-system:objectp (defproto test-me)))))

;; (describe (run-test :test-case 'create-proto))




;; (run-tests :suite 'lisp-stat-ut-proto)
