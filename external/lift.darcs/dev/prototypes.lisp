;;;-*- Mode: Lisp; Package: LIFT -*-

(in-package #:lift)

(pushnew :cases *deftest-clauses*)

(add-code-block
 :cases 2 :methods
 (lambda () (def :cases)) 
 '((setf (def :cases) (cleanup-parsed-parameter value)))
 'build-cases-method)

(defun build-cases-method ()
  (when (atom (car (def :cases)))
    (setf (def :cases) (list (def :cases))))
  ;(spy (def :cases))
  (let ((cases (standardize-cases-form (def :cases))))
    `(defmethod initialize-prototypes :after ((test ,(def :testsuite-name)))
       (setf (prototypes test) 
             (rest (process-cases-form 
		    ,(first cases) 
		    ,@(mapcar (lambda (a) `',a) (rest cases))))))))

;; goal is spec := (<tag> <spec>+)
;;         spec := (<var value>+)
(defun standardize-cases-form (cases)
  (cond ((atom (first cases))
         (cond ((valid-tag-p (first cases)) 
                `(,(first cases) ,@(mapcar #'standardize-cases-form (rest cases))))
               (t
                cases)))
        ((and (length-1-list-p cases)
              (consp (first cases))
              (valid-tag-p (first (first cases))))
         (standardize-cases-form (first cases)))
        (t
         `(:cross ,@(mapcar #'standardize-cases-form cases)))))

;;; ---------------------------------------------------------------------------

(defun check-subcases (cases)
  (cond ((not (valid-tag-p (first cases)))
         `(,(default-cases-tag) ,@(mapcar #'standardize-cases-form cases)))
        (t
         (mapcar #'standardize-cases-form cases))))

;;; ---------------------------------------------------------------------------

(defun default-cases-tag ()
  :cross)

;;; ---------------------------------------------------------------------------

(defun valid-tag-p (tag)
  (member tag '(:map :cross)))

;;; ---------------------------------------------------------------------------

(defmethod process-cases-form :around ((type t) &rest forms)
  (apply #'call-next-method type (if (atom (car forms))
                                   (list forms) forms)))

;;; ---------------------------------------------------------------------------
  
(defmethod process-cases-form ((type t) &rest forms)
  (cond ((atom (first type))
         (apply #'process-cases-form (first type) (append (rest type) forms)))
        (t (apply #'process-cases-form :cross (append type forms)))))

#+Old
(defmethod process-cases-form ((type (eql :map)) &rest forms)
  (let ((vars (mapcar #'car forms))
        (values (mapcar #'rest forms)))
    `(let (,@(mapcar (lambda (var value) `(,var ,@value))
                     vars values))
       (mapcar (lambda ,vars
                 (list ,@(mapcar (lambda (var) `(cons ',var ,var)) vars)))
               ,@vars))))

;;; ---------------------------------------------------------------------------

(defmethod process-cases-form ((type (eql :map)) &rest forms)
  (let ((vars (ensure-list (flatten (vars-from-assignment forms))))
        (values (values-from-assignment forms)))
    `(:b ,@(apply #'mapcar
                  (lambda (&rest args)
                    (mapcar (lambda (var value)
                              (cons var value))
                            vars args))
                  values))))

;;; ---------------------------------------------------------------------------

(defmethod process-cases-form ((type (eql :cross)) &rest forms)
  (let ((vars (ensure-list (flatten (vars-from-assignment forms))))
        (values (values-from-assignment forms))
        (result nil))
    (iterate-over-indexes
     (mapcar #'length values)
     (lambda (indexes)
       (let ((datum nil))
         (mapcar (lambda (name var index)
                   (push (cons name (elt var index)) datum))
                 vars
                 values
                 indexes)
         (push (nreverse datum) result))) 
     :right)
    `(:b ,@(nreverse result))))

;;; ---------------------------------------------------------------------------

(defun vars-from-assignment (assignment)
  (cond ((is-binding-p assignment)
         (mapcar #'car (second assignment)))
        ((metatilities:dotted-pair-p assignment)
         (car assignment))
        ((atom (car assignment))
         (car assignment))
        ((length-1-list-p assignment)
         (vars-from-assignment (first assignment)))
        (t (loop for assignment in assignment collect
                 (vars-from-assignment assignment)))))
         
;;; ---------------------------------------------------------------------------

(defun values-from-assignment (assignment)
  (cond ((is-binding-p assignment)
         (apply #'mapcar (lambda (&rest bindings)
                   (mapcar (lambda (binding)
                             (cdr binding))
                           bindings))
                 (rest assignment)))
        ((dotted-pair-p assignment)
         (cdr assignment))
        ((atom (car assignment))
         (list (eval (first (rest assignment)))))
        (t 
         (loop for assignment in assignment nconc
                 (ensure-list (values-from-assignment assignment))))))

;;; ---------------------------------------------------------------------------

(defun is-binding-p (assignment)
  (eq (first assignment) :b))


#|


(export '(map-prototypes-of
          prototypes-of
          prototype-of
          prototype-exists-p))

;;; ---------------------------------------------------------------------------
;;; API
;;; ---------------------------------------------------------------------------

(defgeneric map-prototypes-of (fn thing)
  (:documentation ""))

;;; ---------------------------------------------------------------------------

(defgeneric prototypes-of (thing)
  (:documentation ""))

;;; ---------------------------------------------------------------------------

(defgeneric prototype-of (thing)
  (:documentation ""))

;;; ---------------------------------------------------------------------------

(defgeneric prototype-exists-p (thing)
  (:documentation ""))

;;; ---------------------------------------------------------------------------
;;; implementation
;;; ---------------------------------------------------------------------------

(defmethod map-prototypes-of :around (fn thing)
  (declare (ignore fn))
  (when (prototype-exists-p thing)
    (call-next-method)))

;;; ---------------------------------------------------------------------------

(defmethod map-prototypes-of (fn (thing standard-class))
  (map-subclass-prototypes fn thing))

;;; ---------------------------------------------------------------------------

(defmethod map-prototypes-of (fn (thing built-in-class))
  (map-subclass-prototypes fn thing))

;;; ---------------------------------------------------------------------------

(defun map-subclass-prototypes (fn thing)
  (mopu:map-subclasses thing
                       (lambda (subclass)
                         (when (prototype-exists-p subclass)
                           (funcall fn (prototype-of subclass)))))
  (values))
   
;;; ---------------------------------------------------------------------------

(defmethod prototypes-of (thing)
  (containers:collect-using 'map-prototypes-of nil thing))

;;; ---------------------------------------------------------------------------

(defmethod prototype-exists-p (thing)
  ;; the expensive way to see if a prototype exists is to try and make one
  ;; and see if it works...
  (handler-case 
    (let ((creator-method (compute-applicable-methods #'prototype-of (list thing))))
      (when creator-method
        (let ((x (prototype-of thing)))
          (declare (optimize (safety 3) (debug 3) (speed 0) (space 0)))
          x
          (values t))))
    (error (c) (inspect c) nil)))

;;; ---------------------------------------------------------------------------

(defmethod prototype-of ((thing standard-class))
  (allocate-instance thing))

;;; ---------------------------------------------------------------------------

(defmethod prototype-of ((thing (eql 'fixnum)))
  (variates:integer-random variates:*random-generator* -10 10))


|#



(defmethod more-prototypes-p :before ((testsuite test-mixin))
  (setf (current-step testsuite) 'more-prototypes-p))

;;; ---------------------------------------------------------------------------

(defmethod initialize-prototypes :before ((testsuite test-mixin))
  (setf (current-step testsuite) 'initialize-prototypes))

;;; ---------------------------------------------------------------------------

(defmethod next-prototype :before ((testsuite test-mixin))
  (setf (current-step testsuite) 'next-prototype))

;;; ---------------------------------------------------------------------------

(defmethod testsuite-teardown :before ((testsuite test-mixin))
  (setf (current-step testsuite) 'testsuite-teardown))

;;; ---------------------------------------------------------------------------

(defmethod start-test :before
    ((result test-result) (testsuite test-mixin) method-name)
  (declare (ignore method-name)) 
  (setf (current-step testsuite) 'start-test))

;;; ---------------------------------------------------------------------------

(defmethod end-test :before
    ((result test-result) (testsuite test-mixin) method-name)
  (declare (ignore method-name))
  (setf (current-step testsuite) 'end-test))

;;; ---------------------------------------------------------------------------

(defmethod setup-test :before ((testsuite test-mixin))
  (setf (current-step testsuite) 'setup-test))

;;; ---------------------------------------------------------------------------

#+Ignore
(defmethod teardown-test :before ((testsuite test-mixin))
  (setf (current-step testsuite) 'teardown-test))

