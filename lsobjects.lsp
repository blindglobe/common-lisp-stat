;;; -*- mode: lisp -*-
;;; Copyright (c) 2005--2007, by A.J. Rossini <blindglobe@gmail.com>
;;; See COPYRIGHT file for any additional restrictions (BSD license).
;;; Since 1991, ANSI was finally finished.  Edited for ANSI Common Lisp. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;                    LISP-STAT Object System
;;;;
;;;;
;;;; Simple CL implementation of the object system for Lisp-Stat (LSOS)
;;;; as described in Tierney (1990). 
;;;; 
;;;; Copyright (c) 1991, by Luke Tierney. Permission is granted for
;;;; unrestricted use.
;;;;
;;;;
;;;; NOTES:
;;;;
;;;;    If your CL's handling of packages is compliant with CLtL, 2nd 
;;;;    Edition (like Macintosh CL version 2), add the feature :CLtL2
;;;;    before loading or compiling this code.
;;;;
;;;;    This implementation does not make use of CLOS. It can coexist
;;;;    with CLOS, but there are two name conflicts: slot-value and
;;;;    call-next-method. These two symbols are shadowed in the LSOS
;;;;    package and must be shadowed in any package that uses LSOS.
;;;;    Evaluating the function (lsos::use-lsos) from a package after
;;;;    loading this code shadows these two symbols and does a
;;;;    use-package for LSOS.
;;;;
;;;;    The :compile-method method uses function-lambda-expression
;;;;    defined in CLtL, 2nd Edition. (This method is only needed if
;;;;    you want to force compilation of an interpreted method. It is
;;;;    not used by the compiler.)
;;;;
;;;;    The efficiency of this code could be improved by low level 
;;;;    coding of the dispatching functions send, call-method and
;;;;    call-next-method to avoid creating an argument list. Other
;;;;    efficiency improvements are possible as well, in particular 
;;;;    by good use of declarations. It may also be possible to build
;;;;    a more efficient implementation using the CLOS metaclass
;;;;    protocol.
;;;;
;;;;    There are a few minimal tools for experimenting with constraints
;;;;    in the code; they are marked by #+:constreinthooks. Sometime
;;;;    soon I hope to augment or replace these hooks with a CORAL-like
;;;;    constraint system (as used in GARNET).
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Package Setup

(in-package :cl-user)

(defpackage :lisp-stat-object-system
 (:nicknames :ls-objects :lsos)
 (:use :common-lisp)
 (:shadow :call-method :call-next-method :slot-value)
 (:export ls-object objectp *object* kind-of-p make-object *message-hook*
	  *set-slot-hook* slot-value self send call-next-method call-method
	  defmeth defproto instance-slots proto-name))

(in-package :lisp-stat-object-system)

(defun use-lsos ()
  "Formerly set up to import lisp-stat-object-system into current package."
  (shadowing-import (package-shadowing-symbols 'lisp-stat-object-system))
  (use-package 'lisp-stat-object-system))

;;; Structure Implementation of Lisp-Stat Object System

(defvar *object-serial* 0)

(defstruct (ls-object
            (:constructor make-object-structure) ;; why not make-ls-object? 
            (:print-function print-object-structure)
            (:predicate objectp))  ;; why not ls-object-p?
  slots
  methods 
  parents 
  preclist 
  (serial (incf *object-serial*)))

(defun print-object-structure (object stream depth)
  (declare (ignore depth))
  (send object :print stream))

(setf (documentation 'objectp 'function)
  "Args: (x)
Returns T if X is an object, NIL otherwise.")

(defvar *object* (make-object-structure)
  "*object* is the global root object.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;                          Utility Functions
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; special variable to hold current value of SELF.  Assign to current
;;; object that we are working with.  AJR:FIXME:Is this going to cause
;;; issues with concurrency?  (need to appropriately handle
;;; interrupts).
(defvar *self* nil)

;;; FIXME: better as macro?  maybe not?
(defun get-self ()
  (if (not (objectp *self*))
      (error "not in a method"))
  *self*)

(defun has-duplicates (list)
  (do ((next list (rest next)))
      ((not (consp next)) nil)
    (if (member (first next) (rest next)) (return t))))

(defun assoc-eq (item alist)
  "Version of assoc using eq -- should be faster than regular assoc."
  (declare (inline car eq))
  (dolist (i alist)
    (if (eq (car i) item) (return i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;               Predicate and Checking Functions
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun check-non-nil-symbol (x)
  (unless (and x (symbolp x)) (error "bad symbol - ~s" x)))

(defun check-object (x)
  (if (objectp x) x (error "bad object - ~s" x)))

(defun kind-of-p (x y)
"Args: (x y)
Returns T is X and Y are objects and X inherits from Y, NIL otherwise."
  (if (and (objectp x) (objectp y))
      (if (member y (ls-object-preclist x)) t nil)
      nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;                     Precedence List Functions
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-SC (object)
  "find set of object and ancestors. (diff from this and find-S?)"
  (copy-list (ls-object-preclist (check-object object))))

(defun find-S (object)
  "find set of object and ancestors. (diff from this and find-SC?)"
  (do ((result nil)
       (parents (ls-object-parents object) (cdr parents)))
      ((not (consp parents)) 
       (delete-duplicates (cons object result)))
    (setf result (nconc (find-SC (first parents)) result))))

(defun find-RC (object)
  "find local precedence ordering."
  (let ((list (copy-list (ls-object-parents (check-object object)))))
    (do ((next list (rest next)))
        ((not (consp next)) list)
      (setf (first  next) (cons object (first next)))
      (setf object (rest (first next))))))

(defun find-R (S)
  "find partial precedence ordering."
  (do ((result nil)
       (S S (rest S)))
      ((not (consp S)) 
       (delete-duplicates result))
    (setf result (nconc result (find-RC (first S))))))

(defun has-predecessor (x R)
  "check if x has a predecessor according to R."
  (dolist (cell R nil)
    (if (and (consp cell) (eq x (rest cell))) (return t))))

(defun find-no-predecessor-list (S R)
  "find list of objects in S without predecessors, by R."
  (let ((result nil))
    (dolist (x S result)
      (unless (has-predecessor x R) (setf result (cons x result))))))

(defun child-position (x P)
"find the position of child, if any, of x in P, the list found so
far." 
  (let ((count 0))
    (declare (fixnum count))
    (dolist (next P -1)
      (if (member x (ls-object-parents next)) (return count))
      (incf count))))

(defun next-object (no-preds P)
"find the next object in the precedence list from objects with no
predecessor and current list."
  (cond
   ((not (consp no-preds)) nil)
   ((not (consp (rest no-preds))) (first no-preds))
   (t
    (let ((count -1)
          (result nil))
      (declare (fixnum count))
      (dolist (x no-preds result)
        (let ((tcount (child-position x P)))
          (declare (fixnum tcount))
          (when (> tcount count)
            (setf result x)
            (setf count tcount))))))))

(defun trim-S (x S)
  "Remove object x from S."
  (delete x S))

(defun trim-R (x R) 
  "Remove all pairs containing x from R. x is assumed to have no
predecessors, so only the first position is checked."
(delete x R :key #'first))

(defun precedence-list (object)
  "Calculate the object's precedence list."
  (do* ((S (find-S object))
        (R (find-R S))
        (P nil)
        (no-preds nil)
        (next nil))
       ((not (consp S)) P)
    (setf no-preds (find-no-predecessor-list S R))
    (setf next (next-object no-preds P))
    (if (null next) (error "inconsistent precedence order"))
    (setf P (nconc P (list next)))
    (setf S (trim-S next S))
    (setf R (trim-R next R))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;                 Object Construction Functions
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun calculate-preclist (object)
  "Return the precedence list for the object."
  (let ((parents (ls-object-parents (check-object object))))
    (if (not (consp parents)) (error "bad parent list - ~s" parents))
    (if (consp (rest parents)) 
        (precedence-list object)
        (let ((parent (check-object (first parents))))
          (cons object (ls-object-preclist parent))))))

(defun check-parents (parents)
  (cond
   ((or (null parents) (objectp parents)) parents)
   ((consp parents)
    (dolist (x parents) (check-object x))
    (if (has-duplicates parents)
	(error "parents may not contain duplicates")))
   (t (error "bad parents - ~s" parents))))

(defun make-basic-object (parents object)
  (check-parents parents)
  
  (if (not (objectp object)) (setf object (make-object-structure)))

  (setf (ls-object-preclist object) (ls-object-preclist *object*))
  (setf (ls-object-parents object)
        (cond ((null parents) (list *object*))
              ((objectp parents) (list parents))
              (t parents)))
  (setf (ls-object-preclist object) (calculate-preclist object))
  
  object)

(defun make-object (&rest parents)
  "Args: (&rest parents)
Returns a new object with parents PARENTS. If PARENTS is NIL, (list *OBJECT*) is used." 
  (make-basic-object parents NIL))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;                   Constraint Hook Functions
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(pushnew :constrainthooks *features*)

#+:constrainthooks
(progn
  (defvar *message-hook* nil)
  (defvar *set-slot-hook* nil)

  (defun check-constraint-hooks (object sym slot)
    (let ((hook (if slot *set-slot-hook* *message-hook*)))
      (if hook
	  (if slot
	      (let ((*set-slot-hook* nil))
		(funcall hook object sym))
	      (let ((*message-hook* nil))
		(funcall hook object sym)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                     Slot Access Functions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-slot-entry (x y) (cons x y))
(defun slot-entry-p (x) (consp x))
(defun slot-entry-key (x) (first x))
(defun slot-entry-value (x) (rest x))
(defun set-slot-entry-value (x v) (setf (rest x) v))
(defsetf slot-entry-value set-slot-entry-value)

(defun find-own-slot (x slot)
  (if (objectp x) (assoc-eq slot (ls-object-slots x))))

(defun find-slot (x slot)
  (if (objectp x)
    (let ((preclist (ls-object-preclist x)))
      (dolist (object preclist)
        (let ((slot-entry (find-own-slot object slot)))
          (if slot-entry (return slot-entry)))))))

(defun add-slot (x slot value)
  (check-object x)
  (check-non-nil-symbol slot)
  (let ((slot-entry (find-own-slot x slot)))
    (if slot-entry 
      (setf (slot-entry-value slot-entry) value)
      (setf (ls-object-slots x)
            (cons (make-slot-entry slot value) (ls-object-slots x)))))
  nil)

(defun delete-slot (x slot)
  (check-object x)
  (setf (ls-object-slots x)
        (delete slot (ls-object-slots x) :key #'slot-entry-key)))

(defun get-slot-value (x slot &optional no-err)  
  (check-object x)
  (let ((slot-entry (find-slot x slot)))
    (if (slot-entry-p slot-entry)
      (slot-entry-value slot-entry)
      (unless no-err (error "no slot named ~s in this object" slot)))))

(defun set-slot-value (x slot value)
  (check-object x)
  (let ((slot-entry (find-own-slot x slot)))
    (cond
     ((slot-entry-p slot-entry)
      (set-slot-entry-value slot-entry value)
      #+:constrainthooks (check-constraint-hooks x slot t))
     (t
      (if (find-slot x slot)
        (error "object does not own slot ~s" slot)
        (error "no slot named ~s in this object" slot))))))

(defun slot-value (slot)
"Args: (slot)
Must be used in a method. Returns the value of current objects slot
named SLOT."
  (get-slot-value (get-self) slot))

(defun slot-value-setf (slot value)
  (set-slot-value (get-self) slot value))

(defsetf slot-value slot-value-setf)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;                   Method Access Functions;
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-method-entry (x y) (cons x y))
(defun method-entry-p (x) (consp x))
(defun method-entry-key (x) (first x))
(defun method-entry-method (x) (rest x))
(defun set-method-entry-method (x v) (setf (rest x) v))
(defsetf method-entry-method set-method-entry-method)

;(defun find-own-method (x selector)
;  (if (objectp x) (assoc selector (ls-object-methods x))))
(defun find-own-method (x selector)
  (if (objectp x) (assoc-eq selector (ls-object-methods x))))

(defun find-lsos-method (x selector)
  (if (objectp x)
    (let ((preclist (ls-object-preclist x)))
      (dolist (object preclist)
        (let ((method-entry (find-own-method object selector)))
          (if method-entry (return method-entry)))))))

(defun add-lsos-method (x selector value)
  "x = object; selector = name of method; value = form computing the method."
  (check-object x)
  (check-non-nil-symbol selector)
  (let ((method-entry (find-own-method x selector)))
    (if method-entry 
      (setf (method-entry-method method-entry) value)
      (setf (ls-object-methods x)
            (cons (make-method-entry selector value) (ls-object-methods x)))))
  nil)

(defun delete-method (x selector)
  (check-object x)
  (setf (ls-object-methods x)
        (delete selector (ls-object-methods x) :key #'method-entry-key)))

(defun get-message-method (x selector &optional no-err)  
  (check-object x)
  (let ((method-entry (find-lsos-method x selector)))
    (if (method-entry-p method-entry)
      (method-entry-method method-entry)
      (unless no-err (error "no method for selector ~s" selector)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;                   Message Sending Functions
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *current-preclist* nil)
(defvar *current-selector* nil)

(defun sendmsg (object selector preclist args)
  (let ((method-entry nil)
        (method nil))

    ;; look for the message in the precedence list
    (loop
      (setf method-entry (find-own-method (first preclist) selector))
      (if (or method-entry (not (consp preclist))) (return))
      (setf preclist (rest preclist)))
    (cond 
     ((null method-entry) (error "no method for selector ~s" selector))
     ((not (method-entry-p method-entry)) (error "bad method entry"))
     (t (setf method (method-entry-method method-entry))))
   
    ;; invoke the method
    (let ((*current-preclist* preclist)
          (*current-selector* selector)
          (*self* object))
      (multiple-value-prog1
        (apply method object args)
        #+:constrainthooks (check-constraint-hooks object selector nil)))))

;;;; built-in send function
(defun send (object selector &rest args)
"Args: (object selector &rest args)
Applies first method for SELECTOR found in OBJECT's precedence list to
OBJECT and ARGS."
  (sendmsg object selector (ls-object-preclist object) args))

;;;; call-next-method - call inherited version of current method
(defun call-next-method (&rest args)
"Args (&rest args)
Funcalls next method for current selector and precedence list. Can only be
used in a method."
  (sendmsg *self* *current-selector* (rest *current-preclist*) args))
         
;;;; call-method - call method belonging to another object on current object

;; ugly cruft, need better solution for SBCL packagelocks
;; #+sbcl(declare (sb-ext:disable-package-locks ls-objects:call-method))

(defun call-method (object selector &rest args)
"Args (object selector &rest args)
Funcalls method for SELECTOR found in OBJECT to SELF. Can only be used in 
a method."
  (sendmsg *self* selector (ls-object-preclist object) args))

;; #+sbcl(declare (sb-ext:enable-package-locks ls-objects:call-method))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;                  Object Documentation Functions
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-documentation (x sym add)
  (if (objectp x)
    (let ((doc (find-own-slot x 'documentation)))
      (if (and (null doc) add) (add-slot x 'documentation nil))
      (if (slot-entry-p doc) (assoc sym (slot-entry-value doc))))))

(defun add-documentation (x sym value)
  (check-object x)
  (check-non-nil-symbol sym)
  (let ((doc-entry (find-documentation x sym t)))
    (cond
     ((not (null doc-entry))
      (setf (rest doc-entry) value))
     (t
      (set-slot-value x 
                      'documentation
                      (cons (cons sym value)
                            (get-slot-value x 'documentation))))))
  nil)

(defun get-documentation (x sym)
  (check-object x)
  (dolist (object (ls-object-preclist x))
    (let ((doc-entry (find-documentation object sym nil))) ;; FIXME: verify
      (if doc-entry (return (rest doc-entry))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;                         DEFMETH Macro
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro defmeth (object name arglist first &rest body)
  "Syntax: (defmeth object method-name lambda-list [doc] {form}*)
OBJECT must evaluate to an existing object. Installs a method for NAME in
the value of OBJECT and installs DOC in OBJECTS's documentation.
RETURNS: method-name."
  (declare (ignorable self))  ;; hints for the compiler that sometimes it isn't used
  (if (and body (stringp first)) 
    `(progn ;; first=docstring + body
       (add-lsos-method ,object ,name
                 #'(lambda (self ,@arglist) (block ,name ,@body)))
       (add-documentation ,object ,name ,first)
       ,name)
    `(progn ;; first=code + body
       (add-lsos-method ,object ,name
                   #'(lambda (self ,@arglist) (block ,name ,first ,@body)))
       ,name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;             Prototype Construction Functions and Macros
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-instance-slots (x slots)
  (let ((result (nreverse (delete-duplicates (copy-list slots)))))
    (dolist (parent (ls-object-parents x) (nreverse result))
      (dolist (slot (get-slot-value parent 'instance-slots))
        (pushnew slot result)))))

(defun get-initial-slot-value (object slot)
  (let ((entry (find-slot object slot)))
    (if (slot-entry-p entry) (slot-entry-value entry))))

(defun make-prototype (object name ivars cvars doc set)
  (setf ivars (find-instance-slots object ivars))
  (add-slot object 'instance-slots ivars)
  (add-slot object 'proto-name name)
  
  (dolist (slot ivars)
    (add-slot object slot (get-initial-slot-value object slot)))
  
  (dolist (slot cvars)
    (add-slot object slot nil))
    
  (if (and doc (stringp doc))
    (add-documentation object 'proto doc))
  (if set (setf (symbol-value name) object)))


(defmacro defproto (name &optional ivars cvars parents doc)
"Syntax (defproto name &optional ivars cvars (parent *object*) doc)
Makes a new object prototype with instance variables IVARS, 'class'
variables CVARS and parents PARENT. PARENT can be a single object or
a list of objects. IVARS and CVARS must be lists."
  (let ((obsym (gensym))
        (namesym (gensym))
        (parsym (gensym)))
    `(progn
       (let* ((,namesym ',name)
              (,parsym ,parents)
              (,obsym (make-basic-object (if (listp ,parsym) 
					     ,parsym 
					     (list ,parsym)) ;; should this be ,@parsym ? 
                                         nil)))
         (make-prototype ,obsym ,namesym ,ivars ,cvars ,doc t)
	 ,namesym))))


#|
(defmacro defproto (name &optional ivars cvars parents doc)
  "Syntax (defproto name &optional ivars cvars (parent *object*) doc)
Makes a new object prototype with instance variables IVARS, 'class'
variables CVARS and parents PARENT. PARENT can be a single object or
a list of objects. IVARS and CVARS must be lists."
  (if (boundp name)
      (error "name is bound") ; fixme: use real error
      (let ((obsym (gensym))
	    (namesym (gensym))
	    (parsym (gensym)))
	`(progn
	  (let* ((,namesym ',name)
		 (,parsym ,parents)
		 (,obsym (make-basic-object (if (listp ,parsym) 
						,parsym 
						(list ,parsym)) ;; should this be ,@parsym ? 
					    nil)))
	    (make-prototype ,obsym ,namesym ,ivars ,cvars ,doc t)
	    ,namesym)))))
|#



;; recall: 
;; , => turn on evaluation again (not macro substitution)
;; ` => 
;; ' => regular quote (not special in this context).


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;                   Initialize the Root Object
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf (ls-object-preclist *object*) (list *object*))
(add-slot *object* 'instance-slots nil)
(add-slot *object* 'proto-name '*object*)
(add-slot *object* 'documentation nil)  ; AJR - for SBCL compiler
					; issues about macro with
					; unknown slot  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;                       *OBJECT* Methods
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmeth *object* :isnew (&rest args)
"Method args: (&rest args)
Checks ARGS for keyword arguments matching slots and uses them to
initialize slots."
  (if args
    (dolist (slot-entry (ls-object-slots self))
      (let* ((slot (slot-entry-key slot-entry))
             (key (intern (symbol-name slot) (find-package 'keyword)))
             (val (slot-value slot))
             (new-val (getf args key val)))
        (unless (eq val new-val) (setf (slot-value slot) new-val)))))
  self)

(defmeth *object* :has-slot (slot &key own)
"Method args: (slot &optional own)
Returns T if slot SLOT exists, NIL if not. If OWN is not NIL
only checks the object; otherwise check the entire precedence list."
  (let ((entry (if own (find-own-slot self slot) (find-slot self slot))))
    (if entry t nil)))

(defmeth *object* :add-slot (slot &optional value)
"Method args: (slot &optional value)
Installs slot SLOT in object, if it does not already exist, and
sets its value to VLAUE."
  (add-slot self slot value)
  value)

(defmeth *object* :delete-slot (slot)
"Method args: (slot)
Deletes slot SLOT from object if it exists."
  (delete-slot self slot)
  nil)

(defmeth *object* :own-slots ()
"Method args: ()
Returns list of names of slots owned by object."
  (mapcar #'slot-entry-key (ls-object-slots self)))

(defmeth *object* :has-method (selector &key own)
"Method args: (selector &optional own)
Returns T if method for SELECTOR exists, NIL if not. If OWN is not NIL
only checks the object; otherwise check the entire precedence list."
  (let ((entry (if own 
                 (find-own-method self selector) 
                 (find-lsos-method self selector))))
    (if entry t nil)))

(defmeth *object* :add-method (selector method)
"Method args: (selector method)
Installs METHOD for SELECTOR in object."
  (add-lsos-method self selector method)
  nil)

(defmeth *object* :delete-method (selector)
"Method args: (selector)
Deletes method for SELECTOR in object if it exists."
  (delete-method self selector)
  nil)

(defmeth *object* :get-method (selector)
"Method args: (selector)
Returns method for SELECTOR symbol from object's precedence list."
  (get-message-method self selector))

(defmeth *object* :own-methods ()
"Method args ()
Returns copy of selectors for methods owned by object."
  (mapcar #'method-entry-key (ls-object-methods self)))

(defmeth *object* :parents ()
"Method args: ()
Returns copy of parents list."
  (copy-list (ls-object-parents self)))

(defmeth *object* :precedence-list ()
"Method args: ()
Returns copy of the precedence list."
  (copy-list (ls-object-preclist self)))

(defmeth *object* :show (&optional (stream t))
"Method Args: ()
Prints object's internal data."
  (format stream "Slots = ~s~%" (ls-object-slots self))
  (format stream "Methods = ~s~%" (ls-object-methods self))
  (format stream "Parents = ~s~%" (ls-object-parents self))
  (format stream "Precedence List  = ~s~%" (ls-object-preclist self))
  nil)

(defmeth *object* :reparent (&rest parents)
"Method args: (&rest parents)
Changes precedence list to correspond to PARENTS. Does not change descendants."
  (make-basic-object parents self))

(defmeth *object* :make-prototype (name &optional ivars)
  (make-prototype self name ivars nil nil nil)
  self)

(defmeth *object* :internal-doc (sym &optional new)
"Method args (topic &optional value)
Retrieves or installs documentation for topic."
  (if new (add-documentation self sym new))
  (get-documentation self sym))

(defmeth *object* :new (&rest args)
"Method args: (&rest args)
Creates new object using self as prototype."
  (let* ((object (make-object self)))
    (if (slot-value 'instance-slots)
        (dolist (s (slot-value 'instance-slots))
                (send object :add-slot s (slot-value s))))
    (apply #'send object :isnew args)
    object))

(defmeth *object* :retype (proto &rest args)
"Method args: (proto &rest args)
Changes object to inherit directly from prototype PROTO. PROTO
must be a prototype and SELF must not be one."
  (if (send self :has-slot 'instance-slots :own t) 
      (error "can't retype a prototype"))
  (if (not (send proto :has-slot 'instance-slots :own t))
      (error "not a prototype - ~a" proto))
  (send self :reparent proto)
  (dolist (s (send proto :slot-value 'instance-slots))
    (send self :add-slot s (slot-value s)))
  (apply #'send self :isnew args)
  self)

(defmeth *object* :print (&optional (stream *standard-output*))
"Method args: (&optional (stream *standard-output*))
Default object printing method."
  (cond
    ((send self :has-slot 'proto-name) 
     (format stream
             "#<Object: ~D, prototype = ~A>"
	     (ls-object-serial self)
             (slot-value 'proto-name)))
    (t (format stream "#<Object: ~D>" (ls-object-serial self)))))

(defmeth *object* :slot-value (sym &optional (val nil set))
	 "Method args: (sym &optional val)
Sets and retrieves value of slot named SYM. Signals an error if slot
does not exist."
    (if set (setf (slot-value sym) val))
    (slot-value sym))

(defmeth *object* :slot-names () 
"Method args: ()
Returns list of slots available to the object."
  (apply #'append 
         (mapcar #'(lambda (x) (send x :own-slots))
                 (send self :precedence-list))))

(defmeth *object* :method-selectors ()
"Method args: ()
Returns list of method selectors available to object."
  (apply #'append
         (mapcar #'(lambda (x) (send x :own-methods))
                 (send self :precedence-list))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;                      Object Help Methods
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmeth *object* :doc-topics ()
"Method args: ()
Returns all topics with documentation for this object."
  (remove-duplicates 
   (mapcar #'car 
           (apply #'append 
                  (mapcar 
                   #'(lambda (x) 
                       (if (send x :has-slot 'documentation :own t)
                           (send x :slot-value (quote documentation))))
                   (send self :precedence-list))))))

(defmeth *object* :documentation (topic &optional (val nil set))
"Method args: (topic &optional val)
Retrieves or sets object documentation for topic."
  (if set (send self :internal-doc topic val))
  (let ((val (dolist (i (send self :precedence-list))
                     (let ((val (send i :internal-doc topic))) 
                       (if val (return val))))))
    val))

(defmeth *object* :delete-documentation (topic)
"Method args: (topic)
Deletes object documentation for TOPIC."
  (setf (slot-value 'documentation)
        ;;(remove :title nil :test #'(lambda (x y) (eql x (first y)))) ;; original
	(remove topic (send self :documentation) :test #'(lambda (x y) (eql x (first y))))) ;; AJR:PROBLEM?
  nil)

(defmeth *object* :help (&optional topic)
"Method args: (&optional topic)
Prints help message for TOPIC, or genreal help if TOPIC is NIL."
  (if topic 
      (let ((doc (send self :documentation topic)))
        (cond 
          (doc (princ topic) (terpri) (princ doc) (terpri))
          (t (format t "Sorry, no help available on ~a~%" topic))))
      (let ((topics (stable-sort (copy-seq (send self :doc-topics))
                                 #'(lambda (x y)
                                     (string-lessp (string x) (string y)))))
            (proto-doc (send self :documentation 'proto)))
        (if (send self :has-slot 'proto-name)
	    (format t "~s~%" (slot-value 'proto-name)))
        (when proto-doc (princ proto-doc) (terpri))
        (format t "Help is available on the following:~%~%")
        (dolist (i topics) (format t "~s " i))
        (terpri)))
  (values))

(defmeth *object* :compile-method (name)
"Method args: (name)
Compiles method NAME unless it is already compiled. The object must
own the method."
  (unless (send self :has-method name)
	  (error "No ~s method in this object" name))
  (unless (send self :has-method name :own t)
	  (error "Object does not own ~s method" name))
  (let ((fun (send self :get-method name)))
    (unless (compiled-function-p fun)
	    (multiple-value-bind (form env) (function-lambda-expression fun)
		 (if env 
		     (error 
		      "method may have been defined in non-null environment"))
		 (send self :add-method name (compile nil form))))))
