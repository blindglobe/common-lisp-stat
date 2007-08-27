;;;-*- Mode: Lisp; Package: lift -*-

(in-package #:lift)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(test-mixin
	    testsuite-p
	    *test-result*
	    *current-test*
	    last-test-status
	    suite-tested-p
	    failures
	    errors
	    ensure-cases
	    ensure-random-cases
	    deftestsuite
	    addtest
	    remove-test
	    run-test
	    run-tests

	    measure-time
	    measure-conses
	    with-profile-report
          
	    ;; Variables
	    *test-ignore-warnings?*
	    *test-break-on-errors?*
	    *test-print-length*
	    *test-print-level*
	    *test-print-when-defined?*
	    *test-evaluate-when-defined?*
	    *test-describe-if-not-successful?*
	    *test-maximum-time*
	    *test-print-testsuite-names*
	    *test-print-test-case-names*

	    *test-scratchpad*
	    *test-notepad*
	    *lift-equality-test*
	    *lift-debug-output*
          
	    ;; Other
	    ensure
	    ensure-null
	    ensure-same
	    ensure-different
	    ensure-condition
	    ensure-warning
	    ensure-error
          
	    ;;?? Not yet
	    ;; with-test
          
	    list-tests
	    print-tests
	    map-testsuites
	    testsuites
	    testsuite-tests
	    
	    suite
	    find-testsuite
	    ensure-random-cases-failure
	    random-instance-for-suite
	    defrandom-instance
	    ensure-random-cases
	    ensure-random-cases+
	    random-element
	    random-number
	    an-integer
	    a-double-float
	    a-single-float
	    a-symbol

	    lift-result
	    lift-property)))

;;; ---------------------------------------------------------------------------
;;; shared stuff
;;; ---------------------------------------------------------------------------	

(defgeneric get-class (thing &key error?)
  (:documentation "Returns the class of thing or nil if the class cannot be found. Thing can be a class, an object representing a class or a symbol naming a class. Get-class is like find-class only not as particular.")
  (:method ((thing symbol) &key error?)
           (find-class thing error?))
  (:method ((thing standard-object) &key error?)
           (declare (ignore error?))
           (class-of thing))
  (:method ((thing t) &key error?) 
           (declare (ignore error?))
           (class-of thing))
  (:method ((thing class) &key error?)
           (declare (ignore error?))
           thing))

(defun direct-subclasses (thing)
  "Returns the immediate subclasses of thing. Thing can be a class, object or symbol naming a class."
  (class-direct-subclasses (get-class thing)))

(defun map-subclasses (class fn &key proper?)
  "Applies fn to each subclass of class. If proper? is true, then
the class itself is not included in the mapping. Proper? defaults to nil."
  (let ((mapped (make-hash-table :test #'eq)))
    (labels ((mapped-p (class)
               (gethash class mapped))
             (do-it (class root)
               (unless (mapped-p class)
                 (setf (gethash class mapped) t)
                 (unless (and proper? root)
                   (funcall fn class))
                 (mapc (lambda (class)
                         (do-it class nil))
                       (direct-subclasses class)))))
      (do-it (get-class class) t))))

(defun subclasses (class &key (proper? t))
  "Returns all of the subclasses of the class including the class itself."
  (let ((result nil))
    (map-subclasses class (lambda (class)
                            (push class result))
                    :proper? proper?)
    (nreverse result)))

(defun superclasses (thing &key (proper? t))
  "Returns a list of superclasses of thing. Thing can be a class, object or symbol naming a class. The list of classes returned is 'proper'; it does not include the class itself."
  (let ((result (class-precedence-list (get-class thing))))
    (if proper? (rest result) result)))

(defun direct-superclasses (thing)
  "Returns the immediate superclasses of thing. Thing can be a class, object or symbol naming a class."
  (class-direct-superclasses (get-class thing)))

(declaim (inline length-1-list-p)) 
(defun length-1-list-p (x) 
  "Is x a list of length 1?"
  (and (consp x) (null (cdr x))))

(defmacro defclass-property (property &optional (default nil default-supplied?))
  "Create getter and setter methods for 'property' on symbol's property lists." 
  (let ((real-name (intern (format nil "~:@(~A~)" property) :keyword)))
    `(progn
       (defgeneric ,property (symbol))
       (defgeneric (setf ,property) (value symbol))
       (defmethod ,property ((class-name symbol))
          (get class-name ,real-name ,@(when default-supplied? (list default))))
       (defmethod (setf ,property) (value (class-name symbol))
         (setf (get class-name ,real-name) value)))))

(defvar *automatic-slot-accessors?* nil)
(defvar *automatic-slot-initargs?* nil)
(defvar *clos-slot-options* 
  '(:initform :initarg :reader :writer 
    :accessor :documentation :type
    :allocation))

;;; ---------------------------------------------------------------------------

(defun parse-brief-slot
       (slot &optional
	     (automatic-accessors? *automatic-slot-accessors?*)
	     (automatic-initargs? *automatic-slot-initargs?*)
	     conc-name
             (conc-separator "-"))
  "Returns a verbose-style slot specification given a brief style, consisting of
a single symbol, the name of the slot, or a list of the slot name, optional
initform, optional symbol specifying whether there is an initarg, reader, or
accessor, and optional documentation string.  The specification of initarg,
reader and accessor is done by the letters I, R and A, respectively; to specify
none of those, give a symbol containing none of those letters, such as the
symbol *.  This function is used in the macro `defclass-brief,' but has been
broken out as a function in its own right for those writing variants on the
`defclass' macro.  If a verbose-style slot specification is given, it is
returned unchanged.

If `automatic-accessors?  is true, an accessor is defined, whether A is
specified or not _unless_ R is specified.  If `automatic-initargs?  is true, 
an initarg is defined whether I is specified or not.  If `conc-name' is
specified, the accessor name has that prepended, with conc-separator, and then 
the slot name. 

All other CLOS slot options are processed normally."
  
  ;; check types
  (etypecase slot
    (symbol (setf slot (list slot)))
    (list nil))
  
  (let* ((name (pop slot))
	 (new-slot (list name))
         (done-initform? nil)
         (done-spec? nil)
         (done-documentation? nil)
         (reader-added? nil)
         (accessor-added? nil)
         (initargs-added? nil))
    (flet ((make-conc-name ()
             (if conc-name
               (intern (format nil "~@:(~A~A~A~)"
			       conc-name conc-separator name))
               name))
           
           (add-option (option argument)
             (push option new-slot)
             (push argument new-slot))
           
           ;; Remove duplicate options before returning the slot spec.
           (finish-new-slot (slot)
             ;; XXX This code is overly loopy and opaque ---L
             (destructuring-bind (slot-name &rest options) slot
               (let ((opts (make-hash-table)))
                 (loop for (key val . d) = options then d
                       while key
                       doing (pushnew val (gethash key opts nil) :test #'equal))
                 (loop for key being each hash-key of opts using (hash-value vals)
                       nconc (mapcan #'(lambda (x) (list key x)) vals) into spec
                       finally (return (cons slot-name spec)))))))
      
      (do* ((items slot (rest items))
            (item (first items) (first items))
            (process-item? t t)
            (clos-item? (member item *clos-slot-options*) 
                        (member item *clos-slot-options*)))
           ((null items) nil)
        
        (unless done-initform?
          (setf done-initform? t)
          (unless clos-item?
            (setf  process-item? nil)
            (unless (eq item :UNBOUND)
              (push :initform new-slot)
              (push item new-slot))))
        
        (when process-item?
          (unless (or done-spec? (not (symbolp item)) clos-item?)
            (setf done-spec? t)
            (setf process-item? nil)
            ;; If you've got an A, who cares about R
            (when (find #\A (string item))
              (setf accessor-added? t)
              (add-option :accessor (make-conc-name)))
            (when (and (not accessor-added?) (find #\R (string item)))
              (setf reader-added? t)
              (add-option :reader (make-conc-name)))
            (when (find #\I (string item))
              (setf initargs-added? t)
              (add-option :initarg (intern (string name) 
					   (find-package :keyword))))))
        
        (when process-item?
          (unless (or done-documentation? (not (stringp item)))
            (setf done-documentation? t)
            (push :documentation new-slot)
            (push item new-slot)
            ))
        
        (when process-item?
          (when clos-item?
            (push item new-slot)
            (pop items)
            (push (first items) new-slot))))
      
      (when (and automatic-initargs? (not initargs-added?))
        (add-option :initarg (intern (string name) (find-package :keyword))))
      
      (when (and automatic-accessors? 
		 (and (not accessor-added?) (not reader-added?)))
        (add-option :accessor (make-conc-name)))
      
      ;; finish-new-slot cleans up duplicates
      (finish-new-slot (nreverse new-slot)))))

;;; ---------------------------------------------------------------------------

(defun convert-clauses-into-lists (clauses-and-options clauses-to-convert)
  ;; This is useful (for me at least!) for writing macros
  (let ((parsed-clauses nil))
    (do* ((clauses clauses-and-options (rest clauses))
          (clause (first clauses) (first clauses)))
         ((null clauses))
      (if (and (keywordp clause)
               (or (null clauses-to-convert) (member clause clauses-to-convert))
               (not (length-1-list-p clauses)))
        (progn
          (setf clauses (rest clauses))
          (push (list clause (first clauses)) parsed-clauses))
        (push clause parsed-clauses)))
    (nreverse parsed-clauses)))

(defun remove-leading-quote (list)
  "Removes the first quote from a list if one is there."
  (if (and (consp list) (eql (first list) 'quote))
    (first (rest list))
    list))

(defun cleanup-parsed-parameter (parameter)
  (if (length-1-list-p parameter)
    (first parameter)
    parameter))

;;; ---------------------------------------------------------------------------
;;; global environment thingies
;;; ---------------------------------------------------------------------------

(defparameter *make-testsuite-arguments*
  '(:run-setup :test-slot-names :equality-test :log-file :timeout))

(defvar *current-suite-class-name* nil)
(defvar *current-case-method-name* nil)

(defvar *test-is-being-defined?* nil)
(defvar *test-is-being-compiled?* nil)
(defvar *test-is-being-loaded?* nil)
(defvar *test-is-being-executed?* nil)

(defvar *testsuite-test-count* nil
  "Temporary variable used to 'communicate' between deftestsuite and addtest.")
(defvar *lift-debug-output* *debug-io*
  "Messages from LIFT will be sent to this stream. It can set to nil or
to an output stream. It defaults to *debug-io**.")

(defvar *test-break-on-errors?* nil)
(defvar *test-do-children?* t)
(defparameter *test-ignore-warnings?* nil
  "If true, LIFT will not cause a test to fail if a warning occurs while
the test is running. Note that this may interact oddly with ensure-warning.")
(defparameter *test-print-when-defined?* nil)
(defparameter *test-evaluate-when-defined?* t)
(defparameter *test-scratchpad* nil
  "A place to put things. This is set to nil before every test.")
(defparameter *test-notepad* nil
  "Another place to put things (set {ref *test-scratchpad*}.")

(defparameter *lift-equality-test* 'equal
  "The function used in ensure-same to test if two things are equal. If metatilities is loaded, then you might want to use samep.")

(defvar *test-describe-if-not-successful?* nil
  ;; Was t, but this behavior was extremely annoying since each
  ;; time a test-restul appears in a stack backtrace it is printed
  ;; over many unstructured lines.
  "If true, then a complete test description is printed when there are any test warnings or failures. Otherwise, one would need to explicity call describe.")

(defvar *test-print-length* :follow-print
  "The print-length in effect when LIFT prints test results. It works exactly like `*print-length*` except that it can also take on the value :follow-print. In this case, it will be set to the value of  `*print-length*`.")
(defvar *test-print-level* :follow-print
  "The print-level in effect when LIFT prints test results. It works exactly like `*print-level*` except that it can also take on the value :follow-print. In this case, it will be set to whatever `*print-level*` is.")

(defvar *test-print-testsuite-names* t
  "If true, LIFT will print the name of each test suite to *debug-io* before it begins to run the suite. See also: *test-print-test-case-names*.")

(defvar *test-print-test-case-names* nil
  "If true, LIFT will print the name of each test-case before it runs. See also: *test-print-testsuite-names*.")

(defvar *test-result* nil
  "Set to the most recent test result by calls to run-test or run-tests.")

(defvar *test-environment* nil)

(defvar *test-metadata* (list)
  "A place for LIFT to put stuff.")

(defvar *current-test* nil
  "The current testsuite.")

(defvar *lift-dribble-pathname* nil
  "If bound, then test output from run-tests will be sent to this file in 
in addition to *lift-standard-output*. It can be set to nil or to a pathname.")

(defvar *lift-standard-output* *standard-output*
  "Output from tests will be sent to this stream. If can set to nil or
to an output stream. It defaults to *standard-output*.")

(defvar *lift-if-dribble-exists* :append
  "Specifies what to do to any existing file at *lift-dribble-pathname*. It
can be :supersede, :append, or :error.")
  
;;; ---------------------------------------------------------------------------
;;; Error messages and warnings
;;; ---------------------------------------------------------------------------

(defparameter +lift-test-name-not-supplied-with-test-class+
  "if you specify a test-class, you must also specify a test-name.")

(defparameter +lift-test-class-not-found+
  "test class '~S' not found.")

(defparameter +lift-confused-about-arguments+
  "I'm confused about what you said?!")

(defparameter +lift-no-current-test-class+
  "There is no current-test-class to use as a default.")

(defparameter +lift-could-not-find-test+
  "Could not find test: ~S.~S")

(defparameter +run-tests-null-test-case+
  "There is no current testsuite (possibly because
   none have been defined yet?). You can specify the
   testsuite to test by evaluating (run-tests :suite <suitename>).")

(defparameter +lift-unable-to-parse-test-name-and-class+ 
  "")


;;; ---------------------------------------------------------------------------
;;; test conditions
;;; ---------------------------------------------------------------------------

(define-condition lift-compile-error (error)
                  ((msg :initform "" 
                        :reader msg
                        :initarg :lift-message))
  (:report (lambda (c s)
             (format s "Compile error: '~S'" (msg c)))))

;;; ---------------------------------------------------------------------------

(define-condition test-class-not-defined (lift-compile-error)
                  ((test-class-name :reader test-class-name
                                    :initarg :test-class-name))
  (:report (lambda (c s)
             (format s "Test class ~A not defined before it was used."
                     (test-class-name c)))))

;;; ---------------------------------------------------------------------------

(defun build-lift-error-message (context message &rest args)
  (format nil "~A: ~A" 
          context
          (apply #'format nil message args)))

;;; ---------------------------------------------------------------------------

(defun signal-lift-error (context message &rest args)
  (let ((c (make-condition  
            'lift-compile-error
            :lift-message (apply #'build-lift-error-message context message args))))
    (unless (signal c)
      (error c))))

;;; ---------------------------------------------------------------------------

(defun report-lift-error (context message &rest args)
  (format *debug-io* "~&~A."
          (apply #'build-lift-error-message context message args))
  (values))

;;; ---------------------------------------------------------------------------

(defun lift-report-condition (c)
  (format *debug-io* "~&~A." c))

;;; ---------------------------------------------------------------------------

(define-condition test-condition (warning) 
                  ((message :initform ""
                            :initarg :message
                            :accessor message))
  (:report (lambda (c s)
             (when (message c)
               (format s "~%~A" (message c))))))

;;; ---------------------------------------------------------------------------

(define-condition ensure-failed-error (test-condition) 
                  ((assertion :initform "" 
                              :accessor assertion
                              :initarg :assertion))
  (:report (lambda (c s)
             (format s "Ensure failed: ~S ~@[(~a)~]" 
		     (assertion c) (message c)))))

;;; ---------------------------------------------------------------------------

(define-condition ensure-null-failed-error (ensure-failed-error)
  ((value :initform "" 
	  :accessor value
	  :initarg :value))
  (:report (lambda (c s)
             (format s "Ensure null failed: ~S ~@[(~a)~]" 
		     (value c) (message c)))))

;;; ---------------------------------------------------------------------------

(define-condition ensure-expected-condition (test-condition) 
                  ((expected-condition-type
                    :initform nil
                    :accessor expected-condition-type
                    :initarg :expected-condition-type)
                   (the-condition
                    :initform nil
                    :accessor the-condition
                    :initarg :the-condition))
  (:report (lambda (c s)
             (format s "Expected ~A but got ~S" 
                     (expected-condition-type c)
                     (the-condition c)))))

;;; ---------------------------------------------------------------------------

(define-condition ensure-not-same (test-condition) 
                  ((first-value :accessor first-value
                                :initarg :first-value)
                   (second-value :accessor second-value
                                 :initarg :second-value)
                   (test :accessor test
                         :initarg :test))
  (:report (lambda (c s)
             (format s "Ensure-same: ~S is not ~S to ~S~@[ (~a)~]"
                     (first-value c) (test c) (second-value c)
		     (message c)))))

;; hacked list to take arguments in addition to args
(defmacro ensure (predicate &key report arguments)
  "If ensure's `predicate` evaluates to false, then it will generate a 
test failure. You can use the `report` and `arguments` keyword parameters
to customize the report generated in test results. For example:

    (ensure (= 23 12) 
     :report \"I hope ~a does not = ~a\" 
     :arguments (12 23))

will generate a message like

    Warning: Ensure failed: (= 23 12) (I hope 12 does not = 23)
"
  (let ((gpredicate (gensym)))
    `(let ((,gpredicate ,predicate))
       (if ,gpredicate
	   (values ,gpredicate)
	   (let ((condition (make-condition 
			     'ensure-failed-error 
			     :assertion ',predicate
			     ,@(when report
				     `(:message 
				       (format nil ,report ,@arguments))))))
	     (if (find-restart 'ensure-failed)
		 (invoke-restart 'ensure-failed condition) 
		 (warn condition)))))))

(defmacro ensure-null (predicate &key report arguments)
  "If ensure-null's `predicate` evaluates to true, then it will generate a 
test failure. You can use the `report` and `arguments` keyword parameters
to customize the report generated in test results. See [ensure][] for more 
details."
  (let ((g (gensym)))
    `(let ((,g ,predicate))
       (if (null ,g)
	   t
	 (let ((condition (make-condition 'ensure-null-failed-error
			    :value ,g
			    ,@(when report
				`(:message (format nil ,report ,@arguments))))))
	   (if (find-restart 'ensure-failed)
	       (invoke-restart 'ensure-failed condition) 
	     (warn condition)))))))

(defmacro ensure-condition (condition &body body)
  "This macro is used to make sure that body really does produce condition."
  (setf condition (remove-leading-quote condition))
  (destructuring-bind (condition &key report arguments)
                      (if (consp condition) condition (list condition))
    (let ((g (gensym)))
      `(let ((,g nil))
         (unwind-protect
           (handler-case 
             (progn ,@body)
             (,condition (cond) 
                         (declare (ignore cond)) (setf ,g t))
             (condition (cond) 
                        (setf ,g t)
                        (let ((c (make-condition 
                                  'ensure-expected-condition
                                  :expected-condition-type ',condition
                                  :the-condition cond
                                  ,@(when report
                                      `(:message (format nil ,report ,arguments))))))
                          (if (find-restart 'ensure-failed)
                            (invoke-restart 'ensure-failed c) 
                            (warn c)))))
           (when (not ,g)
             (if (find-restart 'ensure-failed)
               (invoke-restart
		'ensure-failed 
		(make-condition 
		 'ensure-expected-condition
		 :expected-condition-type ',condition
		 :the-condition nil
		 ,@(when report
			 `(:message (format nil ,report ,arguments))))) 
               (warn "Ensure-condition didn't get the condition it expected."))))))))

(defmacro ensure-warning (&body body)
  "Ensure-warning evaluates its body. If the body does *not* signal a 
warning, then ensure-warning will generate a test failure."
  `(ensure-condition warning ,@body))

(defmacro ensure-error (&body body)
  "Ensure-error evaluates its body. If the body does *not* signal an 
error, then ensure-error will generate a test failure."
  `(ensure-condition error ,@body))

(defmacro ensure-same
    (form values &key (test nil test-specified-p) 
     (report nil) (arguments nil))
  "Ensure same compares value-or-values-1 value-or-values-2 or each value of value-or-values-1 value-or-values-2 (if they are multiple values) using test. If a problem is encountered ensure-same raises a warning which uses report as a format string and arguments as arguments to that string (if report and arguments are supplied). If ensure-same is used within a test, a test failure is generated instead of a warning"
  (setf test (remove-leading-quote test))
  (when (and (consp test)
             (eq (first test) 'function))
    (setf test (second test)))
  `(progn
     (loop for value in (multiple-value-list ,form)
           for other-value in (multiple-value-list ,values) do
           (unless (funcall ,(if test-specified-p (list 'quote test) '*lift-equality-test*)
                            value other-value)
             (maybe-raise-not-same-condition 
              value other-value
              ,(if test-specified-p (list 'quote test) '*lift-equality-test*)
	      ,report ,@arguments)))
     (values t)))

(defmacro ensure-different
    (form values &key (test nil test-specified-p) 
     (report nil) (arguments nil))
  "Ensure-different compares value-or-values-1 value-or-values-2 or each value of value-or-values-1 and value-or-values-2 (if they are multiple values) using test. If any comparison returns true, then ensure-different raises a warning which uses report as a format string and `arguments` as arguments to that string (if report and `arguments` are supplied). If ensure-different is used within a test, a test failure is generated instead of a warning"
  ;; FIXME -- share code with ensure-same
  (setf test (remove-leading-quote test))
  (when (and (consp test)
             (eq (first test) 'function))
    (setf test (second test)))
  `(progn
     (loop for value in (multiple-value-list ,form)
           for other-value in (multiple-value-list ,values) do
	  ;; WHEN instead of UNLESS
           (when (funcall ,(if test-specified-p
				 (list 'quote test)
				 '*lift-equality-test*)
                            value other-value)
             (maybe-raise-not-same-condition 
              value other-value
              ,(if test-specified-p
		   (list 'quote test)
		   '*lift-equality-test*) ,report ,@arguments)))
     (values t)))

(defun maybe-raise-not-same-condition (value-1 value-2 test 
				       report &rest arguments)
  (let ((condition (make-condition 'ensure-not-same 
                                   :first-value value-1
                                   :second-value value-2
                                   :test test
                                   :message (when report
                                              (apply #'format nil 
						     report arguments)))))
    (if (find-restart 'ensure-failed)
      (invoke-restart 'ensure-failed condition) 
      (warn condition))))

(define-condition ensure-cases-failure (test-condition)
  ((total :initarg :total :initform 0)
   (problems :initarg :problems :initform nil))
  (:report (lambda (condition stream)
	     (format stream "Ensure-cases: ~d out of ~d cases failed. Failing cases are: ~{~%  ~{~s (~a)~}~^, ~}" 
		     (length (slot-value condition 'problems))
		     (slot-value condition 'total)
		     (slot-value condition 'problems)))))

(defmacro ensure-cases ((&rest vars) (&rest cases) &body body)
  (let ((case (gensym))
	(total (gensym))
	(problems (gensym)))
    `(let ((,problems nil) (,total 0))
       (loop for ,case in ,cases do
	    (incf ,total)
	    (destructuring-bind ,vars ,case
	      (restart-case
		  (progn ,@body)
		(ensure-failed (cond)
		  (push (list ,case cond) ,problems)))))
       (when ,problems
	 (let ((condition (make-condition 
			   'ensure-cases-failure
			   :total ,total
			   :problems ,problems)))
	   (if (find-restart 'ensure-failed)
	       (invoke-restart 'ensure-failed condition) 
	       (warn condition)))))))


;;; ---------------------------------------------------------------------------
;;; test-mixin
;;; ---------------------------------------------------------------------------

(defclass test-mixin ()
  ((name :initform nil :initarg :name :accessor name :reader testsuite-name)
   (run-setup :reader run-setup :initarg :run-setup)
   (done-setup? :initform nil :reader done-setup?)
   (done-dynamics? :initform nil :reader done-dynamics?)
   (prototypes :initform (list (list)) :accessor prototypes)
   (prototypes-initialized? :initform nil :reader prototypes-initialized?)
   (current-values :initform nil :accessor current-values)
   (test-slot-names :initform nil :initarg :test-slot-names 
		    :reader test-slot-names)
   (current-step :initform :created :accessor current-step)
   (current-method :initform nil :accessor current-method)
   (save-equality-test :initform nil  :reader save-equality-test)
   (equality-test :initform 'equal :initarg :equality-test 
		  :reader equality-test)
   (log-file :initform nil :initarg :log-file :reader log-file)
   (test-data :initform nil :accessor test-data)
   (expected-failure-p :initform nil :initarg :expected-failure-p
		       :reader expected-failure-p)
   (expected-error-p :initform nil :initarg :expected-error-p
		     :reader expected-error-p)
   (expected-problem-p :initform nil :initarg :expected-problem-p
		       :reader expected-problem-p))
  (:documentation "A test suite")
  (:default-initargs
    :run-setup :once-per-test-case))

(defclass test-result ()
  ((results-for :initform nil 
		:initarg :results-for 
		:accessor results-for)
   (tests-run :initform nil :accessor tests-run)
   (suites-run :initform nil :accessor suites-run)
   (failures :initform nil :accessor failures)
   (expected-failures :initform nil :accessor expected-failures)
   (errors :initform nil :accessor errors)
   (expected-errors :initform nil :accessor expected-errors)
   (test-mode :initform :single :initarg :test-mode :accessor test-mode)
   (test-interactive? :initform nil 
                      :initarg :test-interactive? :accessor test-interactive?)
   (real-start-time :initarg :real-start-time :reader real-start-time)
   (start-time :accessor start-time :initform nil)
   (end-time :accessor end-time)
   (real-end-time :accessor real-end-time)
   (real-start-time-universal
    :initarg :real-start-time-universal :reader real-start-time-universal)
   (start-time-universal :accessor start-time-universal :initform nil)
   (end-time-universal :accessor end-time-universal)
   (real-end-time-universal :accessor real-end-time-universal)
   (properties :initform nil :accessor test-result-properties))
  (:default-initargs
    :test-interactive? *test-is-being-defined?*
    :real-start-time (get-internal-real-time)
    :real-start-time-universal (get-universal-time)))

(defun test-result-property (result property)
  (getf (test-result-properties result) property))

(defun (setf test-result-property) (value result property)
  (setf (getf (test-result-properties result) property) value))

(defun print-lift-message (message &rest args)
  (apply #'format *lift-debug-output* message args)
  (force-output *lift-debug-output*))

(defgeneric testsuite-setup (testsuite result)
  (:documentation "Setup at the testsuite-level")
  (:method ((testsuite test-mixin) (result test-result))
           (values))
  (:method :before ((testsuite test-mixin) (result test-result))
	   (when (and *test-print-testsuite-names*
		      (eq (test-mode result) :multiple))
	     (print-lift-message "~&Start: ~a" (type-of testsuite)))
	   (push (type-of testsuite) (suites-run result))
           (setf (current-step testsuite) :testsuite-setup)))

(defgeneric testsuite-run (testsuite result)
  (:documentation "Run the cases in this suite and it's children."))

(defgeneric testsuite-teardown (testsuite result)
  (:documentation "Cleanup at the testsuite level.")
  (:method ((testsuite test-mixin) (result test-result))
    ;; no-op
    )
  (:method :after ((testsuite test-mixin) (result test-result))
    (setf (current-step testsuite) :testsuite-teardown
	  (real-end-time result) (get-internal-real-time)
	  (real-end-time-universal result) (get-universal-time))))

(defgeneric more-prototypes-p (testsuite)
  (:documentation "Returns true if another prototype set exists for the case."))

(defgeneric initialize-prototypes (testsuite)
  (:documentation "Creates lists of all prototype sets."))

(defgeneric next-prototype (testsuite)
  (:documentation "Ensures that the test environment has the values of the next prototype set."))

(defgeneric make-single-prototype (testsuite))

(defgeneric setup-test (testsuite)
  (:documentation "Setup for a test-case. By default it does nothing."))

(defgeneric teardown-test (testsuite)
  (:documentation "Tear-down a test-case. By default it does nothing.")
  (:method-combination progn :most-specific-first))

(defgeneric testsuite-methods (testsuite)
  (:documentation "Returns a list of the test methods defined for test. I.e.,
the methods that should be run to do the tests for this test."))

(defgeneric lift-test (suite name)
  (:documentation ""))

(defgeneric do-testing (testsuite result fn)
  (:documentation ""))

(defgeneric end-test (result case method-name)
  (:documentation ""))

(defgeneric initialize-test (test)
  (:documentation ""))

(defgeneric run-test-internal (case name result)
  (:documentation ""))

(defgeneric run-tests-internal (case &key result)
  (:documentation ""))

(defgeneric start-test (result case method-name)
  (:documentation ""))

(defgeneric test-report-code (testsuite method)
  (:documentation ""))

(defgeneric testsuite-p (thing)
  (:documentation "Determine whether or not `thing` is a testsuite. Thing can be a symbol naming a suite, a subclass of `test-mixin` or an instance of a test suite. Returns nil if `thing` is not a testsuite and the symbol naming the suite if it is."))

(defgeneric testsuite-name->gf (case name)
  (:documentation ""))

(defgeneric testsuite-name->method (class name)
  (:documentation ""))

(defmethod setup-test :before ((test test-mixin))
  (setf *test-scratchpad* nil
	(current-step test) :test-setup))

(defmethod setup-test ((test test-mixin))
  (values))

(defmethod teardown-test progn ((test test-mixin))
  (values))

(defmethod teardown-test :around ((test test-mixin))
  (setf (current-step test) :test-teardown)
  (call-next-method))

(defmethod initialize-test ((test test-mixin))
  (values))

(defmethod initialize-test :before ((test test-mixin))
  ;; only happens once
  (initialize-prototypes test) 
  (next-prototype test))

(defmethod initialize-instance :after ((testsuite test-mixin) &key)
  (when (null (testsuite-name testsuite))
    (setf (slot-value testsuite 'name) 
	  (symbol-name (type-of testsuite)))))

(defmethod print-object ((tc test-mixin) stream)
  (print-unreadable-object (tc stream :identity t :type t)
    (format stream "~a" (testsuite-name tc))))

;;; ---------------------------------------------------------------------------
;;; macros
;;; ---------------------------------------------------------------------------

(defvar *current-definition* nil
  "An associative-container which saves interesting information about
the thing being defined.")

(defun initialize-current-definition ()
  (setf *current-definition* nil))

(defun set-definition (name value)
  (let ((current (assoc name *current-definition*)))
    (if current
      (setf (cdr current) value)
      (push (cons name value) *current-definition*)))
    (values value))

(defun def (name &optional (definition *current-definition*))
  (when definition (cdr (assoc name definition))))

(defun (setf def) (value name)
  (set-definition name value))

(defvar *code-blocks* nil)

(defstruct (code-block (:type list) (:conc-name nil))
  block-name (priority 0) filter code operate-when)

(defgeneric block-handler (name value)
  (:documentation "")
  (:method ((name t) (value t))
           (error "Unknown clause: ~A" name)))

(defun add-code-block (name priority operate-when filter handler code)
  (let ((current (assoc name *code-blocks*))
        (value (make-code-block
                :operate-when operate-when
                :block-name name
                :priority priority
                :filter filter
                :code code)))
    (if current
      (setf (cdr current) value)
      (push (cons name value) *code-blocks*))
    (eval 
     `(defmethod block-handler ((name (eql ',name)) value)
        (declare (ignorable value))
        ,@handler)))    
    (setf *code-blocks* (sort *code-blocks* #'< 
			      :key (lambda (name.cb)
				     (priority (cdr name.cb))))))

(defmacro with-test-slots (&body body)
  `(symbol-macrolet ((lift-result (getf (test-data *current-test*) :result)))   
     (symbol-macrolet
	 ,(mapcar #'(lambda (local)
		      `(,local (test-environment-value ',local)))
		  (def :slot-names))
       (macrolet
	   ,(mapcar (lambda (spec)
		      (destructuring-bind (name arglist) spec
			`(,name ,arglist 
				`(flet-test-function 
				  *current-test* ',',name ,,@arglist))))
		    (def :function-specs))
	 (progn ,@body)))))

(defvar *deftest-clauses*
  '(:setup :teardown :test :documentation :tests :export-p :export-slots
    :run-setup :dynamic-variables :equality-test :categories :function))

(defmacro deftest (testsuite-name superclasses slots &rest
                                  clauses-and-options) 
  "The `deftest` form is obsolete, see [deftestsuite][]."
  
  (warn "Deftest is obsolete, use deftestsuite instead.")
  `(deftestsuite ,testsuite-name ,superclasses ,slots ,@clauses-and-options))

(setf *code-blocks* nil)

(add-code-block
 :setup 1 :methods
 (lambda () (or (def :setup) (def :direct-slot-names))) 
 '((setf (def :setup) (cleanup-parsed-parameter value)))
 'build-setup-test-method)

(add-code-block
 :teardown 100 :methods
 (lambda () (or (def :teardown) (def :direct-slot-names))) 
 '((setf (def :teardown) (cleanup-parsed-parameter value)))
 'build-test-teardown-method)

(add-code-block
 :function 0 :methods
 (lambda () (def :functions))
 '((push value (def :functions)))
 'build-test-local-functions)

(add-code-block
 :documentation 0 :class-def 
 nil 
 '((setf (def :documentation) (first value)))
 nil)

(add-code-block
 :export-p 0 :class-def
 nil 
 '((setf (def :export-p) (first value)))
 nil)

(add-code-block
 :export-slots 0 :class-def
 nil 
 '((setf (def :export-slots) (first value)))
 nil)

(add-code-block
 :run-setup 0 :class-def
 nil 
 '((push (first value) (def :default-initargs))
   (push :run-setup (def :default-initargs))
   (setf (def :run-setup) (first value)))
 nil)

(add-code-block
 :equality-test 0 :class-def
 nil 
 '((push (first value) (def :default-initargs))
   (push :equality-test (def :default-initargs)))
 nil)

(add-code-block
 :log-file 0 :class-def
 nil 
 '((push (first value) (def :default-initargs))
   (push :log-file (def :default-initargs)))
 nil)

(add-code-block
 :dynamic-variables 0 :class-def
 nil 
 '((setf (def :direct-dynamic-variables) value))
 nil)

(add-code-block
 :categories 0 :class-def
 nil 
 '((push value (def :categories)))
 nil)

(defmacro no-handler-case (form &rest cases)
  (declare (ignore cases))
  `,form)

(defmacro deftestsuite (testsuite-name superclasses slots &rest
                                  clauses-and-options) 
  "
Creates a testsuite named `testsuite-name` and, optionally, the code required for test setup, test tear-down and the actual test-cases. A testsuite is a collection of test-cases and other testsuites.

Test suites can have multiple superclasses (just like the classes that they are). Usually, these will be other test classes and the class hierarchy becomes the test case hierarchy. If necessary, however, non-testsuite classes can also be used as superclasses.

Slots are specified as in defclass with the following additions:

* Initargs and accessors are automatically defined. If a slot is named`my-slot`, then the initarg will be `:my-slot` and the accessors will be `my-slot` and `(setf my-slot)`. 
* If the second argument is not a CLOS slot option keyword, then it will be used as the `:initform` for the slot. I.e., if you have

        (deftestsuite my-test ()
          ((my-slot 23)))

    then `my-slot` will be initialized to 23 during test setup.

Test options are one of :setup, :teardown, :test, :tests, :documentation, :export-p, :dynamic-variables, :export-slots, :function, :categories, :run-setup, or :equality-test. 

* :categories - a list of symbols. Categories allow you to groups tests into clusters outside of the basic hierarchy. This provides finer grained control on selecting which tests to run. May be specified multiple times.

* :documentation - a string specifying any documentation for the test. Should only be specified once.

* :dynamic-variables - a list of atoms or pairs of the form (name value). These specify any special variables that should be bound in a let around the body of the test. The name should be symbol designating a special variable. The value (if supplied) will be bound to the variable. If the value is not supplied, the variable will be bound to nil. Should only be specified once.

* :equality-test - the name of the function to be used by default in calls to ensure-same and ensure-different. Should only be supplied once. 

* :export-p - If true, the testsuite name will be exported from the current package. Should only be specified once.

* :export-slots - if true, any slots specified in the test suite will be exported from the current package. Should only be specified once.

* :function - creates a locally accessible function for this test suite. May be specified multiple times. 

* :run-setup - specify when to run the setup code for this test suite. Allowed values are 

    * :once-per-test-case or t (the default)
    * :once-per-session
    * :once-per-suite
    * :never or nil

    :run-setup is handy when a testsuite has a time consuming setup phase that you do not want to repeat for every test.

* :setup - a list of forms to be evaluated before each test case is run.  Should only be specified once.

* :teardown - a list of forms to be evaluated after each test case is run. Should only be specified once.

* :test - Define a single test case. Can be specified multiple times.

* :tests - Define multiple test cases for this test suite. Can be specified multiple times.

"
  #+no-lift-tests
  `(values)
  #-no-lift-tests
  (let ((test-list nil)
        (options nil)
        (return (gensym)))
    ;; convert any clause like :setup foo into (:setup foo)
    (setf clauses-and-options 
          (convert-clauses-into-lists clauses-and-options *deftest-clauses*))
    (initialize-current-definition)
    (setf (def :testsuite-name) testsuite-name)
    (setf (def :superclasses) (mapcar #'find-testsuite superclasses))
    (setf (def :deftestsuite) t)
    ;; parse clauses into defs
    (loop for clause in clauses-and-options do
          (typecase clause
            (symbol (pushnew clause options))
            (cons (destructuring-bind (kind &rest spec) clause
                    (case kind
                      (:test (push (first spec) test-list))
                      (:tests 
                       (loop for test in spec do
                             (push test test-list)))
                      (t (block-handler kind spec)))))
            (t (error "When parsing ~S" clause))))
    (let ((slot-names nil) (slot-specs nil))
      (loop for slot in (if (listp slots) slots (list slots)) do 
            (push (if (consp slot) (first slot) slot) slot-names)
            (push (parse-brief-slot slot nil nil nil nil) slot-specs))
      (setf (def :slot-specs) (nreverse slot-specs)
            (def :direct-slot-names) (nreverse slot-names)
            (def :slots-parsed) t))
    ;;?? issue 27: breaks 'encapsulation' of code-block mechanism
    (setf (def :function-specs)
	  (loop for spec in (def :functions) collect
	       (destructuring-bind (name arglist &body body) (first spec)
		 (declare (ignore body))
		 `(,name ,arglist))))
    ;;?? needed
    (empty-test-tables testsuite-name)
    (compute-superclass-inheritence)
    (prog2
     (setf *testsuite-test-count* 0)
     `(eval-when (:compile-toplevel :load-toplevel :execute)
        (eval-when (:compile-toplevel)
          (push ',return *test-is-being-compiled?*))
        (eval-when (:load-toplevel)
          (push ',return *test-is-being-loaded?*))
        (eval-when (:execute)
          (push ',return *test-is-being-executed?*))
	(unwind-protect
	     (let (#+MCL (ccl:*warn-if-redefine* nil)
			 (*test-is-being-defined?* t))
	       (no-handler-case 
                (progn
                  ;; remove previous methods (do this 
		  ;; _before_ we define the class)
					;#+(or)
                  (remove-previous-definitions ',(def :testsuite-name))
                  (setf *current-case-method-name* nil)
                  ;; and then redefine the class
		  ,(build-test-class)
                  (setf *current-suite-class-name* ',(def :testsuite-name)
			(test-slots ',(def :testsuite-name)) 
			',(def :slot-names)
			(testsuite-dynamic-variables ',(def :testsuite-name))
			',(def :dynamic-variables)
			;;?? issue 27: breaks 'encapsulation' of code-block mechanism
			(testsuite-function-specs ',(def :testsuite-name))
			',(def :function-specs))
                  ,@(when (def :export-p)
			  `((export '(,(def :testsuite-name)))))
                  ,@(when (def :export-slots?)
			  `((export ',(def :direct-slot-names))))
                  ;; make a place to save test-case information
                  (empty-test-tables ',(def :testsuite-name))
;;; create methods
                  ;; setup :before
		  (eval-when (:load-toplevel :execute)
		    ,@(build-initialize-test-method) 
		    ,@(loop for (nil . block) in *code-blocks* 
			 when (and block 
				   (code block)
				   (eq (operate-when block) :methods)
				   (or (not (filter block))
				       (funcall (filter block)))) collect
			 (funcall (code block)))
		    ,@(when (def :dynamic-variables)
			    `((defmethod do-testing :around
				  ((suite ,(def :testsuite-name)) result fn) 
				(declare (ignore result fn))
				(cond ((done-dynamics? suite)
				       (call-next-method))
				      (t
				       (setf (slot-value suite 'done-dynamics?) t)
				       (let* (,@(build-dynamics))
					 (call-next-method)))))))
		    ;; tests
		    ,@(when test-list
			    `((let ((*test-evaluate-when-defined?* nil))
				,@(loop for test in (nreverse test-list) collect
				       `(addtest (,(def :testsuite-name)) 
					  ,@test))
				(setf *testsuite-test-count* nil))))
		    ,(if *test-evaluate-when-defined?* 
			 `(unless (or *test-is-being-compiled?*
				      *test-is-being-loaded?*)
			    (let ((*test-break-on-errors?* *test-break-on-errors?*))
			      (run-tests :suite ',testsuite-name)))
			 `(find-class ',testsuite-name))))
                (condition (c) 
			   (break)
			   (setf *testsuite-test-count* nil)
			   (lift-report-condition c))))
	  ;; cleanup
	  (setf *test-is-being-compiled?* 
		(remove ',return *test-is-being-compiled?*))
	  (setf *test-is-being-loaded?* 
		(remove ',return *test-is-being-loaded?*))
	  (setf *test-is-being-executed?* 
		(remove ',return *test-is-being-executed?*)))))))
 
(defun compute-superclass-inheritence ()
  ;;?? issue 27: break encapsulation of code blocks
  ;;?? we assume that we won't have too deep a hierarchy or too many 
  ;; dv's or functions so that having lots of duplicate names is OK
  (let ((slots nil)
	(dynamic-variables nil)
	(function-specs nil))
    (dolist (super (def :superclasses))
      (cond ((find-testsuite super)
	     (setf slots (append slots (test-slots super))
		   dynamic-variables 
		   (append dynamic-variables 
			   (testsuite-dynamic-variables super))
		   function-specs
		   (append function-specs 
			   (testsuite-function-specs super))))
	    (t
	     (error 'test-class-not-defined :test-class-name super))))
    (setf (def :slot-names) 
	  (remove-duplicates (append (def :direct-slot-names) slots))
	  (def :dynamic-variables)
	  (remove-duplicates 
	   (append (def :direct-dynamic-variables) dynamic-variables))
	  (def :function-specs)
	  (remove-duplicates 
	   (append (def :function-specs) function-specs)))
    (setf (def :superclasses)
	  (loop for class in (def :superclasses) 
	     unless (some (lambda (oter)
			    (and (not (eq class oter))
				 (member class (superclasses oter))))
			  (def :superclasses)) collect
	     class))))

(defmacro addtest (name &body test)
  "Adds a single new test-case to the most recently defined testsuite."
  #+no-lift-tests
  `nil
  #-no-lift-tests
  (no-handler-case 
    (let ((body nil)
          (return (gensym))
	  (options nil)
	  (looks-like-suite-name (looks-like-suite-name-p name))
	  (looks-like-code (looks-like-code-p name)))
      (cond ((and looks-like-suite-name looks-like-code)
	     (error "Can't disambiguate suite name from possible code."))
	    (looks-like-suite-name
             ;; testsuite given
             (setf (def :testsuite-name) (first name) 
		   options (rest name)
		   name nil body test))
            (t
             ;; the 'name' is really part of the test...
             (setf body (cons name test))))
      (unless (def :testsuite-name)
        (when *current-suite-class-name*
          (setf (def :testsuite-name) *current-suite-class-name*)))
      (unless (def :testsuite-name)
        (signal-lift-error 'add-test +lift-no-current-test-class+))
      (unless (or (def :deftestsuite) 
                  (find-testsuite (def :testsuite-name)))
        (signal-lift-error 'add-test +lift-test-class-not-found+
                           (def :testsuite-name)))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (eval-when (:compile-toplevel)
           (push ',return *test-is-being-compiled?*))
         (eval-when (:load-toplevel)
           (push ',return *test-is-being-loaded?*))
         (eval-when (:execute)
           (push ',return *test-is-being-executed?*))
         (unwind-protect
           (let ((*test-is-being-defined?* t))
             ,(build-test-test-method (def :testsuite-name) body options)
             (setf *current-suite-class-name* ',(def :testsuite-name))
             (if *test-evaluate-when-defined?*
               (unless (or *test-is-being-compiled?*
                           *test-is-being-loaded?*)
                 (let ((*test-break-on-errors?* (testing-interactively-p)))
                   (run-test)))
               (values)))
           ;; cleanup
           (setf *test-is-being-compiled?* 
		 (remove ',return *test-is-being-compiled?*)
		 *test-is-being-loaded?*
		 (remove ',return *test-is-being-loaded?*)
		 *test-is-being-executed?*
		 (remove ',return *test-is-being-executed?*)))))
    (condition (c) 
               (lift-report-condition c))))

(defun looks-like-suite-name-p (form)
  (and (consp form)
       (atom (first form))
       (find-testsuite (first form))
       (property-list-p (rest form))))

(defun property-list-p (form)
  (and (listp form)
       (block check-it
	 (let ((even? t))
	   (loop for x in form 
	      for want-keyword? = t then (not want-keyword?) do
		(when (and want-keyword? (not (keywordp x)))
		  (return-from check-it nil))
		(setf even? (not even?)))
	   (return-from check-it even?)))))

#|
(property-list-p '(:a :b))
(property-list-p '(:a 2 :b 3 :c 5 :d 8))
(property-list-p nil)

(property-list-p 3)
(property-list-p '(3))
(property-list-p '(3 :a))
(property-list-p '(:a 3 :b))
|#

(defun looks-like-code-p (name)
  (declare (ignore name))
  ;; FIXME - stub
  nil)

(defun remove-test (&key (name *current-case-method-name*)
                         (suite *current-suite-class-name*))
  (assert suite nil "Test suite could not be determined.")
  (assert name nil "Test name could not be determined.")
  (setf (testsuite-tests suite)
	(remove name (testsuite-tests suite))))

(defun run-test (&rest args
		 &key (name *current-case-method-name*)
		 (suite *current-suite-class-name*) 
		 (break-on-errors? *test-break-on-errors?*)
		 (do-children? *test-do-children?*)
		 (result nil))
  (assert suite nil "Test suite could not be determined.")
  (assert name nil "Test name could not be determined.")
  (let* ((*test-break-on-errors?* break-on-errors?)
         (*test-do-children?* do-children?)
         (*current-test* (make-testsuite suite args)))
    (unless result
      (setf result (make-test-result suite :single)))
    (setf *current-case-method-name* name 
          *current-suite-class-name* suite)
    (do-testing *current-test* result 
                (lambda () 
                  (run-test-internal *current-test* name result)))))

(defun make-testsuite (suite args)
  (let ((make-instance-args nil))
    (loop for keyword in *make-testsuite-arguments* do
       (when (member keyword args)
	 (push keyword make-instance-args)
	 (push (getf args keyword) make-instance-args)))
    (apply #'make-instance (find-testsuite suite) make-instance-args)))

(defmethod do-testing ((testsuite test-mixin) result fn)
  (unwind-protect
       (progn
	 (testsuite-setup testsuite result)
	 (let ((*lift-equality-test* (equality-test testsuite)))
	   (do ()
	       ((not (more-prototypes-p testsuite)) result)
	     (initialize-test testsuite) 
	     (funcall fn))))
    ;; cleanup
    (testsuite-teardown testsuite result))
  (values result))

(defmethod run-tests-internal ((suite symbol) &rest args &key &allow-other-keys)
  (let ((*current-test* (make-testsuite suite args))) 
    (apply #'run-tests-internal 
	   *current-test*
	   args)))

(defmethod run-tests-internal 
    ((case test-mixin) &key 
     (result (make-test-result (class-of case) :multiple))
     (do-children? *test-do-children?*))
  (let ((*test-do-children?* do-children?))
    (do-testing case result
		(lambda ()
		  (testsuite-run case result)))
    (setf *test-result* result)))

#+Later
(defmacro with-test (&body forms)
  "Execute forms in the context of the current test class."
  (let* ((test-class-name *current-suite-class-name*)
         (test-case (make-instance test-class)))
    `(eval-when (:execute)
       (prog2
        (setup-test ,test-case)
        (progn
          (with-test-slots ,@forms))
        (teardown-test ,test-case)))))

(defun map-testsuites (fn start-at)
  (let ((visited (make-hash-table)))
    (labels ((do-it (suite level)
	       (unless (gethash suite visited)
		 (setf (gethash suite visited) t)
		 (funcall fn suite level)
		 (loop for subclass in (subclasses suite :proper? t) do
		      (do-it subclass (1+ level))))))
    (do-it (find-class (find-testsuite start-at) nil) 0))))

(defun testsuites (&optional (start-at 'test-mixin))
  "Returns a list of testsuite classes. The optional parameter provides
control over where in the test hierarchy the search begins."
  (let ((result nil))
    (map-testsuites (lambda (suite level)
		      (declare (ignore level))
		      (push suite result))
		    start-at)
    (nreverse result)))

(defun print-tests (&key (include-cases? t) (start-at 'test-mixin) (stream t))
  "Prints all of the defined test classes from :start-at on down." 
  (map-testsuites
   (lambda (suite level)
     (let ((indent (coerce (make-list (* level 3) :initial-element #\Space)
			   'string))
	   (name (class-name suite)))
       (format stream "~&~a~s (~:d)" 
	       indent
	       name
	       (length (testsuite-methods name)))
       (when include-cases?
	 (loop for method-name in (testsuite-tests name) do
	      (format stream "~&~a  ~a" indent method-name)))))
   start-at))
     
(defun list-tests (&key (include-cases? t) (start-at 'test-mixin) (stream t))
  "Lists all of the defined test classes from :start-at on down." 
  (mapc (lambda (subclass-name)
          (format stream "~&~s (~:d)" 
                  subclass-name
                  (length (testsuite-methods subclass-name)))
          (when include-cases?
            (loop for method-name in (testsuite-tests subclass-name) do
                  (format stream "~&  ~a" method-name))))
        (testsuites start-at))
  (values))

(defun testsuite-test-count (testsuite)
  (or (and *testsuite-test-count* 
           (prog1 *testsuite-test-count* (incf *testsuite-test-count*))) 
      (length (testsuite-methods testsuite))))

(defun run-tests (&rest args &key 
		  (suite nil)
		  (break-on-errors? *test-break-on-errors?*)
		  (config nil)
		  (dribble *lift-dribble-pathname*)
		  (result (make-test-result (or suite config) :multiple))
					;run-setup
		  &allow-other-keys)
  "Run all of the tests in a suite. Arguments are :suite, :result,
:do-children? and :break-on-errors?" 
  (remf args :suite)
  (remf args :break-on-errors?)
  (remf args :run-setup)
  (remf args :dribble)
  (cond ((and suite config)
	 (error "Specify either configuration file or test suite
but not both."))
	(config
	 (run-tests-from-file config))
	((or suite (setf suite *current-suite-class-name*))
	 (let* ((*test-break-on-errors?* break-on-errors?)
		(dribble-stream
		 (when dribble
		   (open dribble
			 :direction :output
			 :if-does-not-exist :create
			 :if-exists *lift-if-dribble-exists*)))
		(*standard-output* (maybe-add-dribble 
				    *lift-standard-output* dribble-stream))
		(*error-output* (maybe-add-dribble 
				 *error-output* dribble-stream))
		(*debug-io* (maybe-add-dribble 
			     *debug-io* dribble-stream)))
	   (unwind-protect
		(dolist (name (if (consp suite) suite (list suite)))
		  (setf *current-suite-class-name* name)
		  (apply #'run-tests-internal name :result result args))
	     ;; cleanup
	     (when dribble-stream 
	       (close dribble-stream)))
	   ;; FIXME -- ugh!
	   (setf (tests-run result) (reverse (tests-run result)))
	   (values result)))
	(t
	 (error "There is not a current test suite and neither suite
nor configuration file options were specified."))))

(defun maybe-add-dribble (stream dribble-stream)
  (if dribble-stream
      (values (make-broadcast-stream stream dribble-stream) t)
      (values stream nil)))

(defmethod testsuite-run ((case test-mixin) (result test-result))
  (unless (start-time result)
    (setf (start-time result) (get-internal-real-time)
	  (start-time-universal result) (get-universal-time)))
  (unwind-protect
       (let ((methods (testsuite-methods case)))
	 (loop for method in methods do
	      (run-test-internal case method result))
	 (when *test-do-children?*
	   (loop for subclass in (direct-subclasses (class-of case))		
	      when (and (testsuite-p subclass)
			(not (member (class-name subclass) 
				     (suites-run result)))) do
	      (run-tests-internal (class-name subclass)
				  :result result))))
    (setf (end-time result) (get-universal-time))))

(defmethod more-prototypes-p ((testsuite test-mixin))
  (not (null (prototypes testsuite))))

(defmethod initialize-prototypes ((testsuite test-mixin))
  (setf (prototypes testsuite)
	(list (make-single-prototype testsuite))))
	
(defmethod make-single-prototype ((testsuite test-mixin))
  nil)

(defmethod initialize-prototypes :around ((suite test-mixin))
  (unless (prototypes-initialized? suite)
    (setf (slot-value suite 'prototypes-initialized?) t)
    (call-next-method)))

(defmethod next-prototype ((testsuite test-mixin))
  (setf (current-values testsuite) (first (prototypes testsuite)) 
        (prototypes testsuite) (rest (prototypes testsuite)))
    (dolist (key.value (current-values testsuite))
      (setf (test-environment-value (car key.value)) (cdr key.value))))

(defmethod run-test-internal ((case test-mixin) (name symbol) result) 
  (when (and *test-print-test-case-names*
	     (eq (test-mode result) :multiple))
    (print-lift-message "~&  run: ~a" name))
  (let ((problem nil))
    ;;??
    (declare (ignorable problem))
    (tagbody 
      :test-start
      (restart-case
        (handler-bind ((warning #'muffle-warning)       
					; ignore warnings... 
                       (error 
                        (lambda (cond)
                          (setf problem 
                                (report-test-problem
				 'test-error result case name cond
				 :backtrace (get-backtrace cond)))
                          (if *test-break-on-errors?*
                            (invoke-debugger cond)
                            (go :test-end))))
		       #+(or)
		       ;; FIXME - too much! should we catch serious-conditions?
                       (t (lambda (cond)
                            (setf problem 
                                  (report-test-problem
				   'test-error result case name cond
				   :backtrace (get-backtrace cond))))))
          (setf problem nil
		(current-method case) name)
          (start-test result case name)
          (setup-test case)
          (unwind-protect
	       (let ((result nil))
		 (declare (ignorable result))
		 (setf (current-step case) :testing
		       result
		       (measure
			   (getf (test-data case) :seconds)
			   (getf (test-data case) :conses)
			 (lift-test case name)))
		 (check-for-surprises result case name))
            (teardown-test case)
            (end-test result case name)))
        (ensure-failed (cond) 
	  (setf problem 
		(report-test-problem
		 'test-failure result case name cond)))
        (retry-test () :report "Retry the test." 
                    (go :test-start)))
      :test-end))
  (setf (third (first (tests-run result))) (test-data case))
  (setf *test-result* result))

(define-condition unexpected-success-failure (test-condition)
  ((expected :reader expected :initarg :expected)
   (expected-more :reader expected-more :initarg :expected-more))
  (:report (lambda (c s)
	     (format s "Test succeeded but we expected ~s (~s)"
		     (expected c)
		     (expected-more c)))))

(defun check-for-surprises (results testsuite name)
  (declare (ignore results name))
  (let* ((options (getf (test-data testsuite) :options))
	 (expected-failure-p (second (member :expected-failure options)))
	 (expected-error-p (second (member :expected-error options)))
	 (expected-problem-p (second (member :expected-problem options)))
	 (condition nil))
    (cond 
      (expected-failure-p
       (setf (slot-value testsuite 'expected-failure-p) expected-failure-p))
      (expected-error-p
       (setf (slot-value testsuite 'expected-error-p) expected-error-p))
      (expected-problem-p
       (setf (slot-value testsuite 'expected-problem-p) expected-problem-p)))
    (cond
      ((expected-failure-p testsuite)
       (setf condition 
	     (make-condition 'unexpected-success-failure
			     :expected :failure
			     :expected-more (expected-failure-p testsuite))))
      ((expected-error-p testsuite)
       (setf condition 
	     (make-condition 'unexpected-success-failure
			     :expected :error
			     :expected-more (expected-error-p testsuite))))
      ((expected-problem-p testsuite)
       (setf condition 
	     (make-condition 'unexpected-success-failure
			     :expected :problem
			     :expected-more (expected-problem-p testsuite)))))
    (when condition
      (if (find-restart 'ensure-failed)
	  (invoke-restart 'ensure-failed condition)
	  (warn condition)))))

(defun report-test-problem (problem-type result suite method condition
			    &rest args)
  ;; ick
  (let ((docs nil)
	(options (getf (test-data suite) :options))
	(option nil))
    (declare (ignore docs option))
    (cond ((and (eq problem-type 'test-failure)
		(not (typep condition 'unexpected-success-failure))
		(member :expected-failure options))
	   (setf problem-type 'test-expected-failure 
		 option :expected-failure))
	  ((and (eq problem-type 'test-error)
		(member :expected-error (getf (test-data suite) :options)))
	   (setf problem-type 'test-expected-error
		 option :expected-error))
	  ((and (or (eq problem-type 'test-failure) 
		    (eq problem-type 'test-error))
		(member :expected-problem (getf (test-data suite) :options)))
	   (setf problem-type (or (and (eq problem-type 'test-failure) 
				       'test-expected-failure)
				  (and (eq problem-type 'test-error)
				       'test-expected-error))
		 option :expected-problem)))
    (let ((problem (apply #'make-instance problem-type
			  :testsuite suite
			  :test-method method 
			  :test-condition condition
			  :test-step (current-step suite) args)))
      (setf (getf (test-data suite) :problem) problem)
      (etypecase problem
	(test-failure (push problem (failures result)))
	(test-expected-failure (push problem (expected-failures result)))
	(test-error (push problem (errors result)))
	(test-expected-error (push problem (expected-errors result))))
      problem)))

;;; ---------------------------------------------------------------------------
;;; test-result and printing
;;; ---------------------------------------------------------------------------

(defun get-test-print-length ()
  (let ((foo *test-print-length*))
    (if (eq foo :follow-print) *print-length* foo)))

(defun get-test-print-level ()
  (let ((foo *test-print-level*))
    (if (eq foo :follow-print) *print-level* foo)))

(defmethod start-test ((result test-result) (case test-mixin) name) 
  (push (list (type-of case) name nil) (tests-run result))
  (setf (current-step case) :start-test
	(test-data case) 
	`(:start-time ,(get-internal-real-time)
	  :start-time-universal ,(get-universal-time))))

(defmethod end-test ((result test-result) (testsuite test-mixin) name)
  (declare (ignore name))
  (setf (current-step testsuite) :end-test
	(getf (test-data testsuite) :end-time) (get-internal-real-time)
	(end-time result) (get-internal-real-time)
	(getf (test-data testsuite) :end-time-universal) (get-universal-time)
	(end-time-universal result) (get-universal-time)))

(defun make-test-result (for test-mode)
  (make-instance 'test-result
    :results-for for
    :test-mode test-mode))

(defun testing-interactively-p ()
  (values nil))

(defmethod print-object ((tr test-result) stream)
  (let ((complete-success? (and (null (errors tr))
                                (null (failures tr))
				(null (expected-failures tr))
				(null (expected-errors tr)))))
    (let* ((*print-level* (get-test-print-level))
           (*print-length* (get-test-print-length)))
      (print-unreadable-object (tr stream)
        (cond ((null (tests-run tr))
               (format stream "~A: no tests defined" (results-for tr)))
              ((eq (test-mode tr) :single)
               (cond ((test-interactive? tr)
                      ;; interactive
                      (cond (complete-success?
                             (format stream "Test passed"))
                            ((errors tr)
                             (format stream "Error during testing"))
                            ((expected-errors tr)
                             (format stream "Expected error during testing"))
                            ((failures tr)
                             (format stream "Test failed"))
                            (t
                             (format stream "Test failed expectedly"))))
                     (t
                      ;; from run-test
                      (format stream "~A.~A ~A" 
                              (results-for tr) 
                              (first (first (tests-run tr)))
                              (cond (complete-success?
                                     "passed")
                                    ((errors tr)
                                     "Error")
                                    (t
                                     "failed")))
		      (when (or (expected-errors tr) (expected-failures tr))
			(format stream "(~[~:;, ~:*~A expected failure~:P~]~[~:;, ~:*~A expected error~:P~])" 
				(expected-failures tr) (expected-errors tr))))))
              (t
               ;; multiple tests run
               (format stream "Results for ~A " (results-for tr))
               (if complete-success?
                 (format stream "[~A Successful test~:P]"
                         (length (tests-run tr)))
                 (format stream "~A Test~:P~[~:;, ~:*~A Failure~:P~]~[~:;, ~:*~A Error~:P~]~[~:;, ~:*~A Expected failure~:P~]~[~:;, ~:*~A Expected error~:P~]" 
                         (length (tests-run tr))
                         (length (failures tr))
                         (length (errors tr))
                         (length (expected-failures tr))
                         (length (expected-errors tr))))))
        ;; note that suites with no tests think that they are completely 
        ;; successful. Optimistic little buggers, huh?
        (when (and (not complete-success?) *test-describe-if-not-successful?*)
          (format stream "~%") 
          (print-test-result-details stream tr))))))

(defmethod describe-object ((result test-result) stream)
  (let ((number-of-failures (length (failures result)))
	(number-of-expected-failures (length (expected-failures result)))
        (number-of-errors (length (errors result)))
	(number-of-expected-errors (length (expected-errors result))))
    (unless *test-is-being-defined?*
      (format stream "~&Test Report for ~A: ~D test~:P run" 
              (results-for result) (length (tests-run result))))
    (let* ((*print-level* (get-test-print-level))
           (*print-length* (get-test-print-length)))
      (cond ((or (failures result) (errors result)
		 (expected-failures result) (expected-errors result))
             (format stream "~[~:;, ~:*~A Failure~:P~]~[~:;, ~:*~A Expected failure~:P~]~[~:;, ~:*~A Error~:P~]~[~:;, ~:*~A Expected error~:P~]." 
                     number-of-failures
                     number-of-expected-failures
                     number-of-errors
                     number-of-expected-errors)
             (format stream "~%~%")             
             (print-test-result-details stream result))
	    ((or (expected-failures result) (expected-errors result))
             (format stream ", all passed *~[~:;, ~:*~A Expected failure~:P~]~[~:;, ~:*~A Expected error~:P~])." 
                     number-of-expected-failures
                     number-of-expected-errors)
             (format stream "~%~%")             
             (print-test-result-details stream result))
	    (t
             (unless *test-is-being-defined?*
               (format stream ", all passed!")))))    
    (values)))

(defun print-test-result-details (stream result)
  (loop for report in (failures result) do
       (print-test-problem "Failure: " report stream))  
  (loop for report in (errors result) do
       (print-test-problem "ERROR  : " report stream))
  (loop for report in (expected-failures result) do
       (print-test-problem "Expected failure: " report stream))
  (loop for report in (expected-errors result) do
       (print-test-problem "Expected Error : " report stream)))

(defun print-test-problem (prefix report stream)
  (let* ((suite (testsuite report))
         (method (test-method report))
         (condition (test-condition report))
         (code (test-report-code suite method))
         (testsuite-name method))
    (format stream "~&~A~(~A : ~A~)" prefix (type-of suite) testsuite-name)
    (let ((doc-string (gethash testsuite-name
                               (test-case-documentation 
				(class-name (class-of suite))))))
      (when doc-string 
        (format stream "~&~A" doc-string)))
    (format stream "~&~<  ~@;~
                    ~@[Condition: ~<~@;~A~:>~]~
                    ~@[~&Code     : ~S~]~
                    ~&~:>" (list (list condition) code))))


;;; ---------------------------------------------------------------------------
;;; test-reports
;;; ---------------------------------------------------------------------------

(defclass test-problem-mixin ()
  ((testsuite :initform nil :initarg :testsuite :reader testsuite)
   (test-method :initform nil :initarg :test-method :reader test-method)
   (test-condition :initform nil
		   :initarg :test-condition 
		   :reader test-condition)
   (test-problem-kind :reader test-problem-kind :allocation :class)
   (test-step :initform nil :initarg :test-step :reader test-step)))

(defmethod print-object ((problem test-problem-mixin) stream)
  (print-unreadable-object (problem stream)
    (format stream "TEST-~@:(~A~): ~A in ~A" 
            (test-problem-kind problem) 
            (name (testsuite problem))
	    (test-method problem))))

(defclass generic-problem (test-problem-mixin)
  ((test-problem-kind :initarg :test-problem-kind
		      :allocation :class)))

(defclass expected-problem-mixin ()
  ((documentation :initform nil 
		  :initarg :documentation
		  :accessor failure-documentation)))

(defclass test-expected-failure (expected-problem-mixin generic-problem)
  ()
  (:default-initargs 
   :test-problem-kind "Expected failure"))

(defclass test-failure (generic-problem)
  ()
  (:default-initargs 
   :test-problem-kind "failure"))

(defclass test-error-mixin (generic-problem) 
  ((backtrace :initform nil :initarg :backtrace :reader backtrace)))
  
(defclass test-expected-error (expected-problem-mixin test-error-mixin)
  ()
  (:default-initargs 
   :test-problem-kind "Expected error"))

(defclass test-error (test-error-mixin)
  ()
  (:default-initargs 
   :test-problem-kind "Error"))

(defmethod test-report-code ((testsuite test-mixin) (method symbol))
  (let* ((class-name (class-name (class-of testsuite))))
    (gethash method
             (test-name->code-table class-name))))

;;; ---------------------------------------------------------------------------
;;; utilities
;;; ---------------------------------------------------------------------------

(defun remove-test-methods (test-name)
  (prog1
      (length (testsuite-tests test-name))
    (setf (testsuite-tests test-name) nil)))

(defun remove-previous-definitions (classname)
  "Remove the methods of this class and all its subclasses."
  (let ((classes-removed nil)
        (class (find-class classname nil))
        (removed-count 0))
    (when class
      (loop for subclass in (subclasses class :proper? nil) do
            (push subclass classes-removed)
            (incf removed-count
                  (remove-test-methods (class-name subclass)))
            #+Ignore
            ;;?? causing more trouble than it solves...??
            (setf (find-class (class-name subclass)) nil))
      
      (unless (length-1-list-p classes-removed)
        (format *debug-io* 
                "~&;;; Removed Test suite ~(~A~) and its subclasses (~{~<~s~>~^, ~})."
                classname (sort 
                           (delete classname 
				   (mapcar #'class-name classes-removed))
                           #'string-lessp)))
      (unless (zerop removed-count)
        (format *debug-io* 
                "~&;;; Removed ~D methods from test suite ~(~A~)~@[ and its subclasses~]."
                removed-count classname 
		(not (length-1-list-p classes-removed)))))))

(defun build-initialize-test-method ()
  (let ((initforms nil)
        (slot-names nil)
        (slot-specs (def :slot-specs)))
    (loop for slot in slot-specs do
	 (when (and (member :initform (rest slot))
		    (not (eq :unbound (getf (rest slot) :initform))))
	   (push (getf (rest slot) :initform) initforms)
	   (push (first slot) slot-names)))
    (setf slot-names (nreverse slot-names)
          initforms (nreverse initforms))    
    (when initforms
      `((defmethod make-single-prototype ((testsuite ,(def :testsuite-name)))
	  (with-test-slots
	    (append 
	     (when (next-method-p)
	       (call-next-method))
	     (let* (,@(mapcar (lambda (slot-name initform)
				`(,slot-name ,initform))
			      slot-names initforms))
	       (list ,@(mapcar (lambda (slot-name)
				 `(cons ',slot-name ,slot-name))
			       slot-names))))))))))

(defun (setf test-environment-value) (value name)
  (pushnew (cons name value) *test-environment* :test #'equal)
  (values value))

(defun test-environment-value (name)
  (cdr (assoc name *test-environment*)))

(defun remove-from-test-environment (name)
  (setf *test-environment* 
        (remove name *test-environment* :key #'car)))

(defun build-test-local-functions ()
  `(progn
     ,@(mapcar 
	(lambda (function-spec)
	  (destructuring-bind (name arglist &body body) (first function-spec)
	    `(defmethod flet-test-function ((testsuite ,(def :testsuite-name))
					    (function-name (eql ',name))
					    &rest args)
	       (with-test-slots 
		 ,(if arglist
		      `(destructuring-bind ,arglist args
			 ,@body)
		      `(progn ,@body))))))
	(def :functions))))

(defun build-test-teardown-method ()
  (let ((test-name (def :testsuite-name))
        (slot-names (def :direct-slot-names))
        (teardown (def :teardown)))
    (when teardown
      (unless (consp teardown)
        (setf teardown (list teardown)))
      (when (length-1-list-p teardown)
        (setf teardown (list teardown)))
      (when (symbolp (first teardown))
        (setf teardown (list teardown))))
    (let* ((teardown-code `(,@(when teardown
                                `((with-test-slots ,@teardown)))))
           (test-code `(,@teardown-code
                        ,@(mapcar (lambda (slot)
                                    `(remove-from-test-environment ',slot))
                                  slot-names))))
      `(progn
         ,@(when teardown-code
             `((defmethod teardown-test progn ((testsuite ,test-name))
		 (when (run-teardown-p testsuite :test-case)
		   ,@test-code))))
         ,@(when teardown-code
             `((defmethod testsuite-teardown ((testsuite ,test-name)
					      (result test-result))
                 (when (run-teardown-p testsuite :testsuite)
		   ,@test-code))))))))

(defun build-setup-test-method ()
  (let ((test-name (def :testsuite-name))
        (setup (def :setup)))
    (when setup
      (unless (consp setup)
        (setf setup (list setup)))
      (when (length-1-list-p setup)
        (setf setup (list setup)))
      (when (symbolp (first setup))
        (setf setup (list setup)))
      (let ((code `((with-test-slots ,@setup))))
	`(progn
	   (defmethod setup-test :after ((testsuite ,test-name))
	     ,@code))))))

(defmethod setup-test :around ((test test-mixin))
  (when (run-setup-p test)
    (call-next-method)
    (setf (slot-value test 'done-setup?) t)))

(defun run-setup-p (testsuite)
  (case (run-setup testsuite)
    (:once-per-session (error "not implemented"))
    (:once-per-suite (not (done-setup? testsuite)))
    ((:once-per-test-case t) t)
    ((:never nil) nil)
    (t (error "Don't know about ~s for run-setup" (run-setup testsuite)))))

(defun run-teardown-p (testsuite when)
  (ecase when
    (:test-case
     (ecase (run-setup testsuite)
       (:once-per-session nil)
       (:once-per-suite nil)
       ((:once-per-test-case t) t)
       ((:never nil) nil)))
    (:testsuite
     (ecase (run-setup testsuite)
       (:once-per-session nil)
       (:once-per-suite t)
       ((:once-per-test-case t) nil)
       ((:never nil) nil)))))
     
(defun build-test-test-method (test-class test-body options)
  (multiple-value-bind (test-name body documentation name-supplied?)
                       (parse-test-body test-body)
    (declare (ignorable name-supplied?))
    (unless (consp (first body))
      (setf body (list body)))
    `(progn
       (setf (gethash ',test-name (test-name->code-table ',test-class)) ',body
             (gethash ',body (test-code->name-table ',test-class)) ',test-name)
       ,(when documentation
          `(setf (gethash ',test-name (test-case-documentation ',test-class))
                 ,documentation))
       #+MCL
       ,@(when name-supplied?
           `((ccl:record-source-file ',test-name 'test-case)))
       (unless (find ',test-name (testsuite-tests ',test-class))
	 (setf (testsuite-tests ',test-class)
	       (append (testsuite-tests ',test-class) (list ',test-name))))
       (defmethod lift-test ((testsuite ,test-class) (case (eql ',test-name)))
	 ,@(when options
		 `((setf (getf (test-data testsuite) :options) ',options))) 
	 (with-test-slots ,@body))
       (setf *current-case-method-name* ',test-name)
       (when (and *test-print-when-defined?*
                  (not (or *test-is-being-compiled?*
                           )))
         (format *debug-io* "~&;Test Created: ~(~S.~S~)." 
		 ',test-class ',test-name))
       *current-case-method-name*)))

(defun build-dynamics ()
  (let ((result nil))
    (dolist (putative-pair (def :dynamic-variables))
      (if (atom putative-pair)
        (push (list putative-pair nil) result)
        (push putative-pair result)))
    (nreverse result)))

(defun parse-test-body (test-body)
  (let ((test-name nil)
        (body nil)
        (parsed-body nil)
        (documentation nil)
        (test-number (1+ (testsuite-test-count *current-suite-class-name*)))
        (name-supplied? nil))
    ;; parse out any documentation
    (loop for form in test-body do
          (if (and (consp form)
                   (keywordp (first form))
                   (eq :documentation (first form)))
            (setf documentation (second form))
            (push form parsed-body)))
    (setf test-body (nreverse parsed-body))
    (setf test-name (first test-body))
    (cond ((symbolp test-name)
           (setf test-name 
		 (intern (format nil "~A" test-name))
                 body (rest test-body)
                 name-supplied? t))
          ((and (test-code->name-table *current-suite-class-name*)
                (setf test-name 
                 (gethash test-body
			  (test-code->name-table *current-suite-class-name*))))
           (setf body test-body))
          (t
           (setf test-name 
		 (intern (format nil "TEST-~A" 
				 test-number))
                 body test-body)))
    (values test-name body documentation name-supplied?)))

(defun build-test-class ()
  ;; for now, we don't generate code from :class-def code-blocks
  ;; they are executed only for effect.
  (loop for (nil . block) in *code-blocks* 
     when (and block 
	       (code block)
	       (eq (operate-when block) :class-def)
	       (or (not (filter block))
		   (funcall (filter block)))) collect
     (funcall (code block)))
  (unless (some (lambda (superclass)
		  (testsuite-p superclass))
		(def :superclasses))
    (pushnew 'test-mixin (def :superclasses)))
  ;; build basic class and standard class
  `(defclass ,(def :testsuite-name) (,@(def :superclasses))
     nil
     ,@(when (def :documentation)
	     `((:documentation ,(def :documentation))))
     (:default-initargs
	 :test-slot-names ',(def :slot-names)
       ,@(def :default-initargs))))

(defun parse-test-slots (slot-specs)
  (loop for spec in slot-specs collect
        (let ((parsed-spec spec))
          (if (member :initform parsed-spec)
            (let ((pos (position :initform parsed-spec)))
              (append (subseq parsed-spec 0 pos)
                      (subseq parsed-spec (+ pos 2))))
            parsed-spec))))

(defmethod testsuite-p ((classname symbol))
  (let ((class (find-class classname nil)))
    (handler-case
      (and class
           (typep (allocate-instance class) 'test-mixin)
	   classname)
      (error (c) (declare (ignore c)) (values nil)))))

(defmethod testsuite-p ((object standard-object))
  (testsuite-p (class-name (class-of object))))

(defmethod testsuite-p ((class standard-class))
  (testsuite-p (class-name class)))

(defmethod testsuite-methods ((classname symbol))
  (testsuite-tests classname))

(defmethod testsuite-methods ((test test-mixin))
  (testsuite-methods (class-name (class-of test))))

(defmethod testsuite-methods ((test standard-class))
  (testsuite-methods (class-name test)))


;; some handy properties
(defclass-property test-slots)
(defclass-property test-code->name-table)
(defclass-property test-name->code-table)
(defclass-property test-case-documentation)
(defclass-property testsuite-prototype)
(defclass-property testsuite-tests)
(defclass-property testsuite-dynamic-variables)

;;?? issue 27: break encapsulation of code blocks
(defclass-property testsuite-function-specs)

(defun empty-test-tables (test-name)
  (when (find-class test-name nil)
    (setf (test-code->name-table test-name)
          (make-hash-table :test #'equal)
          (test-name->code-table test-name)
          (make-hash-table :test #'equal)
          (test-case-documentation test-name)
          (make-hash-table :test #'equal))))


(define-condition timeout-error (error)
                  ()
  (:report (lambda (c s)
	     (declare (ignore c))
	     (format s "Process timeout"))))

(defmacro with-timeout ((seconds) &body body)
  #+allegro
  `(mp:with-timeout (,seconds (error 'timeout-error)) 
       ,@body)
  #+cmu
  `(mp:with-timeout (,seconds) ,@body)
  #+sb-thread
  `(handler-case 
       (sb-ext:with-timeout ,seconds ,@body)
     (sb-ext::timeout (c)
       (cerror "Timeout" 'timeout-error)))
  #+(or digitool openmcl)
  (let ((checker-process (format nil "Checker ~S" (gensym)))
        (waiting-process (format nil "Waiter ~S" (gensym)))
	(result (gensym))
	(process (gensym)))
    `(let* ((,result nil)
	    (,process (ccl:process-run-function 
		,checker-process
		(lambda ()
		  (setf ,result (progn ,@body)))))) 
       (ccl:process-wait-with-timeout
        ,waiting-process
        (* ,seconds #+openmcl ccl:*ticks-per-second* #+digitool 60)
        (lambda ()
          (not (ccl::process-active-p ,process)))) 
       (when (ccl::process-active-p ,process)
	 (ccl:process-kill ,process)
	 (cerror "Timeout" 'timeout-error))
       (values ,result)))
  #-(or allegro cmu sb-thread openmcl digitool)
  `(progn ,@body))

(defvar *test-maximum-time* 2
  "Maximum number of seconds a process test is allowed to run before we give up.")

(pushnew :timeout *deftest-clauses*)

(add-code-block
 :timeout 1 :class-def
 (lambda () (def :timeout)) 
 '((setf (def :timeout) (cleanup-parsed-parameter value)))
 (lambda ()
   (unless (some (lambda (super)
		   (member (find-class 'process-test-mixin)
			   (superclasses super)))
		 (def :superclasses))
     (pushnew 'process-test-mixin (def :superclasses)))
   (push (def :timeout) (def :default-initargs))
   (push :maximum-time (def :default-initargs))
   nil))

(defclass process-test-mixin ()
  ((maximum-time :initform *test-maximum-time* 
                 :accessor maximum-time
                 :initarg :maximum-time)))

(defclass test-timeout-failure (test-failure)
  ((test-problem-kind :initform "Timeout" :allocation :class)))

(define-condition test-timeout-condition (test-condition) 
                  ((maximum-time :initform *test-maximum-time* 
                                 :accessor maximum-time
                                 :initarg :maximum-time))
  (:report (lambda (c s)
             (format s "Test ran out of time (longer than ~S-second~:P)" 
                     (maximum-time c)))))

(defmethod do-testing :around ((testsuite process-test-mixin) result fn)
  (declare (ignore fn))
  (handler-case
      (with-timeout ((maximum-time testsuite))
	(call-next-method))
    (timeout-error 
	(c)
      (declare (ignore c))
      (report-test-problem
       'test-timeout-failure result testsuite (current-method testsuite)
       (make-instance 'test-timeout-condition
		      :maximum-time (maximum-time testsuite))))))

;;;;;

(defmethod find-testsuite ((suite symbol))
  (or (testsuite-p suite)
      (find-testsuite (symbol-name suite))))

(defmethod find-testsuite ((suite-name string))
  (let* ((temp nil)
	 (possibilities (remove-duplicates 
			 (loop for p in (list-all-packages) 
			    when (and (setf temp (find-symbol suite-name p))
				      (find-class temp nil)
				      (subtypep temp 'test-mixin)) collect
			    temp))))
    (cond ((null possibilities) 
	   (error 'test-class-not-defined :test-class-name suite-name))
	  ((= (length possibilities) 1)
	   (first possibilities))
	  (t 
	   (error "There are several test suites named ~s: they are ~{~s~^, ~}"
		  suite-name possibilities)))))
			     
(defun last-test-status ()
  (cond ((typep *test-result* 'test-result)
	 (cond ((and (null (errors *test-result*))
		     (null (failures *test-result*)))
		:success)
	       ((and (errors *test-result*)
		     (failures *test-result*))
		:errors-and-failures)
	       ((errors *test-result*)
		:errors)
	       ((failures *test-result*)
		:failures)))
	(t
	 nil)))

(defun suite-tested-p (suite &key (result *test-result*))
  (and result
       (typep *test-result* 'test-result)
       (slot-exists-p result 'suites-run)
       (slot-boundp result 'suites-run)
       (consp (suites-run result))
       (find suite (suites-run result))))

(defun unique-filename (pathname)
  (let ((date-part (date-stamp)))
    (loop repeat 100
       for index from 1
	 for name = 
	 (merge-pathnames 
	  (make-pathname
	   :name (format nil "~a-~a-~d" 
			 (pathname-name pathname)
			 date-part index))
	  pathname) do
	 (unless (probe-file name)
	   (return-from unique-filename name)))
    (error "Unable to find unique pathname for ~a" pathname)))
	    
(defun date-stamp (&key (datetime (get-universal-time)) (include-time? nil))
  (multiple-value-bind
	(second minute hour day month year day-of-the-week)
      (decode-universal-time datetime)
    (declare (ignore day-of-the-week))
    (let ((date-part (format nil "~d-~2,'0d-~2,'0d" year month day))
	  (time-part (and include-time? 
			  (list (format nil "-~2,'0d-~2,'0d-~2,'0d"
					hour minute second)))))
      (apply 'concatenate 'string date-part time-part))))

#+(or)
(date-stamp :include-time? t)	

;;?? might be "cleaner" with a macrolet (cf. lift-result)
(defun lift-property (name)
  (when *current-test*
    (getf (getf (test-data *current-test*) :properties) name)))

#+(or)
(setf (getf (getf (third (first (tests-run *test-result*))) :properties) :foo)
      3)

(defun (setf lift-property) (value name)
  (when *current-test*
    (setf (getf (getf (test-data *current-test*) :properties) name) value)))
