(in-package #:lift)

;;; ---------------------------------------------------------------------------
;;; randomized testing helpers
;;; ---------------------------------------------------------------------------

(u:export-exported-symbols "VARIATES" "LIFT")

;;; ---------------------------------------------------------------------------

(defclass randomized-testsuite-mixin (test-mixin)
  ((generator 
    :reader generator)
   (shuffle-methods? 
    :initform t
    :initarg :shuffle-methods?
    :reader shuffle-methods?)))

;;; ---------------------------------------------------------------------------

(defmethod testsuite-methods :around ((case randomized-testsuite-mixin))
  (let ((methods (call-next-method)))
    (if (shuffle-methods? case)
      (variates:shuffle-elements! (copy-list methods) :generator (generator case))
      methods)))

;;; ---------------------------------------------------------------------------
;;; single-setup testing
;;; ---------------------------------------------------------------------------

(defclass single-setup-mixin (test-mixin)
  ((single-setup? 
    :initform nil
    :initargs :single-setup?
    :reader single-setup?)))

;;; ---------------------------------------------------------------------------

(defmethod initialize-instance :after ((object single-setup-mixin) &key)
  ;; no shuffling of single setup tests
  (when (slot-exists-p object 'shuffle-methods?)
    (setf (slot-value object 'shuffle-methods?) nil)))


    