(in-package :symbolic-smalltalk-core)

;;; Methods

(defclass symbolic-smalltalk-generic-function (standard-generic-function)
  ((function-selector :initarg :function-selector
                      :accessor function-selector
                      :initform nil)
   (function-arity :initarg :function-arity
                   :accessor function-arity
                   :initform nil))
  (:metaclass closer-mop:funcallable-standard-class))

(defmethod function-selector ((fun t)) nil)

(defmethod (setf function-selector) (new (fun symbolic-smalltalk-generic-function))
  (setf (slot-value fun 'function-selector)
          (if (symbolp new)
              (intern (string new) *method-package*)
              new)))

(defmethod initialize-instance :after ((fun symbolic-smalltalk-generic-function) &key lambda-list)
  (when (null lambda-list)
    (error "Lambda list is required."))
  (setf (function-arity fun) (length lambda-list))
  (closer-mop:set-funcallable-instance-function
   fun
   (lambda (&rest rest)
     (no-applicable-method fun rest))))

(defmethod remove-method :after ((fun symbolic-smalltalk-generic-function) method)
  (when (null (closer-mop:generic-function-methods fun))
    (closer-mop:set-funcallable-instance-function
     fun
     (lambda (&rest rest)
       (apply #'no-applicable-method fun rest)))))
