(in-package :symbolic-smalltalk-proto-system)

;;; Methods

(defclass symbolic-smalltalk-generic-function (standard-generic-function)
  (arity)
  (:metaclass closer-mop:funcallable-standard-class))

(defmethod initialize-instance :after ((self symbolic-smalltalk-generic-function) &key lambda-list)
  (when (null lambda-list)
    (error "Lambda list is required."))
  (setf (slot-value self 'arity) (length lambda-list))
  (closer-mop:set-funcallable-instance-function
   self
   (lambda (&rest rest)
     (no-applicable-method self rest))))

(defmethod remove-method :after ((self symbolic-smalltalk-generic-function) method)
  (when (null (closer-mop:generic-function-methods self))
    (closer-mop:set-funcallable-instance-function
     self
     (lambda (&rest rest)
       (no-applicable-method self rest)))))
