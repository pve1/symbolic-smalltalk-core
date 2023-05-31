(in-package :symbolic-smalltalk-core)

;;; Message

(define-smalltalk-class message (object)
  ((recipient :initarg :recipient :accessor message-recipient :initform nil)
   (selector :initarg :selector :accessor message-selector :initform nil)
   (arguments :initarg :arguments :accessor message-arguments :initform nil)))

;;; Message not understood

(define-condition message-not-understood (error)
  ((recipient :initarg :recipient
              :accessor message-not-understood-recipient)
   (message :initarg :message
            :accessor message-not-understood-message))
  (:report (lambda (c s)
             (format s "~S cannot understand ~A."
                     (message-not-understood-recipient c)
                     (message-selector (message-not-understood-message c))))))

(defmethod does-not-understand (self message)
  (error 'message-not-understood
         :recipient self
         :message message))

(defmethod does-not-understand ((self object) message)
  (send self :does-not-understand message))

(defmethod no-applicable-method ((self symbolic-smalltalk-generic-function) &rest args)
  (if (find-class 'message nil)
      (does-not-understand
       (first args)
       (make-instance 'message :recipient (first args)
                               :arguments (rest args)
                               :selector (closer-mop:generic-function-name self)))
      (error "~S cannot understand ~S." args self)))
