(in-package :symbolic-smalltalk-core)

;;; Message not understood

(define-condition message-not-understood (error)
  ((recipient :initarg :recipient
              :accessor message-not-understood-recipient)
   (arguments :initarg :arguments
              :accessor message-not-understood-arguments)
   (selector :initarg :selector
             :accessor message-not-understood-selector)
   (function :initarg :function :accessor message-not-understood-function))
  (:report (lambda (c s)
             (format s "~S cannot understand ~S~@[ ~S~]."
                     (message-not-understood-recipient c)
                     (message-not-understood-selector c)
                     (message-not-understood-arguments c)))))

;;; Users of this package may want to specialize this on Object.
(defmethod does-not-understand (recipient arguments function)
  (error 'message-not-understood
         :recipient recipient
         :arguments arguments
         :selector (function-selector function)
         :function (closer-mop:generic-function-name function)))

(defmethod no-applicable-method ((self symbolic-smalltalk-generic-function) &rest args)
  (does-not-understand (first args) (rest args) self))
