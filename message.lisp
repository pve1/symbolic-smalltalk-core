(in-package :symbolic-smalltalk-proto-system)

;;; Message

(define-smalltalk-class message (object)
  ((recipient :initarg :recipient :accessor message-recipient :initform nil)
   (selector :initarg :selector :accessor message-selector :initform nil)
   (arguments :initarg :arguments :accessor message-arguments :initform nil)))

(define-smalltalk-method (message recipient)
  (message-recipient self))

(define-smalltalk-method (message :recipient recipient)
  (setf (message-recipient self) recipient))

(define-smalltalk-method (message selector)
  (message-selector self))

(define-smalltalk-method (message :selector selector)
  (setf (message-selector self) selector))

(define-smalltalk-method (message arguments)
  (message-arguments self))

(define-smalltalk-method (message :arguments arguments)
  (setf (message-arguments self) arguments))

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

(define-smalltalk-method (t :does-not-understand message)
  (error 'message-not-understood
         :recipient self
         :message message))

(defmethod no-applicable-method ((self symbolic-smalltalk-generic-function) &rest args)
  (if (find-class 'message nil)
      (m::does-not-understand/1 (first args)
                                (let ((message (m::new/0 (the-class 'message))))
                                  (m::recipient/1 message (first args))
                                  (m::arguments/1 message (rest args))
                                  (m::selector/1 message (closer-mop:generic-function-name self))
                                  message))
      (error "~S cannot understand ~S." args self)))
