;;; This package tries to implement, using CLOS, the system of classes
;;; described in chapter 16 of the Smalltalk-80 "Blue book"
;;; ("Smalltalk-80: the language and its implementation" by Goldberg
;;; and Robson).

(defpackage :symbolic-smalltalk-methods (:use))

(defpackage :symbolic-smalltalk-core
  (:use :cl)
  (:shadow "CLASS")
  (:local-nicknames (:m :symbolic-smalltalk-methods))
           ;; Clos metaclasses
  (:export #:symbolic-smalltalk-class
           #:symbolic-smalltalk-metaclass
           #:symbolic-smalltalk-generic-function

           ;; Smalltalk classes
           #:proto-object
           #:object
           #:behavior
           #:class-description
           #:class
           #:metaclass

           #:new
           #:basic-new
           #:instance-variables
           #:class-variables
           #:add-method-to-class
           #:remove-method-from-class
           #:add-selector-to-class
           #:add-slot-accessor
           #:subclass
           #:the-class

           ;; "Message not understood" condition.
           #:message
           #:message-recipient
           #:message-selector
           #:message-arguments
           #:message-not-understood

           ;; Translating selectors into symbols.
           #:selector
           #:translate-selector

           ;; Definition macros
           #:define-method
           #:define-class-method
           #:define-class

           ;; Sending messages in Lisp
           #:send
           #:send-class
           #:cascade
           #:selector))
