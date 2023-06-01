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
           #:the-class

           ;; Smalltalk classes
           #:proto-object
           #:object
           #:behavior
           #:class-description
           #:class
           #:metaclass

           ;; Instance creation
           #:new
           #:basic-new

           ;; Adding methods
           #:add-method-to-class
           #:remove-method-from-class
           #:add-selector-to-class
           #:add-slot-accessor

           ;; Testing methods
           #:includes-selector-p
           #:can-understand-p

           ;; Variables
           #:instance-variables
           #:class-variables

           ;; Subclassing
           #:subclass

           ;; Accessing class hierarchy
           #:superclass
           #:subclasses
           #:all-superclasses
           #:all-subclasses

           ;; "Message not understood" condition.
           #:does-not-understand
           #:message-not-understood
           #:message-not-understood-recipient
           #:message-not-understood-arguments
           #:message-not-understood-selector
           #:message-not-understood-function

           ;; Translating selectors into symbols.
           #:selector
           #:translate-selector

           ;; Definition macros
           #:define-class
           #:define-method
           #:define-class-method

           ;; Sending messages in Lisp
           #:send
           #:send-class
           #:cascade
           #:selector))
