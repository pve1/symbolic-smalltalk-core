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

           ;; "Message not understood" condition.
           #:message-not-understood
           #:message

           ;; Translating selectors into symbols.
           #:selector
           #:translate-selector

           ;; Definition macros
           #:define-method
           #:define-class-method
           #:define-class

           ;; Sending messages in Lisp
           #:send
           #:sendc
           #:cascade
           #:selector))
