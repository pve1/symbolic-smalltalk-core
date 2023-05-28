(asdf:defsystem #:symbolic-smalltalk-proto-system
  :description "The Symbolic Smalltalk core system."
  :author "Peter von Etter"
  :license "LGPL-3.0"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "selector-translation")
               (:file "macros")
               (:file "send")
               (:file "smalltalk-generic-function")
               (:file "smalltalk-class")
               (:file "base-classes")
               (:file "message")
               (:file "define-method")
               (:file "define-class"))
  :depends-on (:closer-mop))
