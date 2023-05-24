(asdf:defsystem #:symbolic-smalltalk-proto-system
  :description "The Symbolic Smalltalk core system."
  :author "Peter von Etter"
  :license "LGPL-3.0"
  :version "0.0.1"
  :serial t
  :components ((:file "proto-system"))
  :depends-on (:closer-mop))
