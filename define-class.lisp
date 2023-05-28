(in-package :symbolic-smalltalk-proto-system)

;;; Define class

(defmacro define-class (name superclass instance-variables class-variables)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (m::subclass/instance-variable-names/class-variable-names/3
      (the-class ',superclass)
      ',name
      ',instance-variables
      ',class-variables)))
