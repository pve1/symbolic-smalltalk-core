(in-package :symbolic-smalltalk-core)

;;; Define class

(defmacro define-class (name superclass instance-variables class-variables)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (send (the-class ',superclass)
       :subclass ',name
       :instance-variable-names ',instance-variables
       :class-variable-names ',class-variables)))
