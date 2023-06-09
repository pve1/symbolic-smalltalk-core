(in-package :symbolic-smalltalk-core)

(defun self ()
  (intern "SELF" *package*))

;; Helper macros just to get the basics going. These are *not* going to
;; be exported.

(defmacro define-smalltalk-method ((class &rest parameters) &body body)
  (let* ((actual-name (translate-message parameters))
         (actual-parameters (extract-parameters-from-message parameters))
         (parameter-list
           (loop :for i :in actual-parameters
                 :for k :from 0
                 :collect (intern (format nil "X~A" k) *package*))))
    `(progn
       (defgeneric ,actual-name (,(self) ,@parameter-list)
         (:generic-function-class symbolic-smalltalk-generic-function))
       (setf (function-selector #',actual-name)
             ',(extract-selector-from-message parameters))
       (defmethod ,actual-name ((,(self) ,class) ,@actual-parameters)
         ,@body))))

(defmethod metaclass-name ((smalltalk-class-name symbol))
  (intern (format nil "~A CLASS" (string smalltalk-class-name))
          (symbol-package smalltalk-class-name)))

(defmacro define-smalltalk-class (name direct-superclasses direct-slots)
  (let ((metaclass-name (metaclass-name name))
        (metasuperclasses (if (null direct-superclasses)
                              (list 'class)
                              (mapcar #'metaclass-name direct-superclasses))))
    `(progn
       (defclass ,name ,direct-superclasses
         ,direct-slots
         (:metaclass symbolic-smalltalk-class))
       (defclass ,metaclass-name ,metasuperclasses
         ()
         (:metaclass symbolic-smalltalk-metaclass))
       (initialize-class ',name))))
