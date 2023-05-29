(in-package :symbolic-smalltalk-core)

;;; Define method

;; Todo: Investigate using STANDARD-INSTANCE-ACCESS for slot
;; macrolets.

(defmacro with-instance-variables ((self class) &body body)
  (let ((inst-vars (send (the-class class) instance-variables)))
    `(symbol-macrolet ,(mapcar (lambda (slot)
                                 `(,slot (slot-value ,self ',slot)))
                        inst-vars)
       ,@body)))

(defmacro with-class-variables ((self class) &body body)
  (let ((class-vars (send (the-class class) class-variables)))
    `(symbol-macrolet
         ,(mapcar (lambda (slot)
                    `(,slot (slot-value (class ,self) ',slot)))
           class-vars)
       ,@body)))

(defun make-smalltalk-define-method-form (type arguments body)
  (let* ((function-name (translate-arglist arguments))
         (parameters (extract-parameters-from-arglist arguments))
         (self (intern "SELF" *package*))
         (generic-parameters
           (loop :for p :in parameters
                 :for i :from 0
                 :collect (intern
                           (concatenate 'string "X" (princ-to-string i))
                           (find-package :symbolic-smalltalk-core))))
         (lisp-class (class-name (behavior-class (the-class type)))))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (handler-bind ((warning #'muffle-warning))
           (defgeneric ,function-name (self ,@generic-parameters)
             (:generic-function-class symbolic-smalltalk-generic-function))))
       (with-class-variables (,self ,lisp-class)
         (with-instance-variables (,self ,lisp-class)
           (defmethod ,function-name ((,self ,lisp-class) ,@parameters)
             ,@body))))))

(defun make-standard-define-method-form (type arguments body)
  (let* ((function-name (translate-arglist arguments))
         (parameters (extract-parameters-from-arglist arguments))
         (self (intern "SELF" *package*))
         (generic-parameters
           (loop :for p :in parameters
                 :for i :from 0
                 :collect (intern
                           (concatenate 'string "X" (princ-to-string i))
                           (find-package :symbolic-smalltalk-core)))))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (handler-bind ((warning #'muffle-warning))
           (defgeneric ,function-name (self ,@generic-parameters)
             (:generic-function-class symbolic-smalltalk-generic-function))))
       (defmethod ,function-name ((,self ,type) ,@parameters)
         ,@body))))

(defmacro define-method ((type &rest arguments) &body body)
  (cond ((and (symbolp type)
              (typep (find-class type) 'symbolic-smalltalk-class))
         (make-smalltalk-define-method-form type arguments body))
        (t
         (make-standard-define-method-form type arguments body))))

(defmacro define-class-method ((type &rest arguments) &body body)
  (let ((metaclass-name (if (typep (find-class type) 'symbolic-smalltalk-class)
                            (metaclass-name type)
                            `(eql (find-class ',type)))))
    `(define-method (,metaclass-name ,@arguments)
       ,@body)))
