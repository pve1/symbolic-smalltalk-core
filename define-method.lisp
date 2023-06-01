(in-package :symbolic-smalltalk-core)

;;; Define method

;; Todo: Investigate using STANDARD-INSTANCE-ACCESS for slot
;; macrolets.

(defun generate-defgeneric-parameters (n &optional (package :symbolic-smalltalk-methods))
  (loop :for i :from 0 :below n
        :collect (intern
                  (concatenate 'string "X" (princ-to-string i))
                  (find-package package))))

(defun make-smalltalk-define-method-form (type message body)
  (let* ((parameters (extract-parameters-from-message message))
         (self (self))
         (selector (extract-selector-from-message message))
         (lambda-form `(lambda (,self ,@parameters)
                         ,@body)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (add-method-to-class (the-class ',type)
                            ',selector
                            ',lambda-form))))

(defun make-standard-define-method-form (type message body)
  (let* ((function-name (translate-message message))
         (parameters (extract-parameters-from-message message))
         (self (self))
         (generic-parameters
           (generate-defgeneric-parameters (length parameters))))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (handler-bind ((warning #'muffle-warning))
           (defgeneric ,function-name (,self ,@generic-parameters)
             (:generic-function-class symbolic-smalltalk-generic-function)))
         (setf (function-selector #',function-name)
               ',(extract-selector-from-message message)))
       (defmethod ,function-name ((,self ,type) ,@parameters)
         ,@body))))

(defmacro define-method ((type &rest message) &body body)
  (cond ((and (symbolp type)
              (typep (find-class type) 'symbolic-smalltalk-class))
         (make-smalltalk-define-method-form type message body))
        (t
         (make-standard-define-method-form type message body))))

(defmacro define-class-method ((type &rest message) &body body)
  (let ((metaclass-name (if (typep (find-class type) 'symbolic-smalltalk-class)
                            (metaclass-name type)
                            `(eql (find-class ',type)))))
    `(define-method (,metaclass-name ,@message)
       ,@body)))
