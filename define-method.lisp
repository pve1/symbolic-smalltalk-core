(in-package :symbolic-smalltalk-core)

;;; Define method

;; Todo: Investigate using STANDARD-INSTANCE-ACCESS for slot
;; macrolets.

(defun generate-defgeneric-parameters (n &optional (package :symbolic-smalltalk-methods))
  (loop :for i :from 0 :below n
        :collect (intern
                  (concatenate 'string "X" (princ-to-string i))
                  (find-package package))))

(defun make-smalltalk-define-method-form (type arguments body)
  (let* ((parameters (extract-parameters-from-arglist arguments))
         (self (self))
         (lambda-form `(lambda (,self ,@parameters)
                         ,@body)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (add-method-to-class (the-class ',type)
                            ',(arglist-to-selector arguments)
                            ',lambda-form))))

(defun make-standard-define-method-form (type arguments body)
  (let* ((function-name (translate-arglist arguments))
         (parameters (extract-parameters-from-arglist arguments))
         (self (self))
         (generic-parameters
           (generate-defgeneric-parameters (length parameters))))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (handler-bind ((warning #'muffle-warning))
           (defgeneric ,function-name (,self ,@generic-parameters)
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
