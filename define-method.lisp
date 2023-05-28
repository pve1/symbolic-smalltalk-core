(in-package :symbolic-smalltalk-proto-system)

;;; Define method

;; Todo: Investigate using STANDARD-INSTANCE-ACCESS for slot
;; macrolets.

(defmacro define-method ((type &rest arguments) &body body)
  (let* ((function-name (translate-arglist arguments))
         (parameters (extract-parameters-from-arglist arguments))
         (self (intern "SELF" *package*))
         (generic-parameters
           (loop :for p :in parameters
                 :for i :from 0
                 :collect (intern
                           (concatenate 'string "X" (princ-to-string i))
                           (find-package :symbolic-smalltalk-proto-system))))
         (eql-specializer-p (and (consp type)
                                 (eq 'eql (car type))))
         (class-slots (when (and (not eql-specializer-p)
                                 (typep (find-class type) 'symbolic-smalltalk-class))
                        (append (send (the-class type) instance-variables)
                                (send (the-class type) class-variables))))
         (class-slots/not-shadowed (set-difference class-slots parameters))
         (lisp-class (cond (eql-specializer-p type)
                           ((typep (find-class type) 'symbolic-smalltalk-class)
                            (class-name (behavior-class (the-class type))))
                           (t type)))
         ;; Need to access :class allocated slots specially in class methods.
         (class-variables (cond ((and (not eql-specializer-p)
                                      (typep (find-class type) 'symbolic-smalltalk-metaclass))
                                 (send (this-metaclass-instance (find-class type))
                                   class-variables))))
         (class-variables-slot-macrolets
           (loop :for slot :in (set-difference
                                (set-difference class-variables parameters)
                                class-slots)
                 :collect `(,slot (slot-value
                                   (closer-mop:class-prototype
                                    (slot-value
                                     (m::this-class/0 (find-class ',type))
                                     '%class))
                                   ',slot)))))
    `(progn
       (handler-bind ((warning #'muffle-warning))
         (defgeneric ,function-name (self ,@generic-parameters)
           (:generic-function-class symbolic-smalltalk-generic-function)))
       (defmethod ,function-name ((,self ,lisp-class) ,@parameters)
         (symbol-macrolet ,(append class-variables-slot-macrolets
                                   (mapcar (lambda (slot)
                                             `(,slot (slot-value ,self ',slot)))
                                           class-slots/not-shadowed))
           ,@body)))))

(defmacro define-class-method ((type &rest arguments) &body body)
  (let ((metaclass-name (if (typep (find-class type) 'symbolic-smalltalk-class)
                            (metaclass-name type)
                            `(eql (find-class ',type)))))
    `(define-method (,metaclass-name ,@arguments)
       ,@body)))
