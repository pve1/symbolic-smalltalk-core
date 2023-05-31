(in-package :symbolic-smalltalk-core)

;;; Preliminary declarations.
;;;
;;; "Class" must exist for define-smalltalk-class to work.

(defclass proto-object () ()
  (:metaclass symbolic-smalltalk-class))

(defclass object (proto-object) ()
  (:metaclass symbolic-smalltalk-class))

(defclass behavior (object)
  ;; Behavior-class points to the Lisp class with which instances can
  ;; be created. This is needed to make the "new" method work for
  ;; "metaclass instances".
  ((behavior-class :initarg :behavior-class
                   :accessor behavior-class
                   :type symbolic-smalltalk-class))
  (:metaclass symbolic-smalltalk-class))

(defclass class-description (behavior) ()
  (:metaclass symbolic-smalltalk-class))

(defclass metaclass (class-description) ()
  (:metaclass symbolic-smalltalk-class))

(defclass class (class-description) ()
  (:metaclass symbolic-smalltalk-class))

;; This must come before the definition of proto-object, as its
;; metaclass (a subclass of object) will receive "initialize".
(define-smalltalk-method (object initialize)
  self)

;;; Proto Object

(define-smalltalk-class proto-object () ())

;; Should return a metaclass instance for *any* smalltalk object.
(defmethod class ((smalltalk-object proto-object))
  (metaclass-instance (class-of smalltalk-object)))

;;; Object

(define-smalltalk-class object (proto-object) ())

(define-smalltalk-method (object class)
  (class self))

;;; Behavior

(define-smalltalk-class behavior (object)
  ((behavior-class :initarg :behavior-class
                   :accessor behavior-class
                   :type symbolic-smalltalk-class)))

(define-smalltalk-method (behavior new)
  (send (make-instance (behavior-class self)) initialize))

(define-smalltalk-method (behavior basic-new)
  (make-instance (behavior-class self)))

(defmethod instance-variables ((self behavior))
  (loop :for slot-definition :in (closer-mop:class-direct-slots
                                  (behavior-class self))
        :when (and (eq :instance (closer-mop:slot-definition-allocation
                                  slot-definition))
                   ;; Indexed variables
                   (not (eq (closer-mop:slot-definition-name slot-definition)
                            '%array)))
          :collect (closer-mop:slot-definition-name slot-definition)))

(defmethod class-variables ((self behavior))
  (loop :for slot-definition :in (closer-mop:class-direct-slots
                                  (class-of self))
        :when (eq :instance (closer-mop:slot-definition-allocation
                             slot-definition))
          :collect (closer-mop:slot-definition-name slot-definition)))

(define-smalltalk-method (behavior instance-variables)
  (instance-variables self))

(define-smalltalk-method (behavior class-variables)
  (class-variables self))

(defun selector-specializers (class selector)
  (case (classify-selector selector)
    (:unary (list class))
    (:binary (list class (find-class t)))
    (:keyword (list* class (loop :for i :from 0 :below (length selector)
                                 :collect (find-class t))))))

;; CLASS is a symbol that names a class.
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

;; (add-selector-to-class (the-class 'object)
;;                        '(:foo x :bar y)
;;                        '(lambda (self x y) ...))

(defmethod add-method-to-class ((class behavior) selector lambda-form)
  (assert (eq 'lambda (first lambda-form)))
  (let* ((selector-symbol (translate-selector selector))
         (gf (closer-mop:ensure-generic-function
              selector-symbol
              :generic-function-class 'symbolic-smalltalk-generic-function
              :lambda-list (second lambda-form)))
         (specializers (selector-specializers (behavior-class class) selector))
         (the-self (first (second lambda-form))))
    (assert (string= "SELF" the-self))
    (multiple-value-bind (method-lambda initargs)
        (closer-mop:make-method-lambda gf
                                       (closer-mop:class-prototype
                                        (closer-mop:generic-function-method-class gf))
                                       lambda-form
                                       nil)
      (let ((final-lambda-expression
              `(lambda ()
                 (with-class-variables (,the-self ,(class-name (behavior-class class)))
                   (with-instance-variables (,the-self ,(class-name (behavior-class class)))
                     ,method-lambda)))))
        (add-method gf (apply #'make-instance
                              (closer-mop:generic-function-method-class gf)
                              :specializers specializers
                              :lambda-list (second lambda-form)
                              :function (funcall (compile nil final-lambda-expression))
                              initargs))))))

(defmethod remove-method-from-class ((self behavior) selector)
  (let* ((selector-symbol (translate-selector selector))
         (gf (fdefinition selector-symbol))
         (method (find-method gf
                              nil
                              (selector-specializers (behavior-class self)
                                                     selector))))
    (remove-method gf method)))

;; (add-selector-to-class (the-class 'object)
;;                        '(:foo x :bar y)
;;                        '((print x) (list x y)))

(defmethod add-selector-to-class ((class behavior) method-header body
                                  &key (self (self)))
  (setf method-header (alexandria:ensure-list method-header))
  (let ((parameters (extract-parameters-from-message method-header))
        (selector (extract-selector-from-message method-header)))
    (add-method-to-class class
                         selector
                         `(lambda (,self ,@parameters)
                            ,@body))))

;; Object class :add-selector '(:x x :y y)
;;              :with-lambda '(lambda (self x y) ...)

(define-smalltalk-method (behavior :add-selector selector
                                   :with-lambda lambda-form)
  (add-method-to-class self selector lambda-form))

(define-smalltalk-method (behavior :remove-selector selector)
  (remove-method-from-class self selector))

;; Todo: Investigate using STANDARD-INSTANCE-ACCESS.

(defmethod add-slot-accessor ((self behavior) slot-name)
  (let ((value (gensym)))
    (send self
      :add-selector slot-name
      :with-lambda `(lambda (self)
                      (slot-value self ',slot-name)))
    (send self
      :add-selector (list (intern (string slot-name) :keyword))
      :with-lambda `(lambda (self ,value)
                      (setf (slot-value self ',slot-name) ,value)
                      self))))

(define-smalltalk-method (behavior :add-slot-accessor slot-name)
  (add-slot-accessor self slot-name))


;;; Class description

(define-smalltalk-class class-description (behavior)
  ())

;;; Metaclass

(define-smalltalk-class metaclass (class-description) ())

(defmethod print-object ((object metaclass) stream)
  (print-unreadable-object (object stream)
    (prin1 (class-name
            (behavior-class
             (this-metaclass-instance
              (behavior-class object))))
           stream)
    (princ " METACLASS" stream))
  object)

;;; Class

(define-smalltalk-class class (class-description) ())

(defmethod print-object ((object class) stream)
  (print-unreadable-object (object stream)
    (prin1 (class-name (behavior-class object)) stream)
    (princ " CLASS" stream))
  object)

(defun read-variable-list (variables-string)
  (with-input-from-string (s variables-string)
    (loop :for var = (read s nil s)
          :until (eq var s)
          :collect var)))

(defmethod subclass ((self class) name &key instance-variables class-variables)
  (when (stringp instance-variables)
    (setf instance-variables (read-variable-list instance-variables)))
  (when (stringp class-variables)
    (setf class-variables (read-variable-list class-variables)))

  (let* ((initfunction (load-time-value (lambda () nil)))
         (class-slots (mapcar (lambda (x)
                                (list :name x
                                      :initform nil
                                      :initfunction initfunction))
                              class-variables))
         (metaclass (closer-mop:ensure-class
                     (metaclass-name name)
                     :direct-superclasses (list (class-of self))
                     :direct-slots class-slots
                     :metaclass (find-class 'symbolic-smalltalk-metaclass)))
         (instance-slots
           (mapcar (lambda (x)
                     (list :name x :initform nil :initfunction initfunction))
                   instance-variables))
         (lisp-class (closer-mop:ensure-class
                      name
                      :direct-superclasses (list (behavior-class self))
                      :direct-slots instance-slots
                      :metaclass (find-class 'symbolic-smalltalk-class))))

    ;; Will send initialize to the metaclass-instance.
    (initialize-metaclass lisp-class)
    (initialize-metaclass metaclass)
    (the-class name)))

(define-smalltalk-method (class :subclass name)
  (subclass self name))

(define-smalltalk-method (class :subclass name
                                :instance-variable-names instance-variables
                                :class-variable-names class-variables)
  (subclass self name
            :instance-variables instance-variables
            :class-variables class-variables))
