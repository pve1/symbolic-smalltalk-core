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

;;; Proto Object

(define-smalltalk-class proto-object () ())

;; Should return a metaclass instance for *any* smalltalk object.
(defmethod class ((smalltalk-object proto-object))
  (metaclass-instance (class-of smalltalk-object)))

;;; Object

(define-smalltalk-class object (proto-object) ())

(define-smalltalk-method (object class)
  (class self))

(define-smalltalk-method (object initialize)
  self)

;;; Behavior

(define-smalltalk-class behavior (object)
  ((behavior-class :initarg :behavior-class
                   :accessor behavior-class
                   :type symbolic-smalltalk-class)))

(define-smalltalk-method (behavior new)
  (send (make-instance (behavior-class self)) initialize))

(define-smalltalk-method (behavior basic-new)
  (make-instance (behavior-class self)))

(define-smalltalk-method (behavior instance-variables)
  (loop :for slot-definition :in (closer-mop:class-direct-slots
                                  (behavior-class self))
        :when (and (eq :instance (closer-mop:slot-definition-allocation
                                  slot-definition))
                   ;; Indexed variables
                   (not (eq (closer-mop:slot-definition-name slot-definition)
                            '%array)))
          :collect (closer-mop:slot-definition-name slot-definition)))

(define-smalltalk-method (behavior class-variables)
  (loop :for slot-definition :in (closer-mop:class-direct-slots
                                  (behavior-class self))
        :when (eq :class (closer-mop:slot-definition-allocation
                          slot-definition))
          :collect (closer-mop:slot-definition-name slot-definition)))

;; No variable access.

(define-smalltalk-method (behavior :add-selector selector
                                   :with-lambda lambda-form)
  (assert (eq 'lambda (first lambda-form)))
  (let* ((selector-symbol (translate-selector selector))
         (gf (closer-mop:ensure-generic-function
              selector-symbol
              :generic-function-class 'symbolic-smalltalk-generic-function
              :lambda-list (second lambda-form)))
         (specializers (case (classify-selector selector)
                         (:unary (list (behavior-class self)))
                         (:binary (list (behavior-class self)
                                        (find-class t)))
                         (:keyword (list* (behavior-class self)
                                          (loop :for i :from 0 :below (length selector)
                                                :collect (find-class t)))))))
    (multiple-value-bind (method-lambda initargs)
        (closer-mop:make-method-lambda gf
                                       (closer-mop:class-prototype
                                        (closer-mop:generic-function-method-class gf))
                                       lambda-form nil)
      (add-method gf (apply #'make-instance
                            (closer-mop:generic-function-method-class gf)
                            :specializers specializers
                            :lambda-list (second lambda-form)
                            :function (compile nil method-lambda)
                            initargs)))))

;; Todo: Investigate using STANDARD-INSTANCE-ACCESS.

(define-smalltalk-method (behavior :add-slot-accessor slot-name)
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

(define-smalltalk-method (behavior :add-class-slot-accessor slot-name)
  (let ((value (gensym)))
    (send self
      :add-selector slot-name
      :with-lambda `(lambda (self)
                      (slot-value
                       (closer-mop:class-prototype
                        (behavior-class self))
                       ',slot-name)))
    (send self
      :add-selector (list (intern (string slot-name) :keyword))
      :with-lambda `(lambda (self ,value)
                      (setf
                       (slot-value
                        (closer-mop:class-prototype
                         (behavior-class self))
                        ',slot-name)
                       ,value)
                      self))))

;;; Class description

(define-smalltalk-class class-description (behavior)
  ())

;;; Class

(define-smalltalk-class class (class-description) ())

(define-smalltalk-method (class :subclass name)
  (send self
    :subclass name
    :instance-variable-names nil
    :class-variable-names nil))

(define-smalltalk-method (class :subclass name
                                :instance-variable-names instance-variables
                                :class-variable-names class-variables)
  (when (stringp instance-variables)
    (setf instance-variables
          (with-input-from-string (s instance-variables)
            (loop :for var = (read s nil s)
                  :until (eq var s)
                  :collect var))))

  (when (stringp class-variables)
    (setf class-variables
          (with-input-from-string (s class-variables)
            (loop :for var = (read s nil s)
                  :until (eq var s)
                  :collect var))))

  (let* ((initfunction (load-time-value (lambda () nil)))
         (class-slots (mapcar (lambda (x)
                                (list :name x
                                      :initform nil
                                      :initfunction initfunction
                                      :allocation :class))
                              class-variables))
         (metaclass (closer-mop:ensure-class
                     (metaclass-name name)
                     :direct-superclasses (list (class-of self))
                     :direct-slots nil
                     :metaclass (find-class 'symbolic-smalltalk-metaclass)))
         (instance-slots
           (mapcar (lambda (x)
                     (list :name x :initform nil :initfunction initfunction))
                   instance-variables))
         (lisp-class (closer-mop:ensure-class
                      name
                      :direct-superclasses (list (behavior-class self))
                      :direct-slots (append instance-slots class-slots)
                      :metaclass (find-class 'symbolic-smalltalk-class))))

    (initialize-metaclass lisp-class)
    (initialize-metaclass metaclass)

    (dolist (slot class-variables)
      (send (metaclass-instance lisp-class)
        :add-class-slot-accessor slot))

    (dolist (slot class-variables)
      (send (metaclass-instance lisp-class)
        :add-slot-accessor slot))

    (dolist (slot instance-variables)
      (send (metaclass-instance lisp-class)
        :add-slot-accessor slot))

    (the-class name)))

;;; Metaclass

(define-smalltalk-class metaclass (class-description) ())
