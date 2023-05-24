(defpackage :symbolic-smalltalk-methods (:use))

(defpackage :symbolic-smalltalk-proto-system
  (:use :cl)
  (:local-nicknames (:m :symbolic-smalltalk-methods))
  (:export #:symbolic-smalltalk-generic-function
           #:symbolic-smalltalk-class
           #:symbolic-smalltalk-metaclass
           #:proto-object
           #:object
           #:behavior
           #:class-description
           #:class
           #:metaclass
           #:message-not-understood
           #:proto-selector-translation
           #:message
           #:selector
           #:send
           #:sendc
           #:cascade
           #:define-method
           #:define-class-method
           #:define-class)
  (:shadow "CLASS"
           "SET"))

(in-package :symbolic-smalltalk-proto-system)

;; The naming scheme for methods goes like this:

;; Unary: m::foo/0 -> Object foo
;; Binary: m::+/1 -> Object + foo
;; Keyword: m::do/and-then/2 -> Object :do foo :and-then bar

;; Helper macro just to get the basics going.

(defmacro define-method% ((class name &rest parameters) &body body)
  (let ((actual-name (intern (string name) :symbolic-smalltalk-methods))
        (parameter-list
          (loop :for i :in parameters
                :for k :from 0
                :collect (intern (format nil "X~A" k)
                                 :symbolic-smalltalk-proto-system))))
    `(progn
       (defgeneric ,actual-name (self ,@parameter-list)
         (:generic-function-class symbolic-smalltalk-generic-function))
       (defmethod ,actual-name ((self ,class) ,@parameters)
         ,@body))))

;;; ====================================================================
;;;   Methods

(defclass symbolic-smalltalk-generic-function (standard-generic-function)
  (arity)
  (:metaclass closer-mop:funcallable-standard-class))

(define-condition message-not-understood (error)
  ((recipient :initarg :recipient
              :accessor message-not-understood-recipient)
   (message :initarg :message
            :accessor message-not-understood-message))
  (:report (lambda (c s)
             (format s "~S cannot understand ~A."
                     (message-not-understood-recipient c)
                     (m::selector/0 (message-not-understood-message c))))))

(define-method% (t m::does-not-understand/1 message)
  (error 'message-not-understood
         :recipient self
         :message message))

(defmethod no-applicable-method ((self symbolic-smalltalk-generic-function) &rest args)
  (if (find-class 'message nil)
      (m::does-not-understand/1 (first args)
                                (let ((message (m::new/0 (the-class 'message))))
                                  (m::recipient/1 message (first args))
                                  (m::arguments/1 message (rest args))
                                  (m::selector/1 message (closer-mop:generic-function-name self))
                                  message))
      (error "~S cannot understand ~S." args self)))

(defmethod initialize-instance :after ((self symbolic-smalltalk-generic-function) &key lambda-list)
  (when (null lambda-list)
    (error "Lambda list is required."))
  (setf (slot-value self 'arity) (length lambda-list))
  (closer-mop:set-funcallable-instance-function
   self
   (lambda (&rest rest)
     (no-applicable-method self rest))))

(defmethod remove-method :after ((self symbolic-smalltalk-generic-function) method)
  (when (null (closer-mop:generic-function-methods self))
    (closer-mop:set-funcallable-instance-function
     self
     (lambda (&rest rest)
       (no-applicable-method self rest)))))

;;; ====================================================================
;;;   Symbolic smalltalk class

(defclass symbolic-smalltalk-class (standard-class)
  (%metaclass-instance %this-class))

(defclass symbolic-smalltalk-metaclass (symbolic-smalltalk-class)
  ())

(defmethod c2cl:validate-superclass ((class symbolic-smalltalk-class)
                                     (superclass standard-class))
  t)

(defmethod c2cl:validate-superclass ((class symbolic-smalltalk-metaclass)
                                     (superclass symbolic-smalltalk-class))
  t)

(define-method% (t m::class/0)
  (class-of self))

(define-method% (t m::this-class/0)
  (slot-value self '%this-class))

(define-method% (t m::this-class/1 class)
  (setf (slot-value self '%this-class) class))

(define-method% (t m::metaclass-instance/0)
  (slot-value self '%metaclass-instance))

(define-method% (t m::metaclass/1 metaclass-name)
  (let ((metaclass-instance (make-instance metaclass-name)))
    (setf (slot-value metaclass-instance '%class) self)
    (setf (slot-value metaclass-instance 'name) (class-name self))
    (setf (slot-value self '%metaclass-instance) metaclass-instance)
    (setf (slot-value (find-class metaclass-name) '%this-class)
          metaclass-instance)
    metaclass-instance))

(defun the-class (symbol)
  (let ((class (find-class symbol)))
    (if (typep class 'symbolic-smalltalk-class)
        (slot-value (find-class symbol) '%metaclass-instance)
        class)))

;;; ====================================================================
;;;   Preliminary declarations

(defclass proto-object () ()
  (:metaclass symbolic-smalltalk-class))

(defclass object (proto-object) ()
  (:metaclass symbolic-smalltalk-class))

(defclass behavior (object)
  (%class)
  (:metaclass symbolic-smalltalk-class))

(defclass class-description (behavior)
  (name)
  (:metaclass symbolic-smalltalk-class))

(defclass class (class-description) ()
  (:metaclass symbolic-smalltalk-class))

(defclass metaclass (class-description) ()
  (:metaclass symbolic-smalltalk-class))

;;; ====================================================================
;;;   Proto Object

(defclass proto-object ()
  ()
  (:metaclass symbolic-smalltalk-class))

(defclass proto-object\ class (class)
  ()
  (:metaclass symbolic-smalltalk-metaclass))

(m::metaclass/1 (find-class 'proto-object) 'proto-object\ class)
(m::metaclass/1 (find-class 'proto-object\ class) 'metaclass)

(define-method% (proto-object m::class/0)
  (slot-value (class-of self) '%metaclass-instance))

(define-method% (proto-object m::initialize/0)
  self)

;;; ====================================================================
;;;   Object

(defclass object (proto-object)
  ()
  (:metaclass symbolic-smalltalk-class))

(defclass object\ class (class)
  ()
  (:metaclass symbolic-smalltalk-metaclass))

(m::metaclass/1 (find-class 'object) 'object\ class)
(m::metaclass/1 (find-class 'object\ class) 'metaclass)

;;; ====================================================================
;;;   Behavior

(defclass behavior (object)
  (%class)
  (:metaclass symbolic-smalltalk-class))

(defclass behavior\ class (object\ class)
  ()
  (:metaclass symbolic-smalltalk-metaclass))

(m::metaclass/1 (find-class 'behavior) 'behavior\ class)
(m::metaclass/1 (find-class 'behavior\ class) 'metaclass)

(define-method% (behavior m::new/0)
  (m::initialize/0 (make-instance (slot-value self '%class))))

(define-method% (behavior m::basic-new/0)
  (make-instance (slot-value self '%class)))

(define-method% (behavior m::instance-variables/0)
  (loop :for slot-definition :in (closer-mop:class-direct-slots
                                  (slot-value self '%class))
        :when (and (eq :instance (closer-mop:slot-definition-allocation
                                  slot-definition))
                   ;; Indexed variables
                   (not (eq (closer-mop:slot-definition-name slot-definition)
                            '%array)))
          :collect (closer-mop:slot-definition-name slot-definition)))

(define-method% (behavior m::class-variables/0)
  (loop :for slot-definition :in (closer-mop:class-direct-slots
                                  (slot-value self '%class))
        :when (eq :class (closer-mop:slot-definition-allocation
                          slot-definition))
          :collect (closer-mop:slot-definition-name slot-definition)))

;; No variable access.

(define-method% (behavior m::add-selector/with-lambda/2 selector lambda-form)
  (assert (eq 'lambda (first lambda-form)))
  (let* ((tr (m::new/0 (the-class 'proto-selector-translation)))
         (selector-symbol (m::translate-selector/1 tr selector))
         (gf (closer-mop:ensure-generic-function
              selector-symbol
              :generic-function-class 'symbolic-smalltalk-generic-function
              :lambda-list (second lambda-form)))
         (specializers (case (m::classify-selector/1 tr selector)
                         (:unary (list (slot-value self '%class)))
                         (:binary (list (slot-value self '%class)
                                        (find-class t)))
                         (:keyword (list* (slot-value self '%class)
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

(define-method% (behavior m::add-slot-accessor/1 slot-name)
  (let ((value (gensym)))
    (m::add-selector/with-lambda/2 self
                                   slot-name
                                   `(lambda (self)
                                      (slot-value self ',slot-name)))
    (m::add-selector/with-lambda/2 self
                                   (list (intern (string slot-name) :keyword))
                                   `(lambda (self ,value)
                                      (setf (slot-value self ',slot-name) ,value)
                                      self))))

(define-method% (behavior m::add-class-slot-accessor/1 slot-name)
  (let ((value (gensym)))
    (m::add-selector/with-lambda/2 self
                                   slot-name
                                   `(lambda (self)
                                      (slot-value
                                       (closer-mop:class-prototype
                                        (slot-value self '%class))
                                       ',slot-name)))

    (m::add-selector/with-lambda/2 self
                                   (list (intern (string slot-name) :keyword))
                                   `(lambda (self ,value)
                                      (setf
                                       (slot-value
                                        (closer-mop:class-prototype
                                         (slot-value self '%class))
                                        ',slot-name)
                                       ,value)
                                      self))))

;;; ====================================================================
;;;   Class description

(defclass class-description (behavior)
  (name)
  (:metaclass symbolic-smalltalk-class))

(defclass class-description\ class (behavior\ class)
  ()
  (:metaclass symbolic-smalltalk-metaclass))

(m::metaclass/1 (find-class 'class-description) 'class-description\ class)
(m::metaclass/1 (find-class 'class-description\ class) 'metaclass)

(defmethod print-object ((self class-description) stream)
  (if (slot-boundp self 'name)
      (print-unreadable-object (self stream :type t)
        (prin1 (slot-value self 'name) stream))
      (call-next-method)))

;;; ====================================================================
;;;   Class

(defclass class (class-description)
  ()
  (:metaclass symbolic-smalltalk-class))

(defclass class\ class (class-description\ class)
  ()
  (:metaclass symbolic-smalltalk-metaclass))

(m::metaclass/1 (find-class 'class) 'class\ class)
(m::metaclass/1 (find-class 'class\ class) 'metaclass)

(define-method% (class m::subclass/1 name)
  (m::subclass/instance-variable-names/class-variable-names/3
   self name nil nil))

(define-method% (class m::subclass/instance-variable-names/class-variable-names/3
                       name instance-variables class-variables)
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
                     (m::metaclass-name/1 (the-class 'metaclass) name)
                     :direct-superclasses (list (class-of self))
                     :direct-slots nil
                     :metaclass (find-class 'symbolic-smalltalk-metaclass)))
         (instance-slots
          (mapcar (lambda (x)
                    (list :name x :initform nil :initfunction initfunction))
                  instance-variables))
         (lisp-class (closer-mop:ensure-class
                      name
                      :direct-superclasses (list (slot-value self '%class))
                      :direct-slots (append instance-slots class-slots)
                      :metaclass (find-class 'symbolic-smalltalk-class))))

    (m::metaclass/1 lisp-class (class-name metaclass))
    (m::metaclass/1 metaclass 'metaclass)

    (dolist (slot class-variables)
      (m::add-class-slot-accessor/1
       (m::class/0 (the-class name))
       slot))

    (dolist (slot class-variables)
      (m::add-slot-accessor/1
       (m::metaclass-instance/0 lisp-class)
       slot))

    (dolist (slot instance-variables)
      (m::add-slot-accessor/1
       (m::metaclass-instance/0 lisp-class)
       slot))

    (the-class name)))

;;; ====================================================================
;;;   Metaclass

(defclass metaclass (class-description)
  ()
  (:metaclass symbolic-smalltalk-class))

(defclass metaclass\ class (class-description\ class)
  ()
  (:metaclass symbolic-smalltalk-metaclass))

(m::metaclass/1 (find-class 'metaclass) 'metaclass\ class)
(m::metaclass/1 (find-class 'metaclass\ class) 'metaclass)

;; Should this be a defun instead?
(define-method% (metaclass\ class m::metaclass-name/1 class-name)
  (intern (format nil "~A CLASS" (string class-name))
          (symbol-package class-name))) ;; Was *package*

;;; ====================================================================
;;;   Proto selector translation

(defclass proto-selector-translation (object) ()
  (:metaclass symbolic-smalltalk-class))

(defclass proto-selector-translation\ class (object\ class)
  ()
  (:metaclass symbolic-smalltalk-metaclass))

(m::metaclass/1 (find-class 'proto-selector-translation)
                'proto-selector-translation\ class)

(m::metaclass/1 (find-class 'proto-selector-translation\ class)
                'metaclass)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *binary-operators* "-,~!@%&*+/=<>?|\\"))

(define-method% (proto-selector-translation
                 m::intern-unary-selector/1
                 name)
  (intern (format nil "~A/0" (string name)) :symbolic-smalltalk-methods))

(define-method% (proto-selector-translation
                 m::intern-binary-selector/1
                 name)
  (intern (format nil "~A/1" (string name)) :symbolic-smalltalk-methods))

(define-method% (proto-selector-translation
                 m::intern-keyword-selector/1
                 keywords)
  (intern (format nil "~{~A/~}~A"
                  (mapcar #'string keywords)
                  (length keywords))
          :symbolic-smalltalk-methods))

(define-method% (proto-selector-translation
                 m::classify-selector/1
                 selector)
  (cond ((and (consp selector)
              (every #'keywordp selector))
         :keyword)

        ((and (symbolp selector)
              (every (lambda (c)
                       (find c #.*binary-operators*))
                     (symbol-name selector)))
         :binary)

        ((symbolp selector)
         :unary)

        (t (error "Bad selector: ~S" selector))))

(define-method% (proto-selector-translation
                 m::translate-selector/1
                 selector)
  (ecase (m::classify-selector/1 self selector)
    (:keyword (m::intern-keyword-selector/1 self selector))
    (:binary (m::intern-binary-selector/1 self selector))
    (:unary (m::intern-unary-selector/1 self selector))))

(define-method% (proto-selector-translation
                 m::classify-arglist/1
                 arglist)
  (let ((len (length arglist)))
    (cond ((keywordp (first arglist)) ;; keyword
           (assert (evenp len))
           :keyword)

          ((and (= 2 len) ;; + other
                (every (lambda (c)
                         (find c #.*binary-operators*))
                       (string (first arglist))))
           :binary)

          ((and (= 1 len)
                (symbolp (first arglist)))
           :unary)

          (t :invalid))))

(define-method% (proto-selector-translation
                 m::translate-arglist/1
                 arglist)
  (ecase (m::classify-arglist/1 self arglist)
    (:keyword
     (m::intern-keyword-selector/1
      self
      (loop :for (keyword value) :on arglist :by #'cddr
            :do (assert (keywordp keyword))
            :collect (symbol-name keyword))))

    (:binary (m::intern-binary-selector/1 self (string (first arglist))))

    (:unary (m::intern-unary-selector/1 self (first arglist)))))

(define-method% (proto-selector-translation
                 m::extract-parameters-from-arglist/1
                 arglist)
  (ecase (m::classify-arglist/1 self arglist)
    (:keyword
     (loop :for (keyword value) :on arglist :by #'cddr
           :do (assert (keywordp keyword))
           :collect value))
    (:binary (rest arglist))
    (:unary nil)))

(define-method% (proto-selector-translation
                 m::translate-message-to/with-arguments/2
                 recipient arguments)
  (let ((selector-symbol (m::translate-arglist/1 self arguments))
        (parameters (m::extract-parameters-from-arglist/1 self arguments)))
    `(,selector-symbol ,recipient ,@parameters)))

;;; ====================================================================
;;;   Message

(defclass message\ class (object\ class)
  ()
  (:metaclass symbolic-smalltalk-metaclass))

(defclass message (object)
  (recipient selector arguments)
  (:metaclass symbolic-smalltalk-class))

(m::metaclass/1 (find-class 'message)
                'message\ class)

(m::metaclass/1 (find-class 'message\ class)
                'metaclass)

(define-method% (message m::recipient/0)
  (slot-value self 'recipient))

(define-method% (message m::recipient/1 recipient)
  (setf (slot-value self 'recipient) recipient))

(define-method% (message m::selector/0)
  (slot-value self 'selector))

(define-method% (message m::selector/1 selector)
  (setf (slot-value self 'selector) selector))

(define-method% (message m::arguments/0)
  (slot-value self 'arguments))

(define-method% (message m::arguments/1 arguments)
  (setf (slot-value self 'arguments) arguments))

;;; ====================================================================
;;;   Define method

;; Todo: Investigate using STANDARD-INSTANCE-ACCESS for slot
;; macrolets.

(defmacro define-method ((type &rest arguments) &body body)
  (let* ((tr (m::new/0 (the-class 'proto-selector-translation)))
         (function-name (m::translate-arglist/1 tr arguments))
         (parameters (m::extract-parameters-from-arglist/1 tr arguments))
         (self (intern "SELF" *package*))
         (generic-parameters (loop :for p :in parameters
                                   :for i :from 0
                                   :collect (intern
                                             (concatenate 'string "X" (princ-to-string i))
                                             (find-package :symbolic-smalltalk.proto))))
         (eql-specializer-p (and (consp type)
                                 (eq 'eql (car type))))
         (class-slots (when (and (not eql-specializer-p)
                                 (typep (find-class type) 'symbolic-smalltalk-class))
                        (append (m::instance-variables/0 (the-class type))
                                (m::class-variables/0 (the-class type)))))
         (class-slots/not-shadowed (set-difference class-slots parameters))
         (lisp-class (cond (eql-specializer-p type)
                           ((typep (find-class type) 'symbolic-smalltalk-class)
                            (class-name (slot-value (the-class type) '%class)))
                           (t type)))
         ;; Need to access :class allocated slots specially in class methods.
         (class-variables (cond ((and (not eql-specializer-p)
                                      (typep (find-class type) 'symbolic-smalltalk-metaclass))
                                 (m::class-variables/0
                                  (m::this-class/0 (find-class type))))))
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
                            (m::metaclass-name/1
                             (the-class 'metaclass)
                             type)
                            `(eql (find-class ',type)))))
    `(define-method (,metaclass-name ,@arguments)
       ,@body)))

;;; ====================================================================
;;;   Send

(defmacro send (recipient &body arguments)
  (let ((tr (m::new/0 (the-class 'proto-selector-translation))))
    (m::translate-message-to/with-arguments/2 tr recipient arguments)))

(defmacro sendc (recipient &body arguments)
  (let ((tr (m::new/0 (the-class 'proto-selector-translation))))
    (m::translate-message-to/with-arguments/2
     tr
     `(the-class ',recipient)
     arguments)))

(defmacro cascade (recipient &body messages)
  (let* ((r (gensym))
         (cascade (loop :for m :in messages
                        :for m* = (if (consp m)
                                      m
                                      (list m))
                        :collect `(send ,r ,@m*))))
    `(let ((,r ,recipient))
       ,@cascade)))

;;; ====================================================================
;;;   Define class

(defmacro define-class (name superclass instance-variables class-variables)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (m::subclass/instance-variable-names/class-variable-names/3
      (the-class ',superclass)
      ',name
      ',instance-variables
      ',class-variables)))


;;;; From here on, we can use define-class, define-method and send.


;;; ====================================================================
;;;   Selector

(defmacro selector (name)
  (let* ((tr (m::new/0 (the-class 'proto-selector-translation)))
         (translated (m::translate-selector/1 tr name)))
    `(quote ,translated)))


(export '(object

send sendc cascade))
