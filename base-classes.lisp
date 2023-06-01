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

;;; Behavior

(define-smalltalk-class behavior (object)
  ((behavior-class :initarg :behavior-class
                   :accessor behavior-class
                   :type symbolic-smalltalk-class)))

(defmethod new ((behavior behavior))
  (send (make-instance (behavior-class behavior)) initialize))

(defmethod basic-new ((behavior behavior))
  (allocate-instance (behavior-class behavior)))

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

(defun selector-specializers (class selector)
  (when (typep class 'behavior)
    (setf class (behavior-class class)))
  (case (classify-selector selector)
    (:unary (list class))
    (:binary (list class (find-class t)))
    (:keyword (list* class (loop :for i :from 0 :below (length selector)
                                 :collect (find-class t))))))

;; CLASS is a symbol that names a class.
(defmacro with-instance-variables ((self class) &body body)
  (let ((inst-vars (instance-variables (the-class class))))
    `(symbol-macrolet ,(mapcar (lambda (slot)
                                 `(,slot (slot-value ,self ',slot)))
                        inst-vars)
       ,@body)))

(defmacro with-class-variables ((self class) &body body)
  (let ((class-vars (class-variables (the-class class))))
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
        (setf (function-selector gf) selector)
        (add-method gf (apply #'make-instance
                              (closer-mop:generic-function-method-class gf)
                              :specializers specializers
                              :lambda-list (second lambda-form)
                              :function (funcall (compile nil final-lambda-expression))
                              initargs))))))

(defmethod remove-method-from-class ((behavior behavior) selector)
  (let* ((selector-symbol (translate-selector selector))
         (gf (fdefinition selector-symbol))
         (method (find-method gf
                              nil
                              (selector-specializers (behavior-class behavior)
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

;; Todo: Investigate using STANDARD-INSTANCE-ACCESS.

(defmethod add-slot-accessor ((behavior behavior) slot-name)
  (let ((value (gensym)))
    (add-method-to-class behavior
                         slot-name
                         `(lambda (behavior)
                            (slot-value behavior ',slot-name)))
    (add-method-to-class behavior
                         (list (intern (string slot-name) :keyword))
                         `(lambda (behavior ,value)
                            (setf (slot-value behavior ',slot-name) ,value)
                            behavior))))

(defmethod behavior-selectors ((behavior behavior))
  (let (methods)
    (do-symbols (sym *method-package*)
      (when (fboundp sym)
        (let* ((fun (fdefinition sym))
               (sel (function-selector fun))
               (spec (selector-specializers (behavior-class behavior)
                                            sel))
               (method (find-method fun nil spec nil)))
          (when method
            (push sel methods)))))
    methods))

(defmethod superclass ((behavior behavior))
  (let ((super (first
                (closer-mop:class-direct-superclasses
                 (behavior-class behavior)))))
    ;; Pretend that proto-object has no superclass.
    (if (typep super 'symbolic-smalltalk-class)
        (metaclass-instance super)
        nil)))

(defmethod all-superclasses ((behavior behavior))
  (let ((super (superclass behavior)))
    (if super
        (cons super (all-superclasses super))
        nil)))

(defmethod subclasses ((behavior behavior))
  (mapcar #'metaclass-instance
          (closer-mop:class-direct-subclasses
           (behavior-class behavior))))

(defmethod all-subclasses ((behavior behavior))
  (let* ((direct-subclasses (subclasses behavior))
         (all-subclasses nil)
         (stack1 (list direct-subclasses))
         (stack2 nil))
    ;; Collect subclasses breadth-first.
    ;; Loop until both stacks are empty.
    (loop :while (or stack1 stack2)
          :do (when (null stack1)
                ;; Swap stacks
                (setf stack1 stack2
                      stack2 nil))
              (loop :for classes = (pop stack1)
                    :while classes
                    ;; Collect into stack2
                    :do (loop :for class :in classes
                              :do (push class all-subclasses)
                                  (let ((subclasses (subclasses class)))
                                    (when subclasses
                                      (push subclasses stack2))))))
    (nreverse all-subclasses)))

(defmethod includes-selector-p ((behavior behavior) selector)
  (let* ((sel (translate-selector selector))
         (spec (selector-specializers behavior sel)))
     (and (fboundp sel)
          (find-method (fdefinition sel) nil spec nil)
          t)))

(defmethod can-understand-p ((behavior behavior) selector)
  (let* ((sel (translate-selector selector)))
    (when (fboundp sel)
      (let* ((spec (selector-specializers behavior sel))
             (classes (cons behavior (all-superclasses behavior)))
             (fun (fdefinition sel)))
        (loop :for class :in classes
              :when (find-method fun
                                 nil
                                 (cons (behavior-class class) (rest spec))
                                 nil)
                :return class)))))

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

(defmethod subclass ((class class) name &key instance-variables class-variables)
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
                     :direct-superclasses (list (class-of class))
                     :direct-slots class-slots
                     :metaclass (find-class 'symbolic-smalltalk-metaclass)))
         (instance-slots
           (mapcar (lambda (x)
                     (list :name x :initform nil :initfunction initfunction))
                   instance-variables))
         (lisp-class (closer-mop:ensure-class
                      name
                      :direct-superclasses (list (behavior-class class))
                      :direct-slots instance-slots
                      :metaclass (find-class 'symbolic-smalltalk-class))))

    ;; Will send initialize to the metaclass-instance.
    (initialize-metaclass lisp-class)
    (initialize-metaclass metaclass)
    (the-class name)))
