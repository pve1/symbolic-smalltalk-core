(in-package :symbolic-smalltalk-core)

;;; Symbolic smalltalk class

(defclass symbolic-smalltalk-class (standard-class)
  ;; The single instance of the metaclass for the
  ;; class in question.
  ((metaclass-instance :initarg :metaclass-instance
                       :accessor metaclass-instance
                       :initform nil)))

(defclass symbolic-smalltalk-metaclass (symbolic-smalltalk-class)
  ((this-metaclass-instance :initarg :this-metaclass-instance
                            :accessor this-metaclass-instance
                            :initform nil)))

(defmethod c2cl:validate-superclass ((class symbolic-smalltalk-class)
                                     (superclass standard-class))
  t)

(defmethod c2cl:validate-superclass ((class symbolic-smalltalk-metaclass)
                                     (superclass symbolic-smalltalk-class))
  t)


(defun the-class (symbol)
  (let ((class (find-class symbol)))
    (if (typep class 'symbolic-smalltalk-class)
        (metaclass-instance (find-class symbol))
        class)))

;; Set the metaclass instance.
(defmethod initialize-metaclass ((smalltalk-class symbol))
  (initialize-metaclass (find-class smalltalk-class)))

(defmethod initialize-metaclass ((smalltalk-class symbolic-smalltalk-class))
  (let* ((metaclass-name (metaclass-name (class-name smalltalk-class)))
         (metaclass-class (find-class metaclass-name))
         (metaclass-instance (make-instance metaclass-class
                                            :behavior-class smalltalk-class)))
    (send metaclass-instance initialize)
    (setf (metaclass-instance smalltalk-class) metaclass-instance
          (this-metaclass-instance metaclass-class) metaclass-instance)))

(defmethod initialize-metaclass ((smalltalk-class symbolic-smalltalk-metaclass))
  (setf (metaclass-instance smalltalk-class)
        (make-instance 'metaclass :behavior-class smalltalk-class)))
