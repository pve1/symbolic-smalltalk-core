(in-package :symbolic-smalltalk-core)

(defun call-super (gf args)
  (let* ((methods (closer-mop:compute-applicable-methods-using-classes
                   gf
                   (mapcar #'class-of args))))
    (funcall (closer-mop:method-function (second methods)) args nil)))

(defmacro send (recipient &body message)
  (if (and (symbolp recipient)
           (string= "SUPER" recipient))
      (let ((gf (translate-message message)))
        `(call-super #',gf (list ,(self) ,@(extract-parameters-from-message
                                            message))))
      (translate-send recipient message)))

(defmacro send-class (recipient &body message)
  (translate-send `(the-class ',recipient) message))

(defmacro cascade (recipient &body messages)
  (let* ((r (gensym))
         (cascade (loop :for m :in messages
                        :for m* = (if (consp m)
                                      m
                                      (list m))
                        :collect `(send ,r ,@m*))))
    `(let ((,r ,recipient))
       ,@cascade)))

;; Special cascade* to make parsing easier.
(defmacro cascade* (recipient &body messages)
  (destructuring-bind (send operand &rest message) recipient
    (declare (ignore send))
    `(cascade ,operand
       ,message
       ,@(mapcar #'cddr messages))))
