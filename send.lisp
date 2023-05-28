(in-package :symbolic-smalltalk-core)

(defmacro send (recipient &body arguments)
  (translate-message recipient arguments))

(defmacro sendc (recipient &body arguments)
  (translate-message `(the-class ',recipient) arguments))

(defmacro cascade (recipient &body messages)
  (let* ((r (gensym))
         (cascade (loop :for m :in messages
                        :for m* = (if (consp m)
                                      m
                                      (list m))
                        :collect `(send ,r ,@m*))))
    `(let ((,r ,recipient))
       ,@cascade)))
