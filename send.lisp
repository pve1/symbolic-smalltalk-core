(in-package :symbolic-smalltalk-core)

(defmacro send (recipient &body message)
  (translate-send recipient message))

(defmacro send-class (recipient &body message)
  (translate-send `(the-class ,recipient) message))

(defmacro cascade (recipient &body messages)
  (let* ((r (gensym))
         (cascade (loop :for m :in messages
                        :for m* = (if (consp m)
                                      m
                                      (list m))
                        :collect `(send ,r ,@m*))))
    `(let ((,r ,recipient))
       ,@cascade)))
