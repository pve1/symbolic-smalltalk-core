(in-package :symbolic-smalltalk-core)

;;; Selector translation

;; The naming scheme for methods goes like this:

;; Unary: Object foo -> m::foo/0
;; Binary: Object + foo -> m::+/1
;; Keyword: Object :do foo :and-then bar -> m::do/and-then/2

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *binary-operators* "-,~!@%&*+/=<>?|\\"))

(defvar *method-package* :symbolic-smalltalk-methods)

(defun intern-unary-selector (name)
  (intern (format nil "~A/0" (string name)) *method-package*))

(defun intern-binary-selector (name)
  (intern (format nil "~A/1" (string name)) *method-package*))

(defun intern-keyword-selector (keywords)
  (intern (format nil "~{~A/~}~A"
                  (mapcar #'string keywords)
                  (length keywords))
          *method-package*))

;; foo bar
;; + , -
;; (:foo :bar) (:add :to)
(defun classify-selector (selector)
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

(defun translate-selector (selector)
  (ecase (classify-selector selector)
    (:keyword (intern-keyword-selector selector))
    (:binary (intern-binary-selector selector))
    (:unary (intern-unary-selector selector))))

(defun classify-message (message)
  (let ((len (length message)))
    (cond ((keywordp (first message)) ;; keyword
           (assert (evenp len))
           :keyword)
          ((and (= 2 len) ;; + other
                (every (lambda (c)
                         (find c #.*binary-operators*))
                       (string (first message))))
           :binary)
          ((and (= 1 len)
                (symbolp (first message)))
           :unary)
          (t :invalid))))

(defun translate-message (message)
  (ecase (classify-message message)
    (:keyword
     (intern-keyword-selector
      (loop :for (keyword value) :on message :by #'cddr
            :do (assert (keywordp keyword))
            :collect (symbol-name keyword))))
    (:binary (intern-binary-selector (string (first message))))
    (:unary (intern-unary-selector (first message)))))

(defun extract-parameters-from-message (message)
  (ecase (classify-message message)
    (:keyword
     (loop :for (keyword value) :on message :by #'cddr
           :do (assert (keywordp keyword))
           :collect value))
    (:binary (rest message))
    (:unary nil)))

(defun extract-selector-from-message (message)
  (ecase (classify-message message)
    (:keyword
     (loop :for (keyword value) :on message :by #'cddr
           :do (assert (keywordp keyword))
           :collect keyword))
    (:binary (first message))
    (:unary (first message))))

(defun translate-send (recipient message)
  (let ((selector-symbol (translate-message message))
        (parameters (extract-parameters-from-message message)))
    `(,selector-symbol ,recipient ,@parameters)))

(defmacro selector (name)
  `(quote ,(translate-selector name)))
