(in-package :cl-user)
(defpackage cl-annot.helper
  (:nicknames :annot.helper)
  (:use :cl
        :annot.util
        :annot.core
        :annot.syntax)
  (:export :defannotation
           :annotation))
(in-package :annot.helper)

(defun set-annotation-options (name options)
  (when (getf options :alias)
    (setf (annotation-real (getf options :alias)) name))
  (when (getf options :arity)
    (setf (annotation-arity name) (getf options :arity)))
  (when (getf options :inline)
    (setf (annotation-inline-p name) t)))

;;; (y2q) `arity' という値を lambda-list を解析して自動で補完できるはず。
(defmacro defannotation (name lambda-list options &body body)
  ;; (y2q) `options' can be written with `&key'.
  `(progn
     (set-annotation-options ',name ',options)
     (defmacro ,name ,lambda-list ,@body)))

(defannotation annotation (options function-definition-form)
    (:arity 2)
  (let ((name (definition-form-symbol
                  (progn-form-last function-definition-form))))
    `(progn
       (set-annotation-options ',name ',options)
       ,function-definition-form)))
