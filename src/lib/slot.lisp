(in-package :cl-user)
(defpackage cl-annot.slot
  (:nicknames :annot.slot)
  (:use :cl
        :annot.util
        :annot.helper)
  (:import-from :alexandria
                :make-keyword
                :with-gensyms)
  (:export :optional
           :required))
(in-package :annot.slot)

(defmacro def-slot-annotation (name args &body body)
  (with-gensyms (slot-specifier)
    `(defannotation ,name ,(append args (list slot-specifier))
         (:inline t :arity ,(1+ (length args)))
       ;; (y2q) Oh, this macro automagically binds SLOT-NAME and SLOT-OPTIONS !
       (destructuring-bind (slot-name . slot-options)
           (if (consp ,slot-specifier)
               ,slot-specifier
               (list ,slot-specifier))
         ,@body
         (cons slot-name slot-options)))))

;;; (y2q) moved from utils.lisp -- it seems used only by this file.
(defun plist-member (plist prop)
  "Return t if PLIST contains PROP as a property."
  (let ((undef '#:undef))               ; (y2q) why don't use `get-properties'?
    (not (eq (getf plist prop undef) undef))))

(def-slot-annotation optional (init-form)
  (unless (plist-member slot-options :initarg)
    (setf (getf slot-options :initarg)
          (make-keyword slot-name)))
  (unless (plist-member slot-options :initform)
    (setf (getf slot-options :initform) init-form)))

(defun required-argument (name)         ; (y2q) too obscure..
  (error "Must supply ~S" name))

(def-slot-annotation required ()
  (when (plist-member slot-options :initform)
    (error "Required slot ~A must not have :initform" slot-name))
  (unless (plist-member slot-options :initarg)
    (setf (getf slot-options :initarg)
          (make-keyword slot-name)))
  (setf (getf slot-options :initform)
        ;; (y2q) I think it should uses `cerror'.
        `(required-argument ,(getf slot-options :initarg))))
