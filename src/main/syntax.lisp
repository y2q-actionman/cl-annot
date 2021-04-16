(in-package :cl-user)
(defpackage cl-annot.syntax
  (:nicknames :annot.syntax)
  (:use :cl
        :annot.core
        :annot.expand)
  (:export :annotation-syntax-reader
           :enable-annot-syntax))
(in-package :annot.syntax)

(defun read-annotation (stream)
  (let ((annot (read stream t nil t)))  ; (y2q) This accepts '(@    export ...) form.
    (or (annotation-real annot) annot)))

(defun read-annotation-arguments (stream arity)
  (loop repeat arity collect (read stream t nil t)))

(defun annotation-syntax-reader (stream char)
  (declare (ignore char))
  (let* ((annot (read-annotation stream)) ; (y2q) If this is not a symbol, simply an error raised.
         (arity (annotation-arity annot))
         (args (read-annotation-arguments stream arity)))
    (if (annotation-inline-p annot)
        (expand-annotation annot args)
        (annotation-form annot args))))

(defun %enable-annot-syntax ()
  (setf *readtable* (copy-readtable))
  ;; (y2q) I think making '@' as a terminating macro char is bad idea.
  (set-macro-character #\@ #'annotation-syntax-reader))

(defmacro enable-annot-syntax ()
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (%enable-annot-syntax)))
