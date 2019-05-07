(in-package :cl-user)
(defpackage cl-annot.expand
  (:nicknames :annot.expand)
  (:use :cl
        :annot.util
        :annot.core)
  (:export :expand-annotation))
(in-package :annot.expand)

;;; (y2q) moved from util.lisp. It is called only by here.
(defun macroexpand-some (form)
  "Expand FORM once. The result form won't be nil."
  (multiple-value-bind (new-form expanded-p)
      (macroexpand-1 form)
    ;; (y2q) `macroexpand-1' との違いは、展開結果が nil だった場合に、元の form を返す点だけ。
    ;; そうする理由はなんだろう？
    (if (or (not expanded-p) (null new-form))
        (values form nil)
        (values new-form expanded-p))))

(defun expand-annotation (annot args)
  "Expand ANNOT. ARGS will be expanded prior to this
form (call-by-value)."
  ;; (y2q)
  ;; おそらく、ここで annotation 以降を先に expand することに、ほぼ全ての annotation が依存しているのだが、
  ;; それをするかどうかは個々の annotation に任されるべきだと思う。
  ;; ここで展開すると、ユーザー定義の def-系 form に作用する annotation を作ることが出来ないのでは？
  (let ((args (mapcar #'expand-annotation-form args)))
    (values (macroexpand-some `(,annot ,@args)))))

(defun expand-annotation-form (form)
  "Expand annotation FORM if possible."
  (if (annotation-form-p form)
      (expand-annotation (cadr form) (cddr form))
      form))

(defmacro %annotation (annot &rest args)
  "Annotation Expansion Engine."
  (expand-annotation annot args))
