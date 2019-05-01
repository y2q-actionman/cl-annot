(in-package :cl-user)
(defpackage cl-annot.util
  (:nicknames :annot.util)
  (:use :cl)
  (:export 
           ;; Macros
           :macrop
           :macroexpand-some
           :macroexpand-until-normal-form
           ;; Progns
           :progn-form-last
           :progn-form-replace-last
           ;; Definitions
           :definition-form-symbol
           :definition-form-type
           ;; Functions
           :replace-function-body
           ))
(in-package :annot.util)

(defun macrop (symbol)
  "Return non-nil if SYMBOL is a macro."
  (and (symbolp symbol)
       (macro-function symbol)
       t))

(defun macroexpand-some (form)
  "Expand FORM once. The result form won't be nil."
  (multiple-value-bind (new-form expanded-p)
      (macroexpand-1 form)
    (if (or (not expanded-p) (null new-form))
        (values form nil) ; (y2q) 展開結果が nil だった場合に、元の form を返す理由はなんだろう？
        (values new-form expanded-p))))

(defun macroexpand-until-normal-form (form)
  "Expand FORM until it brecomes normal-form."
  ;; (y2q) What is 'normal'?
  ;; Code says:
  ;; - Its car is a macro symbol in common-lisp package. What it means?
  ;; - If FORM is not expanded, the original FORM is returned. This is just `macroexpand-1'.
  ;; - If FORM is expanded to nil, the original FORM is returned. What?
  ;; What is the difference from `macroexpand'? (stopping at 'CL' package?)
  (if (and (consp form)
           (macrop (car form))
           (let ((package (symbol-package (car form))))
             (and package
                  (member package
                          (list (find-package :cl)
                                #+clisp (find-package :clos))))))
      (values form nil)
      (multiple-value-bind (new-form expanded-p)
          (macroexpand-1 form)
        (if (or (not expanded-p) (null new-form))
            (values form nil)
            (values (macroexpand-until-normal-form new-form) t)))))

(defun progn-form-last (progn-form)
  "Return the last form of PROGN-FORM which should be evaluated at
last. If macro forms seen, the macro forms will be expanded using
MACROEXPAND-UNTIL-NORMAL-FORM."
  (let ((progn-form (macroexpand-until-normal-form progn-form)))
    (if (and (consp progn-form)
             (eq (car progn-form) 'progn))
        (progn-form-last (car (last progn-form)))
        progn-form)))

(defun progn-form-replace-last (last progn-form)
  "Replace the last form of PROGN-FORM with LAST. If LAST is a
function, the function will be called with the last form and used for
replacing. If macro forms seen, the macro forms will be expanded using
MACROEXPAND-UNTIL-NORMAL-FORM."
  ;; (y2q) I think the order of arguments should be: (PROGN-FORM LAST).
  ;; A strange point: many annot finds the definition-form by
  ;; this function. But it returns only last one -- what occurs two or more expansion?
  ;; What we want is a one like `flatten-progn'.
  (let ((progn-form (macroexpand-until-normal-form progn-form)))
    (if (and (consp progn-form)
             (eq (car progn-form) 'progn))
        `(,@(butlast progn-form)
            ,(progn-form-replace-last last (car (last progn-form))))
        (if (functionp last)
            (funcall last progn-form)
            last))))

(defun definition-form-symbol (definition-form)
  "Return the symbol of DEFINITION-FORM."
  (let* ((form (progn-form-last definition-form))
         (second (when (consp form)
                   (second form))))
    (if (consp second)
        (cond
          ((eq (car second) 'setf) (second second))
          (t (first second)))    ; fix for the long-form of defstruct
        second)))

(defun definition-form-type (definition-form)
  "Return the type of DEFINITION-FORM."
  (let* ((form (progn-form-last definition-form))
         ;; (y2q) Because `progn-form-last' does not see if it is not `progn', this is weird.
         ;; -- what occurs on `let'?
         (type (when (consp form)
                 (car form))))
    type))

;;; (y2q) This is not used anywhere.
#+ ()
(defun replace-function-body (function function-definition-form)
  "Replace the body of FUNCTION-DEFINITION-FORM by calling FUNCTION
with name, lambda-list and the body as arguments."
  (progn-form-replace-last
   (lambda (function-definition-form)
     (destructuring-bind (type name lambda-list . body)
         function-definition-form
       (let (header)
         (when (and (stringp (car body))
                    (cdr body))
           (setf header (list (car body))
                 body (cdr body)))
         `(,type ,name ,lambda-list
                 ,@header
                 ,(funcall function name lambda-list body)))))
   function-definition-form))

;;; (y2q) This is not used anywhere.
#+ ()
(defun replace-slot-specifiers (function class-definition-form)
  "Replace slot-specifiers of CLASS-DEFINITION-FORM with FUNCTION. The
result value will be a class definition form also."
  (progn-form-replace-last
   (lambda (class-definition-form)
     (destructuring-bind (type name supers slots &rest options)
         class-definition-form
       `(,type ,name ,supers ,(mapcar function slots) ,@options)))
   class-definition-form))
