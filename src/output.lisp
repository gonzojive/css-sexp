(in-package :css-sexp)
(declaim (optimize (debug 3)))
(defparameter *format-shortcut* 'fmt)
(defparameter *raw-string-shortcut* 'raw)
(defparameter *css-value-shortcut* 'val)

;;; conditions and errors
(define-condition css-sexp-condition (condition)
  ()
  (:documentation "Any condition related to a problem with CSS."))

(define-condition css-sexp-error (error css-sexp-condition)
  ()
  (:documentation "Any CSS-SEXP error."))

(define-condition css-sexp-unexpected-value (css-sexp-error)
  ((unexpected-value :initarg :unexpected-value :accessor unexpected-value)
   (expected-value-description :initarg :expected-description :accessor expected-value-description
			       :type (or null string) :initform nil))
  (:report (lambda (condition stream)
	     (write-string (expected-value-description condition) stream)))
  (:documentation "Thrown when the type of VALUE is unexpected."))

;;; the *-to-commands functions all return a single eval-able expression,
;;; not a list of eval-able expressions.  (lists are progn's)

(defmacro with-css-output ((stream-var) &body body)
  "VAR is a symbol that designates a stream.  Each statement in the
body should be a valid CSS RULE"
  (sexps-to-commands body stream-var))

(defun simplify-progn-forms (forms)
  (mapcan #'(lambda (form)
	      (cond
		((and (consp form) (eql 'progn (car form)))
		 (simplify-progn-forms (rest form)))
		((null form) nil)
		(t
		 (list form))))
	  forms))

(defun simplify-write-sequence-forms (forms)
  (let ((built-up-write-sequence-string nil)
	(built-up-write-sequence-stream '#:unique))
    (let ((result-forms nil))
      (flet ((push-built-up-form ()
	       (when (and built-up-write-sequence-string
			  (> (length built-up-write-sequence-string) 0))
		 (push `(write-sequence ,built-up-write-sequence-string ,built-up-write-sequence-stream)
		       result-forms))
	       (setf built-up-write-sequence-string ""
		     built-up-write-sequence-stream '#:unique)))
	(map nil #'(lambda (form)
		     (cond
		       ((and (consp form)
			     (member (car form) '(write-sequence write-string))
			     (stringp (second form)))
			(when (not (eql (third form)  built-up-write-sequence-stream))
			  (push-built-up-form))
			(setf built-up-write-sequence-string
			      (concatenate 'string built-up-write-sequence-string (second form)))
			(setf built-up-write-sequence-stream
			      (third form)))
		       (t
			(push-built-up-form)
			(push form result-forms))))
	     forms)
	(push-built-up-form)
	(nreverse result-forms)))))

(defmacro with-css-output-to-string ((stream-var) &body body)
  "Same as with-css-output but the result is a string."
  `(with-output-to-string (,stream-var)
     (with-css-output (,stream-var)
       ,@body)))

(defun sexps-to-commands (sexp-forms stream-var)
  "Converts a list of top-level CSSEXPs into commands (lisp code)."
  `(progn ,@(simplify-write-sequence-forms
	     (simplify-progn-forms
	      (mapcar #'(lambda (r) (sexp-rule-to-commands r stream-var))
		      sexp-forms)))))

(defun sexp-rule-to-commands (css-sexp-form stream-var)
  "Converts a rule of the form (selector [rule-lvalue rule-rvalue]*) into commands (lisp sexps)."
  (if (consp css-sexp-form)
      `(progn
	 ,(sexp-selector-to-commands (car css-sexp-form) stream-var)
	 (write-sequence " {
  " ,stream-var)
	 ,(sexp-rule-body-to-commands (rest css-sexp-form) stream-var)
	 (write-sequence "}
" ,stream-var))
;	 (format ,stream-var "}~%"))
      (form-to-commands 'rule css-sexp-form stream-var)))  

(defvar *rule-macro-alist* nil
  "List of macros used to expand special rule properties into a series of rules.")

(defun register-rule-macro (rule-keyword fn)
  "Registers a function that will"
  (push (cons rule-keyword fn) *rule-macro-alist*))

(defmacro defrule (rule-keyword (arg stream-var) &body body)
  "Defines a custom CSS rule that expands into a form that will output
CSS rules for the current selector."
  `(register-rule-macro ',rule-keyword
                        (lambda (,arg ,stream-var)
                          ,@body)))

(defun sexp-rule-body-to-commands (sexp-rule-body stream)
  "Converts a list of rules of the form [lvalue rvalue] into commands."
  `(progn
     ,@(loop :for (prop value) :on sexp-rule-body :by #'cddr
             :collect
             (let ((macro-expander (cdr (assoc prop *rule-macro-alist*))))
               (if macro-expander
                   (funcall macro-expander value stream)
                   `(progn
                      ,(form-to-commands 'property-lvalue prop stream)
                      (write-sequence ": " ,stream)
                      ,(form-to-commands 'property-rvalue value stream)
                      (write-sequence ";
  " ,stream)))))))

(defun sexp-selector-to-commands (sexp-selector stream-var)
  "Converts a selector CSSEXP into lisp SEXP commands."
  (typecase sexp-selector
    (atom
     (form-to-commands 'selector sexp-selector stream-var))
    (cons 
     (let ((expander (selector-expander (car sexp-selector))))
       (if expander
	   (funcall expander stream-var (rest sexp-selector))
	   (form-to-commands 'selector sexp-selector stream-var))))))
;    (t `(write-string ,(coerce sexp-selector 'string) ,stream-var))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *selector-expander-table* (make-hash-table)
    "Hash from selector predicate symbol to an expander function.

The functions that take as their first argument the stream variable and
as their second argument a CSS-SEXP selector form."))

(defmacro defselector (name (stream-var lambda-list) &body body)
  (let ((%css-sexp (gensym "CSS-SEXP")))
    `(setf (gethash ',name *selector-expander-table*)
	   #'(lambda (,stream-var ,%css-sexp)
	       (destructuring-bind ,lambda-list ,%css-sexp ,@body)))))

(defun selector-expander (op-name)
  "Returns the selector expander function, a function that takes a stream
and a CSS-SEXP selector form as arguments and returns a lisp code expansion,
by the given name"
  (gethash op-name *selector-expander-table*))

(defselector or (stream (&rest selectors))
  "comma-separted selectors"
  (let ((expansions (mapcar #'(lambda (sexp)
				(sexp-selector-to-commands sexp stream))
			    selectors)))
    `(progn
       ,@(loop :for (expansion more?) :on expansions
	    :collect `(progn ,expansion 
			     ,(when more? `(write-sequence ", " ,stream))))))) ;`(write-char #\, ,stream)))))))

(defselector ancestor (stream-var (&rest selectors))
  "comma-separted selectors"
  (let ((expansions (mapcar #'(lambda (sexp)
				(sexp-selector-to-commands sexp stream-var))
			    selectors)))
    `(progn
       ,@(loop :for (expansion more?) :on expansions
	    :collect `(progn ,expansion 
			     ,(when more? `(write-sequence " " ,stream-var)))))))

(defselector direct-ancestor (stream-var (&rest selectors))
  "comma-separted selectors"
  (let ((expansions (mapcar #'(lambda (sexp)
				(sexp-selector-to-commands sexp stream-var))
			    selectors)))
    `(progn
       ,@(loop :for (expansion more?) :on expansions
	    :collect `(progn ,expansion 
			     ,(when more? `(write-sequence " > " ,stream-var)))))))

(defselector class (stream-var (selector css-class))
  "comma-separted selectors"
  (let ((expansion (sexp-selector-to-commands selector stream-var)))
    `(progn ,expansion
	    (write-sequence  "." ,stream-var)
	    ,(form-to-commands 'selector css-class stream-var))))

(defselector id (stream-var (dom-id &optional selector))
  "comma-separted selectors"
  (let ((expansion (sexp-selector-to-commands selector stream-var)))
    `(progn ,expansion
	    (write-sequence  "#" ,stream-var)
	    ,(form-to-commands 'selector dom-id stream-var))))
	    ;,@(when selector (list (form-to-commands 'selector dom-id stream-var))))))

(defgeneric form-to-commands (context form stream-var)
  (:documentation "Generic function to convert an atomic form to a series
of commands that will write appropriate CSS to the stream designated by STREAM-VAR
when they are evaluated.

Context is generally a symbol that describes the context of the atomic form
in the CSS-SEXP document.
Known values are 'PROPERTY-LVALUE, 'PROPERTY-RVALUE, and 'SELECTOR."))

(defmethod form-to-commands (context form stream-var)
  "The default behavior is to simply evaluate the form and not write
anything to the stream."
  (declare (ignore context stream-var))
  form)

(defmethod form-to-commands ((context (eql 'property-rvalue))
			     (sexp-cons cons)
			     stream-var)
  (let ((result-var (gensym "result"))
	(result-var0 (gensym "result"))
	(op (car sexp-cons)))
    (cond
      ((eql op *format-shortcut*)
       `(let ((,result-var0 ,(second sexp-cons))
	      (,result-var ,(third sexp-cons)))
	  (when ,result-var
	    (format ,stream-var ,result-var0 ,result-var))))
      ((eql op *raw-string-shortcut*)
       `(let ((,result-var ,(second sexp-cons)))
	  (when ,result-var (write-string ,result-var ,stream-var))))
      ((eql op *css-value-shortcut*)
       `(let ((,result-var ,(second sexp-cons)))
	  (when ,result-var
	    (write-string (css-value ,result-var) ,stream-var))))
      ((eql op 'rgb)
       `(let ((,result-var (mapcar 'css-value
				   (list ,(second sexp-cons) ,(third sexp-cons) ,(fourth sexp-cons)))))
	  (when ,result-var
	    (format ,stream-var "rgb(~{~A~^, ~})" ,result-var))))
      (t `(write-string ,sexp-cons ,stream-var))
      #+nil
      (t
       (error 'css-sexp-unexpected-value
	      :unexpected-value (car sexp-cons)
	      :expected-description (format nil "Expected ~A, ~A, or ~A as head of list"
					    *format-shortcut*
					    *css-value-shortcut*
					    *raw-string-shortcut*))))))

(defmethod form-to-commands ((context (eql 'property-lvalue))
			     (form symbol) 
			     stream-var)
  "For a keyword, this will write the downcased keyword to the stream.
For any other symbol, it will simply evaluate it."
  (cond
    ((keywordp form)
     `(write-string ,(css-value form) ,stream-var))
    (t
     form)))

(defmethod form-to-commands ((context (eql 'selector))
			     (form symbol)
			     stream-var)
  "For a keyword, this will write the downcased keyword to the stream.
For any other symbol, it will simply evaluate it."
  (cond
    ((keywordp form)
     `(write-string ,(css-value form)  ,stream-var))
    (t
     form)))

(defmethod form-to-commands ((context (eql 'selector))
			     (form string)
			     stream-var)
  "For a keyword, this will write the downcased keyword to the stream.
For any other symbol, it will simply evaluate it."
  `(write-string ,form ,stream-var))

(defmethod form-to-commands ((context (eql 'property-rvalue))
			     form
			     stream-var)
  "This will write any non-nil result of evaluating
the form to the stream after calling CSS-VALUE on the result."
  (typecase form
    ((or real)
     `(write-string ,(css-value form) ,stream-var))
    (string
     `(write-string ,form ,stream-var))
    (t
     (let ((result-var (gensym "result")))
       `(let ((,result-var ,form))
	  (when ,result-var
	    (write-string (css-value ,result-var) ,stream-var)))))))

(defgeneric css-value (lisp-value)
  (:documentation "This will return a string with the CSS equivalent
of the given lisp value.  Lisp values are converted to CSS values
on the following basis:

Lisp real numbers are written in their usual lisp form.
Strings are placed in double quotations marks and escaped according to
   CSS string escape rules.
Keywords are downcased and returned.")
  (:method (lisp-value)
    (error 'css-sexp-unexpected-value
	   :unexpected-value lisp-value
	   :expected-description "CSS-VALUE requires a keyword, real number, or string."))
  (:method ((lisp-value real))
    (format nil (if (integerp lisp-value) "~A" "~F") lisp-value))

  ;; TODO make this actually escape the strings appropriately
  (:method ((lisp-value string))
    (format nil "~S" lisp-value))
  (:method ((lisp-value symbol))
    (if (keywordp lisp-value) 
	(string-downcase (symbol-value lisp-value))
	(call-next-method))))