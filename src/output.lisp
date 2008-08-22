(in-package :css-sexp)

;;; the *-to-commands functions all return a single eval-able expression,
;;; not a list of eval-able expressions.  (lists are progn's)

(defmacro with-css-output ((var) &body body)
  (sexps-to-commands body var))

(defmacro with-css-output-to-string ((var) &body body)
  `(with-output-to-string (,var)
     (with-css-output (,var)
       ,@body)))

(defun sexp-rule-to-commands (css-sexp-form stream)
  (if (consp css-sexp-form)
      `(progn
	 ,(sexp-selector-to-commands (car css-sexp-form) stream)
	 (format ,stream " {~%")
	 ,(sexp-rule-body-to-commands (rest css-sexp-form) stream)
	 (format ,stream "}~%"))
      (stringlike-thing-to-commands css-sexp-form stream)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *selector-expander-table* (make-hash-table)
    "Hash from selector predicate symbol to an expander function.

The functions that take as their first argument the stream variable and
as their second argument a CSS-SEXP selector form."))

(defmacro defselector (name (stream-var lambda-list) &body body)
  (let ((%css-sexp (gensym "CSS-SEXP")))
    (setf (gethash name *selector-expander-table*)
	  `#'(lambda (,stream-var ,%css-sexp)
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
       ,@
       (loop :for (expansion more?) :in expansions
	  :collect `(progn ,expansion 
			   ,@(when more? `(write-char #\, ,stream)))))))

(defun sexp-cons-to-commands (sexp-cons stream)
  ""
  (case (car sexp-cons)
    ('out
     (let ((%result (gensym "RESULT")))
       `(let ((,%result ,(second sexp-cons)))
	  (when ,%result (format ,stream "~A" ,%result)))))
    (t
     (let ((%result (gensym "RESULT")))
       `(let ((,%result ,sexp-cons))
	  (when ,%result (format ,stream "~A" ,%result)))))))
   
(defun sexp-selector-to-commands (sexp-selector stream)
  (typecase sexp-selector
    (symbol `(write-string ,(string-downcase (symbol-name sexp-selector))
			   ,stream))
    (string `(write-string ,sexp-selector ,stream))
    (cons 
     (let ((expander (selector-expander (car sexp-selector))))
       (if expander
	   (funcall expander stream (rest sexp-selector))
	   (sexp-cons-to-commands sexp-selector stream))))
    (t `(write-string ,(coerce sexp-selector 'string) ,stream))))

(defun stringlike-thing-to-commands (form stream-var)
  "Produces code to write a symbol or string out to the stream named STREAM."
  (typecase form
    (symbol
     `(format ,stream-var "~A" ,(string-downcase (symbol-name form))))
    (t
     `(format ,stream-var "~A" ,form))))

(defun sexp-rule-body-to-commands (sexp-rule-body stream)
  `(progn
     ,@(loop :for (prop value) :on sexp-rule-body :by #'cddr
	  :collect
	  `(progn
	     ,(if (consp prop)
		  (sexp-cons-to-commands prop stream)
		  (stringlike-thing-to-commands prop stream))
	     (format ,stream ": ")
	     ,(if (consp value)
		  (sexp-cons-to-commands value stream)
		  (stringlike-thing-to-commands value stream))
	     (format ,stream ";~%")))))
  
(defun sexps-to-commands (sexp-forms stream)
  `(progn ,@(mapcar #'(lambda (r) (sexp-rule-to-commands r stream))
		    sexp-forms)))
