(defpackage :css-sexp
  ;; CSSEXP for CCS-Expressions and PCSS for Parenthetical CSS
  (:nicknames :cssexp :pcss :css) 
  (:use :common-lisp)
  (:export
   ;; output exports
   #:with-css-output #:with-css-output-to-string
   ;; selector exports
   #:*
   #:ancestor
   #:class
   #:id
   #:pseudo
   #:lang
   #:attribute
   #:attribute=
   #:attribute~=
   #:attribute\|=
   ;; macro exports
   #:val #:raw)
  (:documentation "Cascading Style Sheets in S-Expressions."))