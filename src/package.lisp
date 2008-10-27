(defpackage :css-sexp
  ;; CSSEXP for CCS-Expressions and PCSS for Parenthetical CSS
  (:nicknames :cssexp :pcss) 
  (:use :common-lisp)
  (:export
   ;; output exports
   #:with-css-output #:with-css-output-to-string
   ;; selector exports
   #:* #:
   ;; macro exports
   #:val #:raw #:fmt)
  (:documentation "Cascading Style Sheets in S-Expressions."))