(defpackage css-sexp-tests
  (:use :common-lisp :css-sexp :stefil)
  (:nicknames :cssexp-tests)
  (:export #:css-sexp-tests #:run-all-tests))

(in-package css-sexp-tests)

(defsuite css-sexp-tests)

(in-suite css-sexp-tests)

(defun run-all-tests ()
  (css-sexp-tests))