;;; -*- Lisp -*- mode
(defpackage org.iodb.css-sexp-system
  (:use :common-lisp :asdf))

(in-package :org.iodb.css-sexp-system)

(defsystem css-sexp
  :description "Red's Javascript Object Notation encoder for common lisp.
Part of the Suave project."
  :version "0.0.1"
  :author "Red Daly <reddaly at gmail>"
  :license "GPL version 2: http://www.gnu.org/licenses/gpl.html"
  :components ((:module "src"
                        :components ((:file "package")
				     (:file "output" :depends-on ("package")))))
  :depends-on ())

(defsystem css-sexp-tests
  :components ((:module "test"
                        :components ((:file "test-package")
				     (:file "basic-tests" :depends-on ("test-package"))
				     )))
  :depends-on ("css-sexp" "stefil" "cl-ppcre"))
