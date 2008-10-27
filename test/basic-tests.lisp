(in-package :css-sexp-tests)
(in-suite css-sexp-tests)

;; utilities

;; tests
(deftest (test-test :compile-before-run t) ()
  (is (= 0 0)))

(deftest (empty-test :compile-before-run t) ()
  (is (null (ppcre:scan "[^\\s;]+" (cssexp:with-css-output-to-string (stream))))))

(deftest (div-with-no-rules :compile-before-run t) ()
  (is (ppcre:scan
       "\\s*div\\s*{\\s*}\\s*;?\\s*"
       (cssexp:with-css-output-to-string (stream)
	 (:div)))))

(deftest (div-or-span-with-no-rules :compile-before-run t) ()
  (is (ppcre:scan
       "\\s*div\\s*,\\s*span\\s*{\\s*}\\s*;?\\s*"
       (cssexp:with-css-output-to-string (stream)
	 ((or :div :span))))))

(deftest (div-with-width=100 :compile-before-run t) ()
  (is (ppcre:scan
       "\\s*div\\s*{\\s*width\\s*:\\s*100\\s*;?\\s*}\\s*;?\\s*"
       (cssexp:with-css-output-to-string (stream)
	 (:div
	  :width 100)))))
      