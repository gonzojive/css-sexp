(require :css-sexp)
(require :cl-who)

(defpackage css-sexp-webpage
  (:use :common-lisp :who :css-sexp))

(in-package :css-sexp-webpage)

(defun output-webpage (stream)
  (who:with-html-output (stream stream :indent 3)
    (:html
     (:head 
      (:title "CSS-SEXP")
      (:style
       (with-css-output (stream)
	 (:h1
	  :size (raw "130%")
	  :color :blue)
	 ((class :h1 :gigantic)
	  :size (raw "400%")))))
     (:body
      (:h1 "CSS-SEXP")
      (:h2 "CSS in S-Expressions"))))
  (values))

(defun output-webpage-css (stream)
  (css:with-css-output (stream)
    (:div.fluid :width (raw "90% !important"))
    (:div.fixed :width (raw "1050px !important"))
    (:* :background-color (raw "#111111")
	:color (raw "#585858")
	:font-size (raw "9pt")
	:font-family (raw "helvetica, sans-serif"))
    ((or :h1 :h2 :h2 :h3 :h4 :h5 :h6)
     :font-weight :normal
     :letter-spacing (raw "-1px")
     :text-transform :lowercase)
    (:h3.profile-header    :text-transform :none)
    ((or :h3 :h4 :h5 :h6)  :color (raw "#66000F"))

    ((ancestor :h1 :span)    :font-weight :bold)
    ((ancestor :h2 :span)    :font-weight :bold)
    ((ancestor :h3 :span)    :font-weight :bold)
    ((ancestor :h4 :span)    :font-weight :bold)
    (:br.clear :clear :both)
    (:img :padding (raw "3px") :border (raw "solid 1px #e1e1"))
    (:img.tag :padding (raw "0px") :border 0)
    ((class :img :floattl)
     :float :left
     :margin-right (raw "1.5em")
     :margin-bottom (raw "1.5em")
     :margin-top (raw "0.5em"))
    ((class :img :profpic) :float :left :width (raw "200px"))
    (:a\:hover :text-decoration :none)
    (:ul.links :list-style :none)
    ((ancestor :ul.links :li.first))
    (:p :line-height "1.8em")
    (:header


(defun output-webpage2 (stream)
  (who:with-html-output (stream stream :indent 3)
    (:html
     (:head
      (:meta :http-equiv "content-type" :content "text/html; charset=iso-8859-1")
      (:title "CSS-SEXP: CSS in S-Expressions")
      (:style
       :type "text/css"
       (output-webpage-css stream)))
     (:body
      (:div :id "header"
            (:div :id "header_inner" :class "fixed"
		  (:form :method "post" :action ""
			 (:div :id "search"
			       (:h3 "search")
			       (:input :type "text" :class "text" :name "keywords")
			       (:br :class "clear")))))
                  (:div :id "logo"
                        (:h1 "CSS-" (:span "SEXP"))
                        (:h2 "CSS-SEXP: CSS in S-Expressions"))
                  (:div :id "menu"
                        (:ul
                         (dolist (menu-option '(("Project" :href "#") ("Code" :href "#code")))
                           (destructuring-bind (name &key href selected-p) menu-option
                             (who:htm
                              (:li :class (if selected-p "active" "inactive")
                                   (:div :class "more" :style "float: right"
                                         (:a :href "#" (who:esc "v")))
                                   (:a :href href :class "mainlink"
                                       (who:esc name))
                                   (:div :style "clear: both")
                                   ))))))))
;      (funcall content-function stream)
      (:div :id "footer" :class "fixed"
            "Copyright &copy; 2008 Stanford Logic Group")))))


(defun main ()
  (with-open-file (stream "/home/red/projects/floatopia/wwwroot/projects/css-sexp/index.html"
			  :direction :output :if-exists :supersede :if-does-not-exist :create)
    (output-webpage2 stream)
    (finish-output stream)))
