;;; see the CSS spec at http://www.w3.org/TR/CSS21/
(use-package :css-sexp)

;;; all CSS-SEXP rules take the following form
;;; (selector [property value]*))

;;; most CSS rules have only 1 selector word and a few properties:
(with-css-output (stream)
  (:h1
   :color "black"
   :font-weight "bold"))

;;; however, CSS 2.1 offers a full range of possibilities for selectors
;;; CSS Pattern     | CSS-SEXP equivalent    | description
;;; X, Y              (or X Y)                 Matches either selector X or Y
;;; *                 *                        Matches any element
;;; E                 E                        Matches any E element (i.e., an element of type E)
;;; E F               (ancestor E F)           Matches any F element that is a descendant of an E element. Descendant selectors
;;; E > F             (> E F)                  Matches any F element that is a child of an element E.Child selectors
;;; E + F             (+ E F)                  Matches any F element immediately preceded by a sibling element E.Adjacent selectors

;;; E.warning         E.warning                In HTML, the same as DIV[class~="warning"].
;;;                   (class warning E)

;;; E#myid            E#myid                   Matches any E element with ID equal to "myid".ID selectors
;;;                   (id myid E)


;;; E:first-child     E\:first-child           Matches element E when E is the first child of its parent. The :first-child pseudo-class
;;;                   (pseudo E first-child)
;;;                   (pseudo-class E first-child)

;;; E:lang(c)         (lang E c)               Matches element of type E if it is in (human) language c (the document language specifies how language is determined). The :lang() pseudo-class

;;; E[foo]            E[foo]                   Matches any E element with the "foo" attribute set (whatever the value). Attribute selectors
;;;                   (attribute E foo)
;;; E[foo="warning"]  E[foo=\"warning\"]       Matches any E element whose "foo" attribute value is exactly equal to "warning". Attribute selectors
;;;                   (attribute= E foo warning)
;;; E[foo~="warning"]                          Matches any E element whose "foo" attribute value is a list of space-separated values, one of which is exactly equal to "warning". Attribute selectors
;;;                   (attribute~= E foo warning)
;;; E[lang|="en"]                              Matches any E element whose "lang" attribute has a hyphen-separated list of values beginning (from the left) with "en". Attribute selectors
;;;                   (attribute|= E foo warning)

((ancestor "h1.stuff" "more" "another selector")
 :property value


((:h1 :h3 :h4)
 :property value
 :property2 value2
     
     