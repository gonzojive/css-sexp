;;; see the CSS spec at http://www.w3.org/TR/CSS21/

;;; all CSS-SEXP rules take the following form
(selector [property value]*)

;;; most CSS rules have only 1 selector word and a few properties:
(:h1
 (:color "black")
 (:font-weight "bold"))

;;; however, CSS 2.1 offers a full range of possibilities for selectors
;;; CSS Pattern     | CSS-SEXP equivalent    | description
;;; X, Y              (or X Y)                 Matches either selector X or Y
;;; *                 *                        Matches any element
;;; E                 E                        Matches any E element (i.e., an element of type E)
;;; E F               (ancestor E F)           Matches any F element that is a descendant of an E element. Descendant selectors
;;; E > F             (> E F)                  Matches any F element that is a child of an element E.Child selectors
;;; E + F             (+ E F)                  Matches any F element immediately preceded by a sibling element E.Adjacent selectors
;;; DIV.warning       E.warning                In HTML, the same as DIV[class~="warning"].
;;;                   (class E warning)
;;; E#myid            E#myid                   Matches any E element with ID equal to "myid".ID selectors
;;;                   (id E myid)


;;; E:first-child     E\:first-child           Matches element E when E is the first child of its parent. The :first-child pseudo-class
;;;                   (pseudo E first-child)
;;; E:link            ""
;;; E:visited         ""                       Matches element E if E is the source anchor of a hyperlink of which the target is not yet visited (:link) or already visited (:visited). The link pseudo-classes
;;; E:active          ""
;;; E:hover           ""
;;; E:focus           ""                       Matches E during certain user actions. The dynamic pseudo-classes

;;; E:lang(c)         (lang E c)               Matches element of type E if it is in (human) language c (the document language specifies how language is determined). The :lang() pseudo-class

;;; E[foo]            E[foo]                   Matches any E element with the "foo" attribute set (whatever the value). Attribute selectors
;;;                   (attribute E foo)
;;; E[foo="warning"]  E[foo=\"warning\"]       Matches any E element whose "foo" attribute value is exactly equal to "warning". Attribute selectors
;;;                   (attribute= E foo warning)
;;; E[foo~="warning"]                          Matches any E element whose "foo" attribute value is a list of space-separated values, one of which is exactly equal to "warning". Attribute selectors
;;;                   (attribute~= E foo warning)
;;; E[lang|="en"]                              Matches any E element whose "lang" attribute has a hyphen-separated list of values beginning (from the left) with "en". Attribute selectors
;;;                   (attribute|= E foo warning)

("h1.stuff" "more" "another selector"
 (:property value :percentage)


((:h1 :h3 :h4)
 :property value
 :property2 value2
     
     