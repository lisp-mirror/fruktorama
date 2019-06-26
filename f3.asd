(asdf:defsystem #:f3

  :description "Fruktorama 3 Lisp Edition"
  :author "Parasite Network"
  :license "GPL3"

  :depends-on (glas alexandria)
  :pathname ""
  :serial t

  :components
  ((:file "package")
   (:file "highscore")
   (:file "gadgets")
   (:file "game")
   (:file "f3")))