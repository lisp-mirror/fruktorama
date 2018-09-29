(asdf:defsystem #:f3

  :description "Fruktorama 3 Lisp Edition"
  :author "Parasite Network"
  :license "GPL3"

  :depends-on (:sdl2 :sdl2-image alexandria)
  :pathname ""
  :serial t

  :components
  (
    (:file "sprite")
    (:file "widget")
    (:file "window")
    (:file "game")
    (:file "f3")
    ))