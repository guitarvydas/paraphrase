(asdf:defsystem #:paraphrase
  :serial t
  :depends-on (#:peg)
  :components ((:file "package")
               (:file "peg")
               (:file "prolog")
               (:file "ptest")))