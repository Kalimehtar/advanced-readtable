(asdf:defsystem #:advanced-readtable
  :description "Advanced customizable readtable"
  :author "Roman Klochkov <kalimehtar@mail.ru>"
  :version "1.0.1"
  :license "BSD"
  :depends-on (#:named-readtables)
  :serial t
  :components ((:file "package")
               (:file "finders")
               (:file "api")
               (:file "readtable")
               (:file "hierarchy")))
  
