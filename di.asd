(asdf:defsystem #:di
  :name "cl-di"
  :author "Ivan Shvedunov"
  :version "0.1"
  :serial t
  :description "Dependency Injection Container for Common Lisp"
  :depends-on (:alexandria :iterate)
  :components ((:file "package")
               (:file "di")))
