(asdf:defsystem #:di.tests
  :name "di.tests"
  :author "Ivan Shvedunov"
  :version "0.1"
  :serial t
  :description "Dependency Injection Container for Common Lisp -- tests"
  :depends-on (:di :alexandria :iterate :vtf)
  :components ((:file "di-test")))
