(asdf:defsystem #:di.testutils
  :name "di.testutils"
  :author "Ivan Shvedunov"
  :version "0.1"
  :serial t
  :description "Dependency Injection Container for Common Lisp -- test utilities"
  :depends-on (:di :alexandria :iterate :vtf)
  :components ((:file "testutils")))
