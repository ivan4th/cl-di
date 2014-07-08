(defpackage :di.testutils
  (:use :cl :alexandria :iterate :di :vtf)
  (:export #:injected-fixture
           #:setup/injected
           #:fixture-modules
           #:build-instance))

(in-package :di.testutils)

(define-fixture injected-fixture (injected) ()
  (:documentation "Base class for test fixtures that use injection"))

(defgeneric setup/injected (fixture &key &allow-other-keys)
  (:documentation "Set up DI-based fixture")
  (:method ((fixture injected-fixture) &key &allow-other-keys) (values)))

(defgeneric fixture-modules (fixture)
  (:documentation "Return a list of modules for injected fixture")
  (:method append ((fixture injected-fixture))
    (list
     (declarative-bindings
       (:fixture (:value fixture)))))
  (:method-combination append))

(defmethod setup :after ((fixture injected-fixture))
  (inject-instance fixture :via (apply #'di:make-injector (fixture-modules fixture)))
  (setup/injected fixture))

(defun build-instance (key &rest initargs &key (fixture *fixture*) &allow-other-keys)
  "Create an object corresponding to injection key KEY
  passing INITARGS to its factory which is retrieved via
  FIXTURE. By default, the active fixture is used."
  (let ((factory (get-factory fixture key)))
    (apply factory (remove-from-plist initargs :fixture))))
