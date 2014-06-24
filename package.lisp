(in-package :common-lisp-user)

(defpackage :di
  (:use :cl :alexandria :iterate :optima)
  (:export #:make-injector
           #:injector
           #:injection-error
           #:binder
           #:inject
           #:injected
           #:singleton-mixin
           #:configure
           #:config-bind
           #:config-multibind
           #:config-mapbind
           #:provider
           #:make-class-provider
           #:make-value-provider
           #:make-factory-provider
           #:make-recursive-provider
           #:module
           #:defun/injected
           #:defmethod/injected
           #:inject-instance
           #:obtain
           #:get-factory
           #:defmodule
           #:declarative-bindings))
