(in-package :common-lisp-user)

(defpackage :di
  (:use :cl :alexandria :iterate :optima)
  (:export #:make-injector
           #:injector
           #:binder
           #:inject
           #:injected
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
           #:obtain
           #:get-factory
           #:defmodule
           #:declarative-bindings))
