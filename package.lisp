(in-package :common-lisp-user)

(defpackage :di
  (:use :cl :alexandria :iterate)
  (:export #:make-injector
           #:injector
           #:inject
           #:injected
           #:configure
           #:bind-class
           #:bind-class*
           #:bind-value
           #:bind-value*
           #:bind-factory
           #:bind-factory*
           #:module
           #:defun/injected
           #:defmethod/injected
           #:obtain
           #:provider))
