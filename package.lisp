(in-package :common-lisp-user)

(defpackage :di
  (:use :cl :alexandria :iterate)
  (:export #:make-injector
           #:injector
           #:inject
           #:injected
           #:&inject
           #:&provide
           #:configure
           #:bind-class
           #:bind-class*
           #:bind-value
           #:bind-value*
           #:module
           #:defun/injected
           #:defmethod/injected
           #:obtain
           #:provider))
