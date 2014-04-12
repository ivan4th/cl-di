(defpackage :di.tests
  (:use :cl :di :alexandria :vtf))

(in-package :di.tests)

(defclass some-injected (injected)
  ((another :accessor another :initform (inject 'another))
   (whatever :accessor whatever :initarg :whatever)
   (whatever-misc :accessor whatever-misc :initarg :whatever-misc)
   (whatever-etc :accessor whatever-etc :initarg :whatever-etc)
   (whatever-foo :accessor whatever-foo :initarg :whatever-foo)
   (whatever-bar :accessor whatever-bar :initarg :whatever-bar)))

(defclass injected-foobar (injected)
  ((misc-foo :accessor misc-foo :initarg :misc-foo)))

(defclass some-barfoo () ())

(defun make-sample-injector ()
  (make-injector
   #'(lambda (binder)
       (bind-class binder 'some-injected 'some-injected :singleton)
       (bind-class binder 'another 'injected-foobar :singleton))))

(deftest test-initform-injection () ()
  (let* ((injector (make-sample-injector))
         (obj (obtain injector 'some-injected)))
    (is-true (typep (another obj) 'injected-foobar))
    (is (eq obj (obtain injector 'some-injected)))
    (is (eq (another obj) (obtain injector 'another)))
    (is (eq (another obj) (obtain obj 'another)))
    ;; different binding key
    (is (not (eq (another obj) (obtain injector 'injected-foobar))))))

(deftest test-initarg-injection () ()
  (let* ((injector (make-injector
                    #'(lambda (binder)
                        (bind-class binder :misc-foo 'some-barfoo)))))
    (is-true (typep (misc-foo (obtain injector 'injected-foobar))
                    'some-barfoo))))

(defun/injected some-injected-func (abc def &inject (sobj some-injected)
                                        &key (foobar (:inject another)))
  (list abc def sobj foobar))

(deftest test-defun-injected () ()
  (let* ((injector (make-sample-injector))
         (l1 (some-injected-func 1 2 :injector injector))
         (obj (obtain injector 'some-injected))
         (foobar (obtain injector 'another))
         (l2 (some-injected-func 3 4 :foobar 'x :injector obj)))
    (is (equal (list 1 2 obj foobar) l1))
    (is (eq (another obj) (obtain injector 'another)))
    (is (equal (list 3 4 obj 'x) l2))))

(defgeneric some-gf (abc def &key &allow-other-keys))

(defmethod/injected some-gf ((abc number) (def number)
                             &inject (sobj some-injected)
                             &key (foobar (:inject another)))
  (list abc def sobj foobar))

(deftest test-defmethod-injected () ()
  (let* ((injector (make-sample-injector))
         (l1 (some-gf 1 2 :injector injector))
         (obj (obtain injector 'some-injected))
         (foobar (obtain injector 'another))
         (l2 (some-gf 3 4 :foobar 'x :injector obj)))
    (is (equal (list 1 2 obj foobar) l1))
    (is (eq (another obj) (obtain injector 'another)))
    (is (equal (list 3 4 obj 'x) l2))))

(defclass sample-module (module) ())

(defmethod configure :after ((binder binder) (module sample-module))
  (bind-class binder 'some 'some-injected))

(defclass another-module (module) ())

(defmethod configure :after ((binder binder) (module another-module))
  (bind-class binder 'another 'injected-foobar))

(deftest test-modules () ()
  (let ((injector (make-injector '(sample-module another-module))))
    (is-true (typep (obtain injector 'some) 'some-injected))
    (is-true (typep (obtain injector 'another) 'injected-foobar))))

(deftest test-provider-bindings () ()
  (let* ((injector (make-injector
                    #'(lambda (binder)
                        (bind-class binder 'some 'some-injected)
                        (bind-class binder 'another 'injected-foobar))))
         (provide-some (provider injector 'some))
         (provide-some-injected (provider injector 'some-injected))
         (obj (funcall provide-some)))
    (is-true (typep obj 'some-injected))
    (is-true (typep (funcall provide-some) 'some-injected))
    (is-true (typep (another obj) 'injected-foobar))
    (is (not (equal obj (funcall provide-some))))
    (is-true (typep (funcall provide-some-injected) 'some-injected))))

(defun/injected func-with-providers (abc def &provide (make-sobj some-injected)
                                         &key (make-foobar (:provider another)))
  (list abc def (funcall make-sobj) (funcall make-foobar)))

(deftest test-func-provider-bindings () ()
  (let* ((injector (make-injector
                    #'(lambda (binder)
                        (bind-class binder 'some 'some-injected)
                        (bind-class binder 'another 'injected-foobar))))
         (l (func-with-providers 1 2 :injector injector)))
    (is (= 1 (first l)))
    (is (= 2 (second l)))
    (is-true (typep (third l) 'some-injected))
    (is-true (typep (fourth l) 'injected-foobar))))

(defmethod/injected some-gf ((abc symbol) (def symbol)
                             &provide (make-sobj some-injected)
                             &key (make-foobar (:provider another)))
  (list abc def (funcall make-sobj) (funcall make-foobar)))

(deftest test-method-provider-bindings () ()
  (let* ((injector (make-injector
                    #'(lambda (binder)
                        (bind-class binder 'some 'some-injected)
                        (bind-class binder 'another 'injected-foobar))))
         (l (some-gf 'a 'b :injector injector)))
    (is (eq 'a (first l)))
    (is (eq 'b (second l)))
    (is-true (typep (third l) 'some-injected))
    (is-true (typep (fourth l) 'injected-foobar))))

(deftest test-value-injection () ()
  (let ((injector (make-injector
                   #'(lambda (binder)
                       (bind-value binder 'some-injected 42)
                       (bind-value binder 'another "qqq")))))
    (is (equal (list 1 2 42 "qqq")
               (some-injected-func 1 2 :injector injector)))
    (is (= 42 (obtain injector 'some-injected)))
    (is (string= "qqq" (obtain injector 'another)))
    (is (= 42 (funcall (provider injector 'some-injected))))
    (is (string= "qqq" (funcall (provider injector 'another))))))

(deftest test-binding-initargs () ()
  (let* ((injector (make-injector
                    #'(lambda (binder)
                        (bind-class binder
                                    'some '(some-injected
                                            :whatever 42
                                            :whatever-misc "qqq"
                                            :whatever-etc (:instance injected-foobar
                                                           :misc-foo (:value xxx))
                                            :whatever-foo (:value another)
                                            :whatever-bar (:inject another)))
                        (bind-class binder 'another 'injected-foobar :singleton))))
         (obj (obtain injector 'some))
         (another (obtain injector 'another)))
    (is-true (typep obj 'some-injected))
    (is-true (typep (another obj) 'injected-foobar))
    (is (eq another (another obj)))
    (is (= 42 (whatever obj)))
    (is (string= "qqq" (whatever-misc obj)))
    (is-true (typep (whatever-etc obj) 'injected-foobar))
    (is (not (eq another (whatever-etc obj))))
    (is (eq 'xxx (misc-foo (whatever-etc obj))))
    (is (eq 'another (whatever-foo obj)))
    (is (eq another (whatever-bar obj)))))

(deftest test-recursive-bindings () ()
  (let* ((injector (make-injector
                    #'(lambda (binder)
                        (bind-class binder 'another 'injected-foobar :singleton)
                        (bind-class binder 'whatever 'another))))
         (another (obtain injector 'another))
         (whatever (obtain injector 'whatever)))
    (is-true (typep (obtain injector 'another) 'injected-foobar))
    (is (eq another whatever))))

(deftest test-value-multibindings () ()
  (let ((injector (make-injector
                   #'(lambda (binder)
                       (bind-value* binder 'whatever 'some-val)
                       (bind-value* binder 'whatever 'another-val)))))
    (is (equal '(some-val another-val)
               (obtain injector 'whatever)))))

(deftest test-class-multibindings () ()
  (let ((injector (make-injector
                   #'(lambda (binder)
                       (bind-class binder 'another 'injected-foobar)
                       (bind-class* binder 'whatever 'some-injected)
                       (bind-class* binder 'whatever 'another)))))
    (is (equal '(some-injected injected-foobar)
               (mapcar #'type-of (obtain injector 'whatever))))))

(deftest test-class-multibindings-scope () ()
  (let* ((injector (make-injector
                    #'(lambda (binder)
                        (bind-value* binder 'foo 42 :singleton)
                        (bind-class* binder 'foo 'injected-foobar))))
         (foo (obtain injector 'foo)))
    (is (eq foo (obtain injector 'foo)))))

(deftest test-empty-multibindings () ()
  (let* ((injector (make-injector
                    #'(lambda (binder)
                        (bind-empty* binder 'foo)
                        (bind-empty* binder 'bar)
                        (bind-empty* binder 'baz :singleton)
                        (bind-class* binder 'bar 'injected-foobar)
                        (bind-class* binder 'baz 'injected-foobar)
                        ;; note: this does nothing
                        (bind-empty* binder 'baz))))
         (foo (obtain injector 'foo))
         (bar (obtain injector 'bar))
         (baz (obtain injector 'baz)))
    (is (null foo))
    (is (not (eq bar (obtain injector 'bar))))
    (is (typep bar '(cons injected-foobar null)))
    (is (eq baz (obtain injector 'baz)))
    (is (typep baz '(cons injected-foobar null)))))

(deftest test-multibinding-provider () ()
  (let ((injector (make-injector
                   #'(lambda (binder)
                       (bind-class binder 'another 'injected-foobar :singleton)
                       (bind-class* binder 'whatever 'another)
                       (bind-value* binder 'whatever 42)))))
    (is (equal (list (obtain injector 'another) 42)
               (funcall (provider injector 'whatever))))))

(deftest test-factory-bindings () ()
  (let* ((injector (make-injector
                    #'(lambda (binder)
                        (bind-factory binder 'foo #'(lambda () (list 42)))
                        (bind-factory binder 'bar #'(lambda () (list "abc")) :singleton)
                        (bind-factory* binder 'foobar #'(lambda () 'q))
                        (bind-factory* binder 'foobar #'(lambda () 'r))
                        (bind-factory* binder 'baz #'(lambda () 'qq) :singleton)
                        (bind-factory* binder 'baz #'(lambda () 'rr)))))
         (foo (obtain injector 'foo))
         (bar (obtain injector 'bar))
         (foobar (obtain injector 'foobar))
         (baz (obtain injector 'baz)))
    (is (equal (list 42) foo))
    (is (equal foo (obtain injector 'foo)))
    (is (not (eq foo (obtain injector 'foo))))
    (is (equal '("abc") bar))
    (is (eq bar (obtain injector 'bar)))
    (is (not (eq foobar (obtain injector 'foobar))))
    (is (equal '(q r) foobar))
    (is (eq baz (obtain injector 'baz)))
    (is (equal '(qq rr) (obtain injector 'baz)))))

(defmodule sample-decl-module ()
  (another injected-foobar :singleton)
  (some-injected
   (some-injected
    :whatever (:inject :misc-foo)
    :whatever-misc 42
    :whatever-foo (:value 'qqq))
   :singleton)
  (:misc-foo some-barfoo)
  (:= :whatever-bar 4242)
  (:= foo 42))

(defmodule inherited-decl-module (sample-decl-module)
  (:* bar)
  (:* bar (:none))
  (:* bar "abc")
  (:+ baz (:none) :singleton)
  (:+ baz another :singleton)
  (:+ baz some-injected)
  (:! factory1 #'(lambda () (cons 1 2)))
  (:! factory2 #'(lambda () (cons 3 4)) :singleton)
  (:!+ factory3)
  (:!+ factory3 #'(lambda () (cons 5 6)))
  (:!+ factory3 #'(lambda () (cons 7 8)))
  (:!+ factory4 (:none) :singleton)
  (:!+ factory4 #'(lambda () (cons 9 10)) :singleton)
  (:!+ factory4 #'(lambda () (cons 11 12))))

(defun verify-declarative-bindings (injector)
  (let ((another (obtain injector 'another))
        (some-injected (obtain injector 'some-injected))
        (bar (obtain injector 'bar))
        (baz (obtain injector 'baz))
        (fac1 (obtain injector 'factory1))
        (fac2 (obtain injector 'factory2))
        (fac3 (obtain injector 'factory3))
        (fac4 (obtain injector 'factory4)))
    (is (equal another (obtain injector 'another)))
    (is-true (typep another 'injected-foobar))

    (is (equal some-injected (obtain injector 'some-injected)))
    (is-true (typep some-injected 'some-injected))
    (is-true (typep (whatever some-injected) 'some-barfoo))
    (is (= 42 (whatever-misc some-injected)))
    (is (equal 'qqq (whatever-foo some-injected)))

    (is-true (typep (misc-foo another) 'some-barfoo))
    (is (not (eq (misc-foo another) (whatever some-injected)))) ; non-singleton binding

    (is (= 4242 (whatever-bar some-injected)))

    (is (= 42 (obtain injector 'foo)))

    (is (equal '("abc") bar))
    (is (equal '("abc") (obtain injector 'bar)))
    (is (not (eq bar (obtain injector 'bar))))

    (is (= 2 (length baz)))
    (is-true (typep (first baz) 'injected-foobar))
    (is-true (typep (second baz) 'some-injected))
    (is (eq baz (obtain injector 'baz)))

    (is (equal '(1 . 2) fac1))
    (is (equal '(1 . 2) (obtain injector 'factory1)))
    (is (not (eq fac1 (obtain injector 'factory1))))

    (is (equal '(3 . 4) fac2))
    (is (eq fac2 (obtain injector 'factory2)))

    (is (equal '((5 . 6) (7 . 8)) fac3))
    (is (equal '((5 . 6) (7 . 8)) (obtain injector 'factory3)))
    (is (not (eq fac3 (obtain injector 'factory3))))

    (is (equal '((9 . 10) (11 . 12)) fac4))
    (is (eq fac4 (obtain injector 'factory4)))))

(deftest test-defmodule () ()
  (verify-declarative-bindings (make-injector 'inherited-decl-module)))

(deftest test-declarative-bindings () ()
  (verify-declarative-bindings
   (make-injector
    (declarative-bindings
     (another injected-foobar :singleton)
     (some-injected
      (some-injected
       :whatever (:inject :misc-foo)
       :whatever-misc 42
       :whatever-foo (:value 'qqq))
      :singleton)
     (:misc-foo some-barfoo)
     (:= :whatever-bar 4242)
     (:= foo 42)
     (:* bar)
     (:* bar (:none))
     (:* bar "abc")
     (:+ baz (:none) :singleton)
     (:+ baz another :singleton)
     (:+ baz some-injected)
     (:! factory1 #'(lambda () (cons 1 2)))
     (:! factory2 #'(lambda () (cons 3 4)) :singleton)
     (:!+ factory3)
     (:!+ factory3 #'(lambda () (cons 5 6)))
     (:!+ factory3 #'(lambda () (cons 7 8)))
     (:!+ factory4 (:none) :singleton)
     (:!+ factory4 #'(lambda () (cons 9 10)) :singleton)
     (:!+ factory4 #'(lambda () (cons 11 12)))))))

;; TBD: associative bindings (via bind-mapping + in module spec; :map+ / :map= / :map! in module spec)
;; TBD: :- as an alias for default binding in defmodule (for cases where one may want to bind key like :alist+)
;; TBD: don't do initform injection, parse (c2mop:class-default-initargs class) instead
;; (look for second values). (inject key [default]) should return default if it's present
;; in case it's actually run, or throw an error if it's run and there's no default specified
;; TBD: use :via instead of :injector for injected fns/GFs
;; TBD: injection defaults for initargs (incl. :default-initargs) / keyword arguments (for cases when there's no injector)
;; TBD: thread-local scope & scope extensibility (export symbols)
