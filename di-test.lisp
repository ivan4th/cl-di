(defpackage :di.tests
  (:use :cl :di :alexandria :vtf :iterate))

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
       (config-bind binder 'some-injected
                    :to 'some-injected
                    :scope :singleton)
       ;; overrided below
       (config-bind binder 'another
                    :to 'some-injected)
       ;; override
       (config-bind binder 'another
                    :to 'injected-foobar
                    :scope :singleton))))

(deftest test-initform-injection () ()
  (let* ((injector (make-sample-injector))
         (obj (obtain injector 'some-injected)))
    (is-true (typep (another obj) 'injected-foobar))
    (is (eq obj (obtain injector 'some-injected)))
    (is (eq (another obj) (obtain injector 'another)))
    (is (eq (another obj) (obtain obj 'another)))
    ;; different binding key
    (is (not (eq (another obj) (obtain injector 'injected-foobar))))))

(defclass foobar2 (injected)
  ((another :accessor another
            :initform (error "no :ANOTHER specified")
            :initarg :another)
   (misc :accessor misc
         :initform (error "no :MISC specified")
         :initarg :misc))
  (:default-initargs :another (inject 'another)
                     :misc (inject 'some-barfoo)))

(deftest test-default-initarg-injection () ()
  (let* ((injector (make-sample-injector))
         (foobar2 (obtain injector 'foobar2)))
    (is (typep (another foobar2) 'injected-foobar))
    (is (typep (misc foobar2) 'some-barfoo))))

(deftest test-initarg-injection () ()
  (let* ((injector (make-injector
                    #'(lambda (binder)
                        (config-bind binder :misc-foo :to 'some-barfoo)))))
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
  (config-bind binder 'some :to 'some-injected))

(defclass another-module (module) ())

(defmethod configure :after ((binder binder) (module another-module))
  (config-bind binder 'another :to 'injected-foobar))

(deftest test-modules () ()
  (let ((injector (make-injector '(sample-module another-module))))
    (is-true (typep (obtain injector 'some) 'some-injected))
    (is-true (typep (obtain injector 'another) 'injected-foobar))))

(deftest test-get-factory () ()
  (let* ((injector (make-injector
                    #'(lambda (binder)
                        (config-bind binder 'some :to 'some-injected)
                        (config-bind  binder 'another :to 'injected-foobar))))
         (provide-some (get-factory injector 'some))
         (provide-some-injected (get-factory injector 'some-injected))
         (obj (funcall provide-some)))
    (is-true (typep obj 'some-injected))
    (is-true (typep (funcall provide-some) 'some-injected))
    (is-true (typep (another obj) 'injected-foobar))
    (is (not (equal obj (funcall provide-some))))
    (is-true (typep (funcall provide-some-injected) 'some-injected))))

(defun/injected func-with-factory (abc def &factory (make-sobj some-injected)
                                       &key (make-foobar (:factory another)))
  (list abc def (funcall make-sobj) (funcall make-foobar)))

(deftest test-func-factory-bindings () ()
  (let* ((injector (make-injector
                    #'(lambda (binder)
                        (config-bind binder 'some :to 'some-injected)
                        (config-bind binder 'another :to 'injected-foobar))))
         (l (func-with-factory 1 2 :injector injector)))
    (is (= 1 (first l)))
    (is (= 2 (second l)))
    (is-true (typep (third l) 'some-injected))
    (is-true (typep (fourth l) 'injected-foobar))))

(defmethod/injected some-gf ((abc symbol) (def symbol)
                             &factory (make-sobj some-injected)
                             &key (make-foobar (:factory another)))
  (list abc def (funcall make-sobj) (funcall make-foobar)))

(deftest test-method-factory-bindings () ()
  (let* ((injector (make-injector
                    #'(lambda (binder)
                        (config-bind binder 'some :to 'some-injected)
                        (config-bind binder 'another :to 'injected-foobar))))
         (l (some-gf 'a 'b :injector injector)))
    (is (eq 'a (first l)))
    (is (eq 'b (second l)))
    (is-true (typep (third l) 'some-injected))
    (is-true (typep (fourth l) 'injected-foobar))))

(deftest test-value-injection () ()
  (let ((injector (make-injector
                   #'(lambda (binder)
                       (config-bind binder 'some-injected :to-value 42)
                       ;; overrided below
                       (config-bind binder 'another :to-value "aaa")
                       ;; override
                       (config-bind binder 'another :to-value "qqq")))))
    (is (equal (list 1 2 42 "qqq")
               (some-injected-func 1 2 :injector injector)))
    (is (= 42 (obtain injector 'some-injected)))
    (is (string= "qqq" (obtain injector 'another)))
    (is (= 42 (funcall (get-factory injector 'some-injected))))
    (is (string= "qqq" (funcall (get-factory injector 'another))))))

(deftest test-binding-initargs () ()
  (let* ((injector (make-injector
                    #'(lambda (binder)
                        (config-bind binder 'some
                                     :to '(some-injected
                                           (:whatever-etc (injected-foobar :misc-foo xxx)
                                            :whatever-bar (:key another))
                                           :whatever 42
                                           :whatever-misc "qqq"
                                           :whatever-foo another))
                        (config-bind binder 'another
                                     :to 'injected-foobar
                                     :scope :singleton))))
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

(deftest test-recursive-provider () ()
  (let* ((injector (make-injector
                    #'(lambda (binder)
                        (config-bind binder 'another
                                     :to 'injected-foobar
                                     :scope :singleton)
                        (config-bind binder 'whatever
                                     :to (make-recursive-provider 'another)))))
         (another (obtain injector 'another))
         (whatever (obtain injector 'whatever)))
    (is-true (typep (obtain injector 'another) 'injected-foobar))
    (is (eq another whatever))))

(deftest test-value-multibindings () ()
  (let ((injector (make-injector
                   #'(lambda (binder)
                       (config-multibind binder 'whatever :to-value 'some-val)
                       (config-multibind binder 'whatever :to-value 'another-val)))))
    (is (equal '(some-val another-val)
               (obtain injector 'whatever)))))

(deftest test-class-multibindings () ()
  (let ((injector (make-injector
                   #'(lambda (binder)
                       (config-bind binder 'another :to 'injected-foobar)
                       (config-multibind binder 'whatever :to 'some-injected)
                       (config-multibind binder 'whatever
                                         :to (make-recursive-provider 'another))))))
    (is (equal '(some-injected injected-foobar)
               (mapcar #'type-of (obtain injector 'whatever))))))

(deftest test-class-multibindings-scope () ()
  (let* ((injector (make-injector
                    #'(lambda (binder)
                        (config-multibind binder 'foo :to-value 42 :scope :singleton)
                        (config-multibind binder 'foo :to 'injected-foobar))))
         (foo (obtain injector 'foo)))
    (is (eq foo (obtain injector 'foo)))))

(deftest test-empty-multibindings () ()
  (let* ((injector (make-injector
                    #'(lambda (binder)
                        (config-multibind binder 'foo)
                        (config-multibind binder 'bar)
                        (config-multibind binder 'baz :scope :singleton)
                        (config-multibind binder 'bar :to 'injected-foobar)
                        (config-multibind binder 'baz :to 'injected-foobar)
                        ;; note: this does nothing
                        (config-multibind binder 'baz))))
         (foo (obtain injector 'foo))
         (bar (obtain injector 'bar))
         (baz (obtain injector 'baz)))
    (is (null foo))
    (is (not (eq bar (obtain injector 'bar))))
    (is (typep bar '(cons injected-foobar null)))
    (is (eq baz (obtain injector 'baz)))
    (is (typep baz '(cons injected-foobar null)))))

(deftest test-multibinding-factory () ()
  (let ((injector (make-injector
                   #'(lambda (binder)
                       (config-bind binder 'another
                                    :to 'injected-foobar
                                    :scope :singleton)
                       (config-multibind binder 'whatever
                                         :to (make-recursive-provider 'another))
                       (config-multibind binder 'whatever :to-value 42)))))
    (is (equal (list (obtain injector 'another) 42)
               (funcall (get-factory injector 'whatever))))))

(deftest test-factory-bindings () ()
  (let* ((injector (make-injector
                    #'(lambda (binder)
                        (config-bind binder 'foo :to #'(lambda () (list 42)))
                        (config-bind binder 'bar
                                     :to #'(lambda () (list "abc"))
                                     :scope :singleton)
                        (config-multibind binder 'foobar :to #'(lambda () 'q))
                        (config-multibind binder 'foobar :to #'(lambda () 'r))
                        (config-multibind binder 'baz
                                          :to #'(lambda () 'qq)
                                          :scope :singleton)
                        (config-multibind binder 'baz :to #'(lambda () 'rr)))))
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

(deftest test-mapbind () ()
  (let* ((injector (make-injector
                    #'(lambda (binder)
                        (config-mapbind binder 'foo)
                        ;; overrided below
                        (config-mapbind binder 'foo :map-key 'abc :to-value 1)
                        ;; override
                        (config-mapbind binder 'foo :map-key 'abc :to-value 2)
                        (config-mapbind binder 'foo
                                        :map-key 'def
                                        :to '(injected-foobar :misc-foo 42))
                        (config-mapbind binder 'bar
                                        :map-key 'abc
                                        :to-value 42
                                        :scope :singleton)
                        ;; overrided below
                        (config-mapbind binder 'bar
                                        :map-key 'def
                                        :to '(injected-foobar :misc-foo 4200))
                        ;; override
                        (config-mapbind binder 'bar
                                        :map-key 'def
                                        :to '(injected-foobar :misc-foo 4242)))))
         (foo (obtain injector 'foo))
         (foo1 (obtain injector 'foo))
         (bar (obtain injector 'bar))
         (bar1 (obtain injector 'bar)))
    (flet ((frob (alist)
             (sort
              (iter (for (k . v) in alist)
                    (collect
                        (cons k
                              (typecase v
                                (injected-foobar (list :ifoo (misc-foo v)))
                                (t v)))))
              #'string<
              :key #'car)))
      (is (equal '((abc . 2) (def . (:ifoo 42))) (frob foo)))
      (is (frob foo) (frob foo1))
      (is (not (eq foo foo1)))

      (is (equal '((abc . 42) (def . (:ifoo 4242))) (frob bar)))
      (is (eq bar bar1)))))

(defmodule sample-decl-module ()
  (another injected-foobar :singleton)
  (some-injected
   (some-injected
    (:whatever (:key :misc-foo))
    :whatever-misc 42
    :whatever-foo 'qqq)
   :singleton)
  (:misc-foo some-barfoo)
  (:whatever-bar (:value 4242))
  (foo (:value 42)))

(defmodule inherited-decl-module (sample-decl-module)
  (bar (:seq))
  (bar (:seq (:value "abc")))
  (baz (:seq) :singleton)
  (baz (:seq (:key another)) :singleton)
  (baz (:seq (:key some-injected)))
  (factory1 (:factory #'(lambda () (cons 1 2))))
  (factory2 (:factory #'(lambda () (cons 3 4))) :singleton)
  (factory3 (:seq))
  (factory3 (:seq (:factory #'(lambda () (cons 5 6)))
                  (:factory #'(lambda () (cons 7 8)))))
  (factory4 (:seq) :singleton)
  (factory4 (:seq (:factory #'(lambda () (cons 9 10)))) :singleton)
  (factory4 (:seq (:factory #'(lambda () (cons 11 12)))))
  (map1 (:map))
  (map1 (:map abc (:value "abc")))
  (map1 (:map def (:factory #'(lambda () (cons 13 14)))))
  (map2 (:map) :singleton)
  (map2 (:map abc (:value 4242)
              def (:factory #'(lambda () (cons 15 16))))))

(defun verify-declarative-bindings (injector)
  (let ((another (obtain injector 'another))
        (some-injected (obtain injector 'some-injected))
        (bar (obtain injector 'bar))
        (baz (obtain injector 'baz))
        (fac1 (obtain injector 'factory1))
        (fac2 (obtain injector 'factory2))
        (fac3 (obtain injector 'factory3))
        (fac4 (obtain injector 'factory4))
        (map1 (obtain injector 'map1))
        (map2 (obtain injector 'map2)))
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
    (is (eq fac4 (obtain injector 'factory4)))

    (is (equal '((abc . "abc") (def . (13 . 14)))
               (sort map1 #'string< :key #'car)))
    (is (equal '((abc . "abc") (def . (13 . 14)))
               (sort (obtain injector 'map1)
                     #'string< :key #'car)))
    (is (not (eq map1 (obtain injector 'map1))))

    (is (equal '((abc . 4242) (def . (15 . 16)))
               (sort map2 #'string< :key #'car)))
    (is (eq map2 (obtain injector 'map2)))))

(deftest test-defmodule () ()
  (verify-declarative-bindings (make-injector 'inherited-decl-module)))

(deftest test-declarative-bindings () ()
  (verify-declarative-bindings
   (make-injector
    (declarative-bindings
      (another injected-foobar :singleton)
      (some-injected
       (some-injected
        (:whatever (:key :misc-foo))
        :whatever-misc 42
        :whatever-foo 'qqq)
       :singleton)
      (:misc-foo some-barfoo)
      (:whatever-bar (:value 4242))
      (foo (:value 42))
      (bar (:seq))
      (bar (:seq (:value "abc")))
      (baz (:seq) :singleton)
      (baz (:seq (:key another)) :singleton)
      (baz (:seq (:key some-injected)))
      (factory1 (:factory #'(lambda () (cons 1 2))))
      (factory2 (:factory #'(lambda () (cons 3 4))) :singleton)
      (factory3 (:seq))
      (factory3 (:seq (:factory #'(lambda () (cons 5 6)))
                      (:factory #'(lambda () (cons 7 8)))))
      (factory4 (:seq) :singleton)
      (factory4 (:seq (:factory #'(lambda () (cons 9 10)))) :singleton)
      (factory4 (:seq (:factory #'(lambda () (cons 11 12)))))
      (map1 (:map))
      (map1 (:map abc (:value "abc")))
      (map1 (:map def (:factory #'(lambda () (cons 13 14)))))
      (map2 (:map) :singleton)
      (map2 (:map abc (:value 4242)
                  def (:factory #'(lambda () (cons 15 16)))))))))

;; TBD: use :via instead of :injector for injected fns/GFs
;; TBD: test injection defaults for initargs (incl. :default-initargs) / keyword arguments (for cases when there's no injector)
;; Make sure (INJECT ...) form signals an error (of specific class) if there's no current injector
;; (this must be handled by INJECTED clas and /INJECTED macrology)
;; TBD: thread-local scope & scope extensibility (export symbols)
