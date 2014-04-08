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

(defun make-sample-injector ()
  (make-injector
   #'(lambda (injector)
       (bind-class injector 'some-injected 'some-injected :singleton)
       (bind-class injector 'another 'injected-foobar :singleton))))

(deftest test-initform-injection () ()
  (let* ((injector (make-sample-injector))
         (obj (obtain injector 'some-injected)))
    (is-true (typep (another obj) 'injected-foobar))
    (is (eq obj (obtain injector 'some-injected)))
    (is (eq (another obj) (obtain injector 'another)))
    (is (eq (another obj) (obtain obj 'another)))
    ;; different binding key
    (is (not (eq (another obj) (obtain injector 'injected-foobar))))))

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

(defmethod configure :after ((injector injector) (module sample-module))
  (bind-class injector 'some 'some-injected))

(defclass another-module (module) ())

(defmethod configure :after ((injector injector) (module another-module))
  (bind-class injector 'another 'injected-foobar))

(deftest test-modules () ()
  (let ((injector (make-injector '(sample-module another-module))))
    (is-true (typep (obtain injector 'some) 'some-injected))
    (is-true (typep (obtain injector 'another) 'injected-foobar))))

(deftest test-provider-bindings () ()
  (let* ((injector (make-injector
                    #'(lambda (injector)
                        (bind-class injector 'some 'some-injected)
                        (bind-class injector 'another 'injected-foobar))))
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
                    #'(lambda (injector)
                        (bind-class injector 'some 'some-injected)
                        (bind-class injector'another 'injected-foobar))))
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
                    #'(lambda (injector)
                        (bind-class injector 'some 'some-injected)
                        (bind-class injector 'another 'injected-foobar))))
         (l (some-gf 'a 'b :injector injector)))
    (is (eq 'a (first l)))
    (is (eq 'b (second l)))
    (is-true (typep (third l) 'some-injected))
    (is-true (typep (fourth l) 'injected-foobar))))

(deftest test-value-injection () ()
  (let ((injector (make-injector
                   #'(lambda (injector)
                       (bind-value injector 'some-injected 42)
                       (bind-value injector 'another "qqq")))))
    (is (equal (list 1 2 42 "qqq")
               (some-injected-func 1 2 :injector injector)))
    (is (= 42 (obtain injector 'some-injected)))
    (is (string= "qqq" (obtain injector 'another)))
    (is (= 42 (funcall (provider injector 'some-injected))))
    (is (string= "qqq" (funcall (provider injector 'another))))))

(deftest test-initarg-injection () ()
  (let* ((injector (make-injector
                    #'(lambda (injector)
                        (bind-class injector
                                    'some '(some-injected
                                            :whatever 42
                                            :whatever-misc "qqq"
                                            :whatever-etc (:instance injected-foobar
                                                           :misc-foo (:value xxx))
                                            :whatever-foo (:value another)
                                            :whatever-bar (:inject another)))
                        (bind-class injector 'another 'injected-foobar :singleton))))
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
                    #'(lambda (injector)
                        (bind-class injector 'another 'injected-foobar :singleton)
                        (bind-class injector 'whatever 'another))))
         (another (obtain injector 'another))
         (whatever (obtain injector 'whatever)))
    (is-true (typep (obtain injector 'another) 'injected-foobar))
    (is (eq another whatever))))

(deftest test-value-multibindings () ()
  (let ((injector (make-injector
                   #'(lambda (injector)
                       (bind-value* injector 'whatever 'some-val)
                       (bind-value* injector 'whatever 'another-val)))))
    (is (equal '(some-val another-val)
               (obtain injector 'whatever)))))

(deftest test-class-multibindings () ()
  (let ((injector (make-injector
                   #'(lambda (injector)
                       (bind-class injector 'another 'injected-foobar)
                       (bind-class* injector 'whatever 'some-injected)
                       (bind-class* injector 'whatever 'another)))))
    (is (equal '(some-injected injected-foobar)
               (mapcar #'type-of (obtain injector 'whatever))))))

(deftest test-multibinding-provider () ()
  (let ((injector (make-injector
                   #'(lambda (injector)
                       (bind-class injector 'another 'injected-foobar :singleton)
                       (bind-class* injector 'whatever 'another)
                       (bind-value* injector 'whatever 42)))))
    (is (equal (list (obtain injector 'another) 42)
               (funcall (provider injector 'whatever))))))

;; TBD: should support not just initform, but _initarg_ injection
;; (use initarg names as injection keys; warn on ambiguous initarg bindings)
;; TBD: test-initarg-injection should be test-binding-initargs
;; TBD: test initargs spec for binding without scope spec
;; TBD: bind-factory and bind-factory* to specify function
;; TBD: declarative config
;; TBD: empty multibindings (bind-class* or bind-value* without second argument)
;; (note: these should do nothing if the multibinding already exists)
;; TBD: &inject shouldn't be exported (symbol with such name from any package should do)
;; TBD: injection defaults for initforms / initargs / keyword arguments (for cases when there's no injector)
;; TBD: thread-local scope & scope extensibility
