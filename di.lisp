(in-package :di)

(defgeneric configure (binder module))

(defclass binder ()
  ((bindings :reader bindings :initform (make-hash-table :test #'equal))))

(defun binder-get (binder key)
  (values (gethash key (bindings binder))))

(defun (setf binder-get) (binding binder key)
  (setf (gethash key (bindings binder)) binding))

(defclass injector ()
  ((binder :reader binder :initform (make-instance 'binder)))
  (:documentation "The Injector"))

(defmethod initialize-instance :after ((injector injector) &key config &allow-other-keys)
  (config-bind (binder injector) '(:scope :no-scope)
               :to-value (make-instance 'null-scope))
  (config-bind (binder injector) '(:scope :singleton)
               :to-value (make-instance 'singleton-scope))
  (dolist (item (flatten (ensure-list config)))
    (configure (binder injector) item)))

(defmethod injector ((injector injector)) injector)

(defmethod configure ((binder binder) (module function))
  (funcall module binder))

(defmethod configure ((binder binder) (module symbol))
  (configure binder (make-instance module)))

(defclass module () ())

(defmethod configure ((binder binder) (module module))
  (values))

;;; scope

(defgeneric scope-get (scope key factory))

(defclass null-scope () ())

(defmethod scope-get ((scope null-scope) key factory)
  (declare (ignore key))
  (when factory
    (funcall factory)))

(defclass singleton-scope ()
  ((instances :reader instances :initform (make-hash-table :test #'equal))))

(defmethod scope-get ((scope singleton-scope) key factory)
  (or (gethash key (instances scope))
      (when factory
        (setf (gethash key (instances scope))
              (funcall factory)))))

;; TBD: thread-local scope

;;; bindings

(defstruct (binding
            (:type list)
            (:constructor make-binding (provider scope)))
  provider scope)

(defgeneric provider-factory-function (provider injector))

(defgeneric provider-add-child (provider child-provider)
  (:method ((provider t) (child-provider t))
    (error "can't add child to a non-sequence provider")))

;;; provider (marker class)

(defclass provider () ())

;;; class-provider

(defclass class-provider (provider)
  ((class-name :reader class-name :initarg :class-name)
   (wrapped-initargs :accessor wrapped-initargs)))

(defun wrap-initargs (initargs)
  (iter (while initargs)
        (for item = (pop initargs))
        (cond ((consp item)
               (appending item into injected-initargs))
              (t
               (collect item into plain-initargs)
               (collect (pop initargs) into plain-initargs)))
        (finally
         (let ((providers
          (iter (for (name value . nil) on injected-initargs by #'cddr)
                (collect (cons name (auto-provider value))))))
           (return #'(lambda (injector)
                       (append
                        plain-initargs
                        (iter (for (name . provider) in providers)
                              (collect name)
                              (collect (funcall (provider-factory-function provider injector)))))))))))

(defmethod initialize-instance :after ((provider class-provider) &key initargs &allow-other-keys)
  (setf (wrapped-initargs provider) (wrap-initargs initargs)))

(defmethod provider-factory-function ((provider class-provider) (injector injector))
  #'(lambda (&rest initargs)
      (make-instance-for-injector
       injector
       (class-name provider)
       (append initargs
               (apply #'remove-from-plist
                      (funcall (wrapped-initargs provider) injector)
                      (mapcar #'car (plist-alist initargs)))))))

;;; value-provider

(defclass value-provider (provider)
  ((value :reader value :initarg :value)))

(defmethod provider-factory-function ((provider value-provider) (injector injector))
  #'(lambda () (value provider)))

;;; factory-binding

(defclass factory-provider (provider)
  ((factory :reader factory :initarg :factory)))

(defmethod provider-factory-function ((provider factory-provider) (injector injector))
  (factory provider))

;;; sequence-provider

(defclass sequence-provider (provider)
  ((children :accessor children :initform '())))

(defmethod provider-factory-function ((provider sequence-provider) (injector injector))
  (let ((factory-fns (mapcar (rcurry #'provider-factory-function injector)
                             (reverse (children provider)))))
    #'(lambda () (mapcar #'funcall factory-fns))))

(defmethod provider-add-child ((provider sequence-provider) child-provider)
  (push child-provider (children provider)))

;;; recursive-provider

(defclass recursive-provider (provider)
  ((key :accessor key :initarg :key
        :initform (error "must specify the key for RECURSIVE-PROVIDER"))))

(defmethod provider-factory-function ((provider recursive-provider) (injector injector))
  #'(lambda () (obtain injector (key provider))))

;;; provider constructors

(defun make-class-provider (class-name &optional initargs)
  (make-instance 'class-provider
                 :class-name class-name
                 :initargs initargs))

(defun make-value-provider (value)
  (make-instance 'value-provider :value value))

(defun make-factory-provider (factory)
  (make-instance 'factory-provider :factory factory))

(defun make-recursive-provider (key)
  (make-instance 'recursive-provider :key key))

;;; binder configuration

(defun auto-provider (value)
  (match value
    ((type provider) value)
    ((or nil t) (error "invalid binding value ~s" value))
    ((type symbol) (make-class-provider value))
    ((list :key key) (make-recursive-provider key))
    ((list* (guard class-name (symbolp class-name)) initargs)
     (make-class-provider class-name initargs))
    ((type function) (make-factory-provider value))))

(defun config-bind (binder key &key to to-value (scope :no-scope))
  (assert (or to to-value) ()
          "CONFIG-BIND: must specify either :TO or :TO-VALUE")
  (assert (or (not to) (not to-value)) ()
          "CONFIG-BIND: cannot specify both :TO and :TO-VALUE")
  (setf (binder-get binder key)
        (make-binding
         (if to-value (make-value-provider to-value) (auto-provider to))
         scope)))

(defun config-multibind (binder key &key (to nil to-p) (to-value nil to-value-p) (scope :no-scope))
  (cond ((not (binder-get binder key))
         (setf (binder-get binder key)
               (make-binding (make-instance 'sequence-provider) scope)))
        ((not (typep (binding-provider (binder-get binder key))
                     'sequence-provider))
         (error "trying to use CONFIG-MULTIBIND with key that was ~
                 previously used with CONFIG-BIND")))
  (when (or to-p to-value-p)
    (provider-add-child (binding-provider (binder-get binder key))
                        (if to-value
                            (make-value-provider to-value)
                            (auto-provider to)))))

;;; injector stuff

(defun make-injector (&rest configs)
  (make-instance 'injector :config configs))

(defun normalize-lambda-list-item (item)
  (cond ((not (symbolp item)) item)
        ((string= (string-upcase item) "&INJECT") '&inject)
        ((string= (string-upcase item) "&FACTORY") '&factory)
        (t item)))

(defun split-injected-lambda-list (lambda-list)
  (setf lambda-list (mapcar #'normalize-lambda-list-item lambda-list))
  (let ((state :plain))
    (assert (<= (count '&inject lambda-list) 1) () "duplicate &inject")
    (assert (<= (count '&factory lambda-list) 1) () "duplicate &factory")
    (iter (for item in lambda-list)
          (case state
            (:plain
             (case item
               (&inject
                (setf state :inject))
               (&factory
                (setf state :factory))
               (t
                (collect item into plain))))
            (:inject
             (cond ((member item lambda-list-keywords)
                    (collect item into plain)
                    (setf state :plain))
                   ((eq item '&factory)
                    (setf state :factory))
                   (t
                    (collect item into inject))))
            (:factory
             (cond ((member item lambda-list-keywords)
                    (collect item into plain)
                    (setf state :plain))
                   ((eq item '&inject)
                    (setf state :inject))
                   (t
                    (collect item into provide)))))
          (finally
           (return (values plain inject provide))))))

(defun injected-opt-arg (arg &optional allow-inject-p)
  (destructuring-bind (name init supplied-p) arg
    (when (consp name)
      (setf name (second name)))
    (when allow-inject-p
      (when (typep init '(cons (eql :inject) (cons symbol null)))
        (setf init `(obtain injector ',(second init))))
      (when (typep init '(cons (eql :factory) (cons symbol null)))
        (setf init `(get-factory injector ',(second init)))))
    (cond (supplied-p
           (list name init supplied-p))
          (init
           (list name init))
          (t
           name))))

(defun expand-injected (lambda-list body &key allow-specializers)
  (multiple-value-bind (plain inject factory)
      (split-injected-lambda-list lambda-list)
    (multiple-value-bind (required optional rest keyargs allow-other-keys-p aux)
        (parse-ordinary-lambda-list plain :allow-specializers allow-specializers)
      (cons
       (append
        required
        (when optional (cons '&optional (mapcar #'injected-opt-arg optional)))
        (when rest (list '&rest rest))
        ;; add INJECTOR keyword argument
        '(&key (injector (error "injector expected")))
        (mapcar (rcurry #'injected-opt-arg t) keyargs)
        (when allow-other-keys-p '(&allow-other-keys))
        (when aux (cons '&aux aux)))
        (if (or inject factory)
            `((let ,(append
                     (iter (for (name key) in inject)
                           (collect `(,name (obtain injector ',key))))
                     (iter (for (name key) in factory)
                           (collect `(,name (get-factory injector ',key)))))
                ,@body))
            body)))))

(defmacro defun/injected (name lambda-list &body body)
  `(defun ,name ,@(expand-injected lambda-list body)))

;; TBD: defgeneric/injected

(defmacro defmethod/injected (name &rest def)
  (iter (for l on def)
        (if (listp (first l))
            (return `(defmethod ,name ,@qualifiers
                       ,@(expand-injected (first l) (rest l)
                                          :allow-specializers t)))
            (collect (first l) into qualifiers))
        (finally
         (error "malformed DEFMETHOD/INJECTED form"))))

(defclass injected ()
  ((injector
    :accessor injector
    :initarg :injector
    :initform (error "cannot create injected class instance without injector")
    :documentation "Injector instance"))
  (:documentation "Base class for classes utilizing DI"))

(defvar *current-injector* nil) ;; TBD: perhaps wrap slime repl so that *current-injector* is set there

(defun make-instance-for-injector (injector class-spec initargs)
  ;; skip :injector initarg for non-injected classes
  (if (subtypep class-spec 'injected)
      (apply #'make-instance class-spec :injector injector initargs)
      (apply #'make-instance class-spec initargs)))

(defun inject (key)
  (obtain (or *current-injector*
              (error "no current-injector (forgot to add INJECTED base class?)"))
          key))

(defun inject-slot-initargs (injector instance slot-names)
  (dolist (slotd (c2mop:class-slots (class-of instance)))
    (let ((slot-name (c2mop:slot-definition-name slotd)))
      (when (or (eq t slot-names)
                (member slot-name slot-names))
        (let ((targets (iter (for initarg in (c2mop:slot-definition-initargs slotd))
                             (when-let ((injected-instance (obtain injector initarg nil)))
                               (collect injected-instance)))))
          (when (rest targets)
            (warn "ambiguous initarg binding: class ~s slot ~s initargs ~s"
                  (type-of instance)
                  slot-name
                  (c2mop:slot-definition-initargs slotd)))
          (when targets
            (setf (slot-value instance slot-name)
                  (first targets))))))))

(defmethod shared-initialize :around ((injected injected) (slot-names t)
                                      &key injector &allow-other-keys)
  (let ((*current-injector* injector))
    (inject-slot-initargs injector injected slot-names)
    (call-next-method)))

(defun %factory-and-scope (base-instance key &optional (allow-auto-p t))
  (let* ((injector (injector base-instance))
         (binding (binder-get (binder injector) key)))
    (cond (binding
           (values (provider-factory-function
                    (binding-provider binding)
                     injector)
                   (binding-scope binding)))
          ((not allow-auto-p) (values nil nil))
          ((and key (symbolp key)
                (typep (find-class key nil) 'standard-class))
           (values
            #'(lambda (&rest initargs)
                (make-instance-for-injector injector key initargs))
            :no-scope))
          (t
           (error "no binding or class found for injection key ~s" key)))))

(defun get-factory (base-instance key &optional (allow-auto-p t))
  (values (%factory-and-scope base-instance key allow-auto-p)))

(defun obtain (base-instance key &optional (allow-auto-p t))
  (let ((injector (injector base-instance)))
    (multiple-value-bind (factory scope)
        (%factory-and-scope injector key allow-auto-p)
      (when factory
        (let ((scope-factory (get-factory injector (list :scope scope))))
          (scope-get (funcall scope-factory) key factory))))))

;;; defmodule / declarative-bindings

(defun expand-provider-initargs (initargs)
  (iter (while initargs)
        (for item = (pop initargs))
        (cond ((consp item)
               (appending item into injected-initargs))
              (t
               (collect item into plain-initargs)
               (collect (pop initargs) into plain-initargs)))
        (finally
         (return
           (if (or injected-initargs plain-initargs)
               `(list ,@(when injected-initargs `((quote ,injected-initargs)))
                      ,@plain-initargs)
               '())))))

(defun expand-provider-spec (binder-var key provider-spec
                             &optional (scope :no-scope) (bind-fn 'config-bind))
  (match provider-spec
    ((list :value value)
     `(,bind-fn ,binder-var ',key :to-value ,value :scope ',scope))
    ((list :key recursive-key)
     `(,bind-fn ,binder-var ',key
                :to (make-recursive-provider ',recursive-key)
                :scope ',scope))
    ((list :seq)
     (assert (eq 'config-bind bind-fn) () "bad (:SEQ ...) binding")
     `(config-multibind ,binder-var ',key :scope ',scope))
    ((list :seq pspec)
     (assert (eq 'config-bind bind-fn) () "bad (:SEQ ...) binding")
     (expand-provider-spec binder-var key (ensure-list pspec)
                           scope 'config-multibind))
    ((list :factory expr)
     `(,bind-fn ,binder-var ',key :to ,expr :scope ',scope))
    ((list* (guard class-name (case class-name
                                ((nil t) nil)
                                (t (and (symbolp class-name)
                                        (not (keywordp class-name))))))
            initargs)
     `(,bind-fn ,binder-var ',key
                :to (make-class-provider
                     ',class-name
                     ,(expand-provider-initargs initargs))
                :scope ',scope))
    (_
     (error "invalid provider spec: ~s" provider-spec))))

(defun expand-binding (binder-var binding-spec)
  (match binding-spec
    ((or (list key provider-spec)
         (list key provider-spec scope))
     (expand-provider-spec binder-var key
                           (ensure-list provider-spec)
                           (or scope :no-scope)))
    (_ (error "invalid binding spec: ~s" binding-spec))))

(defmacro defmodule (name (&rest supers) &body bindings)
  (with-gensyms (binder module)
    `(progn
       (defclass ,name ,(or supers '(module)) ())
       (defmethod configure :after ((,binder binder) (,module ,name))
         ,@(mapcar (curry #'expand-binding binder) bindings)))))

(defmacro declarative-bindings (&body bindings)
  (with-gensyms (binder)
    `#'(lambda (,binder)
         ,@(mapcar (curry #'expand-binding binder) bindings))))
