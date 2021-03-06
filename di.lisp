(in-package :di)

(define-condition injection-error (simple-error) ())

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

(defclass singleton-mixin ()
  ()
  (:default-initargs :scope :singleton))

;; TBD: thread-local scope

;;; bindings

(defstruct (binding
            (:type list)
            (:constructor %make-binding (provider scope)))
  provider scope)

(defun make-binding (provider scope)
  (%make-binding provider (or scope (provider-scope provider))))

(defgeneric provider-factory-function (provider injector))

(defgeneric provider-add-child (provider child-provider)
  (:method ((provider t) (child-provider t))
    (error "can't add child to a non-sequence provider")))

(defgeneric provider-add-key (provider key child-provider)
  (:method ((provider t) (key t) (child-provider t))
    (error "can't add key to a non-mapping provider")))

(defgeneric provider-scope (provider)
  (:method ((provider t)) :no-scope))

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

(defmethod provider-scope ((provider class-provider))
  (class-scope (class-name provider)))

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

;;; mapping-provider

(defclass mapping-provider (provider)
  ((child-alist :accessor child-alist :initform '())))

(defmethod provider-factory-function ((provider mapping-provider) (injector injector))
  (let ((factory-alist
          (iter (for (key . child-provider) in (reverse (child-alist provider)))
                (collect (cons key (provider-factory-function child-provider injector))))))
    #'(lambda ()
        (iter (for (key . factory) in factory-alist)
              (collect (cons key (funcall factory)))))))

(defmethod provider-add-key ((provider mapping-provider) key child-provider)
  (let ((existing (assoc key (child-alist provider))))
    (if existing
        (setf (cdr existing) child-provider)
        (setf (child-alist provider)
              (acons key child-provider (child-alist provider))))))

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

(defun config-bind (binder key &key to to-value scope)
  (assert (or to to-value) ()
          "CONFIG-BIND: must specify either :TO or :TO-VALUE")
  (assert (or (not to) (not to-value)) ()
          "CONFIG-BIND: cannot specify both :TO and :TO-VALUE")
  (setf (binder-get binder key)
        (make-binding
         (if to-value (make-value-provider to-value) (auto-provider to))
         scope)))

(defun config-multibind (binder key &key (to nil to-p) (to-value nil to-value-p) scope)
  (cond ((not (binder-get binder key))
         (setf (binder-get binder key)
               (make-binding (make-instance 'sequence-provider) scope)))
        ((not (typep (binding-provider (binder-get binder key))
                     'sequence-provider))
         (error "trying to use CONFIG-MULTIBIND with key that was ~
                 previously used with CONFIG-BIND or CONFIG-MAPBIND")))
  (when (or to-p to-value-p)
    (provider-add-child (binding-provider (binder-get binder key))
                        (if to-value
                            (make-value-provider to-value)
                            (auto-provider to)))))

(defun config-mapbind (binder key
                       &key (map-key nil map-key-p)
                         (to nil to-p)
                         (to-value nil to-value-p)
                         scope)
  (cond ((not (binder-get binder key))
         (setf (binder-get binder key)
               (make-binding (make-instance 'mapping-provider) scope)))
        ((not (typep (binding-provider (binder-get binder key))
                     'mapping-provider))
         (error "trying to use CONFIG-MAPBIND with key that was ~
                 previously used with CONFIG-BIND or CONFIG-MULTIBIND")))
  (when map-key-p
    (assert (or to-p to-value-p) ()
            "CONFIG-MAPBIND: must specify either :TO or :TO-VALUE")
    (provider-add-key (binding-provider (binder-get binder key))
                      map-key
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
                    (collect item into factories)))))
          (finally
           (return (values plain inject factories))))))

(defun injected-opt-arg (arg &optional allow-inject-p)
  (destructuring-bind (name init supplied-p) arg
    (when (consp name)
      (setf name (second name)))
    (when allow-inject-p
      (match init
        ((list :inject key)
         (setf init `(%get/default via ',key)))
        ((list :inject key default)
         (setf init `(%get/default via ',key obtain ,default)))
        ((list :factory key)
         (setf init `(%get/default via ',key get-factory)))
        ((list :factory key default)
         (setf init `(%get/default via ',key get-factory ,default)))))
    (cond (supplied-p
           (list name init supplied-p))
          (init
           (list name init))
          (t
           name))))

(defun special-arg-let-bindings (args obtain-func &optional funcs-p)
  (iter (for item in args)
        (destructuring-bind (name key &optional (default nil default-p))
            (ensure-list item)
          (let ((actual-name
                  (if funcs-p
                      (let ((var-name (gensym (symbol-name name))))
                        (with-gensyms (args)
                          (collect `(,name (&rest ,args) (apply ,var-name ,args))
                            into func-bindings))
                        var-name)
                      name)))
            (collect `(,actual-name (%get/default
                                        via ',key
                                        ,obtain-func
                                        ,@(when default-p (list default))))
              into bindings)))
        (finally (return (values bindings func-bindings)))))

;; expand &inject and &factory items
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
        ;; add VIA keyword argument
        '(&key via)
        (mapcar (rcurry #'injected-opt-arg t) keyargs)
        (when allow-other-keys-p '(&allow-other-keys))
        (when aux (cons '&aux aux)))
       (if (null (or inject factory))
           body
           (multiple-value-bind (factory-bindings flet-bindings)
               (special-arg-let-bindings factory 'get-factory t)
             `((let ,(append
                      (special-arg-let-bindings inject 'obtain)
                      factory-bindings)
                 ,@(if flet-bindings
                       `((flet ,flet-bindings ,@body))
                       body)))))))))

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
    :initarg :via
    :initform nil
    :documentation "Injector instance")
   (scope
    :accessor scope
    :initarg :scope
    :documentation "Default scope for the class"))
  (:documentation "Base class for classes utilizing DI"))

(defun class-slot-value (class slot-name &optional (default nil default-p))
  (when (symbolp class)
    (setf class (find-class class)))
  (c2mop:ensure-finalized class)
  (let ((slotd (find slot-name (c2mop:class-slots class)
                     :key #'c2mop:slot-definition-name)))
    (cond ((and (not slotd) (not default-p))
           (error "no such slot ~s in class ~s" slot-name class))
          ((not slotd) default)
          (t
           (let ((slot-initargs (c2mop:slot-definition-initargs slotd))
                 (class-initargs (c2mop:class-default-initargs class)))
             (iter (for initarg in slot-initargs)
                   (when-let ((initarg-func (third (find initarg class-initargs :key #'first))))
                     (return (funcall initarg-func)))
                   (finally
                    (let ((initfunction (c2mop:slot-definition-initfunction slotd)))
                      (return
                        (cond (initfunction
                               (return (funcall initfunction)))
                              (default-p default)
                              (t
                               (error "can't get value of slot ~s in class ~s" slot-name class))))))))))))

(defun class-scope (class)
  (class-slot-value class 'scope :no-scope))

(defvar *inject-catch-nodefault* nil)
(defstruct (injected-value
            (:constructor make-injected-value (value)))
  value)

(defun make-instance-for-injector (injector class-spec initargs)
  ;; skip :via initarg for non-injected classes
  (if (subtypep class-spec 'injected)
      (apply #'make-instance class-spec :via injector initargs)
      (apply #'make-instance class-spec initargs)))

(defun inject (key &optional (default nil default-p) (catch-nodefault *inject-catch-nodefault*))
  (declare (ignore key))
  (make-injected-value
   (cond (default-p default)
         (catch-nodefault
          (error 'injection-error :format-control "injector required"))
         (t nil))))

(defun inject-form-key (form)
  (match form
    ((list* 'inject (list 'quote key) _) key)
    (_ nil)))

(defun inject-default-initargs (injector instance skip-initargs)
  (iter (for (initarg value function) in
         (c2mop:class-default-initargs (class-of instance)))
        (unless (member initarg skip-initargs)
          (when-let ((key (inject-form-key value)))
            (if injector
                (collect (cons initarg (obtain injector key)))
                (let ((*inject-catch-nodefault* t))
                  (funcall function)))))))

(defun inject-slot-initargs (injector instance slot-names skip-initargs)
  (iter (for slotd in (c2mop:class-slots (class-of instance)))
        (let ((slot-name (c2mop:slot-definition-name slotd)))
          (when (or (eq t slot-names)
                    (member slot-name slot-names))
            (let ((targets (iter (for initarg in (c2mop:slot-definition-initargs slotd))
                                 (unless (member initarg skip-initargs)
                                   (when-let ((injected-instance (obtain injector initarg nil)))
                                     (collect injected-instance))))))
              (when (rest targets)
                (warn "ambiguous initarg binding: class ~s slot ~s initargs ~s"
                      (type-of instance)
                      slot-name
                      (c2mop:slot-definition-initargs slotd)))
              (when targets
                (collect (cons (first (c2mop:slot-definition-initargs slotd))
                               (first targets)))))))))

(defun split-injected-initargs (initargs)
  "Unwrap injected initargs from INITARGS plist and return
  injected and non-injected initargs as two separate values.
  Skip :VIA initarg."
  (iter (for (key value) on initargs by #'cddr)
        (cond ((eq :via key))
              ((typep value 'injected-value)
               (collect (cons key (injected-value-value value)) into injected-initargs))
              (t
               (collect (cons key value) into plain-initargs)))
        (finally
         (return (values injected-initargs plain-initargs)))))

(defun initargs-with-injection (instance slot-names &rest initargs
                                &key (via nil via-p) &allow-other-keys)
  ;; Separate injected initargs, i.e. those which are using (inject '... ...) form
  ;; in their default initargs, from plain initargs. INJECT function supplies
  ;; wrapped values which are also unwrapped by SPLIT-INJECTED-INITARGS.
  ;; SPLIT-INJECTED-INITARGS returns two alists as separate values.
  (multiple-value-bind (injected-initargs plain-initargs)
      (split-injected-initargs initargs)
    ;; For non-injected initargs that are directly specified in
    ;; MAKE-INSTANCE / INJECT-INSTANCE / etc. call, no injection
    ;; processing should be done in INJECT-SLOT-INITARGS/INJECT-DEFAULT-INITARGS
    ;; (such processing may involve new object creation).
    (let* ((skip-initargs (mapcar #'first plain-initargs))
           ;; obtain injected initargs based on :DEFAULT-INITARGS of the class
           (injected-default-initargs (inject-default-initargs via instance skip-initargs)))
      ;; avoid new object creation due to slot initarg based injection
      ;; for slots that are already injected via default initargs
      (nconcf skip-initargs (mapcar #'car injected-default-initargs))
      (alist-plist
       ;; Delete duplicate initargs that occur *earlier* in the
       ;; resulting list.
       (delete-duplicates
        (append injected-initargs  ; lowest priority for injected-initargs (defaults from INJECT forms)
                ;; When injector is available, use initarg names specified in the slots
                ;; for injection.
                (when via
                  (inject-slot-initargs via instance slot-names skip-initargs))
                ;; :DEFAULT-INITARGS-based injection takes precedence over slot-initarg-based injection
                injected-default-initargs
                ;; Directly specified initargs are most important
                plain-initargs
                ;; Need to pass on :VIA argument to initialize INJECTOR slot
                (when via-p (list (cons :via (di:injector via)))))
        :key #'car)))))

(defmethod shared-initialize :around ((instance injected) (slot-names t) &rest initargs)
  (apply #'call-next-method instance slot-names
         (apply #'initargs-with-injection instance slot-names initargs)))

(defun inject-instance (instance &rest initargs)
  (apply #'reinitialize-instance instance
         (append
          (apply #'initargs-with-injection instance t initargs))))

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
            (class-scope key)))
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

(defmacro %get/default (base-instance key &optional (func 'obtain) (default nil default-p))
  (once-only (base-instance)
    `(if ,base-instance
         (,func ,base-instance ,key)
         ,(if default-p
              default
              `(error 'injection-error
                      :format-control ":VIA required (key: ~s)"
                      :format-arguments (list ,key))))))

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
                             &optional scope (bind-fn 'config-bind)
                               (map-key nil map-key-p))
  ;; TBD: ,@(when map-key-p `(:map-key ',map-key)) everywhere, use flet
  (flet ((bind-call (&rest args)
           `((,bind-fn ,binder-var ',key ,@args
                       ,@(when map-key-p `(:map-key ',map-key))
                       :scope ',scope))))
    (match provider-spec
      ((list :value value)
       (bind-call :to-value value))
      ((list :key recursive-key)
       (bind-call :to `(make-recursive-provider ',recursive-key)))
      ((list :seq)
       (assert (eq 'config-bind bind-fn) () "bad (:SEQ ...) binding")
       (setf bind-fn 'config-multibind)
       `((config-multibind ,binder-var ',key :scope ',scope)))
      ((list* :seq pspecs)
       (assert (eq 'config-bind bind-fn) () "bad (:SEQ ...) binding")
       (iter (for pspec in pspecs)
             (appending
                 (expand-provider-spec binder-var key (ensure-list pspec)
                                       scope 'config-multibind))))
      ((list :map)
       (assert (eq 'config-bind bind-fn) () "bad (:MAP ...) binding")
       `((config-mapbind ,binder-var ',key :scope ',scope)))
      ((list* :map map-spec)
       (assert (eq 'config-bind bind-fn) () "bad (:MAP ...) binding")
       (iter (for (sub-map-key pspec . nil) on map-spec by #'cddr)
             (appending
                 (expand-provider-spec binder-var key
                                       (ensure-list pspec)
                                       scope 'config-mapbind sub-map-key))))
      ((list :factory expr)
       (bind-call :to expr))
      ((list* (guard class-name (case class-name
                                  ((nil t) nil)
                                  (t (and (symbolp class-name)
                                          (not (keywordp class-name))))))
              initargs)
       (bind-call :to `(make-class-provider
                        ',class-name
                        ,(expand-provider-initargs initargs))))
      (_
       (error "invalid provider spec: ~s" provider-spec)))))

(defun expand-binding (binder-var binding-spec)
  (match binding-spec
    ((or (list key provider-spec)
         (list key provider-spec scope))
     (expand-provider-spec binder-var key
                           (ensure-list provider-spec)
                           scope))
    (_ (error "invalid binding spec: ~s" binding-spec))))

(defmacro defmodule (name (&rest supers) &body bindings)
  (with-gensyms (binder module)
    `(progn
       (defclass ,name ,(or supers '(module)) ())
       (defmethod configure :after ((,binder binder) (,module ,name))
         ,@(mappend (curry #'expand-binding binder) bindings)))))

(defmacro declarative-bindings (&body bindings)
  (with-gensyms (binder)
    `#'(lambda (,binder)
         ,@(mappend (curry #'expand-binding binder) bindings))))
