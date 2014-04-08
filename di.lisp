(in-package :di)

(defgeneric configure (injector module))

(defclass injector ()
  ((bindings :reader bindings :initform (make-hash-table :test #'equal)))
  (:documentation "The Injector"))

(defmethod initialize-instance :after ((injector injector) &key config &allow-other-keys)
  (bind-value injector '(:scope :no-scope) (make-instance 'null-scope))
  (bind-value injector '(:scope :singleton) (make-instance 'singleton-scope))
  (dolist (item (flatten (ensure-list config)))
    (configure injector item)))

(defmethod injector ((injector injector)) injector)

(defmethod configure ((injector injector) (module function))
  (funcall module injector))

(defmethod configure ((injector injector) (module symbol))
  (configure injector (make-instance module)))

(defclass module () ())

(defmethod configure ((injector injector) (module module))
  (values))

;;; scope

(defgeneric scope-get (scope key provider))

(defclass null-scope () ())

(defmethod scope-get ((scope null-scope) key provider)
  (declare (ignore key))
  (funcall provider))

(defclass singleton-scope ()
  ((instances :reader instances :initform (make-hash-table :test #'equal))))

(defmethod scope-get ((scope singleton-scope) key provider)
  (or (gethash key (instances scope))
      (when provider
        (setf (gethash key (instances scope))
              (funcall provider)))))

;; TBD: thread-local scope

;;; binding

(defgeneric binding-provider (binding))

(defgeneric binding-add-child (binding child-binding)
  (:method ((binding t) (child-binding t))
    (error "can't multibind a single binding")))

(defgeneric binding-scope (binding))

(defclass injector-binding ()
  ((injector :reader injector :initarg :injector
             :initform (error "must specify the injector"))
   (scope :reader binding-scope :initarg :scope :initform :no-scope)))

(defclass class-binding (injector-binding)
  ((class-name :reader class-name :initarg :class-name)
   (initargs :reader initargs :initarg :initargs)
   (recursive-p :reader recursive-p :initarg :recursive-p
                :initform t)))

(defun inject-initargs (injector initargs)
  (iter (for (name value . nil) on initargs by #'cddr)
        (collect name)
        (collect
            (cond ((not (proper-list-p value)) value)
                  (t (case (first value)
                       (:value (second value))
                       (:inject (obtain injector (second value)))
                       (:instance
                        (apply (provider injector (second value))
                               (inject-initargs injector (rest (rest value)))))))))))

(defmethod binding-provider ((binding class-binding))
  #'(lambda (&rest initargs)
      (let ((merged-initargs
              (append initargs
                      (inject-initargs
                       (injector binding)
                       (apply #'remove-from-plist
                              (initargs binding)
                              (mapcar #'car (plist-alist initargs)))))))
        (or (when (recursive-p binding)
              ;; set allow-auto-p to nil so we only get non-null value
              ;; when explicit binding exists for the class-name
              (obtain (injector binding) (class-name binding) nil))
            (make-instance-for-injector
             (injector binding)
             (class-name binding)
             merged-initargs)))))

(defclass value-binding (injector-binding)
  ((value :reader value :initarg :value)))

(defmethod binding-provider ((binding value-binding))
  #'(lambda () (value binding)))

(defclass multibinding (injector-binding)
  ((child-bindings :accessor child-bindings :initform '())))

(defmethod binding-provider ((binding multibinding))
  (let ((providers (mapcar #'binding-provider (reverse (child-bindings binding)))))
    #'(lambda () (mapcar #'funcall providers))))

(defmethod binding-add-child ((binding multibinding) child-binding)
  (push child-binding (child-bindings binding)))

(defun bind-class (injector key provider &optional (scope :no-scope))
  ;; TBD: make sure the injector wasn't configured yet
  (setf provider (ensure-list provider))
  (setf (gethash key (bindings injector))
        (make-instance 'class-binding
                       :injector injector
                       :class-name (first provider)
                       :initargs (rest provider)
                       ;; avoid endless recursion
                       :recursive-p (not (eq key (first provider)))
                       :scope scope)))

(defun bind-value (injector key value)
  (setf (gethash key (bindings injector))
        (make-instance 'value-binding
                       :injector injector
                       :value value)))

(defun ensure-multibinding (injector key)
  (or (gethash key (bindings injector))
      (setf (gethash key (bindings injector))
            (make-instance 'multibinding
                           :injector injector))))

(defun bind-value* (injector key value)
  (binding-add-child
   (ensure-multibinding injector key)
   (make-instance 'value-binding
                  :injector injector
                  :value value)))

(defun bind-class* (injector key provider)
  (setf provider (ensure-list provider))
  (binding-add-child
   (ensure-multibinding injector key)
   (make-instance 'class-binding
                  :injector injector
                  :class-name (first provider)
                  :initargs (rest provider))))

(defun make-injector (&rest configs)
  (make-instance 'injector :config configs))

(defun normalize-lambda-list-item (item)
  (cond ((not (symbolp item)) item)
        ((string= (string-upcase item) "&INJECT") '&inject)
        ((string= (string-upcase item) "&PROVIDE") '&provide)
        (t item)))

(defun split-injected-lambda-list (lambda-list)
  (setf lambda-list (mapcar #'normalize-lambda-list-item lambda-list))
  (let ((state :plain))
    (assert (<= (count '&inject lambda-list) 1) () "duplicate &inject")
    (assert (<= (count '&provide lambda-list) 1) () "duplicate &provide")
    (iter (for item in lambda-list)
          (case state
            (:plain
             (case item
               (&inject
                (setf state :inject))
               (&provide
                (setf state :provider))
               (t
                (collect item into plain))))
            (:inject
             (cond ((member item lambda-list-keywords)
                    (collect item into plain)
                    (setf state :plain))
                   ((eq item '&provide)
                    (setf state :provider))
                   (t
                    (collect item into inject))))
            (:provider
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
      (when (typep init '(cons (eql :provider) (cons symbol null)))
        (setf init `(provider injector ',(second init)))))
    (cond (supplied-p
           (list name init supplied-p))
          (init
           (list name init))
          (t
           name))))

(defun expand-injected (lambda-list body &key allow-specializers)
  (multiple-value-bind (plain inject provider)
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
        (if (or inject provider)
            `((let ,(append
                     (iter (for (name key) in inject)
                           (collect `(,name (obtain injector ',key))))
                     (iter (for (name key) in provider)
                           (collect `(,name (provider injector ',key)))))
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

(defun %provider-and-scope (base-instance key &optional (allow-auto-p t))
  (let* ((injector (injector base-instance))
         (binding (gethash key (bindings injector))))
    (cond (binding
           (values (binding-provider binding)
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

(defun provider (base-instance key &optional (allow-auto-p t))
  (values (%provider-and-scope base-instance key allow-auto-p)))

(defun obtain (base-instance key &optional (allow-auto-p t))
  (let ((injector (injector base-instance)))
    (multiple-value-bind (provider scope)
        (%provider-and-scope injector key allow-auto-p)
      (when provider
        (let ((scope-provider (provider injector (list :scope scope))))
          (scope-get (funcall scope-provider) key provider))))))
