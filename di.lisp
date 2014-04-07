(in-package :di)

(defgeneric configure (injector module))

(defclass injector ()
  ((bindings :accessor bindings :initform (make-hash-table :test #'equal))
   (instances :accessor instances :initform (make-hash-table :test #'equal)))
  (:documentation "The Injector"))

(defmethod initialize-instance :after ((injector injector) &key config &allow-other-keys)
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

;;; binding

(defgeneric binding-provider (binding))

(defgeneric binding-add-child (binding child-binding)
  (:method ((binding t) (child-binding t))
    (error "can't multibind a single binding")))

(defclass injector-binding ()
  ((injector :accessor injector :initarg :injector
             :initform (error "must specify the injector"))))

(defclass class-binding (injector-binding)
  ((class-name :accessor class-name :initarg :class-name)
   (initargs :accessor initargs :initarg :initargs)
   (recursive-p :accessor recursive-p :initarg :recursive-p
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
            (apply #'make-instance (class-name binding)
                   :injector (injector binding)
                   merged-initargs)))))

(defclass value-binding (injector-binding)
  ((value :accessor value :initarg :value)))

(defmethod binding-provider ((binding value-binding))
  #'(lambda () (value binding)))

(defclass multibinding (injector-binding)
  ((child-bindings :accessor child-bindings :initform '())))

(defmethod binding-provider ((binding multibinding))
  (let ((providers (mapcar #'binding-provider (reverse (child-bindings binding)))))
    #'(lambda () (mapcar #'funcall providers))))

(defmethod binding-add-child ((binding multibinding) child-binding)
  (push child-binding (child-bindings binding)))

(defun bind-class (injector &rest bindings)
  (doplist (key provider bindings)
    (assert (and key (symbolp key)) ()
            (error "invalid injection key ~s" key))
    (setf provider (ensure-list provider))
    ;; TBD: make sure the injector wasn't configured yet
    (setf (gethash key (bindings injector))
          (make-instance 'class-binding
                         :injector injector
                         :class-name (first provider)
                         :initargs (rest provider)
                         ;; avoid endless recursion
                         :recursive-p (not (eq key (first provider)))))))

(defun bind-value (injector &rest bindings)
  (doplist (key value bindings)
    (assert (and key (symbolp key)) ()
            (error "invalid injection key ~s" key))
    (setf (gethash key (bindings injector))
          (make-instance 'value-binding
                         :injector injector
                         :value value))))

(defun ensure-multibinding (injector key)
  (or (gethash key (bindings injector))
      (setf (gethash key (bindings injector))
            (make-instance 'multibinding
                           :injector injector))))

(defun bind-value* (injector &rest bindings)
  (doplist (key value bindings)
    (assert (and key (symbolp key)) ()
            (error "invalid injection key ~s" key))
    (binding-add-child
     (ensure-multibinding injector key)
     (make-instance 'value-binding
                    :injector injector
                    :value value))))

(defun bind-class* (injector &rest bindings)
  (doplist (key provider bindings)
    (assert (and key (symbolp key)) ()
            (error "invalid injection key ~s" key))
    (setf provider (ensure-list provider))
    (binding-add-child
     (ensure-multibinding injector key)
     (make-instance 'class-binding
                    :injector injector
                    :class-name (first provider)
                    :initargs (rest provider)))))

(defun make-injector (&rest configs)
  (make-instance 'injector :config configs))

(defun split-injected-lambda-list (lambda-list)
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

(defun inject (key)
  (obtain (or *current-injector*
              (error "no current-injector (forgot to add INJECTED base class?)"))
          key))

(defmethod shared-initialize :around ((injected injected) (slot-names t)
                                      &key injector &allow-other-keys)
  (let ((*current-injector* injector))
    (call-next-method)))

(defun provider (base-instance key &optional (allow-auto-p t))
  (let* ((injector (injector base-instance))
         (binding (gethash key (bindings injector))))
    (cond (binding
           (binding-provider binding))
          ((not allow-auto-p) nil)
          ((and key (symbolp key)
                (typep (find-class key nil) 'standard-class))
           #'(lambda (&rest initargs) ;; TBD: &rest initargs
               (apply #'make-instance key :injector injector initargs)))
          (t
           (error "no binding or class found for injection key ~s" key)))))

(defun obtain (base-instance key &optional (allow-auto-p t))
  (let ((injector (injector base-instance)))
    (or (gethash key (instances injector))
        (when-let ((provider (provider injector key allow-auto-p)))
          (setf (gethash key (instances injector))
                (funcall provider))))))
