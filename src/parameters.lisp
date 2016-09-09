;; * Parameters setting framework
;; ** Package declaration
(in-package :cl-user)

(defpackage parameters
  (:use #:cl #:keyword-dispatch)
  (:export #:make-keyword-id
           #:parameter #:instantiate-object
           #:parameter-base #:parameter-parent #:parameter-constructor
           #:parameter-id #:parameter-name
           #:parameter-description
           #:parameter-value
           #:single-parameter-units
           #:perturbed-parameter #:perturbed-parameter-perturbation
           #:perturb-parameter!
           #:parameter-options
           #:parameter-options-selection
           #:parameter-container #:parameter-container-children
           #:parameter-broadcast
           #:parameter-ref
           #:single-parameter
           #:traverse-parameter))

(in-package :parameters)

;; ** Some utils 
(defun replace-characters (string parts replacement &key (test #'char=))
  "Returns a new string in which all the occurences of any part of the PARTS
are replaced with REPLACEMENT. If parts occur in the string in succession,
they will be replaced with one REPLACEMENT onyl"
  (with-output-to-string (out)
    (let ((replaced nil))
      (with-input-from-string (in (string-trim '(#\space) string))
        (loop for character = (read-char in nil nil)
           until (null character)
           if (find character parts :test test)
           do (unless replaced
                (write-char replacement out)
                (setf replaced t))
           else
           do (write-char character out)
           (setf replaced nil)
           end)))))

(defvar *illigal-keyword-string-characters*
  (list #\space #\. #\, #\: #\_ #\- #\;)
  "The list of characters that are not permitted in the string to be 
converted into keyword. See MAKE-KEYWORD-ID")

(defun make-keyword-id (string
                        &optional (illigal-characters
                                   *illigal-keyword-string-characters*))
  "Interns keyword from a string by replacing some illigal string characters
with '-'. See *ILLIGAL-KEYWORD-STRING-CHARACTERS* for the list of characters"
  (if (symbolp string)
      string
      (intern (string-upcase
               (replace-characters string illigal-characters #\-))
              'keyword)))

;; ** Keyword-generic =PARAMETER=

(define-keyword-generic parameter
    (:documentation
     "Generic parameter instantiator. Which parameter will instantiate,
will depend on the set of keyword arguments provided"))


;; ** Common structure
(defclass parameter-base()
  ((parent
    :initarg :parent
    :initform nil
    :accessor parameter-parent
    :documentation "Parent parameter, responsible for setting the current one")
   (constructor
    :initarg :constructor
    :initform #'identity
    :accessor parameter-constructor
    :documentation
    "Function that creates an instance corresponding to a parameter")
   (id
    :initarg :id
    :type :keyword
    :accessor parameter-id
    :documentation "Parameter ID for quick access")
   (name
    :initarg :name
    :type string
    :accessor parameter-name
    :documentation "Parameter name")
   (description
    :initarg :description
    :initform ""
    :type string
    :accessor parameter-description
    :documentation "Description of the parameter"))
  (:documentation "Base class for all parameters"))

(defmethod initialize-instance :after ((object parameter-base) &key)
  (unless (slot-boundp object 'id)
    (with-slots (id name) object
      (setf id (make-keyword-id name)))))

;; ** Generics
;; The full set of generics that every kind of parameter must implement:

;; - These are implemented automatically by inhereting from
;;   =PARAMETER-BASE=:
;;   - =PARAMETER-NAME= :: name of the parameter (with =SETF=)
;;   - =PARAMETER-ID= :: id of the parameter (with =SETF=)
;;   - =PARAMETER-DESCRIPTION= :: longer description of the parameter (with =SETF=)
;;   - =PARAMETER-PARENT= :: parent of the parameter (with =SETF=)
;;   - =PARAMETER-CONSTRUCTOR= :: constructor of actual value out of the
;;        parameter (with =SETF=)
;; - These need to be implemented specially:
;;   - =INSTANTIATE-OBJECT= :: produces actual object out of parameter
;;   - =PARAMETER-VALUE= :: gets the raw (uninstantiated) parameter
;;        value. For some kind of parameters it might make sense to
;;        create =SETF= variant of it, but not for all.

(defgeneric instantiate-object (parameter)
  (:documentation
   "Instantiates object based on PARAMETER"))

(defmethod instantiate-object ((parameter parameter-base))
  (funcall (parameter-constructor parameter) (parameter-value parameter)))

(defgeneric parameter-value (parameter)
  (:documentation
   "Gets the value of the PARAMETER.
Note: not instantiated object, see INSTANTIATE-OBJECT for that"))

(defgeneric (setf parameter-value) (newvalue parameter)
  (:documentation
   "Sets current parameter value.
Not all kinds of parameter might allow for it!"))

;; ** Single value parameters
;; *** Simple parameter
;; =VALUE-TRANSFORMER= might not be necessary but it is convenient: it
;; provides an extra level of decopling
(defclass single-parameter (parameter-base)
  ((value
    :initarg :value
    :accessor parameter-value
    :type t
    :documentation "Value of the parameter")
   (units
    :initarg :units
    :initform "-"
    :type string
    :accessor single-parameter-units
    :documentation "Parameter units of measure"))
  (:documentation "Single parameter representation"))

(defmethod print-object ((object single-parameter) out)
  (print-unreadable-object (object out :type t)
    (with-slots (name id value units description) object
      (format out "~A (~A) ~A ~A"
              name id value units))))

;; *** Perturbed parameter
(defclass perturbed-parameter (single-parameter)
  ((perturbation
    :initarg :perturbation
    :accessor perturbed-parameter-perturbation
    :documentation
    "Perturbation of the parameter value. Actual value will be set as
    VALUE +/- RANDOM(PERTURBATION) * 100%"))
  (:documentation
   "Single parameter with (randomly) perturbed value: actual value
set when the object is instantiated is VALUE +/- RANDOM(PERTURBATION) * 100%"))

(defun make-perturbed-constructor (object constructor)
  #'(lambda (x)
      (with-slots ((p perturbation)) object
        (if (= p 0d0)
            (funcall constructor x)
            (funcall constructor
                     (* x (+ 1d0 (random (* 2d0 p)) (- p))))))))

(defmethod initialize-instance :after ((object perturbed-parameter) &key)
  (let ((constructor (slot-value object 'constructor)))
    (setf (slot-value object 'constructor)
          (make-perturbed-constructor object constructor))))

(defmethod parameter-constructor ((object perturbed-parameter))
  (call-next-method))

(defmethod (setf parameter-constructor) (newvalue (object perturbed-parameter))
  (setf (slot-value object 'constructor)
        (make-perturbed-constructor object newvalue)))

;; (defmethod parameter-constructor ((object perturbed-parameter))
;;   (break)
;;   (let ((constructor (call-next-method))
;;         (perturbation (perturbed-parameter-perturbation object)))
;;     (if (zerop perturbation)
;;         constructor
;;         (lambda (x)
;;           (funcall constructor
;;                    (* x (+ 1d0 (random (* 2d0 perturbation)) (- perturbation))))))))

(defmethod update-instance-for-different-class :before ((previous single-parameter)
                                                        (current perturbed-parameter)
                                                        &rest initargs)
  (declare (ignore initargs))
  (let ((constructor (slot-value previous 'constructor)))
    (setf (slot-value current 'constructor)
          (make-perturbed-constructor current constructor))))

(defgeneric perturb-parameter! (parameter perturbation)
  (:documentation
   "Destructively perturb parameter with given perturbation level."))

(defmethod perturb-parameter! ((parameter single-parameter)
                               #+clisp
                               (perturbation number)
                               #-clisp
                               (perturbation double-float))
  "Destructively change PARAMETER to become perturbed with PERTURBATION"
  (change-class parameter 'perturbed-parameter :perturbation perturbation))

(defmethod perturb-parameter! ((parameter perturbed-parameter)
                               #+clisp (perturbation number)
                               #-clisp (perturbation double-float))
  (setf (perturbed-parameter-perturbation parameter) perturbation))

;; ** Parameter-container of other parameters
;; =CHILDREN= are the vector of =PARAMETER= (or it subclass =PARAMETER-OPTIONS=)
(defclass parameter-container (parameter-base)
  ((children
    :initarg :children
    :initform nil
    :reader parameter-container-children
    :documentation "Vector of individual parameters")
   (constructor :initform (lambda (&rest x) x)))
  (:documentation
   "Represents the group of parameters. Each parameter in CHILDREN can be
 either individual parameter, options or container itself
 (or possible other extensions). To qualify for the parameter it must implement
 INSTANTIATE-OBJECT and PARAMETER-VALUE and be a subclass of PARAMETER-BASE.
 Container's CONSTRUCTOR has a different form: it must accept a plist of the form
 (id1 instance1 id2 instance2 ...)
 Note, it is not responsible for instantiating of its children,
 only for assigning them to right slots"))

(defmethod initialize-instance :after ((object parameter-container) &key)
  "Sets the parent of each child"
  (with-slots (children constructor) object
    (mapc (lambda (child) (setf (parameter-parent child) object))
          children)))

(defmethod print-object ((object parameter-container) out)
  (print-unreadable-object (object out :type t)
    (with-slots (name id children) object
      (format out "~@<~A (~A) ~:_~A~:>"
              name id children))))

(defmethod parameter-value ((object parameter-container))
  "Returns a plist of children values"
  (with-slots (children) object
    (loop for child in children
       append (list (parameter-id child) (parameter-value child)))))

(defmethod (setf parameter-value) (newvalue (object parameter-container))
  (with-slots (children) object
    (loop for (id value) on newvalue by #'cddr
       do (let ((child (find id children :key #'parameter-id :test #'eq)))
            (if child
                (setf (parameter-value child) value))))))

;; Access parameter by id
(defmethod parameter-ref ((parameter parameter-container) id)
  "Get subparameter in parameter-container by id"
  (with-slots (children) parameter
    (find id children :key #'parameter-id)))

;; Constructor for =PARAMETER-CONTAINER= is slightly different: it takes
;; the (as =&rest=) plist of the form =(id1 instance1 id2 instance2 ...)=, in contrast
;; to single-value parameters (where it is just a function of value only).
(defmethod instantiate-object ((parameter parameter-container))
  (with-slots (children) parameter
    (let ((children-objects (mapcan
                             (lambda (p)
                               (list (parameter-id p)
                                     (instantiate-object p)))
                             children)))
      (apply (parameter-constructor parameter) children-objects))))

(defmethod perturb-parameter! ((parameter parameter-container) (perturbation list))
  "Perturb all children if perturbation containes corresponding entries"
  (when perturbation
    (with-slots (children) parameter
      (dolist (child children parameter)
        (when (getf perturbation (parameter-id child))
          (perturb-parameter! child (getf perturbation (parameter-id child))))))))

;; ** Parameter-container-of-options
(defclass parameter-options (parameter-container)
  ((selection
    :initarg :selection
    :initform 0
    :accessor parameter-options-selection
    :documentation "Index of the current selection")
   (constructor :initform #'identity))
  (:documentation "Selection of alternative parameters (OPTIONS).
Is useful if alternative models are possible. PARAMETER-VALUE will return
current selection.
When constructed, OPTIONS must be supplied as a list,
but it will be stored in a vector!"))

(defmethod print-object ((object parameter-options) out)
  (print-unreadable-object (object out :type t)
    (with-slots (name id children selection) object
      (format out "~@<~A (~A) ~:_~A~:>"
              name id (parameter-value object)))))

(defmethod parameter-value ((parameter parameter-options))
  "Returns the value of the current selection among options"
  (with-slots (children selection) parameter
    (parameter-value (elt children selection))))

(defmethod (setf parameter-value) (newvalue (parameter parameter-options))
  "SETF the value of the current selection"
  (with-slots (children selection) parameter
    (setf (parameter-value (elt children selection)) newvalue)))

(defmethod instantiate-object ((parameter parameter-options))
  (with-slots (children selection) parameter
    (let ((selected-parameter (elt children selection)))
      (funcall (parameter-constructor parameter)
               (instantiate-object selected-parameter)))))

;; ** Parameter broadcast
(defclass parameter-broadcast (parameter-container)
  ()
  (:documentation
   "Broadcasts parameter at the time of instantiation. Each child is
broadcasted to the list of instances. Each instance is instantiated using
INSTANTIATE-OBJECT. This is useful when used with PERTURBED-PARAMETER
Extra arguments for MAKE-INSTANCE:
  :NUMBER-OF-INSTANCES - number of instances of each sub-parameter
  :NUMBER-NAME - text used to identify the number of instances
  :NUMBER-DESCRIPTION - description of number instances"))

(defmethod initialize-instance :around
    ((object parameter-broadcast) &rest args
     &key
       children number-of-instances
       (number-name "Number of instances")
       (number-description
        "Number of broadcasted instances to create")
       &allow-other-keys)
  "Adds the number of instances (for broadcasting) to the list of children"
  (let* ((p (parameter :name number-name
                       :id :instances-number
                       :value number-of-instances
                       :description number-description)))
    (setf (getf args :children) (cons p children))
    (apply #'call-next-method object args)))

(defmethod instantiate-object ((parameter parameter-broadcast))
  "Instantiates every child NUMBER-OF-INSTANCES number of times"
  (with-slots (children) parameter
    (let ((number-of-instances (instantiate-object (first children)))
          (children (rest children)))
      (let ((children-instances
             (loop for child in children
                append (list (parameter-id child)
                             (loop repeat number-of-instances
                                collect (instantiate-object child))))))
        (apply (parameter-constructor parameter)
               children-instances)))))

;; ** Methods of =PARAMETER=
(define-keyword-method parameter (:value) (&rest args)
  (apply #'make-instance 'single-parameter args))

(define-keyword-method parameter (:value :perturbation) (&rest args)
  (apply #'make-instance 'perturbed-parameter args))

(define-keyword-method parameter (:single-parameter :perturbation)
    (&rest args &key single-parameter perturbation)
  (declare (ignore args))
  (perturb-parameter! single-parameter perturbation))

(define-keyword-method parameter (:children) (&rest args)
  (apply #'make-instance 'parameter-container args))

(define-keyword-method parameter (:children :selection) (&rest args)
  (apply #'make-instance 'parameter-options args))

(define-keyword-method parameter (:options) (&rest args)
  (let ((args (loop for (key value) on args by #'cddr
                 if (eq key :options)
                 append (list :children value)
                 else
                 append (list key value)
                 end)))
   (apply #'make-instance 'parameter-options args)))

(define-keyword-method parameter (:children :number-of-instances)
    (&rest args)
  (apply #'make-instance 'parameter-broadcast args))

;; ** Traversing (remove?)
(defgeneric traverse-parameter (parameter traversing-function)
  (:documentation
   "Applies TRAVERSING-FUNCTION to PARAMETER. If PARAMETER is a container
of other parameters or options and if TRAVERSING-FUNCTION returns non-NIL
value on PARAMETER, will traverse all subparameters in depth-first order"))

(defmethod traverse-parameter ((parameter single-parameter) traversing-function)
  (funcall traversing-function parameter))

(defmethod traverse-parameter ((parameter parameter-container) traversing-function)
  (if (funcall traversing-function parameter)
      (mapc (lambda (x) (traverse-parameter x traversing-function))
            (parameter-container-children parameter))))

(defmethod traverse-parameter ((parameter parameter-options) traversing-function)
  (if (funcall traversing-function parameter)
      (loop for option across (parameter-container-children parameter)
         do (funcall traversing-function option))))
