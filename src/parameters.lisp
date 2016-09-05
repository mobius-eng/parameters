(in-package parameters)

;; * Parameters setting framework

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
    :reader parameter-id
    :documentation "Parameter ID for quick access")
   (name
    :initarg :name
    :type string
    :reader parameter-name
    :documentation "Parameter name")
   (description
    :initarg :description
    :initform ""
    :type string
    :reader parameter-description
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
    :reader single-parameter-units
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

(defmethod parameter-constructor ((object perturbed-parameter))
  (let ((constructor (call-next-method))
        (perturbation (perturbed-parameter-perturbation object)))
    (if (zerop perturbation)
        constructor
        (lambda (x)
          (funcall constructor
                   (* x (+ 1d0 (random (* 2d0 perturbation)) (- perturbation))))))))

(defun perturb-parameter (parameter &optional (perturbation 0d0))
  "Perturb existing PARAMETER by creating a copy with provided PERTURBATION"
  (let* ((slots (closer-mop:class-slots (class-of parameter)))
         (initargs
          (loop for slot in slots
             append (list
                     (first (closer-mop:slot-definition-initargs slot))
                     (slot-value parameter
                                 (closer-mop:slot-definition-name slot))))))
    (apply #'make-instance 'perturbed-parameter
           :perturbation perturbation
           initargs)))

;; ** Parameter-container-of-options
(defclass parameter-options (parameter-base)
  ((options
    :initarg :options
    :reader parameter-options-options
    :documentation "A vector of parameter options")
   (selection
    :initarg :selection
    :initform 0
    :accessor parameter-options-selection
    :documentation "Index of the current selection"))
  (:documentation "Selection of alternative parameters (OPTIONS).
Is useful if alternative models are possible. PARAMETER-VALUE will return
current selection.
When constructed, OPTIONS must be supplied as a list,
but it will be stored in a vector!"))

(defmethod initialize-instance :after ((object parameter-options) &key)
  (with-slots (options) object
    (mapc (lambda (option)
            (setf (parameter-parent option) object))
          options)
    (setf options (apply #'vector options))))

(defmethod print-object ((object parameter-options) out)
  (print-unreadable-object (object out :type t)
    (with-slots (name id options selection) object
      (format out "~@<~A (~A) ~:_~A~:>"
              name id (parameter-value object)))))

(defmethod parameter-value ((parameter parameter-options))
  "Returns the value of the current selection among options"
  (with-slots (options selection) parameter
    (parameter-value (aref options selection))))

(defmethod (setf parameter-value) (newvalue (parameter parameter-options))
  "SETF the value of the current selection"
  (with-slots (options selection) parameter
    (setf (parameter-value (aref options selection)) newvalue)))

(defmethod instantiate-object ((parameter parameter-options))
  (with-slots (options selection) parameter
    (let ((selected-parameter (aref options selection)))
      (funcall (parameter-constructor parameter)
               (instantiate-object selected-parameter)))))

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


;; ** Keyword-generic =PARAMETER=

(define-keyword-generic parameter
    (:documentation
     "Generic parameter instantiator. Which parameter will instantiate,
will depend on the set of keyword arguments provided"))

(define-keyword-method parameter (:value) (&rest args)
  (apply #'make-instance 'single-parameter args))

(define-keyword-method parameter (:value :perturbation) (&rest args)
  (apply #'make-instance 'perturbed-parameter args))

(define-keyword-method parameter (:children) (&rest args)
  (apply #'make-instance 'parameter-container args))

(define-keyword-method parameter (:options) (&rest args)
  (apply #'make-instance 'parameter-options args))

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
      (loop for option across (parameter-options-options parameter)
         do (funcall traversing-function option))))
