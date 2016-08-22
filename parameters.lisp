(in-package parameters)

;; * Parameters setting framework

;; ** Some utils 
(defun replace-characters (string parts replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part
is replaced with replacement."
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
  (list #\space #\. #\, #\: #\_)
  "The list of characters that are not permitted in the string to be 
converted into keyword. See MAKE-KEYWORD-ID")

(defun make-keyword-id (string
                        &optional (illigal-characters
                                   *illigal-keyword-string-characters*))
  "Interns keyword from a string by replacing some illigal string characters
with '-'. See *ILLIGAL-KEYWORD-STRING-CHARACTERS* for the list of characters"
  (intern (string-upcase
           (replace-characters string illigal-characters #\-))
          'keyword))

;; ** Common structure
(defclass parameter-base()
  ((parent
    :initarg :parent
    :initform nil
    :accessor parameter-base-parent
    :documentation "Parent parameter, responsible for setting the current one")
   (constructor
    :initarg :constructor
    :initform #'identity
    :reader parameter-base-constructor
    :documentation
    "Function that creates an instance corresponding to a parameter")
   (id
    :initarg :id
    :type :keyword
    :reader parameter-base-id
    :documentation "Parameter ID for quick access")
   (name
    :initarg :name
    :type string
    :reader parameter-base-name
    :documentation "Parameter name")
   (description
    :initarg :description
    :initform ""
    :type string
    :reader parameter-base-description
    :documentation "Description of the parameter"))
  (:documentation "Base class for all parameters"))

(defmethod initialize-instance :after ((object parameter-base) &key)
  (unless (slot-boundp object 'id)
    (with-slots (id name) object
      (setf id (make-keyword-id name)))))

;; ** Generics
(defgeneric instantiate-object (parameter)
  (:documentation
   "Instantiates object based on PARAMETER"))

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
(defclass parameter (parameter-base)
  ((value
    :initarg :value
    :accessor parameter-value
    :type t
    :documentation "Value of the parameter")
   (units
    :initarg :units
    :initform ""
    :reader parameter-units
    :documentation "Parameter units of measure")
   (value-transformer
    :initarg :value-transformer
    :initform #'identity
    :reader parameter-value-transformer
    :documentation
    "Function transforming the parameter value before setting it"))
  (:documentation "Single parameter representation"))

(defmethod instantiate-object ((parameter parameter))
  (with-slots (constructor value value-transformer) parameter
    (funcall constructor (funcall value-transformer value))))


(defmethod print-object ((object parameter) out)
  (print-unreadable-object (object out :type t)
    (with-slots (name id value units description) object
      (format out "~A (~A) ~A ~A"
              name id value units))))

;; *** Perturbed parameter
(defclass perturbed-parameter (parameter)
  ((perturbation
    :initarg :perturbation
    :accessor perturbed-parameter-perturbation
    :documentation
    "Perturbation of the parameter value. Actual value will be set as
    VALUE +/- RANDOM(PERTURBATION) * 100%"))
  (:documentation
   "Single parameter with (randomly) perturbed value: actual value
set when the object is instantiated is VALUE +/- RANDOM(PERTURBATION) * 100%"))

;; Introduce perturbation into VALUE-TRANSFORMER
(defmethod initialize-instance :after ((obj perturbed-parameter) &key)
  (with-slots (value-transformer) obj
    ;; save old value-transformer
    (let ((straight-transformer value-transformer))
      (setf
       value-transformer
       (lambda (x)
         ;; make sure to capture OBJ: need current PERTURBATION
         (with-slots (perturbation) obj
           (funcall
            straight-transformer
            (* x (+ 1d0 (random (* 2d0 perturbation)) (- perturbation))))))))))

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

;; *** Parameter-container-of-options
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
            (setf (parameter-base-parent option) object))
          options)
    (setf options (apply #'vector options))))

(defmethod print-object ((object parameter-options) out)
  (print-unreadable-object (object out :type t)
    (with-slots (name id options selection) object
      (format out "~@<~A (~A) ~:_~A~:>"
              name id (parameter-value object)))))

(defmethod parameter-value ((parameter parameter-options))
  "Returns current selection among options.
NOTE: (SETF PARAMETER-VALUE) is not implemented on purpose!"
  (with-slots (options selection) parameter
    (aref options selection)))

(defmethod instantiate-object ((parameter parameter-options))
  "Instantiates current selection"
  (instantiate-object (parameter-value parameter)))

;; Disable direct setting of PARAMETER-VALUE: this is a safe in this
;; configuration as =PARAMETER-BASE= does not have =VALUE= slot.
(defmethod (setf parameter-value) (newvalue (parameter parameter-options))
  (error "Cannot set value directly to PARAMETER-OPTIONS:\
          must select from the list of options"))

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
    (mapc (lambda (child) (setf (parameter-base-parent child) object))
          children)))

(defmethod print-object ((object parameter-container) out)
  (print-unreadable-object (object out :type t)
    (with-slots (name id children) object
      (format out "~@<~A (~A) ~:_~A~:>"
              name id children))))

(defmethod parameter-value ((object parameter-container))
  "Just returns itself as a value"
  object)

;; Constructor for =PARAMETER-CONTAINER= is slightly different: it takes
;; the (as =&rest=) plist of the form =(id1 instance1 id2 instance2 ...)=, in contrast
;; to single-value parameters (where it is just a function of value only).
(defmethod instantiate-object ((parameter parameter-container))
  (with-slots (children constructor) parameter
    (let ((children-objects (mapcan
                             (lambda (p)
                               (list (parameter-base-id p)
                                     (instantiate-object p)))
                             children)))
      (apply constructor children-objects))))

;; ** Helpers: to reduce verbosity
(defun single-parameter (name value
                  &key (id (make-keyword-id name)) (description "") (units "")
                    (value-transformer #'identity) (constructor #'identity)
                    parent)
  "Creates a single parameter with a given name and value (and other args)"
  (make-instance 'parameter
    :name name
    :id id
    :value value
    :units units
    :description description
    :value-transformer value-transformer
    :constructor constructor
    :parent parent))

(defun perturbed-parameter (name value
                            &key (id (make-keyword-id name)) (description "")
                              (units "") (value-transformer #'identity)
                              (constructor #'identity) (perturbation 0d0)
                              parent)
  "Creates a perturbed parameter with given name and value (and other args)"
  (make-instance 'perturbed-parameter
    :name name
    :id id
    :value value
    :units units
    :description description
    :value-transformer value-transformer
    :constructor constructor
    :parent parent
    :perturbation perturbation))


(defun parameter-options (name options
                          &key (id (make-keyword-id name)) (description "")
                            (constructor #'identity) (selection 0)
                            parent)
  "Creates parameter options named NAME and the list of OPTIONS"
  (make-instance 'parameter-options
    :name name
    :id id
    :options options
    :selection selection
    :description description
    :constructor constructor
    :parent parent))

(defun parameter-container (name children
                            &key (id (make-keyword-id name)) (description "")
                              (constructor (lambda (&rest x) x)) parent)
  "Creates a container of parameters from NAME and the list of CHILDREN"
  (make-instance 'parameter-container
    :name name
    :id id
    :children children
    :constructor constructor
    :description description
    :parent parent))


(defun parameter (&rest args &key value perturbation options children &allow-other-keys)
  "Construct a correct type of parameter based on options supplied"
  (cond ((and value perturbation)
         (apply #'make-instance 'perturbed-parameter args))
        (value (apply #'make-instance 'parameter args))
        (options (apply #'make-instance 'parameter-options args))
        (children (apply #'make-instance 'parameter-container args))
        (t (error "Inconsistent parameter list: one of\
             VALUE PERTURBATION OPTIONS CHILDREN\
             are required"))))

(defgeneric traverse-parameter (parameter traversing-function)
  (:documentation
   "Applies TRAVERSING-FUNCTION to PARAMETER. If PARAMETER is a container
of other parameters or options and if TRAVERSING-FUNCTION returns non-NIL
value on PARAMETER, will traverse all subparameters in depth-first order"))

(defmethod traverse-parameter ((parameter parameter) traversing-function)
  (funcall traversing-function parameter))

(defmethod traverse-parameter ((parameter parameter-container) traversing-function)
  (if (funcall traversing-function parameter)
      (mapc (lambda (x) (traverse-parameter x traversing-function))
            (parameter-container-children parameter))))

(defmethod traverse-parameter ((parameter parameter-options) traversing-function)
  (if (funcall traversing-function parameter)
      (loop for option across (parameter-options-options parameter)
         do (funcall traversing-function option))))
