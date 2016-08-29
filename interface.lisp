(in-package parameters-interface)

(in-readtable :qtools)


;; * Generics
(defgeneric update-parameter-view (view)
  (:documentation
   "Signals to VIEW (parameter representation) that parameter
has been changed and the VIEW must re-draw it. Use it to propogate the
parameter changed information down the parameter hierarchy, i.e. from
containers and options down to individual parameters.
See signal PARAMETER-CHANGED for propogating signal up the hierachy"))

(defgeneric make-ui (parameter)
  (:documentation
   "Constructs the UI representation for the parameter"))

(defgeneric add-to-grid (object grid row column)
  (:documentation
   "Adds OBJECT to an existing layout GRID at ROW and COLUMN.
Returns the list of (BOTTOM RIGHT)-corner of the rectangle that
OBJECT occupies"))

;; * Single Parameter UI representation
;; ** Class definition
(define-widget parameter-ui (QObject)
  ((name :initform (q+:make-qlabel)
         :documentation "Label showing parameter name")
   (entry :initform (q+:make-qlineedit)
          :documentation "Text entry for parameter value")
   (units :initform (q+:make-qlabel)
          :documentation "Label showing parameter units")
   (parameter
    :initarg :parameter
    :accessor parameter-ui-parameter
    :documentation "Reference to representing parameter"))
  (:documentation "Representation of the single parameter"))

;; Initialize it: since it is not a widget, needs to be done in
;; INITIALIZE-INSTANCE method
(defmethod initialize-instance :after ((instance parameter-ui) &key)
  (with-slots (name entry units parameter) instance
    (setf (q+:text name) (parameter-base-name parameter))
    (setf (q+:text entry) (format nil "~A" (parameter-value parameter)))
    (setf (q+:alignment entry) (q+:qt.align-right))
    (setf (q+:tool-tip entry) (parameter-base-description parameter))
    (setf (q+:text units) (parameter-units parameter))))

;;  ** Parameter changed signal
;; This signal is used to propogate up in hierarchy of parameters:
;; from elementary parameter to containers and options.
;; Don't use it for down propogation! Otherwise the program might be
;; caught in infinite loop of catching and re-emitting the same signal!
;; See method UPDATE-PARAMETER-VIEW for moving down the hierarchy
(define-signal (parameter-ui parameter-changed) ())

;; Emit PARAMETER-CHANGED on the change of entry value
(define-slot (parameter-ui entry) ((new-text string))
  (declare (connected entry (text-changed string)))
  (handler-case
      (let ((new-number (parse-number new-text)))
        (setf (parameter-value parameter) new-number)
        (signal! parameter-ui (parameter-changed)))
    (error nil)))

;; ** Generic functions
;; Factory method
(defmethod make-ui ((object parameter))
  (make-instance 'parameter-ui :parameter object))

;; Add to an existing grid (for proper alignment)
(defmethod add-to-grid ((object parameter-ui) grid row column)
  (with-slots (name entry units) object
    (q+:add-widget grid name row column 1 1)
    (q+:add-widget grid entry row (1+ column) 1 1)
    (q+:add-widget grid units row (+ column 2) 1 1)
    (list (1+ row) (+ column 3))))

;; Was informed: parameter changed
(defmethod update-parameter-view ((widget parameter-ui))
  (with-slots (entry parameter) widget
    (setf (q+:text entry) (format nil "~A" (parameter-value parameter)))))

;; ** Quick runner/demo
;; Window for the parameter view
(define-widget parameter-ui-window (QWidget)
  ((parameter :initarg :parameter)
   parameter-ui))

;; Layout for the window
(define-subwidget (parameter-ui-window layout)
    (q+:make-qgridlayout parameter-ui-window))

;; Connects the signal to the slot of the parameter changed.
;; Adds parameter-ui to the grid layout
(defmethod initialize-instance :after ((instance parameter-ui-window) &key)
  (with-slots (parameter parameter-ui layout) instance
    (setf parameter-ui (make-ui parameter))
    (setf (q+:parent parameter-ui) instance)
    (connect! parameter-ui (parameter-changed)
              instance (parameter-changed))
    (add-to-grid parameter-ui layout 0 0)))

;; Just informing that parameter has changed
(define-slot (parameter-ui-window parameter-changed) ()
  (format t "~&Parameter changed: ~A~%" parameter))

(defun parameter-demo (parameter)
  "Runs simple demo showing the UI for PARAMETER.
Works with any kind of PARAMETER"
  (with-main-window (window (make-instance 'parameter-ui-window
                              :parameter parameter))))

;; * Parameter container UI
;; ** Class definition
;; Following the convention: PARAMETER-CONTAINER-UI is
;; just a QObject and not a widget
(define-widget parameter-container-ui (QObject)
  ((parameter-container
    :initarg :parameter-container
    :documentation
    "Parameter container (of other parameters) that UI represents")
   (children-ui
    :initform nil
    :documentation "Collection of children UI parameter representations")))

;; Catch PARAMETER-CHANGE from CHILDREN-UI and re-emit to
;; propogate it up the hierachy
(define-slot (parameter-container-ui parameter-changed) ()
  (signal! parameter-container-ui (parameter-changed)))

;; Constructs all children and catches their PARAMETER-CHANGE signal
(defmethod initialize-instance :after ((instance parameter-container-ui) &key)
  (with-slots (parameter-container children-ui) instance
    (loop for parameter in (parameter-container-children parameter-container)
       do (let ((parameter-ui (make-ui parameter)))
            (connect! parameter-ui (parameter-changed)
                      instance (parameter-changed))
            (push parameter-ui children-ui))
       finally (setf children-ui (reverse children-ui)))))

;; ** Generics implementation
(defmethod make-ui ((object parameter-container))
  (make-instance 'parameter-container-ui
    :parameter-container object))

;; To make sure contained and individual parameters lay out properly,
;; this method works as follows: it adds every child, checks it
;; extent (right-bottom corner) and adds the next child below
;; Returns the bottom of the last child and the most right used by a child
(defmethod add-to-grid ((object parameter-container-ui) grid row column)
  (with-slots (children-ui) object
    (let ((current-row row)
          (current-column column)
          (most-right-column column))
      (loop for child-ui in children-ui
         do (destructuring-bind (bottom right)
                (add-to-grid child-ui grid current-row current-column)
              (setf current-row bottom)
              (setf most-right-column (max most-right-column right))))
      (list current-row most-right-column))))

;; Propogates the change down the hierarchy to all the children
(defmethod update-parameter-view ((widget parameter-container-ui))
  (with-slots (children-ui) widget
    (dolist (parameter-ui children-ui)
      (update-parameter-view parameter-ui))))

;; * Parameter Options UI
;; ** Class definition
;; To propogate signals of parameter change up and down
;; parameters hierarchy, need to keep OPTIONS-UI
(define-widget parameter-options-ui (QObject)
  ((parameter-options
    :initarg :parameter-options
    :documentation "Parameter options for which to constuct the UI")
   (switches
    :initform nil
    :documentation "Radio buttons to switch between options")
   (dialogs
    :initform nil
    :documentation "Dialogs for each option")
   (options-ui
    :initform nil
    :documentation "Actual UIs for each option")
   (switches-group
    :initform (q+:make-qbuttongroup)
    :documentation "Switches group to group radio-buttons together")))

;; Re-emit the signal on parameter change
;; Need this slot to propogate the change within individual option
(define-slot (parameter-options-ui parameter-changed) ()
  (signal! parameter-options-ui (parameter-changed)))

;; If option changed, hide all other options dialogs and show
;; only the active option dialog.
;; Signal up that parameter changed
(define-slot (parameter-options-ui option-changed) ((checked bool))
  (loop for switch across switches
     for dialog across dialogs
     for i from 0
     do (if (q+:is-checked switch)
            (progn
              (q+:show dialog)
              (setf (parameter-options-selection parameter-options) i))
            (progn
              (q+:hide dialog))))
  (signal! parameter-options-ui (parameter-changed)))

;; Setups all features and connects the signals
(defmethod initialize-instance :after ((instance parameter-options-ui) &key)
  (with-slots (parameter-options switches dialogs switches-group options-ui) instance
    (loop for parameter across (parameter-options-options parameter-options)
       for index from 0
       do (let ((parameter-ui (make-ui parameter))
                (switch (q+:make-qradiobutton (parameter-base-name parameter)))
                (dialog (q+:make-qdialog ))
                (dialog-layout (q+:make-qgridlayout)))
            ;; Setup dialog:
            ;; non-modal
            (q+:set-modal dialog nil)
            ;; Title
            (q+:set-window-title dialog (parameter-base-name parameter))
            ;; add parameters to dialog layout
            (add-to-grid parameter-ui dialog-layout 0 0)
            ;; add layout to dialog
            (setf (q+:layout dialog) dialog-layout)
            ;; signals
            ;; clicked is better than toggled:
            ;; 1. fires up when button is clicked but not necessarily changed
            ;; 2. does not fire up on the default setting
            (connect! switch (clicked bool)
                      instance (option-changed bool))
            (connect! parameter-ui (parameter-changed)
                      instance (parameter-changed))
            (q+:add-button switches-group switch index)
            (when (= (parameter-options-selection parameter-options) index)
              (setf (q+:checked switch) t))
            ;; TODO: need to take care about destroying dialogs
            ;; add widgets to lists
            (push switch switches)
            (push dialog dialogs)
            (push parameter-ui options-ui)))
    (setf switches (apply #'vector (reverse switches)))
    (setf dialogs (apply #'vector (reverse dialogs)))
    (setf options-ui (apply #'vector (reverse options-ui)))))

;; ** Generics implementations
(defmethod make-ui ((object parameter-options))
  (make-instance 'parameter-options-ui
    :parameter-options object))

;; The only complication here: need to group radio-buttons together.
;; Hence, the use of VBOX
(defmethod add-to-grid ((object parameter-options-ui) grid row column)
  (with-slots (switches) object
    (let ((widget (q+:make-qwidget))
          (vbox (q+:make-qvboxlayout)))
      (loop for switch across switches
         do (q+:add-widget vbox switch))
      (q+:set-layout widget vbox)
      (q+:add-widget grid widget row column 1 1))
    (list (1+ row) (1+ column))))

;; Propogate change down to all the options.
;; Make sure the right switch is on.
(defmethod update-parameter-view ((view parameter-options-ui))
  (with-slots (options-ui switches parameter-options) view
    (q+:set-checked (elt switches (parameter-options-selection
                                   parameter-options))
                    t)
    (loop for parameter-ui across options-ui
       do (update-parameter-view parameter-ui))))

;; * CL Object summary widget
(define-widget object-view (QTextEdit)
  ((object
    :initarg :object
    :accessor object-view-object
    :documentation "Object that is represented")
   (transformer
    :initarg :transformer
    :initform #'identity
    :accessor object-view-transformer
    :documentation
    "Function of OBJECT that adds finer control of how object is\
is represented"))
  (:documentation
   "Summary widget of a CL object"))

(defmethod initialize-instance :after ((widget object-view) &key)
  (with-slots (object transformer) widget
    ;;(q+:set-word-wrap widget t)
    (setf (q+:word-wrap-mode widget) 4)
    (q+:set-read-only widget t)
    (setf (q+:text widget) (format nil "~A" (funcall transformer object)))))

(defmethod update-parameter-view ((widget object-view))
  (with-slots (object transformer) widget
    (setf (q+:text widget) (format nil "~A" (funcall transformer object)))))

;; * Example: model runner
(define-widget model-show (QWidget)
  ((parameter
    :initarg :parameter
    :documentation "Parameter to show"
    :accessor model-show-parameter)
   (object
    :initform nil
    :documentation "Object that will be constructed based on PARAMETER"
    :accessor model-show-object)
   (object-operation-widgets
    :initform nil
    :initarg :object-operation-widgets
    :documentation
    "Widgets used for operations/actions on the object/parameter.
For example, Instantiate, Run Model, Report Results etc.
These are user programmable to extend MODEL-SHOW functionality"
    :accessor model-show-object-operations-widgets)
   (synonyms
    :initform nil
    :initarg :synonyms
    :documentation "List of synonyms to be used when the configuration is
loaded from YAML configuration file"))
  (:documentation
   "A window to show and instantiate the model defined by PARAMETER.
It supports parameter merge from YAML file"))

;; Parameter UI - funny enough works as subwidget.
(define-subwidget (model-show parameter-widget)
    (make-ui parameter)
  (setf (q+:parent parameter-widget) model-show))

;; View of the parameter object
(define-subwidget (model-show parameter-view)
    (make-instance 'object-view :object parameter)
  ;; (setf (q+:parent parameter-view) model-show)
  )

;; Object view for instantiated object
(define-subwidget (model-show object-view)
    (make-instance 'object-view :object object))

;; Layout for the parameter
(define-subwidget (model-show parameter-layout)
    (q+:make-qgridlayout)
  (add-to-grid parameter-widget parameter-layout 0 0))

;; Layout for the operation widgets on object/parameter
(define-subwidget (model-show operations-layout)
    (q+:make-qgridlayout)
  (with-slots (object-operation-widgets) model-show
    (let ((row 0)
          widget-list)
      (loop for widget in object-operation-widgets
         do (progn
              ;; instantiate widget in the context
              (setf widget (funcall widget model-show))
              (destructuring-bind (bottom right)
                  (add-to-grid widget operations-layout row 0)
                (declare (ignore right))
                (setf row bottom))
              (push widget widget-list)))
      (setf widget-list (nreverse widget-list))
      (setf object-operation-widgets widget-list))))

;; Main window layout

(define-subwidget (model-show layout) (q+:make-qgridlayout model-show)
  (q+:add-layout layout parameter-layout 0 0 1 1 (q+:qt.align-top))
  (q+:add-widget layout parameter-view 1 0 1 1)
  (q+:add-layout layout operations-layout 0 1 1 1 (q+:qt.align-top))
  (q+:add-widget layout object-view 1 1 1 1))

;; Updates parameter view widget when parameter was changed
;; via user input or YAML configuration loading
(define-slot (model-show parameter-changed) ()
  (declare (connected parameter-widget (parameter-changed)))
  (update-parameter-view parameter-view))

(defun make-button-in-context (label clicked-action-in-context)
  "Produces the function of the context that creates a button with LABEL.
Connects clicked() signal to function produced by CLICKED-ACTION-IN-CONTEXT.
CLICKED-ACTION-CONTEXT must be a function of context, producing a function
of button, i.e. (LAMBDA (CONTEXT) (LAMBDA (BUTTON) ACTIONS...))"
  (lambda (context)
    (let ((button (q+:make-qpushbutton)))
      (setf (q+:text button) label)
      (connect button "clicked()" button (funcall clicked-action-in-context context))
      button)))

;; Adds a widget to a grid. Note: Qt widgets do not form
;; proper CL classes, have to specialize on QOBJECT
;; By default: takes one cell of the grid
(defmethod add-to-grid ((object qobject) grid row column)
  (q+:add-widget grid object row column 1 1)
  (list (1+ row) (1+ column)))

(defun make-instantiate-button ()
  "Produces a function of MODEL-SHOW that makes Instantiate button.
The button instantiates the PARAMETER into the OBJECT of MODEL-SHOW
and updates the OBJECT-VIEW.
This form is suitable to be used as an entry in the list for
:OBJECT-OPERATION-WIDGETS initarg of MODEL-SHOW"
  (make-button-in-context
   "Instantiate"
   (lambda (model-show)
     (with-slots (parameter object object-view) model-show
       (lambda (self)
         (declare (ignore self))
         (setf object (instantiate-object parameter))
         (setf (object-view-object object-view) object)
         (update-parameter-view object-view))))))

;; CL-ying the result of Qt's exec method
(defun exec-dialog-p (dialog)
  "Converts the Qt's exec() result codes into CL's T and NIL.
Intended to be used only with Ok/Cancel dialogs (e.g. file dialog)"
  (if (= (q+:exec dialog) 1)
      t
      nil))

(defun make-load-configuration-button ()
  "Produces a function of MODEL-SHOW that makes Load configuration... button.
The button load YAML configuration into the PARAMETER of the MODEL-SHOW
and updates PARAMETER-VIEW.
This form is suitable to be used as an entry in the list for
:OBJECT-OPERATION-WIDGETS initarg of MODEL-SHOW"
  (make-button-in-context
   "Load configuration..."
   (lambda (model-show)
     (lambda (self)
       (declare (ignore self))
       (with-slots (parameter parameter-widget parameter-view synonyms) model-show
         (let ((file-dialog (q+:make-qfiledialog model-show
                                                 "Open configuration"
                                                 (namestring (uiop/os:getcwd))
                                                 "YAML Files (*.yaml)")))
           (when (exec-dialog-p file-dialog)
             (let ((*read-default-float-format* 'double-float))
               (let* ((yaml-file (first (q+:selected-files file-dialog)))
                      (configuration (-> (yaml-load-file yaml-file)
                                         (convert-yaml)
                                         (remove-annotations)
                                         (unify-notation synonyms))))
                 (update-parameter-from-config parameter configuration)))
             (update-parameter-view parameter-widget)
             (update-parameter-view parameter-view))))))))

(defun show-model (parameter
                   &optional
                     synonyms
                     (operations (list (make-load-configuration-button)
                                       (make-instantiate-button))))
  "Runs the model input defined by PARAMETER.
Uses SYNONYMS plist when YAML configuration is loaded"
  (with-main-window (window (make-instance 'model-show
                              :parameter parameter
                              :synonyms synonyms
                              :object-operation-widgets operations))
    (q+:set-window-title window "Parameter setter example")))

