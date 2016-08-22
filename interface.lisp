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
(defconstant +align-right+ 2
  "Constant for aligning the text in widgets to the right
Might be defined in QTOOLS somewhere but coudn't find it yet")

(defmethod initialize-instance :after ((instance parameter-ui) &key)
  (with-slots (name entry units parameter) instance
    (setf (q+:text name) (parameter-base-name parameter))
    (setf (q+:text entry) (format nil "~A" (parameter-value parameter)))
    (setf (q+:alignment entry) +align-right+)
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
            (connect! switch (toggled bool)
                      instance (option-changed bool))
            (connect! parameter-ui (parameter-changed)
                      instance (parameter-changed))
            (q+:add-button switches-group switch index)
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
(define-widget object-view (QLabel)
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
    (q+:set-word-wrap widget t)
    (setf (q+:text widget) (format nil "~A" (funcall transformer object)))))

(defmethod update-parameter-view ((widget object-view))
  (with-slots (object transformer) widget
    (setf (q+:text widget) (format nil "~A" (funcall transformer object)))))

;; * Example: model runner
(define-widget model-show (QWidget)
  ((parameter
    :initarg :parameter
    :documentation "Parameter to show")
   (object
    :initform nil
    :documentation "Object that will be constructed based on PARAMETER")
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

;; Button to load configuration
(define-subwidget (model-show load-from-yaml)
    (q+:make-qpushbutton model-show)
  (setf (q+:text load-from-yaml) "Load configuration..."))

;; View of the parameter object
(define-subwidget (model-show parameter-view)
    (make-instance 'object-view :object parameter)
  (setf (q+:parent parameter-view) model-show))

;; Button to instantiate the OBJECT from PARAMETER
(define-subwidget (model-show instantiator) (q+:make-qpushbutton)
  (setf (q+:text instantiator) "Instantiate"))

;; TODO: Fix scroll bars. Right now no scroll bars appear
;; Object view for instantiated object
(define-subwidget (model-show object-view)
    (make-instance 'object-view :object object)
  (q+:set-maximum-size object-view (q+:make-qsize 300 300)))

;; Since object representation can be large,
;; place it inside scroll area
(define-subwidget (model-show scrolled-area) (q+:make-qscrollarea))

;; Layout for the scroll area
(define-subwidget (model-show object-view-layout)
    (q+:make-qvboxlayout scrolled-area)
  (q+:add-widget object-view-layout object-view)
  (q+:set-widget scrolled-area (q+:widget object-view-layout)))

;; Main window layout
(define-subwidget (model-show layout) (q+:make-qgridlayout model-show)
  (destructuring-bind (bottom right) (add-to-grid parameter-widget layout 0 0)
    (q+:add-widget layout load-from-yaml bottom 0 1 1)
    (q+:add-widget layout parameter-view (1+ bottom) 0 1 right)
    (q+:add-widget layout instantiator 0 right 1 1)
    (q+:add-widget layout scrolled-area (1+ bottom) right 1 1)))

;; CL-ying the result of Qt's exec method
(defun exec-dialog-p (dialog)
  "Converts the Qt's exec() result codes into CL's T and NIL.
Intended to be used only with Ok/Cancel dialogs (e.g. file dialog)"
  (if (= (q+:exec dialog) 1)
      t
      nil))

;; Select the configuration file and load it into parameter
;; A bit brittle at the moment
(define-slot (model-show load-from-yaml) ((clicked bool))
  (declare (connected load-from-yaml (clicked bool)))
  (format t "~&Button is clicked~%")
  (let ((file-dialog (q+:make-qfiledialog model-show
                                          "Open configuration"
                                          (namestring (uiop/os:getcwd))
                                          "YAML Files (*.yaml)")))
    (when (exec-dialog-p file-dialog)
      (let* ((yaml-file (first (q+:selected-files file-dialog)))
             (configuration (-> (yaml-load-file yaml-file)
                                (convert-yaml)
                                (remove-annotations)
                                (unify-notation synonyms))))
        (update-parameter-from-config parameter configuration))
      (update-parameter-view parameter-widget)
      (update-parameter-view parameter-view)
      (format t "~&Updated~%"))))

;; Updates parameter view widget when parameter was changed
;; via user input or YAML configuration loading
(define-slot (model-show parameter-changed) ()
  (declare (connected parameter-widget (parameter-changed)))
  (update-parameter-view parameter-view))

;; Process instantiation from pressing INSTANTIATE button
(define-slot (model-show instantiate) ((clicked bool))
  (declare (connected instantiator (clicked bool)))
  (format t "~&Instanitating...~%")
  (setf object (instantiate-object parameter))
  (setf (object-view-object object-view) object)
  (update-parameter-view object-view))

(defun show-model (parameter &optional synonyms)
  "Runs the model input defined by PARAMETER.
Uses SYNONYMS plist when YAML configuration is loaded"
  (with-main-window (window (make-instance 'model-show
                              :parameter parameter
                              :synonyms synonyms))
    (q+:set-window-title window "Parameter setter example")))

