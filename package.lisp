;;;; package.lisp


(defpackage :parameters
  (:use #:cl)
  (:export #:parameter #:instantiate-object
           #:parameter-base #:parameter-base-parent #:parameter-base-constructor
           #:parameter-base-id #:parameter-base-name
           #:parameter-base-description
           #:parameter-value
           #:parameter-units #:parameter-value-transformer
           #:perturbed-parameter #:perturbed-parameter-perturbation
           #:parameter-options #:parameter-options-options
           #:parameter-options-selection
           #:parameter-container #:parameter-container-children
           #:parameter-ref
           #:single-parameter
           #:traverse-parameter))


(defpackage #:parameters-yaml
  (:use #:cl #:parameters #:cl-yy #:alexandria)
  (:export
   #:yaml-load-file
   #:convert-yaml
   #:remove-annotations
   #:unify-notation
   #:update-parameter-from-config))

(defpackage #:parameters-interface
  (:use #:cl+qt #:parameters #:parameters-yaml #:parse-number #:cl-arrows)
  (:export ;; Main interface parts: classes, methods, signals
   #:update-parameter-view
   #:make-ui
   #:add-to-grid
   #:parameter-changed
   #:parameter-ui
   #:parameter-container-ui
   #:parameter-options-ui)
  (:export  ;; Object view widget
   #:object-view
   #:object-view-object
   #:object-view-transformer)
  (:export  ;; Demo and runners
   #:parameter-ui-window
   #:parameter-demo
   #:model-show
   #:show-model
   #:exec-dialog-p
   #:make-button-in-context
   #:model-show-object
   #:model-show-parameter)
  (:export
   #:make-instantiate-button
   #:make-load-configuration-button))

