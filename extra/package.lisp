(in-package cl-user)

(defpackage #:parameters-yaml
  (:use #:cl #:parameters #:cl-arrows)
  (:export
   #:*annotations*
   #:*keyword-value-keys*
   #:load-configuration
   #:load-parameter-configuration
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

