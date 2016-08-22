;;;; parametes.asd

(asdf:defsystem #:parameters
  :description "Generic parameter description"
  :author "Alexey Cherkaev (mobius-eng)"
  :license "MIT"
  :depends-on (#:parse-number
               #:cl-yaclyaml
               #:closer-mop
               #:qtools #:qtcore #:qtgui)
  :serial t
  :components ((:file "package")
               (:file "parameters")
               (:file "yaml")
               (:file "interface")))

