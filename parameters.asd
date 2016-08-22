;;;; parametes.asd

(asdf:defsystem #:parameters
  :description "Generic parameter description"
  :author "Alexey Cherkaev (mobius-eng)"
  :license "BSD"
  :depends-on (#:parse-number
               #:cl-yaclyaml
               #:closer-mop
               #:cl-arrows
               #:qtools #:qtcore #:qtgui)
  :serial t
  :components ((:file "package")
               (:file "parameters")
               (:file "yaml")
               (:file "interface")))

