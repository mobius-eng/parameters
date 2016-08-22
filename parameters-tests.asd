;;;; parametes.asd

(asdf:defsystem #:parameters-tests
  :description "Parameters framework tests"
  :author "Alexey Cherkaev (mobius-eng)"
  :license "BSD"
  :depends-on (#:parameters #:fiveam)
  :serial t
  :components ((:module
                "tests"
                (:file "package")
                (:file "parameters-tests"))))

