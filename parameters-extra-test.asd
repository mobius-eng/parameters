#|
  This file is a part of parameters project.
  Copyright (c) 2016 Alexey Cherkaev
|#

(in-package :cl-user)
(defpackage parameters-extra-test-asd
  (:use :cl :asdf))
(in-package :parameters-extra-test-asd)

(defsystem parameters-extra-test
  :author "Alexey Cherkaev"
  :license "BSD"
  :depends-on (:parameters-extra
               :prove)
  :components ((:module "t"
                :components
                ((:file "pfr")
                 (:test-file "parameters-yaml"
                             :depends-on ("pfr")))))
  :description "Test system for parameters"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
