#|
  This file is a part of parameters project.
  Copyright (c) 2016 Alexey Cherkaev
|#

(in-package :cl-user)
(defpackage parameters-test-asd
  (:use :cl :asdf))
(in-package :parameters-test-asd)

(defsystem parameters-test
  :author "Alexey Cherkaev"
  :license "BSD"
  :depends-on (:parameters
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "parameters"))))
  :description "Test system for parameters"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
