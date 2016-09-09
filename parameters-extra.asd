#|
  This file is a part of parameters project.
  Copyright (c) 2016 Alexey Cherkaev
|#

#|
  Author: Alexey Cherkaev
|#

(in-package :cl-user)

(defpackage parameters-extra-asd
  (:use :cl :asdf))

(in-package :parameters-extra-asd)

(defsystem parameters-extra
  :version "0.1"
  :author "Alexey Cherkaev"
  :license "BSD"
  :depends-on (#:parameters
               #:cl-arrows #:keyword-dispatch
               #:cl-yaclyaml
               #:parse-number
               #:qtools #:qtcore #:qtgui)
  :components ((:module
                "extra"
                :serial t
                :components
                ((:file "package")
                 (:file "yaml")
                 (:file "interface"))))
  :description "Extra functionality to PARAMETERS: YAML configuration"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.org"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op parameters-extra-test))))
