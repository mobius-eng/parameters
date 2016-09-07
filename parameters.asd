#|
  This file is a part of parameters project.
  Copyright (c) 2016 Alexey Cherkaev
|#

#|
  Author: Alexey Cherkaev
|#

(in-package :cl-user)
(defpackage parameters-asd
  (:use :cl :asdf))
(in-package :parameters-asd)

(defsystem parameters
  :version "0.2"
  :author "Alexey Cherkaev"
  :license "BSD"
  :depends-on (#:closer-mop #:keyword-dispatch)
  :components ((:module
                "src"
                :serial t
                :components
                ((:file "parameters"))))
  :description "Input parameters library for complex models"
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
  :in-order-to ((test-op (test-op parameters-test))))
