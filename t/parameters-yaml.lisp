(in-package :cl-user)
(defpackage parameters-yaml-test
  (:use :cl
        :parameters
        :parameters-yaml
        :pfr
        :prove))

(in-package :parameters-yaml-test)

(plan 2)

;; * Floating-point comparison
(defun approx= (x1 x2
                &key (abstol (sqrt double-float-epsilon))
                  (reltol (sqrt double-float-epsilon)))
  (let ((x (max (abs x1) (abs x2))))
    (<= (/ (abs (- x1 x2))
           (+ (* reltol x) abstol))
        1d0)))

;; ** Tests
(defconstant +configuration+
  '(:inlet-flow-rate (:value 50d0
                      :units "mol/h"
                      :perturbation 0.1d0)
    :final-conversion (:value 80d0 :units "%")
    :operating-temperature (:type :F :value 1300d0 :units "F")
    :rate-coefficient (:reference-rate-constant (:value 12d0 :units "1/h")
                       :reference-temperature (:value 700d0)))
  "YAML-style configuration")

(subtest "PFR YAML: update from configuration"
  (format t "~TPFR YAML: update from configuration")
  (let ((pfr (make-default-pfr-phosphine)))
    (update-parameter-from-config pfr +configuration+)
    (ok (approx=
         (parameter-value (parameter-ref pfr :inlet-flow-rate))
         50d0)
        "Child value of the container was loaded")
    (ok (approx=
         (perturbed-parameter-perturbation (parameter-ref pfr :inlet-flow-rate))
         0.1d0)
        "Child perturbation of the container was loaded")
    (let* ((operating-temperature (parameter-ref pfr :operating-temperature))
           (selected-temperature (elt (parameter-container-children
                                       operating-temperature)
                                      (parameter-options-selection
                                       operating-temperature))))
      (ok (string= (single-parameter-units selected-temperature) "F")
          "Child option: selection changed (units)")
      (ok (approx= (parameter-value (parameter-ref pfr :operating-temperature))
                   1300d0)
          "Child option: selection changed (value)"))
    (let ((rate-coefficient (parameter-ref pfr :rate-coefficient)))
      (ok (approx= (parameter-value (parameter-ref rate-coefficient
                                                   :reference-rate-constant))
                   12d0)
          "Deep child value was changed"))))

(defun ptree-in-p (ptree1 ptree2)
  "Checks if PTREE1 is included in PTREE2"
  (loop for (key value) on ptree1 by #'cddr
     do (progn
          (is key (find key ptree2)
              (format nil "Found ~A" key))
          (when (find key ptree2)
            (if (consp value)
                (ptree-in-p value (getf ptree2 key))
                (equalp value (getf ptree2 key)))))))

(subtest "PFR YAML: load configuration from file"
  (format t "~TPFR YAML: load configuration from file")
  (let* ((file-location (make-pathname
                         :defaults (merge-pathnames
                                    #P"t/"
                                    (asdf:system-source-directory :parameters))
                         :name "pfr"
                         :type "yaml"))
         (configuration (load-configuration file-location :synonyms *pfr-synonyms*)))
    (ptree-in-p configuration +configuration+)
    (ptree-in-p +configuration+ configuration)))

(finalize)

;; To run the tests:
;; #+BEGIN_QUOTE
;; (asdf:test-system :parameters-extra)
;; #+END_QUOTE



