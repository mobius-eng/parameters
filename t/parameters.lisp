(in-package :cl-user)

(defpackage parameters-test
  (:use :cl
        :parameters
        :pfr
        :prove))

(in-package :parameters-test)

;; NOTE: To run this test file, execute `(asdf:test-system :parameters)' in your Lisp.

(plan 7)

;; * Floating-point comparison
(defun approx= (x1 x2
                &key (abstol (sqrt double-float-epsilon))
                  (reltol (sqrt double-float-epsilon)))
  (let ((x (max (abs x1) (abs x2))))
    (<= (/ (abs (- x1 x2))
           (+ (* reltol x) abstol))
        1d0)))


;; * Single parameter

(defun make-single-parameter ()
  "Constructs a simple parameter"
  (parameter
   :name "mass"
   :value 100d0
   :units "g"
   :constructor (lambda (x) (/ x 1000d0))
   :description "Mass in g[ramms]. Instantiates in kg"))

(subtest "Single parameter tests"
  (format t "Single parameter tests")
  ;; ** Test parameter construction
  (let ((mass (make-single-parameter)))
    (ok (string= (parameter-name mass) "mass")
        "Name of single parameter is set")
    (ok (eq (parameter-id mass) :mass)
        "Id of the single parameter is deduced correctly")
    (is (parameter-parent mass) nil
        "Default parent is NIL")
    (ok (approx= (parameter-value mass) 100d0)
        "Returned value is raw")
    (ok (approx= (instantiate-object mass) 0.1d0)
        "Instantiated object is gone through the constructor"))
  ;; ** Test parameter modification
  (let ((mass (make-single-parameter)))
    (setf (parameter-value mass) 50d0)
    (ok (approx= (parameter-value mass) 50d0)
        "The value of the parameter is setf-ed")
    (ok (approx= (instantiate-object mass) 0.05d0)
        "Instantiation picked up a new value")))

;; * Perturbed parameter

(defun make-perturbed-parameter ()
  (parameter
   :id :perturbed-mass
   :name "mass"
   :value 100d0
   :units "g"
   :constructor (lambda (x) (/ x 1000d0))
   :perturbation 0.2d0
   :description "Mass in g[ramms]. Instantiates in kg"))

(subtest "Test conversion of single parameter to perturbed parameter"
  (let ((mass (perturb-parameter! (make-single-parameter))))
    (is-type mass 'perturbed-parameter
             "Correct type")
    (setf (perturbed-parameter-perturbation mass) 0.1d0)
    (let* ((average-instance 0.1d0)
           (result (loop repeat 100
                      sum (abs (- (instantiate-object mass) average-instance)))))
      (ok (> result 0d0)
          "Instances are off average value"))))

(subtest "Test perturbed parameter"
  (format t "Test perturbed parameter")
  (let ((mass (make-perturbed-parameter)))
    (ok (approx= (parameter-value mass) 100d0)
        "Value is unperturbed and raw")
    (let ((instance (instantiate-object mass)))
      (ok (< instance 0.12d0)
          "Instance is within upper boundary")
      (ok (> instance 0.08d0)
          "Instance is within lower boundary"))
    (let ((average-instance (/ (loop for i from 1 upto 100000
                                  sum (instantiate-object mass))
                               100000d0)))
      (ok (approx= average-instance 0.1d0 :abstol 1d-4 :reltol 1d-3)
          "Average value of instance of perturbed parameter is transformed value")))
  (let ((mass (make-perturbed-parameter)))
    (setf (parameter-constructor mass) (lambda (x) (* x 1000d0)))
    (let ((instance (instantiate-object mass)))
      (ok (< instance 1.2d5)
          "Instance with new constructor is within upper boundary")
      (ok (> instance 0.8d5)
          "Instance with new constructor is within lower boundary"))
    (let ((accumulative-perturbation
           (loop for i from 1 upto 10000
              sum (abs (- (instantiate-object mass) 1d5)))))
      (ok (>= accumulative-perturbation (* 0.25d0 0.2d0 1d5 10000))
          "Accumulated error is big enough"))))

;; * Parameter options
(defun make-parameter-options ()
  (parameter
   :name "Alternative temperatures"
   :id :temperature
   :options
   (list
    (parameter
     :name "T(C)"
     :id :celcius
     :value 25d0
     :units "C"
     :constructor (lambda (x) (+ x 273.15d0)))
    (parameter
     :name "T(K)"
     :id :kelvin
     :value 300d0)
    (parameter
     :name "T(F)"
     :id :fahrenheit
     :value 68d0
     :constructor (lambda (x) (+ (/ (- x 32d0) 1.8d0) 273.15d0))))
   :constructor (lambda (x) (cons x "Kelvin"))))


(subtest "Test parameter options"
  (format t "Test parameter options")
  (let ((temperature (make-parameter-options)))
    (is (parameter-options-selection temperature) 0
        "Default options selection is 0")
    (ok (= (parameter-value temperature) 25d0)
        "PARAMETER-VALUE returns the PARAMETER-VALUE of selectio")
    (setf (parameter-options-selection temperature) 2)
    (ok (= (parameter-value temperature) 68d0)
        "Value changed when selection is changed")
    (setf (parameter-value temperature) 100d0)
    (ok (= (parameter-value temperature) 100d0)
        "PARAMETER-VALUE returns newly set value")
    (ok (= (parameter-value (aref (parameter-options-options temperature)
                                  (parameter-options-selection temperature)))
           100d0)
        "The value of the selection has changed indeed"))
  (let ((temperature (make-parameter-options)))
    (setf (parameter-options-selection temperature) 0)
    (let ((result (instantiate-object temperature)))
      (ok (consp result) "Options constructor works")
      (ok (approx= (car result) 298.15d0)))
    (setf (parameter-options-selection temperature) 1)
    (let ((result (instantiate-object temperature)))
      (ok (approx= (car result) 300d0)))
    (setf (parameter-options-selection temperature) 2)
    (let ((result (instantiate-object temperature)))
      (ok (approx= (car result) 293.15d0)))))

(subtest "PFR Phospine: Arrhenius law (Simple Parameter Container)"
  (format t "PFR Phospine: Arrhenius law (Simple Parameter Container)")
  (let ((arrhenius-law (make-phosphine-default-arrhenius-law))
        (expected-result '(:reference-rate-constant 10d0
                           :activation-energy 160d0
                           :reference-temperature 649d0)))
    (ok (= (length (parameter-value arrhenius-law)) (length expected-result))
        "PARAMETER-VALUE of the container: expected length of the plist")
    (ok (loop for (id value) on (parameter-value arrhenius-law) by #'cddr
           unless (approx= value (getf expected-result id :not-a-number))
           return nil
           finally (return t))
        "PARAMETER-VALUE: all the values are preserved")
    (is-type (instantiate-object arrhenius-law)
             'arrhenius-law
             "Instance of PARAMETER-CONTAINER is of expected type")))

;; Container must instantiate objects recursively and the parameters
;; must be properly adjusted
(subtest "PFR Phospine: Full (Complex Parameter Container)"
  (format t "PFR Phospine: Full (Complex Parameter Container)")
  (let ((pfr (make-default-pfr-phosphine)))
    ;; Checks if it propogates and converts
    (setf (parameter-value (parameter-ref pfr :inlet-flow-rate))
          50d0)
    (setf (perturbed-parameter-perturbation (parameter-ref pfr :inlet-flow-rate))
          0d0)
    (let ((result (instantiate-object pfr)))
      (is-type result 'PFR-phosphine
               "Instance of PARAMETER-CONTAINER is of expected type")
      (is-type (phosphine-decomposition-rate-coefficient
                result)
               'arrhenius-law
               "Subobject of the instance is of the right type")
      (ok (approx= (inlet-flow-rate result) (/ 50d0 3600d0))
          "Changed value is propogated down to subparameter"))))

(subtest "PFR: perturbed parameters"
  (let ((pfr (make-default-pfr-phosphine)))
    ))


;; Finally, this test combines the test on parameters and the model:
;; compare the result with the result from Levenspiel, 1999; Example 5.5:
(subtest "Compare result with Levenspiel's example"
  (format t "Compare result with Levenspiel's example")
  (let ((pfr-parameters (make-default-pfr-phosphine)))
    (setf (perturbed-parameter-perturbation
           (parameter-ref pfr-parameters :inlet-flow-rate))
          0d0)
    (let* ((pfr-model (instantiate-object pfr-parameters))
           (volume (pfr-phosphine-volume pfr-model))
           (levenspiel-result 0.148d0))
      (ok (approx= levenspiel-result volume :abstol 1d-3 :reltol 1d-3)
          "Volume of the reactor matches found in the literature"))))

(finalize)

;; To run the tests:
;; #+BEGIN_QUOTE
;; (asdf:test-system :parameters)
;; #+END_QUOTE

;; ** To run the interface :comment:
;; This has been re-worked. Might not work.
;; *** TODO: Check if this is still valid after cleanup
;; To run the interface part, execute the following:
;; #+BEGIN_QUOTE
;; (parameters-interface:show-model (make-default-pfr-phosphine))
;; #+END_QUOTE



