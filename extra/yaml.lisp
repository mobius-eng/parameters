(in-package parameters-yaml)

;; * Utils
;; ** Operations on PLISTs and nested PLISTs
(defun filter-plist (predicate plist)
  "Apply PREDICATE to each pair (KEY VALUE) in PLIST. If
it returns NIL, skip this pair, otherwise -
add them to a newly formed PLIST"
  (loop for (key value) on plist by #'cddr
     if (funcall predicate key value)
     append (list key value)
     end))

(defun filter-map-plist (predicate mapping plist)
  "Applying MAPPING function to each pair of (KEY VALUE) from
PLIST satisfying PREDICATE"
  (loop for (key value) on plist by #'cddr
     if (funcall predicate key value)
     append (funcall mapping key value)
     end))

(defun filter-ptree (predicate ptree)
  "Recursively filters PTREE"
  (filter-map-plist predicate
                    (lambda (key value)
                      (list key
                            (if (consp value)
                                (filter-ptree predicate value)
                                value)))
                    ptree))

;; * Generic protocol: convert YAML to simpler format
;; ** Definition
(defgeneric convert-yaml (object &key keyword-ids)
  (:documentation
   "Convert YAML document representation into nested plists"))

;; ** Defualt: leave an object as is
(defmethod convert-yaml (object &key (keyword-ids t))
  (declare (ignore keyword-ids))
  object)

;; ** Hash-tables to plists
(defmethod convert-yaml ((object hash-table) &key (keyword-ids t))
  (loop for key being the hash-keys of object using (hash-value value)
     appending (list
                (if keyword-ids (make-keyword-id key) key)
                (convert-yaml value :keyword-ids keyword-ids))))

;; ** SINGLE-FLOAT to DOUBLE-FLOAT
;; This is not ideal, better change how the number is read
;; but this change would be very deep in CL-YY package
;; Alternatively, at the moment of reading YAML,
;; *READ-DEFAULT-FLOAT-FORMAT* must be set to 'DOUBLE-FLOAT
(defmethod convert-yaml ((object single-float) &key (keyword-ids t))
  (declare (ignore keyword-ids))
  (coerce object 'double-float))

;; ** VECTOR: convert every element of vector (functor-map)
(defmethod convert-yaml ((object vector) &key (keyword-ids t))
  (apply
   #'vector
   (loop for item across object
      collecting (convert-yaml item :keyword-ids keyword-ids))))

;; ** Treat strings specially (compared to other vectors): don't do anything
(defmethod convert-yaml ((object string) &key (keyword-ids t))
  (declare (ignore keyword-ids))
  object)

;; ** LIST: convert every item (functor-map)
(defmethod convert-yaml ((object list) &key (keyword-ids t))
  (loop for item in object
     collecting (convert-yaml item :keyword-ids keyword-ids)))

;; * Remove annotations from YAML
(defvar *annotations* (list :note :comment)
  "Annotations and comments in YAML file that can be omitted")

(defvar *keyword-value-keys* (list :type)
  "Keys in the YAML file values of which must be keyworded")

(defun remove-annotations (yaml-ptree &optional (annotations *annotations*))
  "Removes keys :NOTE, :COMMENT, etc. For the full list
of annotating keys see *ANNOTATIONS*"
  (filter-ptree (lambda (key value)
                  (declare (ignore value))
                  (not (member key annotations :test #'eq)))
                yaml-ptree))

;; * Unifies notation
;; Convenient for human input to decrease verbosity
(defun unify-notation (configuration synonyms
                       &optional (keyword-value-keys *keyword-value-keys*))
  "Recursively substitutes values from plist SYNONYMS in place of keys in nested
plist CONFIGURATION if their keys match. This mechanism allows CONFIGURATION have
shorter or alternative names.
If the YAML key is part of KEYWORD-VALUE-KEYS, its value is assumed to be a keyword:
the string value is converted into the keyword and then matched against synonyms.
By default, it contains only one key :TYPE"
  (cond ((null synonyms) configuration)
        ((symbolp configuration) (getf synonyms configuration configuration))
        ((stringp configuration) configuration)
        ((vectorp configuration)
         (map 'vector
              (lambda (item)
                (unify-notation item synonyms keyword-value-keys))
              configuration))
        ((not (consp configuration)) configuration)
        (t (loop for (key value) on configuration by #'cddr
              if (find key keyword-value-keys :test #'eq)
              append (list (getf synonyms key key)
                           (unify-notation (make-keyword-id value) synonyms))
              else
              append (list (getf synonyms key key)
                           (unify-notation value synonyms))
              end))))

(defun load-configuration (filename &key synonyms (keyword-value-keys *keyword-value-keys*))
  "Load configuration from FILENAME.
Use SYNONYMS to unify key names.
Use KEYWORD-VALUE-KEYS to convert the value of these keys to keywords"
  (let ((*read-default-float-format* 'double-float))
    (let ((yaml-raw (cl-yy:yaml-load-file filename)))
      (-> yaml-raw
          (convert-yaml)
          (remove-annotations)
          (unify-notation synonyms keyword-value-keys)))))

(defun load-parameter-configuration (parameter filename
                                     &key synonyms (keyword-value-keys
                                                    *keyword-value-keys*))
  "Load configuration from YAML FILENAME to PARAMETER.
Use SYNONYMS to unify key names.
Use KEYWORD-VALUE-KEYS to convert the value of these keys to keywords"
  (let ((configuration (load-configuration
                        filename
                        :synonyms synonyms
                        :keyword-value-keys keyword-value-keys)))
    (update-parameter-from-config parameter configuration)))

;; * Load configuration into parameter
(defgeneric update-parameter-from-config(parameter configuration)
  (:documentation
   "Updates existing PARAMETER object (e.g. PARAMETER, PARAMETER-CONTAINER or
PARAMETER-OPTIONS) with CONFIGURATION (nested plist)
For options CONFIGURATION only describes the selected option. The selection is
deduced based on :TYPE key which specifies selected PARAMETER-ID"))

(defmethod update-parameter-from-config ((parameter single-parameter) configuration)
  "Single parameter: only updates the value.
If :PERTURBATION is present - upgrades the parameter to PERTURBED-PARAMETER
The rest is ignored"
  (when configuration
    (if (getf configuration :perturbation)
        (progn
          (change-class parameter 'perturbed-parameter :perturbation 0d0)
          (update-parameter-from-config parameter configuration))
        (setf (parameter-value parameter) (getf configuration :value)))))

(defmethod update-parameter-from-config ((parameter perturbed-parameter) configuration)
  "Perturbed parameter: updates value and perturbation"
  (when configuration
    (let ((newvalue (getf configuration :value))
          (newperturbation (getf configuration :perturbation)))
      (when newvalue
        (setf (parameter-value parameter) newvalue))
      (when newperturbation
        (setf (perturbed-parameter-perturbation parameter)
              newperturbation))
      (unless (or newvalue newperturbation)
        (warn "Parameter ~A: no value or perturbation found" (parameter-id parameter))))))

(defmethod update-parameter-from-config ((parameter parameter-container) configuration)
  "Container: updates each subparameter (if present in CONFIGURATION)"
  (when configuration
    (loop for subparameter in (parameter-container-children parameter)
       do (update-parameter-from-config
           subparameter
           (getf configuration (parameter-id subparameter))))))

(defmethod update-parameter-from-config ((parameter parameter-options) configuration)
  "Options: only updates selected option. Resets the selection according to specified
type ('type' id in YAML)"
  (when configuration
    (let ((type (make-keyword-id (getf configuration :type))))
      (format t "~&OPTIONS: TYPE ~A~%" type)
      (loop for option across (parameter-options-options parameter)
         for index from 0
         when (eq (parameter-id option) type)
         do (progn
              (format t "~&Found type ~A (~A)" type (parameter-id option))
              (setf (parameter-options-selection parameter) index)
              (update-parameter-from-config option configuration)
              (return-from update-parameter-from-config))
         end))))
