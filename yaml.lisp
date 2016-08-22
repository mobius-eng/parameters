(in-package parameters-yaml)

;; * Utils
;; ** Convert string to keywords
(defvar *illigal-characters*
  (list #\space #\, #\tab #\. #\& #\_))

;; TODO: unify with MAKE-KEYWORD-ID
(defun produce-keyword (string)
  "Produce keyword out of the string by removing `illigal'
characters (e.g, `,', `.' ` ', etc.)"
  (if (symbolp string)
      string
      (let ((string (string-upcase string)))
        (intern
         (let ((output-replacement t))
           (with-output-to-string (str)
             (loop for char across string
                if (member char *illigal-characters* :test #'eql)
                when output-replacement
                do (progn
                     (setf output-replacement nil)
                     (princ #\- str))
                end
                else
                do (progn
                     (setf output-replacement t)
                     (princ char str)))))
         'keyword))))

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
                (if keyword-ids (produce-keyword key) key)
                (convert-yaml value :keyword-ids keyword-ids))))

;; ** SINGLE-FLOAT to DOUBLE-FLOAT
;; This is not ideal, better change how the number is read
;; but this change would be very deep in CL-YY package
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

(defun remove-annotations (yaml-ptree)
  "Removes keys :NOTE, :COMMENT, etc. For the full list
of annotating keys see *ANNOTATIONS*"
  (filter-ptree (lambda (key value)
                  (declare (ignore value))
                  (not (member key *annotations* :test #'eq)))
                yaml-ptree))

;; * Unifies notation
;; Convenient for human input to decrease verbosity
(defun unify-notation (configuration synonyms)
  "Recursively substitutes values from plist SYNONYMS in place of keys in nested
plist CONFIGURATION if their keys match. This mechanism allows CONFIGURATION have
shorter or alternative names.
NB. Key :TYPE is treated specially: the value is converted to symbol and then
matched against SYNONYMS"
  (cond ((null synonyms) configuration)
        ((symbolp configuration) (getf synonyms configuration configuration))
        ((not (consp configuration)) configuration)
        (t (loop for (key value) on configuration by #'cddr
              if (eq key :type)
              append (list :type
                           (unify-notation (produce-keyword value) synonyms))
              else
              append (list (getf synonyms key key)
                           (unify-notation value synonyms))
              end))))

;; * Load configuration into parameter
(defgeneric update-parameter-from-config(parameter configuration)
  (:documentation
   "Updates existing PARAMETER object (e.g. PARAMETER, PARAMETER-CONTAINER or
PARAMETER-OPTIONS) with CONFIGURATION (nested plist)
For options CONFIGURATION only describes the selected option. The selection is
deduced based on :TYPE key"))

(defmethod update-parameter-from-config ((parameter parameter) configuration)
  "Single parameter: only updates the value. The rest is ignored"
  (when configuration
    (setf (parameter-value parameter) configuration)))


(defmethod update-parameter-from-config ((parameter parameter-container) configuration)
  "Container: updates each subparameter (if present in CONFIGURATION)"
  (when configuration
    (loop for subparameter in (parameter-container-children parameter)
       do (update-parameter-from-config
           subparameter
           (getf configuration (parameter-base-id subparameter))))))

(defmethod update-parameter-from-config ((parameter parameter-options) configuration)
  "Options: only updates selected option. Resets the selection according to specified
type ('type' id in YAML)"
  (when configuration
    (let ((type (produce-keyword (getf configuration :type))))
      (format t "~&OPTIONS: TYPE ~A~%" type)
      (loop for option across (parameter-options-options parameter)
         for index from 0
         when (eq (parameter-base-id option) type)
         do (progn
              (format t "~&Found type ~A (~A)" type (parameter-base-id option))
              (setf (parameter-options-selection parameter) index)
              (update-parameter-from-config option configuration)
              (return-from update-parameter-from-config))
         end))))
