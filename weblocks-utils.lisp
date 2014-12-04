(in-package :weblocks-utils)

(defvar *packages-used* nil)

(defun memory-poweredp (&key store)
  (let ((store (or store *default-store*)))
    (if (find-package 'weblocks-memory)
      (equal (type-of  store) (intern "MEMORY-STORE" "WEBLOCKS-MEMORY"))
      nil)))

(defun prevalence-poweredp (&key store)
  "returns T if *default-store* is prevalence store"
  (let ((store (or store *default-store*)))
    (if (find-package 'cl-prevalence)
      (equal (type-of store) (intern "GUARDED-PREVALENCE-SYSTEM" "CL-PREVALENCE"))
      nil)))

(defun clsql-poweredp (&key store)
  "returns T if *default-store* is clsql store"
  (let ((store (or store *default-store*)))
    (and 
      (find-package 'clsql)
      (find-package 'clsql-fluid-bt)
      (or 
        (equal (type-of store) (intern "FLUID-DATABASE" "CLSQL-FLUID-BT"))
        (typep store (intern "FLUID-DATABASE" "CLSQL-SYS"))))))

(defun find-by-in-sql-store (class fun &key order-by range (store *default-store*))
  (if (not range)
    (remove-if-not 
      fun 
      (find-persistent-objects 
        store class 
        :order-by order-by 
        :range range))
    (let* ((result)
           (needed-items-count (funcall (find-symbol "RANGE-TO-LIMIT" "WEBLOCKS-CLSQL") range))
           (new-range (cons 0 needed-items-count))
           (items-found 0))
      (loop do 
            (let ((items 
                    (find-persistent-objects 
                      store class 
                      :order-by order-by 
                      :range new-range)))
              (loop for item in items 
                    if 
                    (and 
                      (or (< (length result) needed-items-count) (return-from find-by-in-sql-store result))
                      (funcall fun item))
                    do 
                    (if (< items-found (car range))
                      (incf items-found)
                      (push item result)))
              (format t "Range ~A ~A ~A~%" new-range range items-found)
              (incf (car new-range) needed-items-count)
              (incf (cdr new-range) needed-items-count)))
      result)))

; XXX Not tested
(defun count-by-in-sql-store (class fun &key order-by range store)
  (if (not range)
    (remove-if-not 
      fun 
      (find-persistent-objects 
        store class 
        :order-by order-by 
        :range range))
    (let* ((result-count 0)
           (needed-items-count (funcall (find-symbol "RANGE-TO-LIMIT" "WEBLOCKS-CLSQL") range))
           (new-range (cons 0 needed-items-count))
           (items-found 0))
      (loop do 
            (let ((items 
                    (find-persistent-objects 
                      store class 
                      :order-by order-by 
                      :range new-range)))
              (loop for item in items 
                    if (funcall fun item)
                    do 
                    (if (< items-found (car range))
                      (incf items-found)
                      (incf result-count)))
              (format t "Range ~A ~A ~A~%" new-range range items-found)
              (incf (car new-range) needed-items-count)
              (incf (cdr new-range) needed-items-count)))
      result)))

(defun find-by (class fun &key order-by range (store *default-store*))
  "Takes as arguments class and predicate and filters all data by predicate. For clsql store predicate also used though it is very slow.
   Also accepts :order-by and :range parameters which are equal to 'find-persistent-objects' ones and :store parameter which is equal to 'find-persistent-objects' first parameter."
    (cond 
      ((clsql-poweredp :store store)
       (if fun 
         (find-by-in-sql-store 
           class fun 
           :order-by order-by 
           :range range 
           :store store)
         (find-persistent-objects 
           store class 
           :order-by order-by 
           :range range)))
      (t (find-persistent-objects store class 
                                  :filter fun
                                  :order-by order-by 
                                  :range range))))

; TODO count-by-in-sql-store not used yet, so should not work with mysql
(defun count-by (class fun &key store &allow-other-keys)
  "Similar to find-by but returns count of items instead of items list."
  (let ((store (or store *default-store*)))
    (count-persistent-objects store class :filter fun)))

(defun all-of (cls &key store order-by range)
  "Simple wrapper around 'find-persistent-objects', returns all elements of persistent class, useful when debugging."
  (find-persistent-objects (or store *default-store*) cls :order-by order-by :range range))

(defun count-of (cls &key store)
  (count-persistent-objects (or store *default-store*) cls))

(defun count-by-values (&rest args)
  (length (apply #'find-by-values args)))

(defun first-of (cls &key order-by range (store *default-store*))
  "Returns first element of persistent class."
  (first (all-of cls :order-by order-by :range range :store store)))

(defun first-by (class fun &key order-by range)
  "Similar to 'find-by' but returns only first element of a list."
  (first (find-by class fun :order-by order-by :range range)))

(defun find-by-values (class &rest args &key (test #'equal) order-by range (store *default-store*) &allow-other-keys)
  "Returns items of specified class. Filters passed as key arguments (key is slot name, value is value compared). 
   :test parameter is used to set default predicate for filters. You can also use (cons <filter-value <predicate>) instead of <filter-value> to override predicate for specific key."
  (setf args (alexandria:remove-from-plist args :order-by :range :store))
  (let ((filter-data 
          (loop for key in args by #'cddr 
                collect
                (let ((value (getf args key))
                      (test-fun test)
                      (slot (intern (string  key) 
                                    (package-name (symbol-package class)))))

                  (when (and (consp value) (functionp (cdr value)))
                    (setf test-fun (cdr value))
                    (setf value (car value)))
                  (pushnew (list args *package*) *packages-used*)
                  (list test-fun slot value)))))

    (flet ((filter-by-values (object)
             (loop for (test-fun slot value) in filter-data do 
                   (unless (funcall test-fun value 
                                    (if (equal slot 'id)
                                      (object-id object)
                                      (when (slot-boundp object slot)
                                        (slot-value object slot))))
                     (return-from filter-by-values nil)))
             t))

      (if (clsql-poweredp :store store)
        (find-by-in-sql-store class #'filter-by-values :order-by order-by :range range :store store)
        (find-persistent-objects store class :filter #'filter-by-values :order-by order-by :range range)))))

(defun first-by-values (&rest args)
  "Similar to 'find-by-values' but returns only first item"
  (first (apply #'find-by-values args)))

(defun delete-all (model &key (store *default-store*))
  "Deletes all items of specified class, useful for debugging."
  (loop for i in (all-of model :store store) do 
        (delete-persistent-object store i)))

(defun delete-one (obj &key (store *default-store* ))
  "Wrapper around 'find-persistent-objects' deletes specific object from *default-store*"
  (delete-persistent-object store obj))

(defmacro with-object-cache ((obj key) &body body)
  `(with-cache ((object-cache-key ,obj ,key) :store *default-cache-store*)
     ,@body))

(defun object-cache-key (obj key)
  (format nil "~A-~A-~A" (type-of obj) (object-id obj) key))

(defun prepend-webapp-path (value &optional (app (current-webapp)))
  "Prepends app prefix to url. Leading slashes in value will be automatically inserted when needed."
  (if (ppcre:scan "//" value)
    value
    (format nil "~A/~A" 
            (string-right-trim "/" (weblocks::weblocks-webapp-prefix app))
            (string-left-trim "/" value))))
