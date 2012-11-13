(in-package :weblocks-utils)
(defvar *packages-used* nil)

(defun prevalence-poweredp (&key store)
  (let ((store (or store *default-store*)))
    (if (find-package 'cl-prevalence)
      (eq (class-name  (class-of store)) (intern "GUARDED-PREVALENCE-SYSTEM" "CL-PREVALENCE"))
      nil)))

(defun clsql-poveredp (&key store)
  (let ((store (or store *default-store*)))
    (and 
      (find-package 'clsql)
      (find-package 'clsql-fluid-bt)
      (string= (string (class-name (class-of store))) (string (intern "FLUID-DATABASE" "CLSQL-FLUID-BT"))))))

(defun find-by-in-sql-store (class fun &key order-by range store)
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

(defun find-by (class fun &key order-by range store)
  (declare (special weblocks:*default-store*))
  (let ((store (or store weblocks:*default-store*)))
    (cond 
      ((prevalence-poweredp :store store)
       (find-persistent-objects store class 
                                :filter fun 
                                :order-by order-by 
                                :range range))
      ((clsql-poveredp :store store)
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
                                  :filter-fn 
                                  (lambda (item) (not (funcall fun item))) 
                                  :order-by order-by 
                                  :range range)))))

; TODO count-by-in-sql-store not used yet
(defun count-by (class fun &key store &allow-other-keys)
  (let ((store (or store *default-store*)))
    (if (prevalence-poweredp :store store)
      (count-persistent-objects store class 
                               :filter fun)
      (count-persistent-objects store class 
                               :filter-fn 
                               (lambda (item) (not (funcall fun item)))))))


(defun all-of (cls &key order-by range)
  (find-persistent-objects *default-store* cls :order-by order-by :range range))

(defun first-by (class fun &key order-by range)
  (first (find-by class fun :order-by order-by :range range)))

(defun find-by-values (class &rest args &key (test #'equal) &allow-other-keys)
  (flet ((filter-by-values (object)
           (loop for key in args by #'cddr do 
                 (let ((value (getf args key))
                       (test-fun test))
                   (when (and (consp value) (functionp (cdr value)))
                     (setf test-fun (cdr value))
                     (setf value (car value)))
                   (pushnew (list args *package*) *packages-used*)
                   (unless (funcall test value (slot-value object 
                                                           (intern (string  key) 
                                                                   (package-name (symbol-package (type-of object))))))
                     (return-from filter-by-values nil))))
           t))

    (find-persistent-objects *default-store* class :filter #'filter-by-values)))

(defun first-by-values (&rest args)
  (first (apply #'find-by-values args)))

(defun delete-all (model)
  (loop for i in (all-of model) do 
        (delete-persistent-object *default-store* i)))

(defun delete-one (obj)
  (delete-persistent-object *default-store* obj))

(defun object->simple-plist (object &rest filters)
  (loop for i in (sb-mop:class-direct-slots (find-class (class-name  (class-of object)))) append 
        (let* ((slot (intern (string (sb-mop:slot-definition-name i)) "KEYWORD"))
               (value (if (slot-boundp object (sb-mop:slot-definition-name i))
                        (slot-value object (sb-mop:slot-definition-name i))
                        "Unbound")))
          (list slot (if (getf filters slot) (funcall (getf filters slot) value) value)))))
