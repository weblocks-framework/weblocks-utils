(in-package :weblocks-utils)

(defun object->simple-plist (object &rest filters)
  "Displays class values in readable way for debugging purposes. Handles not bound slots."
  (loop for i in (sb-mop:class-direct-slots (find-class (class-name  (class-of object)))) append 
        (let* ((slot (intern (string (sb-mop:slot-definition-name i)) "KEYWORD"))
               (value (if (slot-boundp object (sb-mop:slot-definition-name i))
                        (slot-value object (sb-mop:slot-definition-name i))
                        "Unbound")))
          (list slot (if (getf filters slot) (funcall (getf filters slot) value) value)))))

(defun first-active-webapp ()
  "Returns first element from weblocks::*active-webapps* list"
  (first weblocks::*active-webapps*))

(defmacro maybe-with-first-active-webapp (&body body)
  "Bounds weblocks::*current-webapp* to first active webapp if it is not bound and executes code in this context"
  `(if (boundp 'weblocks::*current-webapp*)
     (progn ,@body) 
     (with-webapp (first-active-webapp)
       (progn ,@body))))
