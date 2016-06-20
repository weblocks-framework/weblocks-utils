(in-package :weblocks-utils)

(defun object->simple-plist (object &rest filters)
  "Displays class values in readable way for debugging purposes. Handles not bound slots."
  (loop for i in (c2mop:class-direct-slots (find-class (class-name  (class-of object)))) append 
        (let* ((slot (intern (string (c2mop:slot-definition-name i)) "KEYWORD"))
               (value (if (slot-boundp object (c2mop:slot-definition-name i))
                        (slot-value object (c2mop:slot-definition-name i))
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

(defun as-string (integer-or-string)
  (format nil "~A" integer-or-string))

(defun implode (glue-or-pieces &optional (pieces nil pieces-given-p))
  (unless pieces-given-p 
    (return-from implode (implode "" glue-or-pieces)))
 
  (format nil "~{~A~}"
          (cdr (loop for i in pieces append 
                     (list glue-or-pieces i)))))
 
(setf (fdefinition 'join) (fdefinition 'implode))

(defun describe-set (objects)
  "Used to print list of objects into table"
  (flet ((text-before-newline (text)
           (let ((pos (position #\Newline text)))
             (if pos 
               (subseq text 0 pos)
               text))))
    (let* ((cls (class-of (first objects)))
           (slots (weblocks::class-visible-slots (class-of (first objects))))
           (length 15)
           (row-length (+ (* (length slots) length) (* (1- (length slots)) 3))))
      (flet ((output-row (row)
               (join " | "
                     (loop for i in row
                           collect  (format nil (format nil "~~~AA" length) (weblocks-utils::truncate-string (text-before-newline i) :length length))))))

        (format t "~A~%" (output-row (loop for i in slots
                                           collect (string (c2mop:slot-definition-name i)))))
        (loop for i from 0 to row-length do
              (format t "-"))
        (format t "~%")
        (loop for i in objects do 
              (format t "~A~%" 
                (output-row (loop for j in slots 
                                  collect (as-string 
                                            (if (slot-boundp i (c2mop:slot-definition-name j)) 
                                              (slot-value i (c2mop:slot-definition-name j))
                                              "<Unbound slot>"))))))))))
