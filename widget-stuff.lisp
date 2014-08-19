(in-package :weblocks-utils)

(defun multiple-related-records-field (field record title-function &optional body)
  "Similar to related-record-field but allows to connect multiple records"
  `(,field 
     ,@body
     :reader (lambda (item)
               (mapcar #'write-to-string (mapcar #'object-id (slot-value item ',field))))
     :writer (lambda (vals item)
               (setf (slot-value item ',field)
                     (remove-if #'null 
                                (mapcar (lambda (value) 
                                          (first-by-values ',record :id (parse-integer (string value))))
                                        vals 
                                        ))))
     :present-as (checkboxes 
                   :choices (lambda (item)
                              (loop for i in (all-of ',record) 
                                    collect (cons (funcall ,title-function i) (object-id i)))))
     :parse-as checkboxes))

; v5
(defun related-record-field (record title-function &optional body &key list-records-func (field-name record))
  "Makes it easy to connect two records, for example if we have catalog-item which has relation to manager following code will create view which has select with list of all managers, we can use it in catalog-item form

   (eval `(defview nil (:type form :inherit-from '(:scaffold catalog-item))
                   ,(related-record-field 'manager #'manager-name)))"
  `(,field-name 
     ,@body
     :writer (lambda (value item)
               (setf (slot-value item ',field-name)
                     (when value
                       (first-by-values ',record :id (parse-integer value)))))
     :present-as (dropdown 
                   :choices 
                   (lambda (item)
                     (loop for i in ,(if list-records-func 
                                       `(funcall ,list-records-func item) 
                                       `(all-of ',record)) 
                           collect (cons (funcall ,title-function i) (object-id i)))))))

(defun make-form (&key data-object on-success on-cancel fields (buttons '((:submit . "Сохранить"))) (persistp nil) (caption "") (answerp nil))
  (make-quickform
    (eval `(defview nil (:type form :caption ,caption :persistp ,persistp :buttons (quote ,buttons) 
                         ;:inherit-from ,(when data-object 
                         ;`(quote (:scaffold ,(type-of data-object))))

                         )
                    ,@fields))
    :data data-object
    :on-success on-success 
    :on-cancel on-cancel
    :answerp answerp))
