(in-package :weblocks-utils)

(defwebapp debug-app
           :prefix "/debug-app"
           :init-user-session 'init-user-session
           :description "Debugging app for weblocks"
           :autostart nil                   ;; have to start the app manually
           :debug t)

#+this-causes-performance-troubles-and-errors(defparameter weblocks-util:*process-html-parts-p* 
  (lambda ()
    (and 
      (webapp-debug)
      (ignore-errors (weblocks:current-webapp))
      (not (typep (weblocks:current-webapp) 'debug-app)))))

(defun remove-session-action (&rest args &key id action)
  (hunchentoot:remove-session (hunchentoot::get-stored-session (parse-integer id)))
  (redirect (weblocks::weblocks-webapp-prefix (weblocks::current-webapp))))

(defstore 
  *list-store* 
  :custom 
  :classes 
  (list 
    'webapp-cls 
    (list
      :object-id (lambda (item item-data)
                   (weblocks::weblocks-webapp-name item-data))
      :slots (list 
               (list 
                 'title 
                 (lambda (item item-data)
                   (weblocks::weblocks-webapp-name item-data))))
      :find-all-objects (lambda ()
                          (remove-if #'null (loop for i in weblocks::*registered-webapps* collect (ignore-errors (weblocks::find-app i))))))
    'session-cls 
    (list 
      :object-id (lambda (item item-data)
                   (format nil "~A-~A"
                           (weblocks::weblocks-webapp-name (slot-value (getf item-data :app) 'weblocks-custom::data))
                           (hunchentoot:session-id (getf item-data :session))))
      :slots (list 
               (list 
                 'string-id 
                 (lambda (item item-data)
                   (format nil "~A-session-~A"
                           (weblocks::weblocks-webapp-name (slot-value (getf item-data :app) 'weblocks-custom::data))
                           (hunchentoot:session-id (getf item-data :session)))))
               (list 'hunchentoot-session-id
                    (lambda (item item-data)
                      (hunchentoot:session-id (getf item-data :session))))
               (list 
                 'webapp 
                 (lambda (item item-data)
                   (getf item-data :app)))
               (list 'session 
                     (lambda (item item-data)
                       (getf item-data :session))))
      :find-all-objects (lambda ()
                          (loop for i in (all-of 'webapp-cls :store *list-store*) append 
                                (with-webapp (slot-value i 'weblocks-custom::data)
                                  (loop for j in (weblocks::active-sessions) 
                                        collect (list :session j :app i))))))
    'session-data-cls
    (list 
      :object-id (lambda (item item-data)
                   (format nil "~A-~A-~A"
                           (weblocks::weblocks-webapp-name (slot-value (slot-value (getf item-data :session) 'webapp) 'weblocks-custom::data))
                           (hunchentoot:session-id (slot-value (getf item-data :session) 'session))
                           (getf item-data :key)))
                   
      :slots (list 
               (list 
                 'key 
                 (lambda (item item-data)
                   (getf item-data :key)))
               (list 
                 'value 
                 (lambda (item item-data)
                   (getf item-data :value)))
               (list 
                 'session 
                 (lambda (item item-data)
                   (getf item-data :session))))
      :find-all-objects (lambda ()
                          (loop for i in (all-of 'session-cls :store *list-store*)
                                append (with-webapp 
                                         (slot-value (slot-value i 'webapp) 'weblocks-custom::data)
                                         (let ((session-data (weblocks::webapp-session-hash (slot-value i 'session))))
                                           (when session-data 
                                             (loop for j from 1
                                                   for key being the hash-key of session-data 
                                                   for value being the hash-value of session-data 
                                                   collect
                                                   (list :key key :value value :session i))))))))))

(defun tree-title (obj)
  (typecase obj
    (webapp-cls (object-id obj))
    (session-cls (arnesi:escape-as-html (format nil " session #~A" (slot-value obj 'hunchentoot-session-id))))
    (session-data-cls (arnesi:escape-as-html (format nil " key: ~A, value: - ~A" (slot-value obj 'key) 
                                                     (truncate-string (prin1-to-string (slot-value obj 'value)) :length 20))))))
(defun memory-html-info ()
  (nl2br 
    (let ((*standard-output* (make-string-output-stream)))
      (room)
      (get-output-stream-string *standard-output*))))

(defun get-system-free-memory ()
  "Returns bytes of system free memory"
  (with-open-file (in "/proc/meminfo" :direction :input :if-does-not-exist nil)
    (when in 
      (loop for i = (read-line in nil) 
            while i do 
            (when (ppcre:scan "MemFree" i)
              (return-from 
                get-system-free-memory 
                (* 1024 
                   (parse-integer 
                     (ppcre:scan-to-strings "\\d+" i)
                     :junk-allowed t))))))))

(defun bytes-as-pretty-megabytes (bytes-number)
  (format nil "~A mb" (floor (/ bytes-number 1024 1024))))

(defun init-user-session (root)
  (make-action #'remove-session-action "remove-session")
  (let ((tree-grid)
        (view)
        (summary-widget))
    (setf view (defview nil (:type tree)
                        (weblocks-custom::data 
                          :present-as (tree-branches :straight-column-captions nil)
                          :reader (lambda (item)
                                    (tree-title item)))
                        (action-buttons 
                          :present-as html 
                          :reader (lambda (item)
                                    (format nil "~A ~A"
                                            (if (typep item 'session-data-cls)
                                              ""
                                              (funcall (weblocks-tree-widget::action-links-reader tree-grid view 
                                                                                                  :adding-allowed-p nil
                                                                                                  :editing-allowed-p nil 
                                                                                                  :deleting-allowed-p nil) item))
                                            (if (typep item 'session-cls)
                                              (weblocks::with-html-to-string 
                                                (:str "| ")
                                                (:a :href (add-get-param-to-url 
                                                            (make-action-url "remove-session")
                                                            "id"
                                                            (write-to-string (slot-value item 'hunchentoot-session-id)))
                                                 :onclick (format nil "initiateActionWithArgs(\"~A\", \"~A\", {id: '~A'}); return false;"
                                                                  "remove-session" (session-name-string-pair) (slot-value item 'hunchentoot-session-id))
                                                 "remove session"))

                                              "") 
                                            )))))
    (setf tree-grid (make-instance 'tree-widget 
                                   :expand-all-items-p nil
                                   :class-store *list-store* 
                                   :allow-add-p nil
                                   :view view
                                   :data-class 'webapp-cls))
    (setf summary-widget 
          (make-instance 'composite 
                         :widgets (list 
                                    (make-widget 
                                      (lambda (&rest args)
                                        (with-html 
                                          (:h1 "Application Summary")
                                          (:h2 "Memory")
                                          (str "System free memory ")
                                          (:b (str (bytes-as-pretty-megabytes (get-system-free-memory))))
                                          (:br)
                                          (str "Application used memory ")
                                          (:b (str (bytes-as-pretty-megabytes (sb-kernel:dynamic-usage))))
                                          (str ", ")
                                          (render-link (lambda (&rest args)
                                                         (do-information (memory-html-info)))
                                                       "more info"
                                                       )
                                          (str ",")
                                          (render-link 
                                            (lambda (&rest args)
                                              (if (find-package :tg)
                                                (progn 
                                                  (funcall (intern "GC" "TG") :full t)
                                                  (mark-dirty summary-widget)
                                                  (do-information "Success"))
                                                (do-information "trivial-garbage package not found")))
                                            "free unused memory")
                                          (:br)))))))
    (setf (widget-children root)
          (make-navigation 
            "toplevel"
            (list "Application summary" 
                  summary-widget nil)
            (list "Debug sessions" 
                  (make-instance 'composite 
                                 :widgets (list 
                                            (make-widget 
                                              (lambda (&rest args)
                                                (with-html 
                                                  (:h1 "Debug sessions")
                                                  (:span :style "font-family:monospace"
                                                   "[&#8505;] "
                                                   (:i "This is debug tree with applications (1st level), application sessions (2nd level) and application session values (3rd level)"))
                                                  (:br)
                                                  (:br))))
                                            tree-grid)) "debug-sessions")
            (list "Debug page" 
                  (eval 
                    `(make-navigation 
                       "debug page"
                       ,@(loop for i in weblocks::*active-webapps* 
                               unless (string= "debug-app" (weblocks::weblocks-webapp-name i))
                               collect `(list (weblocks::weblocks-webapp-name ,i)
                                              (make-instance 
                                                'composite 
                                                :widgets (list 
                                                           (make-widget 
                                                             (lambda (&rest args)
                                                               (with-html 
                                                                 (:h1 "Debug page"))
                                                               (let ((*html-parts-debug-app* ,(alexandria:make-keyword 
                                                                                                (string-upcase 
                                                                                                  (weblocks::weblocks-webapp-name i)))))
                                                                 (if (with-debugged-app-md5-hash 
                                                                       (ignore-errors (weblocks-util:get-html-parts-root-hash)))
                                                                   (progn 
                                                                     (with-html 
                                                                       (:span :style "font-family:monospace"
                                                                        "[&#8505;] "
                                                                        (:i "This is debug tree with rendered page parts from last page render."))
                                                                       (:div :id "eval-message" :style "display:none;font-family:monospace"
                                                                        (:br)
                                                                        "[&#8505;] "
                                                                        (:i "Evaluate "
                                                                         (:br)
                                                                         "&nbsp;&nbsp;"
                                                                         (:b (str (ps:ps 
                                                                                    ((ps:@ window open) 
                                                                                     (ps:LISP ,(format nil "/debug-app/debug-page/~A" (weblocks::webapp-name i)))))))
                                                                         (:br)
                                                                         (cl-who:fmt 
                                                                           "on your Weblocks-powered page of application ~A to receive debugging page mirror below (useful for visual debugging). This page then can be closed." ,(weblocks::webapp-name i))))
                                                                       (:br)
                                                                       (:br))
                                                                     (render-debug-page-tree))
                                                                   (with-html 
                                                                     (:span :style "font-family:monospace"
                                                                      "[&#8505;] "
                                                                      (:i "It seems "
                                                                       (:b (str (weblocks::weblocks-webapp-name ,i)))
                                                                       " is not started yet. Please open its url in browser")))))))))
                                              (attributize-name (weblocks::weblocks-webapp-name ,i))))))
                   "debug-page")))))

(defun start-debug-app (&rest args)
  (apply #'start-weblocks args)
  (start-webapp 'debug-app))

(defmethod tree-data ((obj tree-widget))
  (loop for i in (all-of 'webapp-cls :store *list-store*) 
        collect (list 
                  :item i 
                  :children (loop for j in 
                                  (find-by-values 'session-cls 
                                                  :webapp (cons i (lambda (item1 item2)
                                                                    (string= (object-id item1) (object-id item2))))
                                                  :store *list-store*) append 
                                  (let ((item (list :item j 
                                                    :children 
                                                    (loop for i in 
                                                          (find-by-values 
                                                            'session-data-cls 
                                                            :session (cons j (lambda (item1 item2)
                                                                               (string= (object-id item1) (object-id item2)))) 
                                                            :store *list-store*) 
                                                          collect (list :item i)))))
                                    (list item))))))
