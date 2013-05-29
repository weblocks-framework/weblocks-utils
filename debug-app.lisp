(in-package :weblocks-utils)

(defwebapp debug-app
           :prefix "/debug-app"
           :init-user-session 'init-user-session
           :description "Debugging app for weblocks"
           :autostart nil                   ;; have to start the app manually
           :debug t)

(defun remove-session-action (&rest args &key id action)
  (hunchentoot:remove-session (hunchentoot::get-stored-session (parse-integer id)))
  (redirect (weblocks::weblocks-webapp-prefix (weblocks::current-webapp))))

(defun render-debug-output (&rest args)
  (with-html 
    (:h1 "Webapps")
    (loop for i in weblocks::*registered-webapps* 
          do 
          (let ((webapp (ignore-errors (weblocks::find-app i))))
            (when webapp 
              (with-webapp webapp
                (cl-who:htm 
                  (:li 
                    (str (weblocks::weblocks-webapp-name webapp))
                    (:h2 :style "margin-left:20px;" "Sessions")
                    (:ul
                      (loop for i in (weblocks::active-sessions) do 
                            (cl-who:htm 
                              (:li (cl-who:fmt 
                                     "id - ~A &nbsp;" 
                                     (hunchentoot:session-id i))
                               (:a :href (add-get-param-to-url 
                                           (make-action-url "remove-session")
                                           "id"
                                           (write-to-string (hunchentoot:session-id i)))
                                :onclick (format nil "initiateActionWithArgs(\"~A\", \"~A\", {id: '~A'}); return false;"
                                                 "remove-session" (session-name-string-pair) (hunchentoot:session-id i))
                                "Remove session")
                               (:h3 "session data")
                               (:ul
                                 (let ((session-data (weblocks::webapp-session-hash i)))
                                   (when session-data 
                                     (loop for i from 1
                                           for key being the hash-key of session-data 
                                           for value being the hash-value of session-data 
                                           do 
                                           (cl-who:htm 
                                             (:li 
                                               (str i)
                                               (:br)
                                               (str key)
                                               (:br)
                                               (cl-who:esc (prin1-to-string value))))))))))))))))))))

(defun init-user-session (root)
  (make-action #'remove-session-action "remove-session")
  (setf (widget-children root)
        (list (make-widget #'render-debug-output))))

(defun start-debug-app (&rest args)
  (apply #'start-weblocks args)
  (start-webapp 'debug-app))
