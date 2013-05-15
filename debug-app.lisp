(in-package :weblocks-utils)

(defwebapp debug-app
           :prefix "/debug-app"
           :init-user-session 'init-user-session
           :description "Debugging app for weblocks"
           :autostart nil                   ;; have to start the app manually
           :debug t)

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
                                     "id - ~A" 
                                     (hunchentoot:session-id i) )
                               (:h3 "session data")
                               (:ul
                                 (let ((session-data (weblocks::webapp-session-hash i)))
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
                                             (cl-who:esc (prin1-to-string value)))))))))))))))))))

(defun init-user-session (root)
  (setf (widget-children root)
        (list #'render-debug-output)))

(defun start-debug-app (&rest args)
  (apply #'start-weblocks args)
  (start-webapp 'debug-app))
