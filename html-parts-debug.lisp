(in-package :weblocks-utils)

(defun truncate-string (s &key (length 300) (etc "...") (explicit t))
  "If string exceeds specified LENGTH truncates it to this LENGTH and adds additional text"
  (if (> (length s) length)
    (format nil "~A~A" (subseq s 0 (if explicit (- length (length etc)) length)) etc)
    s))

(defun nl2br (text)
  "Replaces string #\Newline symbols with html br elements"
  (cl-ppcre:regex-replace-all #\newline text "<br/>"))

(defun pretty-print-to-html (str)
  "Replaces spaces with non-breaking spaces - &nbsp; 
   Replaces #\Newline with html br tags"
  (nl2br 
    (ppcre:regex-replace-all " " str "&nbsp;")))

(defmacro with-debugged-app-md5-hash (&body body)
  `(let ((weblocks-util::*parts-md5-hash* (get-debugged-application-md5-hash))
         (weblocks-util::*parts-md5-context-hash* (get-debugged-application-md5-context-hash)))
     ,@body))

(defun print-tree (&key (indent 0) (children nil))
  "Recursively prints html parts tree"
  (flet ((html-newline-and-indent ()
           (with-html (:br))
           (loop for j from 0 to indent 
                 do
                 (with-html (str "&nbsp;")))))

    (loop for i in (reverse children) do 
          (unless (zerop indent)
            (html-newline-and-indent))
          (with-html 
            (:a :href (ps:ps-inline (show-element-rect (get-dom-by-hash (ps:LISP i))))
             (cl-who:esc (format nil "~A" (truncate-string (with-debugged-app-md5-hash (weblocks-util:get-html-part i)) :length 20))))
            (str " ")
            (let ((i-copy i))
              (render-link 
                (lambda (&rest args)
                  (declare (ignore args))
                  (mark-dirty (root-widget))
                  (do-page 
                    (make-widget 
                      (lambda/cc (&rest args)
                                 (declare (ignore args))
                                 (let ((back-action (lambda (&rest args)
                                                      (declare (ignore args))
                                                      (answer (first (widget-children (root-widget)))))))
                                   (with-html 
                                     (:h1 "Element context")
                                     (str (pretty-print-to-html 
                                            (arnesi:escape-as-html 
                                              (with-output-to-string (s)
                                                (pprint (with-debugged-app-md5-hash (weblocks-util:get-html-part-context i)) s)))))
                                     (:h1 "Element text")
                                     (render-link back-action "back")
                                     (:br)
                                     (str (pretty-print-to-html 
                                            (arnesi:escape-as-html 
                                              (or (cl-tidy:clean-up-and-indent-html-part 
                                                    (with-debugged-app-md5-hash (weblocks-util:get-html-part i))) ""))))
                                     (:br)
                                     (render-link back-action "back")))))))
                "info"))
            (let ((type (getf (with-debugged-app-md5-hash (weblocks-util:get-html-part-context i)) :type)))
              (case type 
                (:template 
                  (html-newline-and-indent)
                  (cl-who:htm 
                    "Weblocks template "
                    (:b 
                      (:i :style "text-align:right;"
                       (str (string-downcase (prin1-to-string (getf (with-debugged-app-md5-hash (weblocks-util:get-html-part-context i)) :template-name))))))))
                (:widget 
                  (html-newline-and-indent)
                  (cl-who:htm 
                    "Widget of type "
                    (:b 
                      (:i :style "text-align:right;"
                       (str (string-downcase (prin1-to-string (type-of (getf (with-debugged-app-md5-hash (weblocks-util:get-html-part-context i)) :widget)))))))
                    ", "
                    (:b 
                      (:i :style "text-align:right;"
                       (cl-who:esc (string-downcase (prin1-to-string (getf (with-debugged-app-md5-hash (weblocks-util:get-html-part-context i)) :widget)))))))))))
          (print-tree :indent (+ indent 2)
            :children (with-debugged-app-md5-hash (weblocks-util:get-html-part-children i))))))

(defvar *html-parts-debug-app* nil)

(defun get-debugged-application-md5-hash ()
  (declare (special *html-parts-debug-app*))
  (when *html-parts-debug-app* 
    (weblocks:with-webapp (weblocks::find-app *html-parts-debug-app*)
      (with-first-active-session 
        (webapp-session-value 
          'weblocks::parts-md5-hash 
          weblocks::*session*
          (weblocks::find-app *html-parts-debug-app*))))))

(defun get-debugged-application-md5-context-hash ()
  (when *html-parts-debug-app* 
    (weblocks:with-webapp (weblocks::find-app *html-parts-debug-app*)
      (with-first-active-session 
        (webapp-session-value 
          'weblocks::parts-md5-context-hash 
          weblocks::*session*
          (weblocks::find-app *html-parts-debug-app*))))))

(defun render-debug-page-ps ()
  "Scripts for debug page"
  (let ((md5-hash (get-debugged-application-md5-hash)))
    (send-script 
      (ps:ps 
        (setf document-data 
              (eval 
                (ps:LISP 
                  (format 
                    nil "(~A)"
                    (json:encode-json-to-string md5-hash)))))))
    (send-script 
      (ps:ps 
        (unless (ps:@ window opener)
          (setf (ps:@ ((ps:@ document get-element-by-id) "eval-message") style display) "block")
          (set-timeout 
            (lambda () 
              (ps:chain ($ "page-mirror-container") (hide))
              (setf (ps:chain ($ "page-tree") style width) "100%"))
            0))
        (setf parent-doc (ps:@ window opener document document-element))
        (setf j-query (ps:@ window opener j-query))
        (setf doc-root (ps:LISP (with-debugged-app-md5-hash (weblocks-util:get-html-parts-root-hash))))

        (defun get-dom-by-hash (hash)
          (when (string= "undefined" (ps:typeof doc-root))
            (alert "Visual page debugging is not available yet. See instructions above for page debugging")
            (return-from get-dom-by-hash))

          (if (string= hash doc-root)
            (j-query parent-doc) 
            (ps:chain 
              (get-similar-elements (j-query (ps:getprop (ps:getprop document-data hash) 0))))))

        (setf (ps:@ j-query fn elements-with-same-id) 
              (lambda ($item)
                (ps:chain (j-query this)
                          (find 
                            (concatenate 'string 
                                         "#"
                                         (ps:chain $item (attr "id")))))))

        (setf (ps:@ j-query fn elements-with-same-name) 
              (lambda ($item)
                (ps:chain (j-query this)
                          (find 
                            (concatenate 'string 
                                         "[name="
                                         (ps:chain $item (attr "name"))
                                         "]")))))

        (setf (ps:@ j-query fn elements-with-same-classes) 
              (lambda ($item)
                (let* ((classes (ps:chain $item (attr "class") (split (ps:regex "\\s+"))))
                       (ret (ps:chain 
                              (j-query this)
                              (find (concatenate 'string "." (ps:aref classes 0))))))

                  (loop for i in (ps:chain classes (slice 1)) do
                        (setf ret (ps:chain ret (filter (concatenate 'string "." i)))))

                  ret)))

        (setf (ps:@ j-query fn outer-h-t-m-l) 
              (lambda()
                (when (> (length this) 1)
                  ((ps:@ console log) "TODO: Unsupported length"))
                (ps:chain (ps:aref this 0) outer-h-t-m-l)))

        (setf (ps:@ j-query fn remove-style-attr-recursively)
              (lambda()
                (ps:chain (j-query this) 
                          (remove-attr "style") 
                          (find "*") (remove-attr "style")
                          (end))))

        (setf (ps:@ j-query fn elements-with-same-html) 
              (lambda ($item)
                (let* ((html (ps:chain $item (outer-h-t-m-l))))
                  (ps:chain 
                    (j-query this)
                    (filter (lambda ()
                              ; Here we ignoring style attribute when comparing
                              (string= (ps:chain (j-query this) 
                                                 (clone) 
                                                 (remove-style-attr-recursively)
                                                 (outer-h-t-m-l))
                                       (ps:chain $item 
                                                 (remove-style-attr-recursively)
                                                 (outer-h-t-m-l)))))))))

        (defun get-similar-elements (elem)
          (let ((similar-elements-return (array)))
            (ps:chain  
              elem 
              (map 
                (lambda (key item)
                  (setf $item (j-query item))
                  (cond 
                    ((ps:chain $item (attr "id")) 
                     ((ps:@ similar-elements-return push) 
                      (ps:getprop 
                        (ps:chain 
                          (j-query parent-doc)
                          (elements-with-same-id $item))
                        0)))
                    ((ps:chain $item (attr "name"))
                     (let ((elements (ps:chain 
                                       (j-query parent-doc)
                                       (elements-with-same-name $item))))
                       (if (= (length elements) 1)
                         ((ps:@ similar-elements-return push) (ps:aref elements 0))
                         ((ps:@ console log) "TODO, multiple elements with name" elements))))
                    ((ps:chain $item (attr "class"))
                     (let ((elements (ps:chain 
                                       (j-query parent-doc)
                                       (elements-with-same-classes $item))))
                       (if (= (length elements) 1)
                         ((ps:@ similar-elements-return push) (ps:aref elements 0))
                         (progn 
                           (setf elements (ps:chain (j-query elements)
                                                    (elements-with-same-html $item)))

                           (if (= (length elements) 1)
                             ((ps:@ similar-elements-return push) (ps:aref elements 0))
                             (setf similar-elements-return ((ps:@ similar-elements-return concat) ((ps:@ j-query make-array) elements))))))))

                    (t (let ((tags ((ps:@ window opener document get-elements-by-tag-name) 
                                    (ps:chain item tag-name (to-lower-case))))
                             (ret))

                         (setf ret (ps:chain (j-query tags)
                                             (elements-with-same-html $item)))

                         (if (= (length ret) 1)
                           ((ps:@ similar-elements-return push) (ps:aref ret 0))
                           (setf similar-elements-return ((ps:@ similar-elements-return concat) ((ps:@ j-query make-array) ret))))))))))
            similar-elements-return))

        (defun remove-generated-elements ()
          (ps:chain (j-query ".generated") (remove)))

        (defun show-element-rect-on-user-page(elems)
          (remove-generated-elements)

          (defun show-single-element-rect-on-user-page ()
            (setf elem (j-query this))
            (let ((offset (ps:chain elem (offset)))
                  (width (ps:chain elem (outer-width)))
                  (height (ps:chain elem (outer-height)))
                  (head-element-p (ps:chain elem (parents "head") length)))

              (setf elem (ps:chain 
                           (j-query "<div/>")
                           (add-class "generated")
                           (css "width" width)
                           (css "height" height)
                           (css "position" "absolute")
                           (css "opacity" "0.5")
                           (css "filter" "alpha(opacity=100)")
                           (css "top" (ps:getprop offset 'top))
                           (css "left" (ps:getprop offset 'left))
                           (css "background-color" "black")
                           (click (lambda ()
                                    (remove-generated-elements)))))

              (cond 
                (head-element-p
                  (progn 
                    (when (ps:chain (j-query ".generated.head")  length)
                      (ps:return-from show-single-element-rect-on-user-page))
                    (ps:chain elem (add-class "head") 
                              (width "200")
                              (height "35")
                              (css "left" "50%")
                              (css "margin-left" "-100px")
                              (css "color" "white")
                              (css "padding-top" "15px")
                              (css "text-align" "center")
                              (html "<i>&lt; head element &gt; </i>"))))
                ((or (< width 10) (< height 10))
                 (progn 
                   (ps:chain elem (css "border" "1px dashed black"))
                   (ps:chain 
                     (j-query "<div/>")
                     (add-class "generated")
                     (css "width" (if (< width 10) "50" width))
                     (css "height" (if (< height 10) "50" height))
                     (css "position" "absolute")
                     (css "opacity" "0.2")
                     (css "filter" "alpha(opacity=100)")
                     (css "top" (+ (ps:getprop offset 'top) (if (< height 10) -25 0)))
                     (css "left" (+ (ps:getprop offset 'left) (if (< width 10) -25 0)))
                     (css "background-color" "black")
                     (html "Here")
                     (click (lambda ()
                              (remove-generated-elements)))
                     (append-to window.opener.document.body)))))

              (ps:chain elem (append-to window.opener.document.body))
              elem))

          (ps:chain 
            (j-query elems)  
            (each #'show-single-element-rect-on-user-page)))

        (defun show-element-rect-on-debug-page(elems)
          (let* (($page-mirror-div (ps:chain (j-query document) (find  "#page-mirror")))
                 (debug-div-to-user-page-width-relation 
                   (/ (ps:chain $page-mirror-div (width))
                      window.opener.document.width)))

            (defun with-translated-size(some-value)
              (* debug-div-to-user-page-width-relation some-value))

            (defun show-single-element-rect()
              (setf elem (j-query this))
              (let ((offset (ps:chain elem (offset)))
                    (width (with-translated-size (ps:chain elem (outer-width))))
                    (height (with-translated-size (ps:chain elem (outer-height))))
                    (head-element-p (ps:chain elem (parents "head") length)))

                (unless offset 
                  (ps:return-from show-single-element-rect))

                (setf elem (ps:chain 
                             (j-query "<div/>")
                             (add-class "generated")
                             (css "width" width)
                             (css "height" height)
                             (css "position" "absolute")
                             (css "opacity" "0.2")
                             (css "filter" "alpha(opacity=100)")
                             (css "top" (with-translated-size (ps:getprop offset 'top)))
                             (css "left" (with-translated-size (ps:getprop offset 'left)))
                             (css "background-color" "black")
                             (click (lambda ()
                                      (remove-generated-elements)))))

                (cond 
                  (head-element-p (ps:return-from show-single-element-rect nil))
                  ((or (< width 10) (< height 10))
                   (ps:return-from show-single-element-rect nil)))

                (ps:chain elem (append-to $page-mirror-div))
                elem))

            (ps:chain 
              (j-query elems)  
              (each #'show-single-element-rect))))

        (defun show-element-rect (elems)
          ;(show-element-rect-on-debug-page elems)
          (show-element-rect-on-user-page elems))

        (ps:chain 
          (j-query window)
          (load (lambda()
                  (ps:for-in (hash document-data)
                             (show-element-rect-on-debug-page (get-dom-by-hash hash))))))))))

(defun render-debug-page-tree (&rest args)
  "Renders debug page interface - scripts, html for page mirror widget and html parts tree"
  (declare (ignore args))
  (render-debug-page-ps)
  (with-html 
    (:div :style "clear:both")
    (:div :id "page-mirror-container" :style "float:left;width:55%;position:relative;min-height:10px;"
     (:div :style "float:right"
      (render-link 
        (lambda (&rest args)
          (declare (ignore args))
          (do-information 
            "This is the \"mirror\" of html parts page contained - a visual representation of the right tree.")) "( &#8505; )"))
     (:div :style "clear:both")
     (:div :id "page-mirror" :style "position:relative;width:100%;"))
    (:div :id "page-tree" :style "float:left;width:45%"
     (:div :style "float:right;" 
      (render-link (lambda (&rest args)
                     (declare (ignore args))
                     (do-information 
                       "This is a tree of html parts page consists. For more information about html part click info, for highlighting part on the parent page click on the part link, return to parent page to see result"

                       )) "( &#8505; )"))
     (:br)
     (print-tree :children (list (with-debugged-app-md5-hash (weblocks-util:get-html-parts-root-hash)))))
    (:div :style "clear:both")))
