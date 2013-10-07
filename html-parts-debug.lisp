(in-package :weblocks-utils)

(defun truncate-string (s &key (length 300) (etc "..."))
  "If string exceeds specified LENGTH truncates it to this LENGTH and adds additional text"
  (if (> (length s) length)
    (format nil "~A~A" (subseq s 0 length) etc)
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
  `(let ((weblocks-util::*parts-md5-hash* (get-debugged-application-md5-hash)))
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
                  (mark-dirty (root-widget))
                  (do-page 
                    (make-widget 
                      (lambda/cc (&rest args)
                                 (let ((back-action (lambda (&rest args)
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
              (when (and type (equal type :template))
                (html-newline-and-indent)
                (cl-who:htm 
                  (:i :style "text-align:right;"
                   (str (string-downcase (prin1-to-string (getf (with-debugged-app-md5-hash (weblocks-util:get-html-part-context i)) :template-name)))))))))
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
                    (cl-json:encode-json-to-string md5-hash)))))))
    (send-script 
      (ps:ps 
        (unless window.opener 
          (setf (ps:@ (document.get-element-by-id "eval-message") style display) "block"))
        (setf parent-doc window.opener.document.document-element)
        (setf j-query window.opener.j-query)
        (setf doc-root (ps:LISP (with-debugged-app-md5-hash (weblocks-util:get-html-parts-root-hash))))

        (defun get-dom-by-hash (hash)
          (if (string= hash doc-root)
            (j-query parent-doc) 
            (ps:chain 
              (get-similar-elements (j-query (slot-value (slot-value document-data hash) 0))))))

        (setf j-query.fn.elements-with-same-id 
              (lambda ($item)
                (ps:chain (j-query this)
                          (find 
                            (concatenate 'string 
                                         "#"
                                         (ps:chain $item (attr "id")))))))

        (setf j-query.fn.elements-with-same-name 
              (lambda ($item)
                (ps:chain (j-query this)
                          (find 
                            (concatenate 'string 
                                         "[name="
                                         (ps:chain $item (attr "name"))
                                         "]")))))

        (setf j-query.fn.elements-with-same-classes 
              (lambda ($item)
                (let* ((classes (ps:chain $item (attr "class") (split (ps:regex "\\s+"))))
                       (ret (ps:chain 
                              (j-query this)
                              (find (concatenate 'string "." (slot-value classes 0))))))

                  (loop for i in (ps:chain classes (slice 1)) do
                        (setf ret (ps:chain ret (filter (concatenate 'string "." i)))))

                  ret)))

        (setf j-query.fn.outer-h-t-m-l 
              (lambda()
                (when (> (length this) 1)
                  (console.log "TODO: Unsupported length"))
                (ps:chain (slot-value this 0) outer-h-t-m-l)))

        (setf j-query.fn.remove-style-attr-recursively
              (lambda()
                (ps:chain (j-query this) 
                          (remove-attr "style") 
                          (find "*") (remove-attr "style")
                          (end))))

        (setf j-query.fn.elements-with-same-html 
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
                     (similar-elements-return.push 
                       (slot-value 
                         (ps:chain 
                           (j-query parent-doc)
                           (elements-with-same-id $item))
                         0)))
                    ((ps:chain $item (attr "name"))
                     (let ((elements (ps:chain 
                                       (j-query parent-doc)
                                       (elements-with-same-name $item))))
                       (if (= (length elements) 1)
                         (similar-elements-return.push (slot-value elements 0))
                         (console.log "TODO, multiple elements with name" elements))))
                    ((ps:chain $item (attr "class"))
                     (let ((elements (ps:chain 
                                       (j-query parent-doc)
                                       (elements-with-same-classes $item))))
                       (if (= (length elements) 1)
                         (similar-elements-return.push (slot-value elements 0))
                         (progn 
                           (setf elements (ps:chain (j-query elements)
                                                    (elements-with-same-html $item)))

                           (if (= (length elements) 1)
                             (similar-elements-return.push (slot-value elements 0))
                             (setf similar-elements-return (similar-elements-return.concat (j-query.make-array elements))))))))

                    (t (let ((tags (window.opener.document.get-elements-by-tag-name 
                                     (ps:chain item tag-name (to-lower-case))))
                             (ret))

                         (setf ret (ps:chain (j-query tags)
                                             (elements-with-same-html $item)))

                         (if (= (length ret) 1)
                           (similar-elements-return.push (slot-value ret 0))
                           (setf similar-elements-return (similar-elements-return.concat (j-query.make-array ret))))))))))
            similar-elements-return))

        (defun remove-generated-elements ()
          (ps:chain (j-query ".generated") (remove)))

        (defun show-element-rect-on-user-page(elems)
          (remove-generated-elements)
          (ps:chain 
            (j-query elems)  
            (each (lambda ()
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
                                   (css "top" (slot-value offset 'top))
                                   (css "left" (slot-value offset 'left))
                                   (css "background-color" "black")
                                   (click (lambda ()
                                            (remove-generated-elements)))))

                      (cond 
                        (head-element-p
                          (progn 
                            (when (ps:chain (j-query ".generated.head")  length)
                              (return))
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
                             (css "top" (+ (slot-value offset 'top) (if (< height 10) -25 0)))
                             (css "left" (+ (slot-value offset 'left) (if (< width 10) -25 0)))
                             (css "background-color" "black")
                             (html "Here")
                             (click (lambda ()
                                      (remove-generated-elements)))
                             (append-to window.opener.document.body)))))

                      (ps:chain elem (append-to window.opener.document.body))
                      elem)))))

        (defun show-element-rect-on-debug-page(elems)
          (let* (($page-mirror-div (ps:chain (j-query document) (find  "#page-mirror")))
                 (debug-div-to-user-page-width-relation 
                   (/ (ps:chain $page-mirror-div (width))
                      window.opener.document.width)))

            (defun with-translated-size(some-value)
              (* debug-div-to-user-page-width-relation some-value))

            (ps:chain 
              (j-query elems)  
              (each (lambda ()
                      (setf elem (j-query this))
                      (let ((offset (ps:chain elem (offset)))
                            (width (with-translated-size (ps:chain elem (outer-width))))
                            (height (with-translated-size (ps:chain elem (outer-height))))
                            (head-element-p (ps:chain elem (parents "head") length)))

                        (unless offset 
                          (return))

                        (setf elem (ps:chain 
                                     (j-query "<div/>")
                                     (add-class "generated")
                                     (css "width" width)
                                     (css "height" height)
                                     (css "position" "absolute")
                                     (css "opacity" "0.2")
                                     (css "filter" "alpha(opacity=100)")
                                     (css "top" (with-translated-size (slot-value offset 'top)))
                                     (css "left" (with-translated-size (slot-value offset 'left)))
                                     (css "background-color" "black")
                                     (click (lambda ()
                                              (remove-generated-elements)))))

                        (cond 
                          (head-element-p (return nil))
                          ((or (< width 10) (< height 10))
                           (return nil)))

                        (ps:chain elem (append-to $page-mirror-div))
                        elem))))))

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
  (render-debug-page-ps)
  (with-html 
    (:div :style "clear:both")
    (:div :style "float:left;width:55%;position:relative;min-height:10px;"
     (:div :style "float:right"
      (render-link 
        (lambda (&rest args)
          (do-information 
            "This is the \"mirror\" of html parts page contained - a visual representation of the right tree.")) "( i )"))
     (:div :style "clear:both")
     (:div :id "page-mirror" :style "position:relative;width:100%;"))
    (:div :style "float:left;width:45%"
     (:div :style "float:right;" 
      (render-link (lambda (&rest args)
                     (do-information 
                       "This is a tree of html parts page consists. Under parts related to Weblocks templates there are template names. For more information about html part click info, for highlighting part on the parent page click on the part link, return to parent page to see result"

                       )) "( i )"))
     (:br)
     (print-tree :children (list (with-debugged-app-md5-hash (weblocks-util:get-html-parts-root-hash)))))
    (:div :style "clear:both")))
