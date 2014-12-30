(defpackage #:weblocks-utils
  (:use :cl :weblocks :weblocks-stores :clache :weblocks-tree-widget)
  (:documentation
    "Utility functions for weblocks framework")
  (:export 
    ; weblocks-utils
    #:memory-poweredp
    #:prevalence-poweredp 
    #:clsql-poweredp 
    #:first-by 
    #:count-by 
    #:count-of 
    #:count-by-values
    #:all-of 
    #:find-by-values 
    #:find-by 
    #:first-by-values 
    #:delete-all 
    #:delete-one 
    #:first-of 

    ; debug-utils
    #:object->simple-plist 
    #:first-active-webapp
    #:maybe-with-first-active-webapp

    ; session-debug
    #:first-active-session
    #:debug-session
    #:debug-first-session
    #:debug-single-session 
    #:enter-first-active-session
    #:with-first-active-session 
    #:with-active-session-by-id 
    #:start-debug-app 
    #:with-object-cache
    #:*default-cache-store* 
    #:related-record-field
    #:multiple-related-records-field 
    #:require-assets 
    #:prepend-webapp-path 
    #:ps-with-scripts 
    #:ps-with-styles
    #:ps-with-scripts-and-styles
    #:make-form 
    #:truncate-string 
    #:describe-set))

