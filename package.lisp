(defpackage #:weblocks-utils
  (:use :cl :weblocks :weblocks-stores :clache)
  (:documentation
    "Utility functions for weblocks framework")
  (:export 
    ; weblocks-utils
    #:memory-poweredp
    #:prevalence-poweredp 
    #:clsql-poweredp 
    #:find-by 
    #:first-by 
    #:count-by 
    #:all-of 
    #:find-by-values 
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
    #:start-debug-app 
    #:with-object-cache
    #:*default-cache-store*))

