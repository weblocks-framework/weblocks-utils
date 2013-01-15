(defpackage #:weblocks-utils
  (:use :cl :weblocks)
  (:documentation
    "Utility functions for weblocks framework")
  (:export 
    ; weblocks-utils
    #:prevalence-poveredp 
    #:clsql-poveredp 
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
    #:debug-single-session))

