(defpackage #:weblocks-utils
  (:use :cl :weblocks)
  (:documentation
    "Filtering widget for weblocks framework")
  (:export 
    #:prevalence-poveredp 
    #:clsql-poveredp 
    #:find-by 
    #:first-by 
    #:count-by 
    #:all-of 
    #:find-by-values 
    #:first-by-values 
    #:object->simple-plist 
    #:delete-all))

