;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:weblocks-utils-asd
  (:use :cl :asdf))

(in-package :weblocks-utils-asd)

(defsystem weblocks-utils
     :name "Weblocks utils"
     :version (:read-from-file "version.lisp-expr")
     :author "Olexiy Zamkoviy"
     :licence "Public Domain"
     :description "Utils for weblocks framework"
     :depends-on (:weblocks :weblocks-jquery-js :alexandria :weblocks-stores :clache :weblocks-custom :weblocks-tree-widget :arnesi :cl-fad :drakma :cl-tidy :uiop :cl-json)
     :components ((:file "package")
         (:file "weblocks-utils" :depends-on ("package"))
         (:file "debug-utils" :depends-on ("package"))
         (:file "sessions-debug" :depends-on ("package" "debug-utils"))
         (:file "debug-app" :depends-on ("package" "debug-utils" "html-parts-debug"))
         (:file "widget-stuff" :depends-on ("package"))
         (:file "assets-packages" :depends-on ("weblocks-utils"))
         (:file "html-parts-debug" :depends-on ("tidy-patch" "package" "sessions-debug"))
         (:file "tidy-patch")
         (:file "jquery-seq-debug-macros")))

