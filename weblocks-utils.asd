;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:weblocks-utils-asd
  (:use :cl :asdf))

(in-package :weblocks-utils-asd)

(defsystem weblocks-utils
     :name "Weblocks utils"
     :version "0.5.0"
     :author "Olexiy Zamkoviy"
     :licence "Public Domain"
     :description "Utils for weblocks framework"
     :depends-on (:weblocks :alexandria :weblocks-stores :clache :weblocks-custom :weblocks-tree-widget :arnesi :cl-fad :drakma)
     :components ((:file "package")
         (:file "weblocks-utils" :depends-on ("package"))
         (:file "debug-utils" :depends-on ("package"))
         (:file "sessions-debug" :depends-on ("package" "debug-utils"))
         (:file "debug-app" :depends-on ("package" "debug-utils"))
         (:file "widget-stuff" :depends-on ("package"))
         (:file "assets-packages")))

