;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:weblocks-utils-asd
  (:use :cl :asdf))

(in-package :weblocks-utils-asd)

(defsystem weblocks-utils
     :name "Weblocks utils"
     :version "0.1.1"
     :author "Olexiy Zamkoviy"
     :licence "Public Domain"
     :description "Utils for weblocks framework"
     :depends-on (:weblocks)
     :components ((:file "package")
         (:file "weblocks-utils" :depends-on ("package"))))

