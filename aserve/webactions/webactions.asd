;;; -*- lisp -*-

(defpackage #:webactions-system
  (:use #:cl #:asdf))
(in-package #:webactions-system)

(defclass acl-file (cl-source-file) ())
(defmethod asdf:source-file-type ((c acl-file) (s module)) "cl")

(defsystem webactions
    :author "John K. Foderaro"
    :licence "LLGPL"
    :default-component-class acl-file
    :components
    ((:file "websession")
     (:file "webact" :depends-on ("websession"))
     (:file "clpage" :depends-on ("webact"))
     (:module :clpcode
              :components
              ((:file "clp")
               (:file "http")
               (:file "time")
               (:file "wa"))
              :depends-on ("clpage")))
    :depends-on (aserve #-allegro acl-compat htmlgen))
