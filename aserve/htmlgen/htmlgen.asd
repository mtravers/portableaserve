;;; -*- mode: lisp -*-

(defpackage #:htmlgen-system
  (:use #:cl #:asdf))
(in-package #:htmlgen-system)

(defclass acl-file (cl-source-file) ())
(defmethod source-file-type ((c acl-file) (s module)) "cl")

(defsystem htmlgen
    :author "John K. Foderaro"
    :licence "LLGPL"
    :default-component-class acl-file
    :components ((:file "htmlgen"))
    :depends-on (acl-compat)
    :perform (load-op :after (op htmlgen)
                      (pushnew :htmlgen cl:*features*)))

