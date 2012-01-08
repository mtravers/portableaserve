;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: CL-USER; Base: 10 -*-
;;;
;;; Compiles paserve for LispWorks

(in-package :cl-user)

#-lispworks
(error "This only works with Lispworks.")

(load (merge-pathnames "logical-hostnames.lisp" *load-truename*))

(load "acl-compat:defsys.lisp")
(let ((lw:*handle-warn-on-redefinition* :warn))
  (scm:compile-system 'acl-compat :load t))
(load "aserve:defsys.lisp")
(let ((lw:*handle-warn-on-redefinition* :warn))
  (scm:compile-system 'aserve :load t))
