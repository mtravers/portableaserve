;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: CL-USER; Base: 10 -*-
;;;
;;; Builds a LispWorks image of paserve.

(in-package :cl-user)

#-lispworks
(error "This only works with Lispworks.")

(load (merge-pathnames "compile-aserve-lw.lisp" *load-truename*))

(deliver #'net.aserve::start-cmd
	 (merge-pathnames "aserve-lw" *load-truename*)
	0 
	 :multiprocessing t
	 :keep-pretty-printer t
	 :keep-clos t
	))
