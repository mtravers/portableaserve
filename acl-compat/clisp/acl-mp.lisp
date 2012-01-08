;; Stubs for multiprocessing functions under clisp.  Clisp does not
;; provide threads at the time of writing, so these functions are here
;; only to compile aserve with a minimum of changes in the main code.
;;
;; Written by Rudi Schlatte


(in-package :acl-compat-mp)

(defvar *current-process*)

(defun process-allow-schedule ()
  (values))

(defun process-allow-scheduling ()
  (values))

(defun process-plist (process)
  (declare (ignore process))
  (error "Attempting to use multithreading with clisp."))

(defun (setf process-plist) (new-value process)
  (declare (ignore new-value process))
  (error "Attempting to use multithreading with clisp."))

(defun process-run-reasons (process)
  (declare (ignore process))
  (error "Attempting to use multithreading with clisp."))

(defun (setf process-run-reasons) (new-value process)
  (declare (ignore new-value process))
  (error "Attempting to use multithreading with clisp."))

(defun process-revoke-run-reason (process object)
  (declare (ignore process object))
  (error "Attempting to use multithreading with clisp."))

(defun process-add-run-reason (process object)
  (declare (ignore process object))
  (error "Attempting to use multithreading with clisp."))

(defun process-run-function (name function &rest arguments)
  (declare (ignore name function arguments))
  (error "Attempting to use multithreading with clisp."))

(defun process-kill (process)
  (declare (ignore process))
  (error "Attempting to use multithreading with clisp."))

(defmacro with-gensyms (syms &body body)
  "Bind symbols to gensyms.  First sym is a string - `gensym' prefix.
Inspired by Paul Graham, <On Lisp>, p. 145."
  `(let (,@(mapcar (lambda (sy) `(,sy (gensym ,(car syms)))) (cdr syms)))
    ,@body))

(defun interrupt-process (process function &rest args)
  (declare (ignore process function args))
  (error "Attempting to use multithreading with clisp."))

(defun make-process-lock (&key name)
  (declare (ignore name))
  (error "Attempting to use multithreading with clisp."))

(defmacro with-process-lock ((lock &key norecursive whostate timeout)
			     &body forms)
  (declare (ignore lock norecursive whostate timeout))
  `(progn ,@forms))

(defmacro with-timeout ((seconds &body timeout-forms) &body body)
  (declare (ignore seconds timeout-forms))
  `(progn ,@body))

(defmacro without-scheduling (&body body)
  `(progn ,@body))
