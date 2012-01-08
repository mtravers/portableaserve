;; This package is designed for cmucl.  It implements ACL-style
;; multiprocessing on top of cmucl (basically, process run reasons and
;; some function renames).
;;
;; Written by Rudi Schlatte, based on the work done by Jochen Schmidt
;; for Lispworks.

(in-package :acl-compat.mp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Import equivalent parts from the CMU MP package ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(shadowing-import '(mp:*current-process*
                    ;; mp::process-preset
                    mp::process-reset
                    mp:process-interrupt
                    mp::process-name
                    mp::process-wait-function
                    mp:process-run-reasons
                    mp:process-add-run-reason
                    mp:process-revoke-run-reason
                    mp:process-arrest-reasons
                    mp:process-add-arrest-reason
                    mp:process-revoke-arrest-reason
                    mp:process-whostate
                ;    mp:without-interrupts
                    mp:process-wait
                    mp:with-timeout
		    mp:without-scheduling
		    mp:process-active-p 
                    ))

(export '(*current-process*
          ;; process-preset
          process-reset
          process-interrupt
          process-name
          process-wait-function
          process-whostate
          process-wait
          with-timeout
          without-scheduling
          process-run-reasons
          process-add-run-reason
          process-revoke-run-reason
          process-arrest-reasons
          process-add-arrest-reason
          process-revoke-arrest-reason
	  process-active-p
          ))


(defun process-allow-schedule ()
  (mp:process-yield))

(defvar *process-plists* (make-hash-table :test #'eq)
  "maps processes to their plists.
See the functions process-plist, (setf process-plist).")

(defun process-property-list (process)
  (gethash process *process-plists*))

(defun (setf process-property-list) (new-value process)
  (setf (gethash process *process-plists*) new-value))

#||

;;; rudi 2002-06-09: This is not needed as of cmucl 18d, thanks to Tim
;;; Moore who added run reasons to cmucl's multithreading.  Left in
;;; for the time being just in case someone wants to get acl-compat
;;; running on older cmucl's.  Can be deleted safely.

(defvar *process-run-reasons* (make-hash-table :test #'eq)
  "maps processes to their run-reasons.
See the functions process-run-reasons, (setf process-run-reasons),
process-add-run-reason, process-revoke-run-reason.")

(defun process-run-reasons (process)
  (gethash process *process-run-reasons*))

(defun (setf process-run-reasons) (new-value process)
  (mp:without-scheduling
   (prog1
       (setf (gethash process *process-run-reasons*) new-value)
     (if new-value
         (mp:enable-process process)
       (mp:disable-process process)))))

(defun process-revoke-run-reason (process object)
  (without-scheduling
   (setf (process-run-reasons process)
	 (remove object (process-run-reasons process))))
  (when (and (eq process mp:*current-process*))
    (mp:process-yield)))

(defun process-add-run-reason (process object)
  (setf (process-run-reasons process)
        (pushnew object (process-run-reasons process))))
||#

(defun process-run-function (name-or-options preset-function
                             &rest preset-arguments)
  (let ((process (etypecase name-or-options
                   (string (make-process :name name-or-options
                                         :run-reasons '(t)))
                   (list (apply #'make-process :run-reasons '(t)
                                name-or-options)))))
    (apply #'acl-mp::process-preset process preset-function preset-arguments)
    process))

(defun process-preset (process preset-function &rest arguments)
  (mp:process-preset process
                     #'(lambda ()
                         (apply-with-bindings preset-function
                                              arguments
                                              (process-initial-bindings process)))))

(defvar *process-initial-bindings* (make-hash-table :test #'eq))

(defun process-initial-bindings (process)
  (gethash process *process-initial-bindings*))

(defun (setf process-initial-bindings) (bindings process)
  (setf (gethash process *process-initial-bindings*) bindings))


;;;                          ;;;
;;; Contributed by Tim Moore ;;;
;;;	                     ;;;
(defun apply-with-bindings (function args bindings)
  (if bindings
      (progv
	  (mapcar #'car bindings)
	  (mapcar #'(lambda (binding)
		      (eval (cdr binding)))
                  bindings)
	(apply function args))
      (apply function args)))

(defun make-process (&key (name "Anonymous") reset-action run-reasons
                     arrest-reasons (priority 0) quantum resume-hook
                     suspend-hook initial-bindings run-immediately)
  (declare (ignore priority quantum reset-action resume-hook suspend-hook
                   run-immediately))
  (mp:make-process nil :name name
                   :run-reasons run-reasons
                   :arrest-reasons arrest-reasons
                   :initial-bindings initial-bindings))

(defun process-kill (process)
  (mp:destroy-process process))


(defun make-process-lock (&key name)
  (mp:make-lock name))

(defun process-lock (lock)
  (mp::lock-wait lock (mp:process-whostate mp:*current-process*)))

(defun process-unlock (lock)
  (setf (mp::lock-process lock) nil))


(defmacro with-process-lock ((lock &key norecursive whostate timeout) &body forms)
  (declare (ignore norecursive))
  `(mp:with-lock-held (,lock
		  ,@(when whostate (list :whostate whostate))
		  ,@(when timeout (list :timeout timeout)))
    ,@forms))
