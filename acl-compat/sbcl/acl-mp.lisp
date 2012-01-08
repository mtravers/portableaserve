;; Threading for sbcl, or stub functions for single-threaded sbcl.
;;
;; Written by Rudi Schlatte, intended to be distributed along with the
;; acl-compat library, under the same license as the rest of it.

;; Inspirations taken from Dan Barlow<dan@metacircles.com>'s work for
;; McCLIM; cut, pasted and mutilated with permission.

(in-package :acl-compat.mp)

(defstruct (process
            (:constructor %make-process)
            (:predicate processp))
  name
  state
  whostate
  function                              ; function wot will be run
  arguments                             ; arguments to the function
  id                                    ; pid of unix thread or nil
  %lock                                 ; lock for process structure mutators
  run-reasons                           ; primitive mailbox for IPC
  %queue                                ; queue for condition-wait
  initial-bindings                      ; special variable bindings
  property-list)

(defparameter *current-process* 
  #-sb-thread 
  (%make-process)
  #+sb-thread
  ;; We don't fill in the process id, so the process compiling this
  ;; (the REPL, in most cases) can't be killed by accident. (loop for
  ;; p in (all-processes) do (kill-process p)), anyone?
  (%make-process :name "initial process" :function nil))

(defparameter *all-processes-lock*
  (sb-thread:make-mutex :name "all processes lock"))

(defparameter *all-processes* 
  (list *current-process*))

#-sb-thread
(defun make-process (&key (name "Anonymous") reset-action run-reasons
                           arrest-reasons (priority 0) quantum resume-hook
                           suspend-hook initial-bindings run-immediately)
   (declare (ignore reset-action arrest-reasons priority quantum resume-hook
		    suspend-hook run-immediately))
   (%make-process :name "the only process"
		  :run-reasons run-reasons
		  :initial-bindings initial-bindings))

#+sb-thread
(defun make-process  (&key (name "Anonymous") reset-action run-reasons
                      arrest-reasons (priority 0) quantum resume-hook
                      suspend-hook initial-bindings run-immediately)
  (declare (ignore reset-action arrest-reasons priority quantum resume-hook
                   suspend-hook run-immediately))
  (let ((p (%make-process
            :name name
            :run-reasons run-reasons
            :initial-bindings initial-bindings
            :%lock (sb-thread:make-mutex
                    :name (format nil "Internal lock for ~A" name))
            :%queue (sb-thread:make-waitqueue
                     :name (format nil "Blocking queue for ~A" name)))))
    (sb-thread:with-mutex (*all-processes-lock*)
      (push p *all-processes*))
    p))

(defmacro defun/sb-thread (name args &body body)
  #-sb-thread (declare (ignore body))
  `(defun ,name ,args
     #-sb-thread
     (declare (ignore ,@(remove-if
			 (lambda (x)
			   (member x '(&optional &rest &key &allow-other-keys
                                       &aux)))
			 (mapcar (lambda (x) (if (consp x) (car x) x))
				 args))))
     #-sb-thread
     (error
      "~A: Calling a multiprocessing function on a single-threaded sbcl build"
      ',name)
     #+sb-thread
     ,@body))

(defun/sb-thread process-interrupt (process function)
  (sb-thread:interrupt-thread (process-id process) function))

;; TODO: why no such function was in +sb-thread part?
(defun/sb-thread process-wait-function (process)
  (declare (ignore process)))

(defun/sb-thread process-wait (reason predicate &rest arguments)
  (declare (type function predicate))
  (let ((old-state (process-whostate *current-process*)))
    (unwind-protect
        (progn
          (setf old-state (process-whostate *current-process*)
                (process-whostate *current-process*) reason)
          (loop 
           (let ((it (apply predicate arguments)))
             (when it (return it)))
           (process-allow-schedule)))
      (setf (process-whostate *current-process*) old-state))))

(defun/sb-thread process-allow-schedule (&optional process)
  (declare (ignore process))
  (sleep .01))

(defun/sb-thread process-revoke-run-reason (process object)
  (sb-thread:with-recursive-lock ((process-%lock process))
    (prog1
        (setf (process-run-reasons process)
              (delete object (process-run-reasons process)))
      (when (and (process-id process) (not (process-run-reasons process)))
        (disable-process process)))))

(defun/sb-thread process-add-run-reason (process object)
  (sb-thread:with-recursive-lock ((process-%lock process))
    (prog1
        (push object (process-run-reasons process))
      (if (process-id process)
          (enable-process process)
          (restart-process process)))))

(defun/sb-thread process-run-function (name-or-options preset-function
                                                       &rest preset-arguments)
  (let* ((make-process-args (etypecase name-or-options
                              (list name-or-options)
                              (string (list :name name-or-options))))
         (process (apply #'make-process make-process-args)))
    (apply #'process-preset process preset-function preset-arguments)
    (setf (process-run-reasons process) :enable)
    (restart-process process)
    process))

(defun/sb-thread process-preset (process function &rest arguments)
  (setf (process-function process) function
        (process-arguments process) arguments)
  (when (process-id process) (restart-process process)))

(defun/sb-thread process-kill (process)
  (when (process-id process)
    (sb-thread:destroy-thread (process-id process))
    (setf (process-id process) nil))
  (sb-thread:with-mutex (*all-processes-lock*)
    (setf *all-processes* (delete process *all-processes*))))

#+sb-thread
(defun make-process-lock (&key name)
  (sb-thread:make-mutex :name name))
#-sb-thread
(defun make-process-lock (&key name)
  (declare (ignore name))
  nil)

(defun/sb-thread process-lock (lock &optional lock-value whostate timeout)
  (declare (ignore whostate timeout))
  (sb-thread:get-mutex lock lock-value))

(defun/sb-thread process-unlock (lock &optional lock-value)
  (declare (ignore lock-value))
  (sb-thread:release-mutex lock))

#-sb-thread
(defmacro with-process-lock ((lock &key norecursive timeout whostate)
                             &body forms)
  (declare (ignore lock norecursive timeout whostate))
  `(progn ,@forms))

#+sb-thread
(defmacro with-process-lock ((place &key timeout whostate norecursive)
			     &body body)
  (declare (ignore norecursive timeout))
  (let ((old-whostate (gensym "OLD-WHOSTATE")))
    `(sb-thread:with-recursive-lock (,place)
      (let (,old-whostate)
	(unwind-protect
	     (progn
	       (when ,whostate
		 (setf ,old-whostate (process-whostate *current-process*))
		 (setf (process-whostate *current-process*) ,whostate))
	       ,@body)
	  (setf (process-whostate *current-process*) ,old-whostate))))))


#-sb-thread
(defmacro without-scheduling (&body forms)
  `(progn ,@forms))                     ; *

;;; FIXME but, of course, we can't.  Fix whoever wants to use it,
;;; instead
#+sb-thread
(defmacro without-scheduling (&body body)
  `(progn ,@body))

;;; Same implementation for multi- and uni-thread
(defmacro with-timeout ((seconds &body timeout-forms) &body body)
  (let ((c (gensym "TIMEOUT-")))
    `(handler-case
      (sb-ext::with-timeout ,seconds (progn ,@body))
      (sb-ext::timeout (,c) (declare (ignore ,c)) ,@timeout-forms))))

(defun/sb-thread restart-process (process)
  (labels ((boing ()
                  (let ((*current-process* process)
                        (bindings (process-initial-bindings process))
                        (function (process-function process))
                        (arguments (process-arguments process)))
		    (declare (type function function))
                    (if bindings
                        (progv
                            (mapcar #'car bindings)
                            (mapcar #'(lambda (binding)
                                        (eval (cdr binding)))
                                    bindings)
                          (apply function arguments))
                      (apply function arguments)))))
    (when (process-id process)
      (sb-thread:terminate-thread (process-id process)))
    ;; XXX handle run-reasons in some way?  Should a process continue
    ;; running if all run reasons are taken away before
    ;; restart-process is called?  (process-revoke-run-reason handles
    ;; this, so let's say (setf (process-run-reasons process) nil) is
    ;; not guaranteed to do the Right Thing.)
    (when (setf (process-id process)
                (sb-thread:make-thread #'boing :name (process-name process)))
      process)))

(defun current-process ()
  *current-process*)

(defun all-processes ()
  (copy-list *all-processes*))

(defun/sb-thread process-wait-with-timeout (reason timeout predicate)
  (declare (type function predicate))
  (let ((old-state (process-whostate *current-process*))
        (end-time (+ (get-universal-time) timeout)))
    (unwind-protect
        (progn
          (setf old-state (process-whostate *current-process*)
                (process-whostate *current-process*) reason)
          (loop 
           (let ((it (funcall predicate)))
             (when (or (> (get-universal-time) end-time) it)
               (return it)))
           (sleep .01)))
      (setf (process-whostate *current-process*) old-state))))

(defun/sb-thread disable-process (process)
  ;; TODO: set process-whostate
  ;; Can't figure out how to safely block a thread from a different one
  ;; and handle all the locking nastiness.  So punt for now.
  (if (eq sb-thread:*current-thread* (process-id process))
      ;; Keep waiting until we have a reason to run.  GC and other
      ;; things can break a wait prematurely.  Don't know if this is
      ;; expected or not.
      (do ()
          ((process-run-reasons process) nil)
        (sb-thread:with-recursive-lock ((process-%lock process))
          (sb-thread:condition-wait (process-%queue process)
                                    (process-%lock process))))
      (error "Can't safely disable-process from another thread")))

(defun/sb-thread enable-process (process)
  ;; TODO: set process-whostate
  (sb-thread:with-recursive-lock ((process-%lock process))
    (sb-thread:condition-notify (process-%queue process))))

;;; TODO: integrate with McCLIM / system-wide queue for such things
#+sb-thread
(defvar *atomic-spinlock* (sb-thread::make-spinlock))

#-sb-thread
(defmacro atomic-incf (place)
  `(incf ,place))

#+sb-thread
(defmacro atomic-incf (place)
  `(sb-thread::with-spinlock (*atomic-spinlock*)
    (incf ,place)))

#-sb-thread
(defmacro atomic-decf (place)
  `(decf ,place))

#+sb-thread
(defmacro atomic-decf (place)
  `(sb-thread::with-spinlock (*atomic-spinlock*)
    (decf ,place)))

(defun process-active-p (process)
  (sb-thread:thread-alive-p (process-id process)))
