;;; This file implements the process functions for AllegroServe in MCL.
;;; Based on the the work done for cmucl and Lispworks.
;;;
;;; John DeSoi, Ph.D. desoi@users.sourceforge.net


(in-package :acl-compat.mp)

(eval-when (:compile-toplevel :load-toplevel :execute)

; existing stuff from ccl we can reuse directly
(shadowing-import 
 '(ccl:*current-process*
   ccl::lock
   ccl:process-allow-schedule
   ccl:process-name
   ccl:process-preset
   #-openmcl-native-threads ccl:process-run-reasons
   ccl:process-wait
   ccl:process-wait-with-timeout
   ccl:without-interrupts))
)

(eval-when (:compile-toplevel :load-toplevel :execute)

(export 
 '(*current-process*
   lock
   process-allow-schedule
   process-name
   process-preset
   process-run-reasons
   process-wait
   process-wait-with-timeout
   without-interrupts))
)

(eval-when (:compile-toplevel :load-toplevel :execute)
                 
(defmacro without-scheduling (&body forms)
  `(ccl:without-interrupts ,@forms))

#|
; more ideas stolen from acl-mp-lw.lisp
(defun invoke-with-timeout (seconds bodyfn timeoutfn)
  (block timeout
    (let* ((process *current-process*)
           (timer (ccl:process-run-function "with-timeout-timer"
                                            #'(lambda () 
                                                (sleep seconds)
                                                (ccl:process-interrupt process
                                                                       #'(lambda ()
                                                                           (return-from timeout
                                                                             (funcall timeoutfn))))))))
      (unwind-protect (funcall bodyfn)
        (ccl:process-kill timer)))))

|#



(defun invoke-with-timeout (seconds bodyfn timeoutfn)
  (block timeout
    (let* ((timer (ccl::make-timer-request
                    seconds
                    #'(lambda () (return-from timeout (funcall timeoutfn))))))
      (ccl::enqueue-timer-request timer)
      (unwind-protect (funcall bodyfn)
	(ccl::dequeue-timer-request timer)))))


(defmacro with-timeout ((seconds &body timeout-forms) &body body)
  "Execute BODY; if execution takes more than SECONDS seconds, terminate and evaluate TIMEOUT-FORMS."
  `(invoke-with-timeout ,seconds #'(lambda () ,@body)
                        #'(lambda () ,@timeout-forms)))


#+openmcl-native-threads
(progn

;;; The :INITIAL-BINDINGS arg to process creation functions seems to be
;;; quoted, even when it appears in a list (as in the case of
;;; (process-run-function <args>))  By the time that percolates down
;;; to OpenMCL's process creation functions, it should lose the quote.
;;;
;;; Perhaps I imagined that ...
;;;

(defun ccl::openmcl-fix-initial-bindings (initial-bindings)
  (if (and (consp initial-bindings)
           (eq (car initial-bindings) 'quote))
    (cadr initial-bindings)
    initial-bindings))
                             
)
	   

#-openmcl-native-threads
(defmacro process-revoke-run-reason (process reason)
  `(ccl:process-disable-run-reason ,process ,reason) )

#-openmcl-native-threads
(defmacro process-add-run-reason (process reason)
  `(ccl:process-enable-run-reason ,process ,reason) )


(defmacro make-process-lock (&key name)
  (if name
    `(ccl:make-lock ,name)
    `(ccl:make-lock)))

(defmacro with-process-lock ((lock &key norecursive timeout whostate) &body forms)
  (declare (ignore norecursive whostate timeout))
  `(ccl:with-lock-grabbed (,lock) ,@forms))


(defmacro process-kill (process)
  `(progn
    #-openmcl-native-threads
     (unless (ccl:process-active-p ,process) ;won't die unless enabled
       (ccl:process-reset-and-enable ,process) )
     (ccl:process-kill ,process)))
)

(defun process-active-p (process)
  (ccl::process-active-p process))

(defun interrupt-process (process function &rest args)
  "Run FUNCTION in PROCESS."
(apply #'ccl:process-interrupt process function args))

(defun current-process ()
  "The current process."
  ccl:*current-process*)


;property list implementation from acl-mp-cmu.lisp
(defvar *process-plists* (make-hash-table :test #'eq)
  "maps processes to their plists.
See the functions process-plist, (setf process-plist).")

(defun process-property-list (process)
  (gethash process *process-plists*))

(defun (setf process-property-list) (new-value process)
  (setf (gethash process *process-plists*) new-value))

; from acl-mp-lw.lisp
(defun make-process (&key (name "Anonymous") reset-action run-reasons arrest-reasons (priority 0) quantum
                          resume-hook suspend-hook initial-bindings run-immediately)
  (declare (ignore priority quantum reset-action resume-hook suspend-hook run-immediately))
  #-openmcl-native-threads
  (declare (ignore initial-bindings)) ;! need separate lexical bindings for each process?
  #+openmcl-native-threads
  (declare (ignore run-reasons arrest-reasons))
  ;(let ((acl-mp:*process-initial-bindings* initial-bindings))
  #-openmcl-native-threads
  (ccl:make-process name :run-reasons run-reasons :arrest-reasons arrest-reasons)
  #+openmcl-native-threads
  (ccl:make-process name :initial-bindings (ccl::openmcl-fix-initial-bindings initial-bindings)))

(defun process-run-function (name-or-options preset-function &rest preset-arguments)
  (let ((process (ctypecase name-or-options
                   (string (acl-mp:make-process :name name-or-options))
                   (list (apply #'acl-mp:make-process name-or-options)))))
    (apply #'acl-mp:process-preset process preset-function preset-arguments)
    #+openmcl-native-threads (ccl:process-enable process)
    #-openmcl-native-threads (process-add-run-reason process :enable)
    process))

;;; Busy-waiting ...
(defun wait-for-input-available (streams
                                 &key (wait-function #'ccl:stream-listen)
                                 whostate timeout)
  (let ((collected-fds nil))
    (flet ((collect-fds ()
             (setf collected-fds
                   (remove-if-not wait-function streams))))
      
      (if timeout
          (process-wait-with-timeout (or whostate "Waiting for input") timeout #'collect-fds)
          (process-wait (or whostate "Waiting for input") #'collect-fds)))
    collected-fds))
