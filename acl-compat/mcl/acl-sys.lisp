
(in-package :acl-compat.system)


(defun command-line-arguments ()
  #+openmcl (ccl::command-line-arguments)
  #-openmcl nil)

(defun command-line-argument (n)
  #+openmcl (nth n (command-line-arguments))
  #-openmcl nil)

;;; On acl, reap-os-subprocess is needed for (run-shell-command ...
;;; :wait nil), but not on OpenMCL.
(defun reap-os-subprocess (&key (wait nil))
  (declare (ignore wait))
  nil)

#+nil
(export '(command-line-arguments command-line-argument reap-os-subprocess))
