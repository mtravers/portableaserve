(in-package :acl-compat.system)

(ignore-errors
(export 'command-line-arguments)
(export 'command-line-argument)
(export 'reap-os-subprocess)

(defun command-line-arguments ()
  ext:*command-line-strings*)

(defun command-line-argument (n)
  (nth n ext:*command-line-strings*))

(defun reap-os-subprocess (&key (wait nil))
  (declare (ignore wait))
  nil)

)
