
(eval-when (:compile-toplevel :load-toplevel :execute)
    (ext:without-package-lock ()
      (let ((sys-package (find-package "SYSTEM")))
        (export (list (intern "COMMAND-LINE-ARGUMENTS" sys-package)
                      (intern "COMMAND-LINE-ARGUMENT" sys-package)
                      (intern "REAP-OS-SUBPROCESS" sys-package))
                sys-package))))

(ext:without-package-lock ()
  (defun sys:command-line-arguments ()
    ext:*args*))

(ext:without-package-lock ()
  (defun sys:command-line-argument (n)
    (nth n ext:*args*)))

(ext:without-package-lock ()
  (defun sys:reap-os-subprocess (&key (wait nil))
    (declare (ignore wait))
    nil))

