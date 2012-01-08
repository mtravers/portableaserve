;;;;
;;;; ACL-COMPAT - EXCL
;;;;

;;;; Implementation-specific parts of acl-compat.excl (see
;;;; acl-excl-common.lisp)

(in-package :acl-compat.excl)

#+obsolete
(defun stream-input-fn (stream)
  stream)

(defmethod stream-input-fn ((stream stream))
  stream)
	
(defun filesys-type (file-or-directory-name)
	(if (lw::file-directory-p file-or-directory-name)
		:directory
		(if (probe-file file-or-directory-name)
			:file
			nil)))

#-:win32
(defun filesys-inode (path)
  (let ((checked-path (probe-file path)))
    (cond
      (checked-path (let ((stat (system:get-file-stat checked-path)))
		      (system:file-stat-inode stat)))
      (t (error "path ~a does not exist." path)))))

(defmacro atomically (&body forms)
  (declare (ignorable forms))
  #+(or lispworks4 lispworks5)
  `(mp:without-preemption ,@forms)
  #-(or lispworks4 lispworks5)
  (progn
    (warn "Using ATOMICALLY, will give an error at runtime.")
    `(error "Trying to execute ATOMICALLY")))

(defmacro without-package-locks (&body forms)
  `(progn ,@forms))


#|
(defun run-shell-command ()
  (with-open-stream (s (open-pipe "/bin/sh"
                                  :direction :io
                                  :buffered nil))
    (loop for var in environment
          do (format stream "~A=~A~%" (car var) (cdr var)))
|#
  
;; NDL 2004-06-04 -- Missing definition & a package, to allow LispWorks to load webactions

(defun cl-internal-real-time ()
  (round (/ (get-internal-real-time) 1000)))

(defun string-to-octets (string &key (null-terminate t) (start 0)
                         end mb-vector make-mb-vector?
                         (external-format :default))
  "This function returns a lisp-usb8-vector and the number of bytes copied."
  (declare (ignore external-format))
  ;; The end parameter is different in ACL's lambda list, but this
  ;; variant lets us give an argument :end nil explicitly, and the
  ;; right thing will happen
  (unless end (setf end (length string)))
  (let* ((number-of-octets (if null-terminate (1+ (- end start))
                               (- end start)))
         (mb-vector (cond
                      ((and mb-vector (>= (length mb-vector) number-of-octets))
                       mb-vector)
                      ((or (not mb-vector) make-mb-vector?)
                       (make-array (list number-of-octets)
                                   :element-type '(unsigned-byte 8)
                                   :initial-element 0))
                      (t (error "Was given a vector of length ~A, ~
                                 but needed at least length ~A."
                                (length mb-vector) number-of-octets)))))
    (declare (type (simple-array (unsigned-byte 8) (*)) mb-vector))
    (loop for from-index from start below end
       for to-index upfrom 0
       do (progn
            (setf (aref mb-vector to-index)
                  (char-code (aref string from-index)))))
    (when null-terminate
      (setf (aref mb-vector (1- number-of-octets)) 0))
    (values mb-vector number-of-octets)))


(provide 'acl-excl)
