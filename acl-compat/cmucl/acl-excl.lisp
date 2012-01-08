;;;;
;;;; ACL-COMPAT - EXCL
;;;;

;;;; Implementation-specific parts of acl-compat.excl (see
;;;; acl-excl-common.lisp)

(in-package :acl-compat.excl)

(defun stream-input-fn (stream)
  stream)

(defun filesys-type (file-or-directory-name)
       (if (eq :directory (unix:unix-file-kind
                           (namestring file-or-directory-name)))
           :directory
         (if (probe-file file-or-directory-name)
             :file
           nil)))

(defmacro atomically (&body forms)
  `(mp:without-scheduling ,@forms))

(defun unix-signal (signal pid)
  ;; fixxme: did I get the arglist right?  only invocation I have seen
  ;; is (excl::unix-signal 15 0) in net.aserve:start
  (unix:unix-kill pid signal))

(defmacro without-package-locks (&body forms)
  `(progn ,@forms))

(defun filesys-inode (path)
  (multiple-value-bind (found ign inode)
      (unix:unix-lstat path)
    (if found
        inode
        (error "path ~s does not exist" path))))

(defun cl-internal-real-time ()
  (round (/ (get-internal-real-time) internal-time-units-per-second)))

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
