;;;;
;;;; ACL-COMPAT - EXCL
;;;;

;;;; Implementation-specific parts of acl-compat.excl (see
;;;; acl-excl-common.lisp)

(defpackage :acl-compat.excl
	(:use #:common-lisp #:ext)
	(:export
         #:if*
         #:*initial-terminal-io*
         #:*cl-default-special-bindings*
         #:filesys-size
         #:filesys-write-date
         #:stream-input-fn
         #:match-regexp
         #:compile-regexp
         #:*current-case-mode*
         #:intern*
         #:filesys-type
         #:errorset
         #:atomically
         #:fast
         #:without-package-locks
         #:string-to-octets
         #:write-vector

         ;; TODO: find better place for bivalent stream classes
         #:bivalent-input-stream
         #:bivalent-output-stream
         #:bivalent-stream
         #:make-bivalent-input-stream
         #:make-bivalent-output-stream
         #:make-bivalent-stream
         ))

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


;;; Bivalent Gray streams


(defclass lisp-stream-mixin ()
  ;; For bivalent streams, lisp-stream must be a stream of type
  ;; unsigned-byte
  ((lisp-stream :initarg :lisp-stream
		:accessor lisp-stream)))

(defclass bivalent-input-stream (lisp-stream-mixin
                                 fundamental-character-input-stream
                                 fundamental-binary-input-stream))

(defclass bivalent-output-stream (lisp-stream-mixin
                                  fundamental-character-output-stream
                                  fundamental-binary-output-stream))

(defclass bivalent-stream (bivalent-input-stream bivalent-output-stream))


(defun make-bivalent-input-stream (lisp-stream)
  (declare (type system:lisp-stream lisp-stream))
  (make-instance 'bivalent-input-stream :lisp-stream lisp-stream))

(defun make-bivalent-output-stream (lisp-stream)
  (declare (type system:lisp-stream lisp-stream))
  (make-instance 'bivalent-output-stream :lisp-stream lisp-stream))

(defun make-bivalent-stream (lisp-stream)
  (declare (type system:lisp-stream lisp-stream))
  (make-instance 'bivalent-stream :lisp-stream lisp-stream))


(defmethod open-stream-p ((stream lisp-stream-mixin))
  (common-lisp::open-stream-p (lisp-stream stream)))

(defmethod close ((stream lisp-stream-mixin) &key abort)
  (close (lisp-stream stream) :abort abort))

(defmethod input-stream-p ((stream lisp-stream-mixin))
  (input-stream-p (lisp-stream stream)))

(defmethod output-stream-p ((stream lisp-stream-mixin))
  (output-stream-p (lisp-stream stream)))

(defmethod stream-element-type ((stream bivalent-input-stream))
  '(or character (unsigned-byte 8)))

(defmethod stream-read-char ((stream bivalent-input-stream))
  (code-char (read-byte (lisp-stream stream) nil :eof)))

(defmethod stream-read-byte ((stream bivalent-input-stream))
  (read-byte (lisp-stream stream) nil :eof))

;; stream-unread-char

(defmethod stream-read-char-no-hang ((stream bivalent-input-stream))
  (if (listen (lisp-stream stream))
      (code-char (read-byte (lisp-stream stream)))
      nil))

;; stream-peek-char

(defmethod stream-listen ((stream bivalent-input-stream))
  (listen (lisp-stream stream)))

(defmethod stream-clear-input ((stream bivalent-input-stream))
  (clear-input (lisp-stream stream)))

(defmethod stream-read-sequence ((stream bivalent-input-stream)
                                 (seq vector) &optional start end)
  (unless start (setf start 0))
  (unless end (setf end (length seq)))
  (assert (<= end (length seq)))
  (if (subtypep (array-element-type seq) 'character)
      (loop for count upfrom start
            for i from start below end
            do (setf (aref seq i) (code-char (read-byte stream)))
            finally (return count))
      (read-sequence seq (lisp-stream stream)
                     :start start :end end)))

(defmethod stream-read-sequence ((stream bivalent-input-stream)
                                 (seq cons) &optional (start 0) end)
  (unless start (setf start 0))
  (unless end (setf end (length seq)))
  (let ((seq (nthcdr start seq)))
    (loop for count upfrom start
          for head on seq
          for i below (- end start)
          while head
          do (setf (car head) (read-byte stream))
          finally (return count))))

(defmethod stream-read-sequence ((stream bivalent-input-stream)
                                 (seq null) &optional (start 0) end)
  (declare (ignore end))
  start)

(defmethod stream-element-type ((stream bivalent-output-stream))
  '(or character (unsigned-byte 8)))

(defmethod stream-write-char ((stream bivalent-output-stream) character)
  (write-byte (char-code character) (lisp-stream stream)))

(defmethod stream-write-byte ((stream bivalent-output-stream) byte)
  (write-byte byte (lisp-stream stream)))

(defmethod stream-line-column ((stream bivalent-output-stream))
  nil)

(defmethod stream-finish-output ((stream bivalent-output-stream))
  (finish-output (lisp-stream stream)))

(defmethod stream-force-output ((stream bivalent-output-stream))
  (force-output (lisp-stream stream)))

(defmethod stream-clear-output ((stream bivalent-output-stream))
  (clear-output (lisp-stream stream)))

(defmethod stream-write-sequence ((stream bivalent-output-stream)
                                  (seq vector) &optional (start 0) end)
  (let ((length (length seq)))
    (unless end (setf end length))
    (assert (<= end length)))
  (unless start (setf start 0))
  (when (< end start)
    (cerror "Continue with switched start and end ~s <-> ~s"
            "Stream-write-sequence: start (~S) and end (~S) exchanged."
            start end seq)
    (rotatef start end))
  (cond
    ((subtypep (array-element-type seq) '(unsigned-byte 8))
     (write-sequence seq (lisp-stream stream) :start start :end end))
    ((subtypep (array-element-type seq) 'character)
     (loop for i from start below end
            do (stream-write-char stream (aref seq i))))
    ((subtypep (array-element-type seq) 'integer)
     (loop for i from start below end
           do (stream-write-byte stream (aref seq i)))))
  seq)

(defmethod stream-write-sequence ((stream bivalent-output-stream)
                                  (seq cons) &optional (start 0) end)
  (let ((length (length seq)))
    (unless end (setf end length))
    (assert (<= end length)))
  (unless start (setf start 0))
  (when (< end start)
    (cerror "Continue with switched start and end ~s <-> ~s"
            "Stream-write-sequence: start (~S) and end (~S) exchanged."
            start end seq)
    (rotatef start end))
  (let ((seq (nthcdr start seq)))
    (loop for element in seq
          for i below (- end start)
          while seq
          do (etypecase element
               (character (stream-write-char stream element))
               (integer (stream-write-byte stream element)))))
  seq)

(defmethod stream-write-sequence ((stream bivalent-output-stream)
                                  (seq null) &optional (start 0) end)
  (declare (ignore start end))
  seq)

;;; End bivalent Gray streams

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
