;;; OpenMCL layer for ACL sockets.
;;; Most everything is already there, just needs to be in the socket package.
;;;
;;; John DeSoi, Ph.D. desoi@users.sourceforget.net

(in-package :acl-compat.socket)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (shadowing-import
   '(;ccl:make-socket                    ; use our own version
     ccl:accept-connection
     ccl:dotted-to-ipaddr 
     ccl:ipaddr-to-hostname
     ccl:lookup-hostname
     ccl:remote-host 
     ccl:remote-port 
     ccl:local-host 
     ccl:local-port))
)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export
   '(accept-connection
     ipaddr-to-dotted 
     dotted-to-ipaddr 
     ipaddr-to-hostname
     lookup-hostname
     remote-host 
     remote-port 
     local-host 
     local-port
     socket-control))
  )


(defclass server-socket ()
  ((socket :initarg :socket :reader socket
           :initform (error "No value supplied for socket"))
   (port :initarg :port
	 :reader port
         :initform (error "No value supplied for port"))))


(defmethod print-object ((socket server-socket) stream)
  (print-unreadable-object (socket stream :type t :identity nil)
    (format stream "listening on port ~d" (port socket))))


(defmethod accept-connection ((server-socket server-socket)
			      &key (wait t))
  "Return a bidirectional stream connected to socket."
  (let ((stream (accept-connection (socket server-socket) :wait wait)))
    (when stream (make-chunked-stream stream))))


(defun make-socket (&rest args
                    &key (connect :active) port
                    &allow-other-keys)
  "Return a stream connected to remote-host if connect is :active, or
something listening on local-port that can be fed to accept-connection
if connect is :passive.
"
  (let ((socket-or-stream (apply #'ccl:make-socket args)))
    (if (eq connect :active)
        (make-chunked-stream socket-or-stream)
        (make-instance 'server-socket :socket socket-or-stream :port port))))


(defmethod close ((server-socket server-socket) &key abort)
  "Kill a passive (listening) socket.  (Active sockets are actually
streams and handled by their close methods."
  (declare (ignore abort))
  (close (socket server-socket)))

(defmethod local-host ((server-socket server-socket))
  (local-host (socket server-socket)))

(defmethod local-port ((server-socket server-socket))
  (local-port (socket server-socket)))

(defmethod ccl:stream-write-vector
    ((stream gray-stream::buffered-bivalent-stream) vector start end)
  (declare (fixnum start end))
  (let ((fn (gray-stream::%writer-function-for-sequence vector)))
    (do* ((i start (1+ i)))
         ((= i end))
      (declare (fixnum i))
      (funcall fn stream (ccl:uvref vector i)))))

(defmethod ccl:stream-read-vector
    ((stream gray-stream::buffered-bivalent-stream) vector start end)
  (declare (fixnum start end))
  (let ((fn (gray-stream::%reader-function-for-sequence vector)))
    (do* ((i start (1+ i)))
         ((= i end) end)
      (declare (fixnum i))
      (let* ((b (funcall fn stream)))
        (if (eq b :eof)
            (return i)
            (setf (ccl:uvref vector i) b))))))

(defclass chunked-stream (de.dataheaven.chunked-stream-mixin::chunked-stream-mixin
                          gray-stream::buffered-bivalent-stream)
  ((plist :initarg :plist :accessor stream-plist)))

(defun make-chunked-stream (lisp-stream &key plist)
  (make-instance 'chunked-stream :lisp-stream lisp-stream :plist plist))

(defmethod local-host ((chunked-stream chunked-stream))
  (local-host (gray-stream::native-lisp-stream chunked-stream)))

(defmethod local-port ((chunked-stream chunked-stream))
  (local-port (gray-stream::native-lisp-stream chunked-stream)))

(defmethod remote-host ((chunked-stream chunked-stream))
  (remote-host (gray-stream::native-lisp-stream chunked-stream)))

(defmethod remote-port ((chunked-stream chunked-stream))
  (remote-port (gray-stream::native-lisp-stream chunked-stream)))


(defun socket-control (stream &key (output-chunking nil oc-p) output-chunking-eof (input-chunking nil ic-p))
  (when oc-p
    (when output-chunking
      (de.dataheaven.chunked-stream-mixin::initialize-output-chunking stream))
    (setf (de.dataheaven.chunked-stream-mixin::output-chunking-p stream)
          output-chunking))
  (when output-chunking-eof
    (de.dataheaven.chunked-stream-mixin::disable-output-chunking stream))
  (when ic-p
    (when input-chunking
      (de.dataheaven.chunked-stream-mixin::initialize-input-chunking stream))
    (setf (de.dataheaven.chunked-stream-mixin::input-chunking-p stream)
          input-chunking)))

; OpenMCL has a built-in ipaddr-to-dotted. But it appears that sometimes
; the log function is being called after the connection is closed and
; it causes nil to be passed to ipaddr-to-dotted. So we wrap ipaddr-to-dotten
; to ensure only non-nil values are passed.

(defun ipaddr-to-dotted (ipaddr &key values)
  (unless (null ipaddr)
    (ccl:ipaddr-to-dotted ipaddr :values values)))

(provide 'acl-socket)
