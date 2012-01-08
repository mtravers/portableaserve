;; This package is designed for clisp.  It implements the
;; ACL-style socket interface on top of clisp.
;;
;; Written by Rudi Schlatte, based on the work done by Jochen Schmidt
;; for Lispworks and net.lisp in the port library of CLOCC.

(in-package :acl-socket)

(defclass server-socket ()
  ((port :type fixnum
	 :initarg :port
	 :reader port)
   (stream-type :type (member :text :binary :bivalent)
                :initarg :stream-type
                :reader stream-type
                :initform (error "No value supplied for stream-type"))
   (clisp-socket-server :initarg :clisp-socket-server
                        :reader clisp-socket-server)))

(defmethod print-object ((server-socket server-socket) stream)
  (print-unreadable-object (server-socket stream :type t :identity nil)
    (format stream "@port ~d" (port server-socket))))

(defun %get-element-type (format)  
  (ecase format
    (:text 'character)
    (:binary '(unsigned-byte 8))
    (:bivalent '(unsigned-byte 8))) )

(defgeneric accept-connection (server-socket &key wait))
(defmethod accept-connection ((server-socket server-socket)
			      &key (wait t))
  "Return a bidirectional stream connected to socket, or nil if no
client wanted to initiate a connection and wait is nil."
  (when (cond ((numberp wait)
               (socket-wait (clisp-socket-server server-socket) wait))
              (wait (socket-wait (clisp-socket-server server-socket)))
              (t (socket-wait (clisp-socket-server server-socket) 0)))
    (let ((stream (socket-accept (clisp-socket-server server-socket)
                                 :element-type (%get-element-type 
                                                 (stream-type server-socket))
                                 )))
      (if (eq (stream-type server-socket) :bivalent)
          (make-bivalent-stream stream)
          stream))))


(defun make-socket (&key (remote-host "localhost")
			 local-port
			 remote-port 
			 (connect :active)
			 (format :text)
			 &allow-other-keys)
  "Return a stream connected to remote-host if connect is :active, or
something listening on local-port that can be fed to accept-connection
if connect is :passive."
  (check-type remote-host string)
  (ecase connect 
    (:passive
      (make-instance 'server-socket
                     :port local-port
                     :clisp-socket-server (socket-server local-port)
                     :stream-type format))
    (:active
      (let ((stream (socket-connect
                      remote-port remote-host
                      :element-type (%get-element-type format)
                      )))
        (if (eq format :bivalent)
          (make-bivalent-stream stream)
          stream)))))

(defmethod close ((server-socket server-socket) &key abort)
  "Kill a passive (listening) socket.  (Active sockets are actually
streams and handled by their close methods."
  (declare (ignore abort))
  (socket-server-close (clisp-socket-server server-socket)))

(declaim (ftype (function ((unsigned-byte 32)) (values simple-string))
		ipaddr-to-dotted))
(defun ipaddr-to-dotted (ipaddr &key values)
  (declare (type (unsigned-byte 32) ipaddr))
  (let ((a (logand #xff (ash ipaddr -24)))
	(b (logand #xff (ash ipaddr -16)))
	(c (logand #xff (ash ipaddr -8)))
	(d (logand #xff ipaddr)))
    (if values
	(values a b c d)
      (format nil "~d.~d.~d.~d" a b c d))))

(defun string-tokens (string)
  (labels ((get-token (str pos1 acc)
                      (let ((pos2 (position #\Space str :start pos1)))
                        (if (not pos2)
                            (nreverse acc)
                          (get-token str (1+ pos2) (cons (read-from-string (subseq str pos1 pos2))
                                                         acc))))))
    (get-token (concatenate 'string string " ") 0 nil)))

(declaim (ftype (function (string &key (:errorp t))
                          (values (unsigned-byte 32)))
		dotted-to-ipaddr))
(defun dotted-to-ipaddr (dotted &key (errorp t))
  (declare (string dotted))
  (if errorp
      (let ((ll (string-tokens (substitute #\Space #\. dotted))))
	(+ (ash (first ll) 24) (ash (second ll) 16)
	   (ash (third ll) 8) (fourth ll)))
    (ignore-errors 
      (let ((ll (string-tokens (substitute #\Space #\. dotted))))
	(+ (ash (first ll) 24) (ash (second ll) 16)
	   (ash (third ll) 8) (fourth ll))))))


(defun ipaddr-to-hostname (ipaddr &key ignore-cache)
  (when ignore-cache
    (warn ":IGNORE-CACHE keyword in IPADDR-TO-HOSTNAME not supported."))
  (posix::hostent-name (posix:resolve-host-ipaddr ipaddr)))

(defun lookup-hostname (host &key ignore-cache)
  (when ignore-cache
    (warn ":IGNORE-CACHE keyword in LOOKUP-HOSTNAME not supported."))
  (if (stringp host)
      (car (posix::hostent-addr-list (posix:resolve-host-ipaddr host)))
      (dotted-to-ipaddr (ipaddr-to-dotted host))))

(defgeneric get-clisp-stream (stream))

(defmethod get-clisp-stream ((stream gray-stream::native-lisp-stream-mixin))
  (gray-stream::native-lisp-stream stream))

(defmethod get-clisp-stream ((stream t))
  (the stream stream))

(defun remote-host (socket-stream)
  (dotted-to-ipaddr
   (nth-value 0 (socket-stream-peer (get-clisp-stream socket-stream) t))))

(defun remote-port (socket-stream)
  (nth-value 1 (socket-stream-peer (get-clisp-stream socket-stream) t)))

(defun local-host (socket-stream)
  (dotted-to-ipaddr
   (nth-value 0 (socket-stream-local (get-clisp-stream socket-stream) t))))

(defun local-port (socket-stream)
  (nth-value 1 (socket-stream-local (get-clisp-stream socket-stream) t)))

;; Now, throw chunking in the mix

(defclass chunked-stream (de.dataheaven.chunked-stream-mixin::chunked-stream-mixin
                          gray-stream::buffered-bivalent-stream)
  ((plist :initarg :plist :accessor stream-plist)))


(defun make-bivalent-stream (lisp-stream &key plist)
  (make-instance 'chunked-stream :lisp-stream lisp-stream :plist plist))


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

(provide 'acl-socket)
