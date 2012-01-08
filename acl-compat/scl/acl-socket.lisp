;; This package is designed for scl.  It implements the
;; ACL-style socket interface on top of scl.
;;
;; Written by Rudi Schlatte, based on the work done by Jochen Schmidt
;; for Lispworks and net.lisp in the port library of CLOCC.
;;
;; This was modified for SCL by Kevin Rosenberg

(defpackage acl-socket
  (:use "MP" "COMMON-LISP")
  #+cl-ssl (:import-from :ssl "MAKE-SSL-CLIENT-STREAM" "MAKE-SSL-SERVER-STREAM")
  (:export #:socket #:make-socket #:accept-connection
   #:ipaddr-to-dotted #:dotted-to-ipaddr #:ipaddr-to-hostname #:lookup-hostname
   #:remote-host #:remote-port #:local-host #:local-port #:socket-control
   #+cl-ssl #:make-ssl-client-stream #+cl-ssl #:make-ssl-server-stream)
  (:nicknames socket))

(in-package socket)

(defclass socket ()
  ((fd :type fixnum
       :initarg :fd
       :reader fd)))

(defmethod print-object ((socket socket) stream)
  (print-unreadable-object (socket stream :type t :identity t)
    (format stream "@~d" (fd socket))))

(defclass server-socket (socket)
  ((element-type :type (member signed-byte unsigned-byte base-char)
		 :initarg :element-type
		 :reader element-type
                 :initform (error "No value supplied for element-type"))
   (port :type fixnum
	 :initarg :port
	 :reader port
         :initform (error "No value supplied for port"))
   (stream-type :type (member :text :binary :bivalent)
                :initarg :stream-type
                :reader stream-type
                :initform (error "No value supplied for stream-type"))))

#+cl-ssl
(defmethod make-ssl-server-stream ((lisp-stream system:lisp-stream)
                                   &rest options)
  (apply #'make-ssl-server-stream (system:fd-stream-fd lisp-stream) options))

(defmethod print-object ((socket server-socket) stream)
  (print-unreadable-object (socket stream :type t :identity nil)
    (format stream "@~d on port ~d" (fd socket) (port socket))))

(defmethod accept-connection ((server-socket server-socket)
			      &key (wait t))
  "Return a bidirectional stream connected to socket, or nil if no
client wanted to initiate a connection and wait is nil."
  ;; fixxme: perhaps check whether we run multiprocessing and use
  ;; sys:wait-until-fd-usable instead of
  ;; mp:process-wait-until-fd-usable here?

  ;; api pipe fitting: wait t ==> timeout nil
  (when (mp:process-wait-until-fd-usable (fd server-socket) :input
                                         (if wait nil 0))
    (let ((stream (sys:make-fd-stream
                   (ext:accept-tcp-connection (fd server-socket))
                   :input t :output t
                   :element-type (element-type server-socket)
                   :auto-close t)))
      (if (eq (stream-type server-socket) :bivalent)
          (excl:make-bivalent-stream stream)
          stream))))

(defun make-socket (&key (remote-host "localhost")
			 local-port
			 remote-port
			 (connect :active)
			 (format :text)
			 &allow-other-keys)
  "Return a stream connected to remote-host if connect is :active, or
something listening on local-port that can be fed to accept-connection
if connect is :passive.

This is an incomplete implementation of ACL's make-socket function!
It was written to provide the functionality necessary to port
AllegroServe.  Refer to
http://franz.com/support/documentation/6.1/doc/pages/operators/socket/make-socket.htm
to read about the missing parts."
  (check-type remote-host string)
  (let ((element-type (ecase format
			(:text 'base-char)
			(:binary 'signed-byte)
                        (:bivalent 'unsigned-byte))))
    (ecase connect
      (:passive
         (make-instance 'server-socket
		        :port local-port
                        :fd (ext:create-inet-listener local-port)
                        :element-type element-type
                        :stream-type format))
      (:active
       (let ((stream (sys:make-fd-stream
                      (ext:connect-to-inet-socket remote-host remote-port)
                      :input t :output t :element-type element-type)))
         (if (eq :bivalent format)
             (excl:make-bivalent-stream stream)
             stream))))))

(defmethod close ((server server-socket) &key abort)
  "Kill a passive (listening) socket.  (Active sockets are actually
streams and handled by their close methods."
  (declare (ignore abort))
  (unix:unix-close (fd server)))

(declaim (ftype (function ((unsigned-byte 32) &key (:values t))
                          (values simple-string))
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
  (ext:host-entry-name (ext:lookup-host-entry ipaddr)))

(defun lookup-hostname (host &key ignore-cache)
  (when ignore-cache
    (warn ":IGNORE-CACHE keyword in LOOKUP-HOSTNAME not supported."))
  (if (stringp host)
      (car (ext:host-entry-addr-list (ext:lookup-host-entry host)))
      (dotted-to-ipaddr (ipaddr-to-dotted host))))

(defgeneric get-fd (stream))

(defmethod get-fd ((stream excl::lisp-stream-mixin))
  (system:fd-stream-fd (excl::lisp-stream stream)))

(defmethod get-fd ((stream system:lisp-stream))
  (system:fd-stream-fd stream))

(defun remote-host (socket-stream)
  (ext:get-peer-host-and-port (get-fd socket-stream)))

(defun remote-port (socket-stream)
    (multiple-value-bind (host port)
        (ext:get-peer-host-and-port (get-fd socket-stream))
      (declare (ignore host))
      port))

(defun local-host (socket-stream)
  (ext:get-socket-host-and-port (get-fd socket-stream)))

(defun local-port (socket-stream)
  (if (typep socket-stream 'socket::server-socket)
      (port socket-stream)
      (multiple-value-bind (host port)
          (ext:get-socket-host-and-port (get-fd socket-stream))
        (declare (ignore host))
        port)))

(defun socket-control (stream &key output-chunking output-chunking-eof input-chunking)
  (declare (ignore stream))
  (warn "SOCKET-CONTROL function not implemented.")
  (when (or output-chunking output-chunking-eof input-chunking)
    (error "Chunking is not yet supported in scl. Restart the server with chunking off.")))


(provide 'acl-socket)
