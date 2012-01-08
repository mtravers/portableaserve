;;; MCL layer for ACL sockets.
;;; Based on acl-socket-cmu.lisp and acl-socket-lw.lisp.
;;;
;;; John DeSoi, Ph.D. desoi@users.sourceforge.net


(defpackage :acl-compat.socket
  (:nicknames :socket :acl-socket)
  (:use :common-lisp)
  (:export #:make-socket 
           #:accept-connection
           #:ipaddr-to-dotted 
           #:dotted-to-ipaddr 
           #:ipaddr-to-hostname 
           #:lookup-hostname
           #:remote-host 
           #:remote-port 
           #:local-host 
           #:local-port 
           #:socket-control
           ))

(in-package :socket)

(eval-when (:compile-toplevel :load-toplevel :execute)

(require :opentransport)

;OpenTransport.lisp does not export anything, so do this to make it look a bit cleaner.
(import '(ccl::open-tcp-stream
          ccl::opentransport-tcp-stream
          ccl::opentransport-binary-tcp-stream
          ccl::stream-local-port
          ccl::stream-local-host
          ccl::stream-local-port
          ccl::stream-remote-host
          ccl::stream-remote-port
          ccl::inet-host-name
          ccl::tcp-host-address
          ) )

(defmacro connection-state (s)
  `(ccl::opentransport-stream-connection-state ,s))

(defmacro connection-established (s)
  `(eq :dataxfer (connection-state ,s)) )

)


;;; There is a bug in MCL (4.3.1 tested) where read-sequence and
;;; write-sequence fail with binary tcp streams. These two methods 
;;; provide a work-around.
#-carbon-compat ;should be fixed starting with first carbon version (4.3.5)
(defmethod ccl:stream-write-sequence ((s opentransport-binary-tcp-stream)
                                      (sequence ccl::simple-unsigned-byte-vector)
                                      &key (start 0) end)
  (ccl::stream-write-vector s sequence start (or end (length sequence)))
  s)



#-carbon-compat ;should be fixed starting with first carbon version (4.3.5)
(defmethod ccl:stream-read-sequence ((s opentransport-binary-tcp-stream)
                                         (sequence ccl::simple-unsigned-byte-vector) 
                                         &key (start 0) (end (length sequence)))
  (ccl::stream-read-bytes-to-vector s sequence (- end start) start)
  end)
  

 
(defmethod port ((stream opentransport-tcp-stream))
  (stream-local-port stream) )

(defmethod local-host ((s opentransport-tcp-stream))
  (stream-local-host s))

(defmethod local-port ((s opentransport-tcp-stream))
  (stream-local-port s))

(defmethod remote-host ((s opentransport-tcp-stream))
  (stream-remote-host s))

(defmethod remote-port ((s opentransport-tcp-stream))
  (stream-remote-port s))

;? copied from lispworks - don't think it applies to mcl
(defmethod fd ((s opentransport-tcp-stream))
  (declare (ignore s))
  42)



(defvar *passive-socket-listener-count* 10
  "Default number of listen streams to use.")

; With ACL, an unlimited number of connections can be made to the same passive
; socket instance. Nothing like that here, so we have to create our own stream
; listener to create the "real" sockets as connections are made.


; Create a class to monitor streams so we have a data structure to pass to process-wait
(defclass passive-socket (stream) ;inherit stream so we can handle close
  ((port
    :documentation "Port we are listening on."
    :initform 80
    :initarg :port
    :reader local-port)
   (element-type
    :documentation "Stream element type."
    :initarg :element-type
    :initform '(unsigned-byte 8))
   (count
    :documentation "Number of listening streams to monitor."
    :initform *passive-socket-listener-count*)
   (streams
    :documentation "Array of listen streams."
    :initform nil)
   (index
    :documentation "Index of the last listen stream checked."
    :initform *passive-socket-listener-count*)
   (connect-index
    :documentation "Index of a connected stream, next for processing."
    :initform nil)
   )
  (:documentation "Class used to manage listening streams and connections.") )



(defmethod initialize-instance :after ((listener passive-socket) &rest initargs)
  (declare (ignore initargs))
  (with-slots (streams count port element-type) listener
    (setf streams (make-array count :initial-element nil :adjustable t))
    (dotimes (i count)
      (setf (elt streams i) (new-listen-stream listener)) ) ) )


(defmethod ccl:stream-close ((listener passive-socket))
  (with-slots (streams count) listener
    (dotimes (i count)
      (close (elt streams i)))
    (setf count 0)))


(defmethod new-listen-stream ((listener passive-socket))
  (with-slots (port element-type) listener
    (open-tcp-stream nil port ;use nil host to get a passive connection
                          :element-type element-type) ) )


(defmethod local-host ((listener passive-socket))
  (with-slots (streams count) listener
    (when (> count 0)
      (local-host (elt streams 0)))))
              


; See if one of the streams is established. 
(defmethod find-connection-index ((listener passive-socket))
  (with-slots (count streams index connect-index) listener
    (let ((next (if (< (1+ index) count) (1+ index) 0)))
       (when (connection-established (elt streams next))
         (setf index next
               connect-index next)
         connect-index))))


(defmethod process-connected-stream ((listener passive-socket))
  (with-slots (streams connect-index) listener
    (if (null connect-index) nil
        (let ((s (elt streams connect-index))) ;return the connected stream and set a new one
          (setf (elt streams connect-index) (new-listen-stream listener))
          (setf connect-index nil)
          s) ) ) )
      

;! future - determine how many connects we are getting an dynamically increase the number
;  of listeners if necessary.
(defmethod accept-connection ((listener passive-socket) &key (wait t))
  (if wait
    (ccl:process-wait "accept connection..." #'find-connection-index listener) ;apply repeatedly with process wait
    (find-connection-index listener) )
  (process-connected-stream listener) )


(defun make-socket (&key (remote-host "localhost")
			   local-port
			   remote-port 
			   (connect :active)
			   (format :text)
			   &allow-other-keys)
  (let ((element-type (ecase format
			(:text 'base-char)
			(:binary 'signed-byte)
                        (:bivalent 'unsigned-byte))))
    (ecase connect 
      (:passive
       (make-instance 'passive-socket :port local-port :element-type element-type :direction :io))
      (:active
       (let ((host (if (integerp remote-host) ;aparently the acl version also accepts an integer
                     (ipaddr-to-dotted remote-host)
                     remote-host)))
         (check-type host string)
         (open-tcp-stream host remote-port
                          :element-type element-type))))))



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
  (declare (ignore ignore-cache))
  (inet-host-name ipaddr) )

(defun lookup-hostname (host &key ignore-cache)
  (when ignore-cache
    (warn ":IGNORE-CACHE keyword in LOOKUP-HOSTNAME not supported."))
  (if (stringp host)
    (tcp-host-address host)
    (dotted-to-ipaddr (ipaddr-to-dotted host))))


(defun socket-control (stream &key output-chunking output-chunking-eof input-chunking)
  (declare (ignore stream))
  (warn "SOCKET-CONTROL function not implemented.")
  (when (or output-chunking output-chunking-eof input-chunking)
    (error "Chunking is not yet supported in MCL. Restart the server with argument :chunking nil (turns chunking off).") ) )


(provide 'acl-socket)



