
#||
Subject: Session Support in aserve
Date: 18 Jul 2002 16:22:38 -0400
From: Brendan Burns <bburns@genet.cs.umass.edu>
To: opensource@franz.com



Hey folks, I've implemented persistent session support for
AllegroServe.  For every user who connects to AllegroServe, the server
maintains a global hash-table until the user has been inactive for a
specified period of time and then the session is removed.  This is
similar to session support in Java Servlets and PHP.  The code is
attached including several samples (look at "with-session-test" for a
nice starter)

This code is released under the GPL and Franz should feel free to
incorporate it into future versions of allegro serve.

??/Bugs -> Me

- --brendan
||#

(in-package :net.aserve)
(defvar *global-sessions* (make-hash-table :test #'equal))
(defvar *session-reaper-thread* nil)

(defparameter *session-life* 3600)

(defun set-session-limit (time)
  (setf *session-life* time))

(defun get-session-limit ()
  *session-life*)

(defun session-reaper ()
  (loop while (> (hash-table-size *global-sessions*) 0)
      do (let ((test-time (get-universal-time)))
	   (maphash #'(lambda (key val)
			(when (> (- test-time (car val)) *session-life*)
			  (remhash key *global-sessions*)))
		    *global-sessions*))
      do (sleep 30))
  (setf *session-reaper-thread* nil))
  

(defun reset-sessions ()
  (setf *global-sessions* (make-hash-table :test #'equal)))

(defun make-session ()
  (if (not *session-reaper-thread*)
      (setf *session-reaper-thread* 
	(mp::process-run-function "session-reaper" #'session-reaper)))
  (list (get-universal-time) (make-hash-table)))

(defun get-session (req &key (create t))
  (let ((session-id (get-cookie-value req "aserve_session"))
	(session nil))
    (if session-id 	
	(if (gethash session-id *global-sessions*)
	    (setf session (gethash session-id *global-sessions*))
	  (if create 
	      (setf session
		(setf (gethash session-id *global-sessions*)
		  (make-session)))))
      (if create 
	  (let ((new-id (string (gensym))))
	    (setf session (make-session))
	    (setf (gethash new-id *global-sessions*) session)
	    (set-cookie-header req :name "aserve_session" 
			       :value new-id))))
    (if session (setf (car session) (get-universal-time)))
    (cadr session)))

(defmacro with-session (req session-name &body body)
  `(let ((,session-name (get-session ,req)))
     (unwind-protect
	 (progn ,@body))))

(defun get-cookie-hash-table (req &optional (external-format *default-aserve-external-format*))
  (let ((result (make-hash-table)))
    (mapc (lambda (name-value-pair) 
	      (setf (gethash (car name-value-pair) result) 
		   (cdr name-value-pair))
	      (print (gethash (car name-value-pair) result)))
	    (get-cookie-values req external-format))
    result))

(defmacro with-cookie-hash (req hash-name &body body)
  `(let ((,hash-name (get-cookie-hash-table ,req)))
     (unwind-protect
	 (progn ,@body))))

(defun get-cookie-value (req name)
  (let ((result nil))
    (mapc (lambda (name-value-pair)
	    (if (equal name (car name-value-pair))
		  (setf result (cdr name-value-pair))))
	    (get-cookie-values req))
    result))

(defmacro with-cookie-value (req value name &body body)
  `(let ((,value (get-cookie-value ,req ,name)))
     (unwind-protect
	 (progn ,@body))))
    

#+TEST
(defun with-hash-test (req ent)
  (with-http-response (req ent)
    (with-cookie-hash req hash
      (with-http-body
       (req ent)
       (html
	(:html
	 (:body (:princ (gethash "counter" hash)))))))))

#+TEST
(defun cookie-test (req ent)
  (with-http-response
   (req ent)
   (with-cookie-value req counter "counter"
    (with-http-body
     (req ent)
     (html
      (:html
       (:body 
	((:font :color "red") (:princ counter)))))))))

#+TEST
(defun cookie-counter (req ent)
  (with-http-response 
   (req ent)
   (let ((val 0))
     (with-cookie-value 
      req counter "counter"
      (if counter (setf val (parse-integer counter)))
      (set-cookie-header req :name "counter" 
			 :value (format nil "~A" (1+ val)))
      (with-http-body 
       (req ent)
       (html
	(:html (:head (:title "Counter Tester"))
	       (:body
		((:font :color "red") "The value is: " (:princ val))))))))))
  

#+TEST
(defun session-test (req ent)
  (with-http-response (req ent)
    (let ((session (get-session req)))
     (with-http-body (req ent)
      (html
       (:html (:head (:title "Session Test"))
	      (:body
	       ((:font :color "blue") "Your session "
		(:princ (if (gethash 'val session) "is present" 
			  (progn
			    (setf (gethash 'val session) t)
			    "has been started")))))))))))
	  
#+TEST
(defun with-session-test (req ent)
  (with-http-response (req ent)
   (with-session req session
          (with-http-body (req ent)
      (html
       (:html (:head (:title "Session Test"))
	      (:body
	       ((:font :color "blue") "Your session "
		(:princ (if (gethash 'val session) "is present" 
			  (progn
			    (setf (gethash 'val session) t)
			    "has been started")))))))))))

(defun session-reset (req ent)
  (with-http-response (req ent)
    (with-http-body (req ent)
     (reset-sessions)
     (html
      (:html (:head (:title "Session Reset"))
	     (:body "All sessions have been cleared"))))))

#+YOU-OUGHT-TO-DO-THIS
(publish :path "/admin/reset-sessions" :function #'session-reset)

