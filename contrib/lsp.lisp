;;; --------------------------------------------------------------------
;;; Lisp Server Pages (LSP) -- Implements something like Java Server
;;; Pages (JSP), but for Lisp.
;;;
;;; Copyright 2001, 2002 I/NET Inc. (http://www.inetmi.com/)
;;; John Wiseman (jjwiseman@yahoo.com)
;;; 2002-06-10
;;; Licensed under the MIT license:
#||
Copyright (c) 2001, 2002 I/NET Inc.

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
||#
;;;
;;;
;;; * Introduction
;;; 
;;; Java Server Pages are a way to make web pages that are dynamic, by
;;; embedding Java code in HTML.  Similarly, Lisp Server Pages allow
;;; you to make dynamic web pages that contain Lisp code.
;;;
;;; To publish an LSP page, call the PUBLISH-LSP function:
;;;
;;; PUBLISH-LSP (&key path file server)                    [function]
;;; Publishes the LSP file FILE at the URL prefix PATH, on SERVER
;;; (defaults to the default AllegroServe server, *wserver*). Example:
;;; (publish-lsp :path "/temp.html" :file "/Users/wiseman/src/temperature.lsp")
;;;
;;; An LSP file looks just like an HTML file, except for two new tags:
;;; <% ... %> and <%= ... %>.
;;;
;;; <% ... %> is a scriptlet tag (to use the JSP terminology), and
;;; wraps lisp code.  For example, <% (dotimes (i 10) (beep)) %>. The
;;; code inside the tag is executed each time the page is requested.
;;;
;;; <%= ... %> is an expression tag, and the effect of this tag is to
;;; evaluate the contents as if they were wrapped with the Franz
;;; net.html.generator:html macro.  For example,
;;;
;;;   <%= (:h1 "header") "hi" (:princ-safe (generate-footer)) %>
;;;
;;; is equivalent to
;;;
;;;   (net.html.generator:html
;;;     (:h1 "header")
;;;     "hi"
;;;     (:princ-safe (generate-footer)))
;;;
;;; which will output something like the following HTML:
;;;
;;;  <h1>header</h1>hi<hr>2002-06-09
;;;
;;; During execution of LSP code, the following two variables will be
;;; bound:
;;;
;;;   request -- The HTTP request object containing all the
;;;              information about the request.
;;;   entity  -- The information passed to the publish-lsp function.
;;;
;;; (See the AllegroServe documentation for more information on these
;;; objects.)
;;;
;;;
;;; * Tips
;;;
;;; Expressions can be used inside HTML attributes, e.g.,
;;;
;;;  <img src="<%= (img-title request) %>">
;;;
;;; Scriptlets do not need to be complete lisp forms, as long as the
;;; page as a whole is syntactically valid, e.g.,
;;;
;;;   <% (dotimes (i 10) %>
;;;     <img src="mr-yuck.jpg">
;;;   <% ) %>
;;;
;;;
;;; * Implementation Notes and Caveats
;;;
;;; LSP pages are converted to strings containing lisp code, which are
;;; then compiled and cached.  If the source file containing the lsp
;;; code is modified, the next time a request is made for that page
;;; the code will be recompiled and recached.
;;;
;;; In my first attempt to do this, I tried to construct forms instead
;;; of strings.  That just made it trickier to separate forms across
;;; <% ... %> tags (see the dotimes example above).  Just because it's
;;; bad that other languages are often *limited* to manipulating code
;;; as strings doesn't mean there aren't times where it's appropriate.
;;;
;;; There's nothing like JSP's directives or declarations.
;;;
;;; LSP Requires Franz' AllegroServe
;;; (http://allegroserve.sourceforge.net/) or Portable AllegroServe
;;; (http://portableaserve.sourceforge.net/).
;;;
;;; See http://sourceforge.net/projects/lsp for a more serious attempt
;;; at doing this right, by Sunil Mishra and Tim Bradshaw.

(require :aserve)


(defpackage :com.lemonodor.lsp
  (:use #:common-lisp)
  (:export #:publish-lsp #:request #:entity))

(in-package :com.lemonodor.lsp)


(defun publish-lsp (&key path file (server net.aserve:*wserver*))
  "Publishes an LSP file.  PATH is a string containing the name part
   of the URL at which to publish the file, e.g. \"/math/sum.lsp\";
   FILE is a pathname that specifies the file containing the page
   to publish."
  (net.aserve:publish :path path
		      :server server
		      :function #'(lambda (request entity)
				    (do-lsp-request request entity file))))

(defun do-lsp-request (request entity file)
  "Handles the request for an LSP URL." 
  (funcall (get-lsp-function file) request entity))


(defvar *lsp-functions* (make-hash-table :test #'equal)
  "The table mapping LSP filespecs to function-time pairs.")

(defun get-lsp-function (file)
  "Returns the function implementing a given LSP file.  Builds and
   compiles the function the first time it's requested, or if the file
   has been modified."
  (let ((func.time (gethash file *lsp-functions*)))
    (if (or (null func.time)
	    (> (file-write-date file) (cdr func.time)))
      (register-lsp-function file
			     (construct-lsp-function (contents-of-file file)))
      (car func.time))))

(defun register-lsp-function (file function)
  (setf (gethash file *lsp-functions*) (cons function (get-universal-time)))
  function)


(defun construct-lsp-function (lsp-string)
  "Builds and compiles the request-handling LSP function for the page
   whose contents are in LSP-STRING."
  (let ((form 
	 `(lambda (request entity)
	    (net.aserve:with-http-response (request entity)
	      (net.aserve:with-http-body (request entity)
                ;; We punt hard on the issue of package.
                ,(read-from-string
                  (format nil "(progn ~A)"
                          (construct-lsp-body-string lsp-string))))))))
    (compile nil form)))


(defun contents-of-file (pathname)
  "Returns a string with the entire contents of the specified file."
  ;; This is excl:file-contents in ACL.
  (with-output-to-string (contents)
    (with-open-file (in pathname :direction :input)
      (let* ((buffer-size 4096)
             (buffer (make-string buffer-size)))
        (labels ((read-chunks ()
                   (let ((size (read-sequence buffer in)))
                     (if (< size buffer-size)
                       (princ (subseq buffer 0 size) contents)
                       (progn
                         (princ buffer contents)
                         (read-chunks))))))
          (read-chunks))))))


;; (i) Converts text outside <% ... %> tags (straight HTML) into calls
;; to net.html.generator.html, (ii) Text inside <% ... %>
;; ("scriptlets") is straight lisp code, (iii) Text inside <%= ... %>
;; ("expressions") becomes the body of the net.html.generator:html
;; macro.

(defun construct-lsp-body-string (lsp-string &optional (start 0))
  "Takes a string containing an LSP page and returns a string
   containing the lisp code that implements that page."
  (multiple-value-bind (start-tag start-code tag-type)
      (next-code lsp-string start)
    (if (not start-tag)
      (format nil "(net.html.generator:html ~S)" (subseq lsp-string start))
      (let ((end-code (search "%>" lsp-string :start2 start-code)))
	(if (not end-code)
	  (error "EOF reached in LSP inside open '<%' tag.")
	  (format nil "(net.html.generator:html ~S) ~A ~A"
		  (subseq lsp-string start start-tag)
		  (format nil (tag-template tag-type)
			  (subseq lsp-string start-code end-code))
		  (construct-lsp-body-string lsp-string (+ end-code 2))))))))


;; Finds the next scriptlet or expression tag in LSP source.  Returns
;; nil if none are found, otherwise returns 3 values:
;;  1. The position of the opening bracket (<) of the tag.
;;  2. The position of the contents of the tag.
;;  3. The type of tag (:scriptlet or :expression).

(defun next-code (string start)
  (let ((start-tag (search "<%" string :start2 start)))
    (if (not start-tag)
      nil
      (if (and (> (length string) (+ start-tag 2))
	       (eql (char string (+ start-tag 2)) #\=))
	(values start-tag (+ start-tag 3) :expression)
	(values start-tag (+ start-tag 2) :scriptlet)))))


;; Given a tag type (:scriptlet or :expression), returns a format
;; string to be used to generate source code from the contents of the
;; tag.

(defun tag-template (tag-type)
  (ecase tag-type
    ((:scriptlet) "~A")
    ((:expression) "(net.html.generator:html ~A)")))


