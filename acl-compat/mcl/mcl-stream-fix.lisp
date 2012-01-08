

(in-package :ccl)

;;; There are several bugs in MCL functions to read sequences prior to 4.3.5; this fixes them



(eval-when (:compile-toplevel :load-toplevel :execute)
  
(let ((ccl:*warn-if-redefine* nil))
  
(defun %io-buffer-read-bytes-to-vector (io-buffer vector bytes start)
  (loop with fill-pointer = start
        with bytes-remaining = bytes
        until (eql 0 bytes-remaining)
        while (if (eql 0 (io-buffer-incount io-buffer))
                (%io-buffer-advance io-buffer t t)        ; eof may be signalled through this -- JCMa 5/13/1999.
                t)
        for buffer =  (io-buffer-inptr io-buffer)
        for read-bytes = (min (io-buffer-incount io-buffer) bytes-remaining)
        do (%copy-ptr-to-ivector buffer 0 vector fill-pointer read-bytes)                
        (incf fill-pointer read-bytes)
        (%incf-ptr (io-buffer-inptr io-buffer) read-bytes) ;; bug fix from akh on 7/28/2002
        (decf bytes-remaining read-bytes)
        (decf (io-buffer-incount io-buffer) read-bytes)
        (incf (io-buffer-bytes-read io-buffer) read-bytes)))

  
;This function is unchanged, but kept for completeness
(defun io-buffer-read-bytes-to-vector (io-buffer vector bytes &optional (start 0))
  (require-type io-buffer 'io-buffer)
  (with-io-buffer-locked (io-buffer)
    (multiple-value-bind (v v-offset) 
                         (array-data-and-offset vector)
      (%io-buffer-read-bytes-to-vector io-buffer v bytes (+ start v-offset)))))

  
(defmethod stream-read-bytes-to-vector ((stream buffered-output-stream-mixin) vector bytes &optional (start 0))
  (io-buffer-read-bytes-to-vector (stream-io-buffer stream) vector bytes start)) ;original fuction did not get the buffer from the stream
  
  
)
)