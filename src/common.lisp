;;;; common.lisp

(in-package :wet-sprockets)


;;; Structs

(defstruct frame
  (fin            nil      :type boolean)
  (rsv1           nil      :type boolean)
  (rsv2           nil      :type boolean)
  (rsv3           nil      :type boolean)
  (opcode         0        :type integer)
  (opcode-type    :unknown :type keyword)
  (mask           nil      :type boolean)
  (payload-length 0        :type integer)
  (masking-key    0        :type integer)
  (payload-data   nil      :type list))


;;; Functions

(defun bytes-to-number (bytes &key (big-endian t))
  (loop for byte across (if big-endian bytes (reverse bytes))
        for shift from (* 8 (- (length bytes) 1)) downto 0 by 8
        sum (ash byte shift)))


(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))


(defun print-hash-table (hash-table)
  (maphash (lambda (k v)
             (format t "~S: ~S~%" k v))
           hash-table))


;;; Time Functions

(defun hms ()
  (multiple-value-bind (sec min hour)
      (decode-universal-time (get-universal-time))
    (format nil "~2,'0D:~2,'0D:~2,'0D" hour min sec)))


;;; Logging Functions

(defun dbgmsg (&rest args)
  (when (>= *log-level* 3)
    (let ((*print-pretty* nil))
      (format *standard-output* "~A: [DEBUG] " (hms))
      (apply #'format (append (list *standard-output*) args)))
    (force-output *standard-output*)))


(defun errmsg (&rest args)
  (apply #'format (append (list *error-output*) args))
  (force-output *error-output*))


(defun infomsg (&rest args)
  (when (>= *log-level* 2)
    (let ((*print-pretty* nil))
      (format *standard-output* "~A: [INFO] " (hms))
      (apply #'format (append (list *standard-output*) args)))
    (force-output *standard-output*)))


(defun infomsg-no-ts (&rest args)
  (when (>= *log-level* 2)
    (let ((*print-pretty* nil))
      (apply #'format (append (list *standard-output*) args)))
    (force-output *standard-output*)))


(defun logmsg (&rest args)
  (when (>= *log-level* 1)
    (let ((*print-pretty* nil))
      (format *standard-output* "~A: " (hms))
      (apply #'format (append (list *standard-output*) args)))
    (force-output *standard-output*)))


(defun logmsg-no-ts (&rest args)
  (when (>= *log-level* 1)
    (let ((*print-pretty* nil))
      (apply #'format (append (list *standard-output*) args)))
    (force-output *standard-output*)))


(defun spammsg (&rest args)
  (when (>= *log-level* 5)
    (let ((*print-pretty* nil))
      (format *standard-output* "~A: [SPAM] " (hms))
      (apply #'format (append (list *standard-output*) args)))
    (force-output *standard-output*)))
