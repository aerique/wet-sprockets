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

(defun defalias (function alias)
  "Defines an alias for FUNCTION, so it can be called with ALIAS as well."
  (setf (symbol-function alias) function))


(defun append1 (lst obj)
  (append lst (list obj)))


(defun bytes-to-number (bytes &key (big-endian t))
  (loop for byte across (if big-endian bytes (reverse bytes))
        for shift from (* 8 (- (length bytes) 1)) downto 0 by 8
        sum (ash byte shift)))


;; FIXME double-float not needed anymore since we set *READ-FLOAT-FORMAT*?
(defun convert-type (key value)
  (let ((type (cdr (assoc key *types* :test 'string=))))
    (cond ((and type (equal type 'asks-or-bids))
           (loop for lst in value
                 for aob = (first lst)
                 ;for sum = (second lst)
                 ;; not as exact but easier to read when debugging
                 for sum = (coerce (second lst) 'double-float)
                 collect (list (parse-number aob :float-format 'double-float)
                               sum)))
          ((and type (equal type 'boolean))
           (if (equal value "0")
               nil
               t))
          ((and type (equal type 'float) (stringp value))
           (parse-number value :float-format 'double-float))
          ((and type (equal type 'integer) (stringp value))
           (parse-integer value))
          ((and type (equal type 'keyword))
           (make-keyword value))
          ((and type (equal type 'ymdhms))
           (encode-datetime-string value))
          ;; XXX bit of hack for Binance account values
          ((and (string= key "balances") (listp value))
           (loop for obj in value
                 when (or (string/= (jsown:val obj "free")   "0.00000000")
                          (string/= (jsown:val obj "locked") "0.00000000"))
                   collect (json2plist obj)))
          (t
           value))))


(defun ends-with (sequence subsequence)
  (let ((seqlen (length sequence))
        (sublen (length subsequence)))
    (when (and (> sublen 0)
               (<= sublen seqlen))
      (equal (subseq sequence (- seqlen sublen)) subsequence))))


(defun fl8 (number)
  "Returns NUMBER as a stringified float with 8 digits."
  (format nil "~,8F" number))


(defun head (sequence &optional (amount 1))
  "Returns AMOUNT elements from the start of SEQUENCE.  If SEQUENCE is shorter
  than AMOUNT it will return SEQUENCE."
  (if (< (length sequence) amount)
      sequence
      (subseq sequence 0 amount)))


(defun json2plist (json)
  (loop for key in (jsown:keywords json)
        append (list (make-keyword key)
                     (convert-type key (jsown:val json key)))))


(defun last1 (sequence)
  (let ((length (length sequence)))
    (when (> length 0)
      (elt sequence (- length 1)))))


(defun make-keyword (string)
  (intern (string-upcase string) :keyword))

(defalias #'make-keyword 'kw)


(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))


(let ((previous-call-time 0))
  ;; This exists because both Kraken and Poloniex have a maximum number of
  ;; times per second their API can get called.  If we don't have a delay we
  ;; risk getting banned if we flood their API accidentally.
  ;; XXX The delay stuff is untested for multi-threading!
  ;; XXX This hasn't really been thought through in any case.
  ;; XXX This is SBCL-only I think.
  ;; XXX If this is called with different delays you have a problem.
  (defun maybe-delay-api-call (&optional (delay 0.5))
    (let* ((current-call-time (/ (get-internal-real-time)
                                 internal-time-units-per-second))
           (dt (- current-call-time previous-call-time)))
      (setf previous-call-time current-call-time)
      (when (< dt delay)
        (sleep dt)))))


(let ((last-nonce 0)
      (offset     1550000110000))  ;; due to accidental usage of other clients
  (defun next-nonce ()
    (let ((nonce (+ (get-universal-time) offset)))
      (when (<= nonce last-nonce)
        (setf nonce (+ last-nonce 1)))
      (setf last-nonce nonce)
      nonce)))


(defun parse-response (response)
  (handler-case
      (jsown:parse (babel:octets-to-string response))
    ;; Usually a Cloudflare HTML page in the response, pollutes the logs.
    (type-error ()
      (logmsg "TYPE-ERROR while parsing response: ~S~%" (type-of response))
      (dbgmsg "(This is usually a Cloudflare HTML page dump in the logs.)~%")
      (spammsg "---~%~S~%---~%" response)
      (jsown:new-js ("success" nil)
                    ("code" "0")
                    ("error" "TYPE-ERROR while parsing response to JSON")
                    ("msg" "TYPE-ERROR while parsing response to JSON")
                    ("message" "TYPE-ERROR while parsing response to JSON")))
    (t (condition)
      (logmsg "Error while parsing response: ~S~%" condition)
      (jsown:new-js ("success" nil)
                    ("code" "0")  ; for Binance API
                    ("error" "Error while parsing response to JSON")
                    ("msg" "Error while parsing response to JSON")  ; Binance
                    ("message" "Error while parsing response to JSON")))))


(defun print-bytes-as-binary (bytes &optional (stream t))
  (loop for byte across bytes
        do (format stream "~8,'0b " byte))
  (terpri stream))


(defun print-bytes-as-hex (bytes &optional (stream t))
  (loop for byte across bytes
        do (format stream "0x~2,'0x " byte))
  (terpri stream))


(defun print-hash-table (hash-table)
  (maphash (lambda (k v)
             (format t "~S: ~S~%" k v))
           hash-table))


(defun read-bytes (stream)
  (loop for byte = (read-line stream nil nil)
        while byte
        collect byte))


(defun response-error-p (object)
  (and (listp object)
       (equal :error (first object))))


(defun sf (number)
  "COERCE NUMBER to SINGLE-FLOAT."
  (coerce number 'single-float))


(defun split-currency-pair (currency-pair)
  (let* ((pos (position #\_ currency-pair))
         (split (when pos
                  (list (subseq currency-pair 0 pos)
                        (subseq currency-pair (+ pos 1))))))
    (when (and split
               (nth 0 split)
               (nth 1 split)
               (> (length (nth 0 split)) 0)
               (> (length (nth 1 split)) 0))
      split)))


(defun starts-with (sequence subsequence)
  (let ((sublen (length subsequence)))
    (when (and (> sublen 0)
               (<= sublen (length sequence)))
      (equal (subseq sequence 0 sublen) subsequence))))


(defun string-after (substring string)
  (loop with first-substring-char = (elt substring 0)
        with substr-len = (length substring)
        for i from 0 to (- (length string) substr-len)
        for current-char = (elt string i)
        do (when (and (char= current-char first-substring-char)
                      (string= substring (subseq string i (+ i substr-len))))
             (return-from string-after (subseq string (+ i substr-len))))))


(defun string-before (substring string)
  (loop with first-substring-char = (elt substring 0)
        with substr-len = (length substring)
        for i from 0 to (- (length string) substr-len)
        for current-char = (elt string i)
        do (when (and (char= current-char first-substring-char)
                      (string= substring (subseq string i (+ i substr-len))))
             (return-from string-before (subseq string 0 i)))))


(defun tail (sequence &optional (amount 1))
  "Returns AMOUNT elements from the end of SEQUENCE.  If SEQUENCE is shorter
  than AMOUNT it will return SEQUENCE."
  (let ((length (length sequence)))
    (if (< length amount)
        sequence
        (subseq sequence (- (length sequence) amount)))))


;;; Digest Functions

(defun sha256 (string)
  (ironclad:digest-sequence :sha256
                            (ironclad:ascii-string-to-byte-array string)))


(defun hmac-sha256 (secret string)
  (let ((hmac (ironclad:make-hmac (ironclad:ascii-string-to-byte-array secret)
                                  :sha256))
        (bytes (ironclad:ascii-string-to-byte-array string)))
    (ironclad:update-hmac hmac bytes)
    (ironclad:byte-array-to-hex-string (ironclad:hmac-digest hmac))))


(defun hmac-sha512 (secret string)
  (let ((hmac (ironclad:make-hmac (ironclad:ascii-string-to-byte-array secret)
                                  :sha512))
        (bytes (ironclad:ascii-string-to-byte-array string)))
    (ironclad:update-hmac hmac bytes)
    (ironclad:byte-array-to-hex-string (ironclad:hmac-digest hmac))))


;;; Terminal Functions
;;;
;;; (code-char 27) = ESC

(defun clear-terminal ()
  (format t "~C~C" (code-char 27) #\c))


;;; Time Functions

(defun encode-datetime-string (datetime-string)
  (let ((yyyy (parse-integer (subseq datetime-string  0  4)))
        (mon  (parse-integer (subseq datetime-string  5  7)))
        (dd   (parse-integer (subseq datetime-string  8 10)))
        (hh   (parse-integer (subseq datetime-string 11 13)))
        (mm   (parse-integer (subseq datetime-string 14 16)))
        (ss   (parse-integer (subseq datetime-string 17 19))))
    (encode-universal-time ss mm hh dd mon yyyy)))


(defun encode-ymdhms (year month day hours minutes seconds)
  (encode-universal-time seconds minutes hours day month year))


(defun hms ()
  (multiple-value-bind (sec min hour)
      (decode-universal-time (get-universal-time))
    (format nil "~2,'0D:~2,'0D:~2,'0D" hour min sec)))


(defun universal-time-to-iso (universal-time)
  (multiple-value-bind (sec min hr day mon year)
      (decode-universal-time universal-time)
    (format nil "~2,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D"
            year mon day hr min sec)))


(defun universal-time-to-ymd (universal-time)
  (multiple-value-bind (sec min hr day mon year)
      (decode-universal-time universal-time)
    (declare (ignore sec min hr))
    (format nil "~2,'0D-~2,'0D-~2,'0D" year mon day)))


(defun universal-time-to-ymdhms (universal-time)
  (multiple-value-bind (sec min hr day mon year)
      (decode-universal-time universal-time)
    (format nil "~2,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
            year mon day hr min sec)))


(defun unix-time (universal-time)
  (- universal-time +unix-epoch-as-universal-time+))


(defun unix-time-to-universal-time (unix-time)
  (+ unix-time +unix-epoch-as-universal-time+))


(defun ymdhms ()
  (multiple-value-bind (sec min hour day mon year)
      (decode-universal-time (get-universal-time))
    (format nil "~2,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D"
            year mon day hour min sec)))


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


;;; Daemon Functions

;; This is just an example and should accept a body with functions.
(defun check-every (&optional (interval 3599))
  (loop do (format t "=== ~A ===~%" (ymdhms))
           ; ...
           (sleep interval)))


;;; Modify Macros

(define-modify-macro appendf (&rest lists) append
  "Modify-macro for APPEND. Appends LISTS to the place designated by the first
  argument.")


(define-modify-macro appendf1 (object) append1)
