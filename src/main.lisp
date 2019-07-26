;;;; main.lisp

(in-package :wet-sprockets)


;;; Functions

(defun connection-upgrade-header-p (headers)
  (let ((connection-header (assoc :connection headers)))
    (if (and connection-header
             (string= "upgrade" (string-downcase (cdr connection-header))))
        t
        (progn (errmsg "Invalid Connection-header in response: ~S~%"
                       connection-header)
               nil))))


(defun status-code-101-p (status-code reason)
  (if (= status-code 101)
      t
      (progn (errmsg "Response returned status code ~D: ~A~%"
                     status-code reason)
             nil)))


(defun upgrade-websocket-header-p (headers)
  (let ((upgrade-header (assoc :upgrade headers)))
    (if (and upgrade-header
             (string= "websocket" (string-downcase (cdr upgrade-header))))
        t
        (progn (errmsg "Invalid Upgrade-header in response: ~S~%"
                       upgrade-header)
               nil))))


(defun validate-sec-websocket-accept-header (headers key)
  (let ((sec-websocket-accept-header (assoc :sec-websocket-accept headers))
        (expected (base64:usb8-array-to-base64-string
                   (ironclad:digest-sequence :sha1
                    (ironclad:ascii-string-to-byte-array
                     (mkstr key "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"))))))
    (if (and sec-websocket-accept-header
             (string= expected (cdr sec-websocket-accept-header)))
        t
        (progn (errmsg "Invalid Sec-Websocket-Accept-header in response: ~S ~
                        (expected \"Sec-WebSocket-Accept: ~S\")~%"
                       sec-websocket-accept-header expected)
               nil))))


(defun validate-sec-websocket-protocol-header (headers protocol)
  (let ((sec-websocket-protocol-header (assoc :sec-websocket-protocol
                                              headers)))
    (if (or (not sec-websocket-protocol-header)
            (and sec-websocket-protocol-header
                 (string= protocol (string-downcase
                                    (cdr sec-websocket-protocol-header)))))
        t
        (progn (errmsg "Invalid Sec-Websocket-Protocol-header in response: ~
                        ~S (expected \"Sec-WebSocket-Protocol: ~S\")~%"
                       sec-websocket-protocol-header protocol)
               nil))))


(defun connect (url &optional protocol)
  (let* (;(common::*verbose* t)        ; XXX for dev
         ;(drakma::*header-stream* t)  ; XXX for dev
         (*random-state* (make-random-state t))
         (key (base64:usb8-array-to-base64-string
               (make-array 16 :element-type '(unsigned-byte 8)
                              :initial-contents (loop repeat 16
                                                      collect (random 255)))))
         (headers `(("Upgrade"               . "WebSocket")
                    ("Connection"            . "Upgrade")
                    ("Sec-WebSocket-Key"     . ,key)
                    ("Sec-WebSocket-Version" . 13))))
    (when protocol
      (setf headers
            (append headers `(("Sec-WebSocket-Protocol" . ,protocol)))))
    (handler-case
        (multiple-value-bind (body-or-stream status-code headers uri stream
                              must-close reason)
            (sb-ext:with-timeout *default-timeout*
              (drakma:http-request url :close nil :additional-headers headers
                                   :want-stream t))
          (dbgmsg "~S~%" headers)
          (if (and (status-code-101-p status-code reason)
                   (upgrade-websocket-header-p headers)
                   (connection-upgrade-header-p headers)
                   (validate-sec-websocket-accept-header headers key)
                   (validate-sec-websocket-protocol-header headers protocol))
              (setf *connection* (list :body-or-stream body-or-stream
                                       :status-code status-code
                                       :headers headers :uri uri :stream stream
                                       :must-close must-close :reason reason
                                       :key key))
              (progn (when stream  ; can STREAM ever be NIL?
                       (close stream))
                     (errmsg "Error connecting to ~A with protocol: ~A~%"
                             url protocol)
                     nil)))
      (t (condition) (errmsg "Error connecting to ~A with protocol ~A: ~S~%"
                             url protocol condition)
                     nil))))


(defun keyword-to-opcode (keyword)
  (case keyword
    (:continuation #x0)
    (:text         #x1)
    (:binary       #x2)
    (:close        #x8)
    (:ping         #x9)
    (:pong         #xa)
    (otherwise (error "Unknown websockets opcode keyword"))))


;; 1005, 1006 and 1015 (:reserved-value) are missing and you need to handle
;; them yourself, because we don't know how to handle them in this case.
(defun keyword-to-status-code (keyword)
  (case keyword
    (:normal-closure         1000)
    (:going-away             1001)
    (:protocol-error         1002)
    (:unacceptable-data      1003)
    (:reserved               1004)
    (:inconsistent-data      1007)
    (:policy-violation       1008)
    (:message-too-big        1009)
    (:unnegotiated-extension 1010)
    (:unexpected-condition   1011)))


(defun mask-payload-data (masking-key payload-data)
  "PAYLOAD-DATA should be an array with element type (UNSIGNED-BYTE 8)."
  (loop for byte across payload-data
        for i from 0
        collect (logxor byte (elt masking-key (mod i 4))) into bytes
        finally (return (coerce bytes 'vector))))


(defun payload-data-to-string (payload-data)
  (babel:octets-to-string
   (make-array (length payload-data) :element-type '(unsigned-byte 8)
               :initial-contents payload-data)))


(defun payload-data-to-json (payload-data)
  (jsown:parse (payload-data-to-string payload-data)))


(defun opcode-to-keyword (opcode)
  (case opcode
    (#x0 :continuation)
    (#x1 :text)
    (#x2 :binary)
    (#x8 :close)
    (#x9 :ping)
    (#xa :pong)
    (otherwise :unknown)))


(defun status-code-to-keyword (status-code)
  (case status-code
    (1000 :normal-closure)
    (1001 :going-away)
    (1002 :protocol-error)
    (1003 :unacceptable-data)
    (1004 :reserved)
    (1005 :reserved-value)
    (1006 :reserved-value)
    (1007 :inconsistent-data)
    (1008 :policy-violation)
    (1009 :message-too-big)
    (1010 :unnegotiated-extension)
    (1011 :unexpected-condition)
    (1015 :reserved-value)
    (otherwise :unknown)))


;; FIXME not supported: FIN
;; FIXME not supported: masked data
(defun read-frame (stream)
  (let ((byte (read-byte stream nil nil))
        fin rsv1 rsv2 rsv3 opcode opcode-type mask payload-length payload-data)
    (unless byte
      (return-from read-frame nil))
    (setf fin         (logand byte #b10000000)
          rsv1        (logand byte #b01000000)
          rsv2        (logand byte #b00100000)
          rsv3        (logand byte #b00010000)
          opcode      (logand byte #b00001111)
          opcode-type (opcode-to-keyword opcode))
    (unless (= 128 fin)
      (error "Fragmentation not supported"))
    (setf byte (read-byte stream nil nil))
    (setf mask           (logand byte #b #b10000000)
          payload-length (logand byte #b #b01111111))
    (cond ((= payload-length 126)
           (setf payload-length
                 (bytes-to-number (vector (read-byte stream nil nil)
                                          (read-byte stream nil nil)))))
          ((= payload-length 127)
           (setf payload-length
                 (bytes-to-number (vector (read-byte stream nil nil)
                                          (read-byte stream nil nil)
                                          (read-byte stream nil nil)
                                          (read-byte stream nil nil)
                                          (read-byte stream nil nil)
                                          (read-byte stream nil nil)
                                          (read-byte stream nil nil)
                                          (read-byte stream nil nil))))))
    ;; FIXME not reading masking-key, assuming we're a client
    (setf payload-data (loop repeat payload-length
                             collect (read-byte stream nil nil)))
    (make-frame :fin            (= 128 fin)
                :rsv1           (=  64 rsv1)
                :rsv2           (=  32 rsv2)
                :rsv3           (=  16 rsv3)
                :opcode         opcode
                :opcode-type    opcode-type
                :mask           (= 128 mask)
                :payload-length payload-length
                :payload-data   payload-data)))


;(defun set-utf-8 (flexi-stream)
;  (setf (flexi-streams:flexi-stream-external-format flexi-stream) :utf-8))


;; XXX no support for CLIENT=NIL, i.e. server mode not supported
(defun write-frame (stream opcode-keyword &key (client t) data)
  (let* ((octets         (if (equal opcode-keyword :text)
                             (babel:string-to-octets data)
                             data))
         (fin            t)
         (rsv1           nil)
         (rsv2           nil)
         (rsv3           nil)
         (opcode         (keyword-to-opcode opcode-keyword))
         (mask           client)
         (masking-key    (vector (random 256) (random 256)
                                 (random 256) (random 256)))
         (payload-length (length octets))
         (bytes          (make-array 0 :element-type '(unsigned-byte 8)
                                       :fill-pointer 0)))
    (vector-push-extend (logior (if fin  #b10000000 0)
                                (if rsv1 #b01000000 0)
                                (if rsv2 #b00100000 0)
                                (if rsv3 #b00010000 0)
                                opcode)
                        bytes)
    (cond ((> payload-length 65535)
           (vector-push-extend (logior (if mask #b10000000 0)
                                       127)
                               bytes)
           (vector-push-extend (ash (logand #b1111111100000000000000000000000000000000000000000000000000000000 payload-length) -56) bytes)
           (vector-push-extend (ash (logand #b0000000011111111000000000000000000000000000000000000000000000000 payload-length) -48) bytes)
           (vector-push-extend (ash (logand #b0000000000000000111111110000000000000000000000000000000000000000 payload-length) -40) bytes)
           (vector-push-extend (ash (logand #b0000000000000000000000001111111100000000000000000000000000000000 payload-length) -32) bytes)
           (vector-push-extend (ash (logand #b0000000000000000000000000000000011111111000000000000000000000000 payload-length) -24) bytes)
           (vector-push-extend (ash (logand #b0000000000000000000000000000000000000000111111110000000000000000 payload-length) -16) bytes)
           (vector-push-extend (ash (logand #b0000000000000000000000000000000000000000000000001111111100000000 payload-length)  -8) bytes)
           (vector-push-extend      (logand #b0000000000000000000000000000000000000000000000000000000011111111 payload-length)     bytes))
          ((> payload-length   125)
           (vector-push-extend (logior (if mask #b10000000 0)
                                       126)
                               bytes)
           (vector-push-extend (ash (logand #b1111111100000000 payload-length) -8) bytes)
           (vector-push-extend      (logand #b0000000011111111 payload-length)    bytes))
          (t
           (vector-push-extend (logior (if mask #b10000000 0)
                                       payload-length)
                               bytes)))
    (vector-push-extend (elt masking-key 0) bytes)
    (vector-push-extend (elt masking-key 1) bytes)
    (vector-push-extend (elt masking-key 2) bytes)
    (vector-push-extend (elt masking-key 3) bytes)
    (when data  ; do we need to send masking key if no data?
      (loop for byte across (mask-payload-data masking-key octets)
            do (vector-push-extend byte bytes)))
    (loop for byte across bytes
          do (write-byte byte stream))
    ;; I spent a fscking day only to discover I needed FINISH-OUTPUT!
    (finish-output stream)
    bytes))


(defun write-binary-frame (stream octets &optional (client t))
  (write-frame stream :binary :data octets :client client))


(defun write-close-frame (stream &optional status-code (client t))
  ;; FIXME barfs on NIL
  ;(declare ((or nil keyword) status-code))
  (if status-code
      (let* ((number (keyword-to-status-code status-code))
             (bytes (vector (ash (logand #b1111111100000000 number) -8)
                                 (logand #b0000000011111111 number)    )))
        (write-frame stream :close :data bytes :client client))
      (write-frame stream :close :client client)))


(defun write-ping-frame (stream &optional (client t))
  (write-frame stream :ping :client client))


(defun write-pong-frame (stream &optional octets (client t))
  (if octets
      (write-frame stream :pong :data octets :client client)
      (write-frame stream :pong :client client)))


;; XXX Shouldn't we convert the text to bytes here?
(defun write-text-frame (stream text &optional (client t))
  (write-frame stream :text :data text :client client))


;; FIXME Handle close frames properly here (i.e. send return close frame
;;       according to spec).
(defun listener (stream)
  (handler-case
      (loop with frame = nil
            do (if (listen stream)
                   (progn
                     (setf frame (read-frame stream))
                     (case (frame-opcode-type frame)
                       (:ping (dbgmsg "• Received ping, sending pong.~%")
                              (write-pong-frame stream))
                       (:text (spammsg "• Received text frame, length ~D.~%"
                                       (frame-payload-length frame))
                              (sb-thread:with-mutex (*frames-queue-mutex*)
                                (push frame *frames-queue*)))
                       (otherwise (dbgmsg "• Unhandled frame: ~S (~D).~%"
                                          (frame-opcode-type frame)
                                          (frame-opcode frame))
                                  (logmsg "--- unhandled~%~S~%~S~%---~%"
                                          frame (payload-data-to-string
                                                 (frame-payload-data frame))))))
                   (sleep *poll-frequency*)))
    (t (condition) (logmsg "Error in listener: ~S~%" condition)
                   (setf *connection* nil))))


(defun terminate-listener-thread ()
  (loop for thread in (sb-thread:list-all-threads)
        for name = (sb-thread:thread-name thread)
        do (when (string= name *listener-thread-name*)
             (sb-thread:terminate-thread thread))))


(defun start-listener (stream &optional (thread-name *listener-thread-name*))
  "Starts a websocket listener thread.
  <talk about name and stream here and sbcl onlyness>"
  (logmsg "Cleaning up old listener thread(s) first.~%")
  (terminate-listener-thread)
  (sb-thread:make-thread #'listener :arguments (list stream)
                         :name thread-name))


(defun connection-active-p ()
  (not (null *connection*)))


(defun empty-frames-queue ()
  (let (frames)
    (sb-thread:with-mutex (*frames-queue-mutex*)
      (setf frames         *frames-queue*
            *frames-queue* nil))
    (reverse frames)))


(defun control-frame-p (frame)
  (declare (frame frame))
  (>= (frame-opcode frame) 8))


(defun close-frame-p (frame)
  (declare (frame frame))
  (equal :close (frame-opcode-type frame)))


(defun ping-frame-p (frame)
  (declare (frame frame))
  (equal :ping (frame-opcode-type frame)))


(defun pong-frame-p (frame)
  (declare (frame frame))
  (equal :pong (frame-opcode-type frame)))


(defun handle-close-frame (stream close-frame)
  (declare (frame close-frame))
  (dbgmsg "• Received close frame, length ~D.~%"
          (frame-payload-length close-frame))
  (if (>= (frame-payload-length close-frame) 2)
      (let* ((bytes (coerce (subseq (frame-payload-data close-frame) 0 2)
                            'vector))
             (status-code (bytes-to-number bytes))
             (kw (status-code-to-keyword status-code)))
        (dbgmsg "• Close frame status code: ~S (~D).~%" status-code kw)
        (write-close-frame stream kw))
      (write-close-frame stream)))


(defun validate-frame (stream frame)
  (declare (frame frame))
  (cond ;; Ping frames must be short than 126 bytes.
        ((and (ping-frame-p frame)
              (>= (frame-payload-length frame) 126))
         (write-close-frame stream :protocol-error)
         nil)
        ;; Reserved bits must not be set.
        ((or (frame-rsv1 frame) (frame-rsv2 frame) (frame-rsv3 frame))
         (write-close-frame stream :protocol-error)
         nil)
        ;; Close on protocol errors.
        ((equal :unknown (frame-opcode-type frame))
         (write-close-frame stream :protocol-error)
         nil)
        ;; Control frames must not be fragmented.
        ((and (control-frame-p frame)
              (not (frame-fin frame)))
         (write-close-frame stream :protocol-error)
         nil)
        (t t)))


;;; Example / Test Functions

(defun connect-to-echo ()
  (connect "http://echo.websocket.org/"))
