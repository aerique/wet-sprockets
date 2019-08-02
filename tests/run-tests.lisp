;;;; run-autobahn-testsuite.lisp

;;; Packages

(format t "~&Loading lisp-unit...~%")
(let ((*muffled-warnings* 'style-warning))
  (load "tests/lisp-unit"))

(format t "Loading WetSprockets...~%")
(ql:quickload :wet-sprockets)
(rename-package :wet-sprockets :wet-sprockets '(:ws))
(in-package :ws)

(use-package :lisp-unit)


;;; Globals

(defparameter *agent* "wet-sprockets")

(defparameter *log-level* 1)


;;; Functions
;;;
;;; (loop do (if (listen stm)
;;;              (format t ">>> ~S~%" (read-frame stm))
;;;              (sleep 0.01)))

(defun update-reports ()
  (connect (mkstr "http://localhost:9001/updateReports?agent=" *agent*)))


(defun simple-open (path)
  (let* ((connection (connect (mkstr "http://localhost:9001/" path)))
         (stream (getf connection :stream))
         (frame (read-frame stream))
         (response (if (equal (frame-opcode-type frame) :text)
                       (payload-data-to-string (frame-payload-data frame))
                       (coerce (frame-payload-data frame) 'vector))))
    (values connection stream frame response)))


(defun simple-run-case (case-number)
  (simple-open (mkstr "runCase?case=" case-number "&agent=" *agent*)))


(defun simple-close (stream frame response)
  (unless (validate-frame stream frame)
    (return-from simple-close))
  (cond ((equal :binary       (frame-opcode-type frame))
         (write-binary-frame stream response))
        ((equal :continuation (frame-opcode-type frame))
         (write-text-frame stream response))
        ((equal :ping         (frame-opcode-type frame))
         (write-pong-frame stream response))
        ((equal :pong         (frame-opcode-type frame))
         #| do nothing |#)
        ((equal :text         (frame-opcode-type frame))
         (write-text-frame stream response))
        (t
         (error "Unknown opcode-type for SIMPLE-CLOSE: ~S"
                (frame-opcode-type frame))))
  (let ((frame (read-frame stream)))
    (assert-equal :close (frame-opcode-type frame))
    (when (close-frame-p frame)
      (handle-close-frame stream frame))))


;;; Tests

(define-test 00-get-case-count ()
  (multiple-value-bind (connection stream frame response)
      (simple-open "getCaseCount")
    (declare (ignore connection stream frame))
    (assert-equal "519" response)))

;;; 1 Framing

;;; 1.1 Text Messages

(define-test case-1-1-1 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 1)
    (declare (ignore connection))
    (assert-equal 1 (frame-opcode frame))
    (assert-equal :text (frame-opcode-type frame))
    (assert-equal 0 (frame-payload-length frame))
    (assert-equal "" response)
    (simple-close stream frame response)))


(define-test case-1-1-2 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 2)
    (declare (ignore connection))
    (assert-equal 1 (frame-opcode frame))
    (assert-equal :text (frame-opcode-type frame))
    (assert-equal 125 (frame-payload-length frame))
    (assert-equal "*****************************************************************************************************************************" response)
    (simple-close stream frame response)))


(define-test case-1-1-3 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 3)
    (declare (ignore connection))
    (assert-equal 1 (frame-opcode frame))
    (assert-equal :text (frame-opcode-type frame))
    (assert-equal 126 (frame-payload-length frame))
    (assert-equal 126 (length response))
    (simple-close stream frame response)))


(define-test case-1-1-4 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 4)
    (declare (ignore connection))
    (assert-equal 1 (frame-opcode frame))
    (assert-equal :text (frame-opcode-type frame))
    (assert-equal 127 (frame-payload-length frame))
    (assert-equal 127 (length response))
    (simple-close stream frame response)))


(define-test case-1-1-5 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 5)
    (declare (ignore connection))
    (assert-equal 1 (frame-opcode frame))
    (assert-equal :text (frame-opcode-type frame))
    (assert-equal 128 (frame-payload-length frame))
    (assert-equal 128 (length response))
    (simple-close stream frame response)))


(define-test case-1-1-6 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 6)
    (declare (ignore connection))
    (assert-equal 1 (frame-opcode frame))
    (assert-equal :text (frame-opcode-type frame))
    (assert-equal 65535 (frame-payload-length frame))
    (assert-equal 65535 (length response))
    (simple-close stream frame response)))


(define-test case-1-1-7 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 7)
    (declare (ignore connection))
    (assert-equal 1 (frame-opcode frame))
    (assert-equal :text (frame-opcode-type frame))
    (assert-equal 65536 (frame-payload-length frame))
    (assert-equal 65536 (length response))
    (simple-close stream frame response)))


(define-test case-1-1-8 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 8)
    (declare (ignore connection))
    (assert-equal 1 (frame-opcode frame))
    (assert-equal :text (frame-opcode-type frame))
    (assert-equal 65536 (frame-payload-length frame))
    (assert-equal 65536 (length response))
    (simple-close stream frame response)))

;;; 1.2 Binary Messages

(define-test case-1-2-1 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 9)
    (declare (ignore connection))
    (assert-equal 2 (frame-opcode frame))
    (assert-equal :binary (frame-opcode-type frame))
    (assert-equal 0 (frame-payload-length frame))
    (assert-equal 0 (length response))
    (simple-close stream frame response)))


(define-test case-1-2-2 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 10)
    (declare (ignore connection))
    (assert-equal 2 (frame-opcode frame))
    (assert-equal :binary (frame-opcode-type frame))
    (assert-equal 125 (frame-payload-length frame))
    (assert-equal 125 (length response))
    (simple-close stream frame response)))


(define-test case-1-2-3 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 11)
    (declare (ignore connection))
    (assert-equal 2 (frame-opcode frame))
    (assert-equal :binary (frame-opcode-type frame))
    (assert-equal 126 (frame-payload-length frame))
    (assert-equal 126 (length response))
    (simple-close stream frame response)))


(define-test case-1-2-4 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 12)
    (declare (ignore connection))
    (assert-equal 2 (frame-opcode frame))
    (assert-equal :binary (frame-opcode-type frame))
    (assert-equal 127 (frame-payload-length frame))
    (assert-equal 127 (length response))
    (simple-close stream frame response)))


(define-test case-1-2-5 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 13)
    (declare (ignore connection))
    (assert-equal 2 (frame-opcode frame))
    (assert-equal :binary (frame-opcode-type frame))
    (assert-equal 128 (frame-payload-length frame))
    (assert-equal 128 (length response))
    (simple-close stream frame response)))


(define-test case-1-2-6 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 14)
    (declare (ignore connection))
    (assert-equal 2 (frame-opcode frame))
    (assert-equal :binary (frame-opcode-type frame))
    (assert-equal 65535 (frame-payload-length frame))
    (assert-equal 65535 (length response))
    (simple-close stream frame response)))


(define-test case-1-2-7 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 15)
    (declare (ignore connection))
    (assert-equal 2 (frame-opcode frame))
    (assert-equal :binary (frame-opcode-type frame))
    (assert-equal 65536 (frame-payload-length frame))
    (assert-equal 65536 (length response))
    (simple-close stream frame response)))


(define-test case-1-2-8 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 16)
    (declare (ignore connection))
    (assert-equal 2 (frame-opcode frame))
    (assert-equal :binary (frame-opcode-type frame))
    (assert-equal 65536 (frame-payload-length frame))
    (assert-equal 65536 (length response))
    (simple-close stream frame response)))

;;; 2 Pings / Pongs

(define-test case-2-01 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 17)
    (declare (ignore connection))
    (assert-equal 9 (frame-opcode frame))
    (assert-equal :ping (frame-opcode-type frame))
    (assert-equal 0 (frame-payload-length frame))
    (assert-equalp #() response)
    (simple-close stream frame response)))


(define-test case-2-02 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 18)
    (declare (ignore connection))
    (assert-equal 9 (frame-opcode frame))
    (assert-equal :ping (frame-opcode-type frame))
    (assert-equal 13 (frame-payload-length frame))
    (assert-equalp #(72 101 108 108 111 44 32 119 111 114 108 100 33) response)
    (simple-close stream frame response)))


(define-test case-2-03 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 19)
    (declare (ignore connection))
    (assert-equal 9 (frame-opcode frame))
    (assert-equal :ping (frame-opcode-type frame))
    (assert-equal 8 (frame-payload-length frame))
    (assert-equalp #(0 255 254 253 252 251 0 255) response)
    (simple-close stream frame response)))


(define-test case-2-04 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 20)
    (declare (ignore connection))
    (assert-equal 9 (frame-opcode frame))
    (assert-equal :ping (frame-opcode-type frame))
    (assert-equal 125 (frame-payload-length frame))
    (assert-equal 125 (length response))
    (simple-close stream frame response)))


(define-test case-2-05 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 21)
    (declare (ignore connection))
    (assert-equal 9 (frame-opcode frame))
    (assert-equal :ping (frame-opcode-type frame))
    (assert-equal 126 (frame-payload-length frame))
    (assert-equal 126 (length response))
    (simple-close stream frame response)))


(define-test case-2-06 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 22)
    (declare (ignore connection))
    (assert-equal 9 (frame-opcode frame))
    (assert-equal :ping (frame-opcode-type frame))
    (assert-equal 125 (frame-payload-length frame))
    (assert-equal 125 (length response))
    (simple-close stream frame response)))


(define-test case-2-07 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 23)
    (declare (ignore connection))
    (assert-equal 10 (frame-opcode frame))
    (assert-equal :pong (frame-opcode-type frame))
    (assert-equal 0 (frame-payload-length frame))
    (assert-equalp #() response)
    (simple-close stream frame response)))


(define-test case-2-08 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 24)
    (declare (ignore connection))
    (assert-equal 10 (frame-opcode frame))
    (assert-equal :pong (frame-opcode-type frame))
    (assert-false (= (frame-payload-length frame) 0))
    (assert-false (equalp #() response))
    (simple-close stream frame response)))


(define-test case-2-09 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 25)
    (declare (ignore connection))
    (assert-equal 10 (frame-opcode frame))
    (assert-equal :pong (frame-opcode-type frame))
    (assert-false (= (frame-payload-length frame) 0))
    (assert-false (equalp #() response))
    (setf frame    (read-frame stream)
          response (coerce (frame-payload-data frame) 'vector))
    (assert-equal 9 (frame-opcode frame))
    (assert-equal :ping (frame-opcode-type frame))
    (assert-false (= (frame-payload-length frame) 0))
    (assert-false (equalp #() response))
    (simple-close stream frame response)))


(define-test case-2-10 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 26)
    (declare (ignore connection))
    (loop repeat 9
          do (assert-equal 9 (frame-opcode frame))
             (assert-equal :ping (frame-opcode-type frame))
             (assert-false (= (frame-payload-length frame) 0))
             (assert-false (equalp #() response))
             (write-pong-frame stream response)
             (setf frame    (read-frame stream)
                   response (coerce (frame-payload-data frame) 'vector)))
    (simple-close stream frame response)))


(define-test case-2-11 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 27)
    (declare (ignore connection))
    (loop repeat 9
          do (assert-equal 9 (frame-opcode frame))
             (assert-equal :ping (frame-opcode-type frame))
             (assert-false (= (frame-payload-length frame) 0))
             (assert-false (equalp #() response))
             (write-pong-frame stream response)
             (setf frame    (read-frame stream)
                   response (coerce (frame-payload-data frame) 'vector)))
    (simple-close stream frame response)))

;;; 3 Reserved Bits

(define-test case-3-1 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 28)
    (declare (ignore connection))
    (assert-equal 1 (frame-opcode frame))
    (assert-equal :text (frame-opcode-type frame))
    (assert-true (>= (frame-payload-length frame) 0))
    (assert-true (>= (length response) 0))
    (assert-false (frame-rsv1 frame))
    (assert-false (frame-rsv2 frame))
    (assert-true  (frame-rsv3 frame))
    (simple-close stream frame response)))


(define-test case-3-2 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 29)
    (declare (ignore connection))
    (assert-equal 1 (frame-opcode frame))
    (assert-equal :text (frame-opcode-type frame))
    (assert-true (>= (frame-payload-length frame) 0))
    (assert-true (>= (length response) 0))
    (write-text-frame stream response)
    (setf frame    (read-frame stream)
          response (payload-data-to-string (frame-payload-data frame)))
    (assert-equal 1 (frame-opcode frame))
    (assert-equal :text (frame-opcode-type frame))
    (assert-true (>= (frame-payload-length frame) 0))
    (assert-true (>= (length response) 0))
    (assert-false (frame-rsv1 frame))
    (assert-true  (frame-rsv2 frame))
    (assert-false (frame-rsv3 frame))
    (simple-close stream frame response)))


(define-test case-3-3 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 30)
    (declare (ignore connection))
    (assert-equal 1 (frame-opcode frame))
    (assert-equal :text (frame-opcode-type frame))
    (assert-true (>= (frame-payload-length frame) 0))
    (assert-true (>= (length response) 0))
    (write-text-frame stream response)
    (setf frame    (read-frame stream)
          response (payload-data-to-string (frame-payload-data frame)))
    (assert-equal 1 (frame-opcode frame))
    (assert-equal :text (frame-opcode-type frame))
    (assert-true (>= (frame-payload-length frame) 0))
    (assert-true (>= (length response) 0))
    (assert-false (frame-rsv1 frame))
    (assert-true  (frame-rsv2 frame))
    (assert-true  (frame-rsv3 frame))
    (simple-close stream frame response)))


(define-test case-3-4 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 31)
    (declare (ignore connection))
    (assert-equal 1 (frame-opcode frame))
    (assert-equal :text (frame-opcode-type frame))
    (assert-true (>= (frame-payload-length frame) 0))
    (assert-true (>= (length response) 0))
    (write-text-frame stream response)
    (setf frame    (read-frame stream)
          response (payload-data-to-string (frame-payload-data frame)))
    (assert-equal 1 (frame-opcode frame))
    (assert-equal :text (frame-opcode-type frame))
    (assert-true (>= (frame-payload-length frame) 0))
    (assert-true (>= (length response) 0))
    (assert-true  (frame-rsv1 frame))
    (assert-false (frame-rsv2 frame))
    (assert-false (frame-rsv3 frame))
    (simple-close stream frame response)))


(define-test case-3-5 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 32)
    (declare (ignore connection))
    (assert-equal 2 (frame-opcode frame))
    (assert-equal :binary (frame-opcode-type frame))
    (assert-true (>= (frame-payload-length frame) 0))
    (assert-true (>= (length response) 0))
    (assert-true  (frame-rsv1 frame))
    (assert-false (frame-rsv2 frame))
    (assert-true  (frame-rsv3 frame))
    (simple-close stream frame response)))


;; Skipped, actual test and case description differ.
;; The frame type send by the testsuite is :BINARY while it should be :PING.
;(define-test case-3-6 ()
;  (multiple-value-bind (connection stream frame response)
;      (simple-run-case 33)
;    (declare (ignore connection))
;    (let ((*print-pretty* nil)) (format t "~&~S~%" frame))
;    (assert-equal 9 (frame-opcode frame))
;    (assert-equal :ping (frame-opcode-type frame))
;    (assert-true  (frame-rsv1 frame))
;    (assert-true  (frame-rsv2 frame))
;    (assert-false (frame-rsv3 frame))
;    (simple-close stream frame response)))


(define-test case-3-7 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 34)
    (declare (ignore connection))
    (assert-equal 8 (frame-opcode frame))
    (assert-equal :close (frame-opcode-type frame))
    (assert-true (frame-rsv1 frame))
    (assert-true (frame-rsv2 frame))
    (assert-true (frame-rsv3 frame))
    (simple-close stream frame response)))

;;; 4 Opcodes

;;; 4.1 Non-control Opcodes

(define-test case-4-1-1 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 35)
    (declare (ignore connection))
    (assert-equal 3 (frame-opcode frame))
    (assert-equal :unknown (frame-opcode-type frame))
    (simple-close stream frame response)))


(define-test case-4-1-2 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 36)
    (declare (ignore connection))
    (assert-equal 4 (frame-opcode frame))
    (assert-equal :unknown (frame-opcode-type frame))
    (assert-true (>= (frame-payload-length frame) 0))
    (assert-true (>= (length response) 0))
    (simple-close stream frame response)))


(define-test case-4-1-3 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 37)
    (declare (ignore connection))
    (assert-equal 1 (frame-opcode frame))
    (assert-equal :text (frame-opcode-type frame))
    (assert-true (>= (frame-payload-length frame) 0))
    (assert-true (>= (length response) 0))
    (write-text-frame stream response)
    (setf frame    (read-frame stream)
          response (payload-data-to-string (frame-payload-data frame)))
    (assert-equal 5 (frame-opcode frame))
    (assert-equal :unknown (frame-opcode-type frame))
    (simple-close stream frame response)))


(define-test case-4-1-4 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 38)
    (declare (ignore connection))
    (assert-equal 1 (frame-opcode frame))
    (assert-equal :text (frame-opcode-type frame))
    (assert-true (>= (frame-payload-length frame) 0))
    (assert-true (>= (length response) 0))
    (write-text-frame stream response)
    (setf frame    (read-frame stream)
          response (payload-data-to-string (frame-payload-data frame)))
    (assert-equal 6 (frame-opcode frame))
    (assert-equal :unknown (frame-opcode-type frame))
    (assert-true (>= (frame-payload-length frame) 0))
    (assert-true (>= (length response) 0))
    (simple-close stream frame response)))


(define-test case-4-1-5 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 39)
    (declare (ignore connection))
    (assert-equal 1 (frame-opcode frame))
    (assert-equal :text (frame-opcode-type frame))
    (assert-true (>= (frame-payload-length frame) 0))
    (assert-true (>= (length response) 0))
    (write-text-frame stream response)
    (setf frame    (read-frame stream)
          response (payload-data-to-string (frame-payload-data frame)))
    (assert-equal 7 (frame-opcode frame))
    (assert-equal :unknown (frame-opcode-type frame))
    (assert-true (>= (frame-payload-length frame) 0))
    (assert-true (>= (length response) 0))
    (simple-close stream frame response)))

;;; 4.2 Control Opcodes

(define-test case-4-2-1 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 40)
    (declare (ignore connection))
    (assert-equal 11 (frame-opcode frame))
    (assert-equal :unknown (frame-opcode-type frame))
    (simple-close stream frame response)))


(define-test case-4-2-2 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 41)
    (declare (ignore connection))
    (assert-equal 12 (frame-opcode frame))
    (assert-equal :unknown (frame-opcode-type frame))
    (assert-true (>= (frame-payload-length frame) 0))
    (assert-true (>= (length response) 0))
    (simple-close stream frame response)))


(define-test case-4-2-3 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 42)
    (declare (ignore connection))
    (assert-equal 1 (frame-opcode frame))
    (assert-equal :text (frame-opcode-type frame))
    (assert-true (>= (frame-payload-length frame) 0))
    (assert-true (>= (length response) 0))
    (write-text-frame stream response)
    (setf frame    (read-frame stream)
          response (payload-data-to-string (frame-payload-data frame)))
    (assert-equal 13 (frame-opcode frame))
    (assert-equal :unknown (frame-opcode-type frame))
    (simple-close stream frame response)))


(define-test case-4-2-4 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 43)
    (declare (ignore connection))
    (assert-equal 1 (frame-opcode frame))
    (assert-equal :text (frame-opcode-type frame))
    (assert-true (>= (frame-payload-length frame) 0))
    (assert-true (>= (length response) 0))
    (write-text-frame stream response)
    (setf frame    (read-frame stream)
          response (payload-data-to-string (frame-payload-data frame)))
    (assert-equal 14 (frame-opcode frame))
    (assert-equal :unknown (frame-opcode-type frame))
    (assert-true (>= (frame-payload-length frame) 0))
    (assert-true (>= (length response) 0))
    (simple-close stream frame response)))


(define-test case-4-2-5 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 44)
    (declare (ignore connection))
    (assert-equal 1 (frame-opcode frame))
    (assert-equal :text (frame-opcode-type frame))
    (assert-true (>= (frame-payload-length frame) 0))
    (assert-true (>= (length response) 0))
    (write-text-frame stream response)
    (setf frame    (read-frame stream)
          response (payload-data-to-string (frame-payload-data frame)))
    (assert-equal 15 (frame-opcode frame))
    (assert-equal :unknown (frame-opcode-type frame))
    (assert-true (>= (frame-payload-length frame) 0))
    (assert-true (>= (length response) 0))
    (simple-close stream frame response)))

;;; 5 Fragmentation

(define-test case-5-01 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 45)
    (declare (ignore connection))
    (assert-equal 9 (frame-opcode frame))
    (assert-equal :ping (frame-opcode-type frame))
    (simple-close stream frame response)))


(define-test case-5-02 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 46)
    (declare (ignore connection))
    (assert-equal 10 (frame-opcode frame))
    (assert-equal :pong (frame-opcode-type frame))
    (simple-close stream frame response)))


(define-test case-5-03 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 47)
    (declare (ignore connection))
    (assert-equal 1 (frame-opcode frame))
    (assert-equal :text (frame-opcode-type frame))
    (assert-true (>= (frame-payload-length frame) 0))
    (assert-true (>= (length response) 0))
    (multiple-value-bind (payload-data frames)
        (read-fragmented-frames stream frame)
      (declare (ignore frames))
      (simple-close stream frame (payload-data-to-string payload-data)))))
    ;; This would be simpler, but the above is perhaps more clear.
    ;(simple-close stream frame (payload-data-to-string
    ;                            (read-fragmented-frames stream frame)))))


(define-test case-5-04 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 48)
    (declare (ignore connection))
    (assert-equal 1 (frame-opcode frame))
    (assert-equal :text (frame-opcode-type frame))
    (assert-true (>= (frame-payload-length frame) 0))
    (assert-true (>= (length response) 0))
    (simple-close stream frame (payload-data-to-string
                                (read-fragmented-frames stream frame)))))


(define-test case-5-05 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 49)
    (declare (ignore connection))
    (assert-equal 1 (frame-opcode frame))
    (assert-equal :text (frame-opcode-type frame))
    (assert-true (>= (frame-payload-length frame) 0))
    (assert-true (>= (length response) 0))
    (simple-close stream frame (payload-data-to-string
                                (read-fragmented-frames stream frame)))))


(define-test case-5-06 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 50)
    (declare (ignore connection))
    (assert-equal 1 (frame-opcode frame))
    (assert-equal :text (frame-opcode-type frame))
    (assert-true (>= (frame-payload-length frame) 0))
    (assert-true (>= (length response) 0))
    (simple-close stream frame (payload-data-to-string
                                (read-fragmented-frames stream frame)))))


(define-test case-5-07 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 51)
    (declare (ignore connection))
    (assert-equal 1 (frame-opcode frame))
    (assert-equal :text (frame-opcode-type frame))
    (assert-true (>= (frame-payload-length frame) 0))
    (assert-true (>= (length response) 0))
    (simple-close stream frame (payload-data-to-string
                                (read-fragmented-frames stream frame)))))


(define-test case-5-08 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 52)
    (declare (ignore connection))
    (assert-equal 1 (frame-opcode frame))
    (assert-equal :text (frame-opcode-type frame))
    (assert-true (>= (frame-payload-length frame) 0))
    (assert-true (>= (length response) 0))
    (simple-close stream frame (payload-data-to-string
                                (read-fragmented-frames stream frame)))))


;; XXX this should be used as an example for LISTENER
(define-test case-5-09 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 53)
    (declare (ignore connection response))
    (assert-equal 0 (frame-opcode frame))
    (assert-equal :continuation (frame-opcode-type frame))
    (write-close-frame stream :protocol-error)))


(define-test case-5-10 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 54)
    (declare (ignore connection response))
    (assert-equal 0 (frame-opcode frame))
    (assert-equal :continuation (frame-opcode-type frame))
    (write-close-frame stream :protocol-error)))


(define-test case-5-11 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 55)
    (declare (ignore connection response))
    (assert-equal 0 (frame-opcode frame))
    (assert-equal :continuation (frame-opcode-type frame))
    (write-close-frame stream :protocol-error)))


(define-test case-5-12 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 56)
    (declare (ignore connection response))
    (assert-equal 0 (frame-opcode frame))
    (assert-equal :continuation (frame-opcode-type frame))
    (write-close-frame stream :protocol-error)))


(define-test case-5-13 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 57)
    (declare (ignore connection response))
    (assert-equal 0 (frame-opcode frame))
    (assert-equal :continuation (frame-opcode-type frame))
    (write-close-frame stream :protocol-error)))


(define-test case-5-14 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 58)
    (declare (ignore connection response))
    (assert-equal 0 (frame-opcode frame))
    (assert-equal :continuation (frame-opcode-type frame))
    (write-close-frame stream :protocol-error)))


(define-test case-5-15 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 59)
    (declare (ignore connection))
    (assert-equal 1 (frame-opcode frame))
    (assert-equal :text (frame-opcode-type frame))
    (assert-true (>= (frame-payload-length frame) 0))
    (assert-true (>= (length response) 0))
    (write-text-frame stream (payload-data-to-string
                              (read-fragmented-frames stream frame)))
    (setf frame (read-frame stream))
    (assert-equal 0 (frame-opcode frame))
    (assert-equal :continuation (frame-opcode-type frame))
    (write-close-frame stream :protocol-error)))


(define-test case-5-16 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 60)
    (declare (ignore connection response))
    (assert-equal 0 (frame-opcode frame))
    (assert-equal :continuation (frame-opcode-type frame))
    (write-close-frame stream :protocol-error)))


(define-test case-5-17 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 61)
    (declare (ignore connection response))
    (assert-equal 0 (frame-opcode frame))
    (assert-equal :continuation (frame-opcode-type frame))
    (write-close-frame stream :protocol-error)))


(define-test case-5-18 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 62)
    (declare (ignore connection))
    (assert-equal 1 (frame-opcode frame))
    (assert-equal :text (frame-opcode-type frame))
    (assert-true (>= (frame-payload-length frame) 0))
    (assert-true (>= (length response) 0))
    (simple-close stream frame (payload-data-to-string
                                (read-fragmented-frames stream frame)))))


;; Commented out for slowing down test run.
;(define-test case-5-19 ()
;  (multiple-value-bind (connection stream frame response)
;      (simple-run-case 63)
;    (declare (ignore connection))
;    (assert-equal 1 (frame-opcode frame))
;    (assert-equal :text (frame-opcode-type frame))
;    (assert-true (>= (frame-payload-length frame) 0))
;    (assert-true (>= (length response) 0))
;    (simple-close stream frame (payload-data-to-string
;                                (read-fragmented-frames stream frame)))))


;; Commented out for slowing down test run.
;(define-test case-5-20 ()
;  (multiple-value-bind (connection stream frame response)
;      (simple-run-case 64)
;    (declare (ignore connection))
;    (assert-equal 1 (frame-opcode frame))
;    (assert-equal :text (frame-opcode-type frame))
;    (assert-true (>= (frame-payload-length frame) 0))
;    (assert-true (>= (length response) 0))
;    (simple-close stream frame (payload-data-to-string
;                                (read-fragmented-frames stream frame)))))

;;; 6 UTF-8 Handling

;;; 6.1 Valid UTF-8 with zero payload fragments

(define-test case-6-1-1 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 65)
    (declare (ignore connection))
    (assert-equal 1 (frame-opcode frame))
    (assert-equal :text (frame-opcode-type frame))
    (assert-equal 0 (frame-payload-length frame))
    (assert-equal 0 (length response))
    (simple-close stream frame response)))


(define-test case-6-1-2 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 66)
    (declare (ignore connection))
    (assert-equal 1 (frame-opcode frame))
    (assert-equal :text (frame-opcode-type frame))
    (assert-equal 0 (frame-payload-length frame))
    (assert-equal 0 (length response))
    (simple-close stream frame (payload-data-to-string
                                (read-fragmented-frames stream frame)))))


(define-test case-6-1-3 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 67)
    (declare (ignore connection))
    (assert-equal 1 (frame-opcode frame))
    (assert-equal :text (frame-opcode-type frame))
    (assert-equal 0 (frame-payload-length frame))
    (assert-equal 0 (length response))
    (simple-close stream frame (payload-data-to-string
                                (read-fragmented-frames stream frame)))))

;;; 6.2 Valid UTF-8 unfragmented, fragmented on code-points and within
;;;     code-points

(define-test case-6-2-1 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 68)
    (declare (ignore connection))
    (assert-equal 1 (frame-opcode frame))
    (assert-equal :text (frame-opcode-type frame))
    (assert-equal 29 (frame-payload-length frame))  ; 29 bytes
    (assert-equal 22 (length response))             ; 22 characters
    (simple-close stream frame response)))


(define-test case-6-2-2 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 69)
    (declare (ignore connection))
    (assert-equal 1 (frame-opcode frame))
    (assert-equal :text (frame-opcode-type frame))
    (assert-equal 15 (frame-payload-length frame))
    (assert-equal 11 (length response))
    (simple-close stream frame (payload-data-to-string
                                (read-fragmented-frames stream frame)))))


(define-test case-6-2-3 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 70)
    (declare (ignore connection))
    (assert-equal 1 (frame-opcode frame))
    (assert-equal :text (frame-opcode-type frame))
    (assert-equal 1 (frame-payload-length frame))
    (assert-equal 1 (length response))
    (simple-close stream frame (payload-data-to-string
                                (read-fragmented-frames stream frame)))))


(define-test case-6-2-4 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 71)
    (declare (ignore connection))
    (assert-equal 1 (frame-opcode frame))
    (assert-equal :text (frame-opcode-type frame))
    (assert-equal 1 (frame-payload-length frame))
    (assert-equal 1 (length response))
    (simple-close stream frame (payload-data-to-string
                                (read-fragmented-frames stream frame)))))



;;; Run the tests.

(format t "Running Autobahn Testsuite...~%")
(run-tests)
(format t "~&")
(update-reports)
(cl-user::quit)
