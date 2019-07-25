;;;; run-autobahn-testsuite.lisp

;;; Packages

(format t "~&Loading lisp-unit...~%")
(let ((*muffled-warnings* 'style-warning))
  (load "tests/lisp-unit"))

(format t "Loading WetSprockets...~%")
(ql:quickload :wet-sprockets)
(in-package :wet-sprockets)

(use-package :lisp-unit)


;;; Globals

(defparameter *agent* "wet-sprockets")

(defparameter *log-level* 1)


;;; Functions

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


(defun simple-close (stream response type)
  (cond ((equal type :binary)
         (write-binary-frame stream response))
        ((equal type :text)
         (write-text-frame stream response))
        (t
         (error "Unknown type for SIMPLE-CLOSE: ~S" type)))
  (let ((frame (read-frame stream)))
    (assert-equal :close (frame-opcode-type frame))
    (when (close-frame-p frame)
      (handle-close-frame stream frame)))
  (update-reports))


;;; Tests
;;;
;;; - (assert-false (btc-eur *exchange*))
;;; - (assert-true (enough-btc-funds-p *exchange*))

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
    (simple-close stream response (frame-opcode-type frame))))


(define-test case-1-1-2 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 2)
    (declare (ignore connection))
    (assert-equal 1 (frame-opcode frame))
    (assert-equal :text (frame-opcode-type frame))
    (assert-equal 125 (frame-payload-length frame))
    (assert-equal "*****************************************************************************************************************************" response)
    (simple-close stream response (frame-opcode-type frame))))


(define-test case-1-1-3 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 3)
    (declare (ignore connection))
    (assert-equal 1 (frame-opcode frame))
    (assert-equal :text (frame-opcode-type frame))
    (assert-equal 126 (frame-payload-length frame))
    (assert-equal 126 (length response))
    (simple-close stream response (frame-opcode-type frame))))


(define-test case-1-1-4 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 4)
    (declare (ignore connection))
    (assert-equal 1 (frame-opcode frame))
    (assert-equal :text (frame-opcode-type frame))
    (assert-equal 127 (frame-payload-length frame))
    (assert-equal 127 (length response))
    (simple-close stream response (frame-opcode-type frame))))


(define-test case-1-1-5 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 5)
    (declare (ignore connection))
    (assert-equal 1 (frame-opcode frame))
    (assert-equal :text (frame-opcode-type frame))
    (assert-equal 128 (frame-payload-length frame))
    (assert-equal 128 (length response))
    (simple-close stream response (frame-opcode-type frame))))


(define-test case-1-1-6 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 6)
    (declare (ignore connection))
    (assert-equal 1 (frame-opcode frame))
    (assert-equal :text (frame-opcode-type frame))
    (assert-equal 65535 (frame-payload-length frame))
    (assert-equal 65535 (length response))
    (simple-close stream response (frame-opcode-type frame))))


(define-test case-1-1-7 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 7)
    (declare (ignore connection))
    (assert-equal 1 (frame-opcode frame))
    (assert-equal :text (frame-opcode-type frame))
    (assert-equal 65536 (frame-payload-length frame))
    (assert-equal 65536 (length response))
    (simple-close stream response (frame-opcode-type frame))))


(define-test case-1-1-8 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 8)
    (declare (ignore connection))
    (assert-equal 1 (frame-opcode frame))
    (assert-equal :text (frame-opcode-type frame))
    (assert-equal 65536 (frame-payload-length frame))
    (assert-equal 65536 (length response))
    (simple-close stream response (frame-opcode-type frame))))

;;; 1.2 Binary Messages

(define-test case-1-2-1 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 9)
    (declare (ignore connection))
    (assert-equal 2 (frame-opcode frame))
    (assert-equal :binary (frame-opcode-type frame))
    (assert-equal 0 (frame-payload-length frame))
    (assert-equalp #() response)
    (simple-close stream response (frame-opcode-type frame))))


(define-test case-1-2-2 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 10)
    (declare (ignore connection))
    (assert-equal 2 (frame-opcode frame))
    (assert-equal :binary (frame-opcode-type frame))
    (assert-equal 125 (frame-payload-length frame))
    (assert-equalp #(254 254 254 254 254 254 254 254 254 254 254 254 254 254
                    254 254 254 254 254 254 254 254 254 254 254 254 254 254
                    254 254 254 254 254 254 254 254 254 254 254 254 254 254
                    254 254 254 254 254 254 254 254 254 254 254 254 254 254
                    254 254 254 254 254 254 254 254 254 254 254 254 254 254
                    254 254 254 254 254 254 254 254 254 254 254 254 254 254
                    254 254 254 254 254 254 254 254 254 254 254 254 254 254
                    254 254 254 254 254 254 254 254 254 254 254 254 254 254
                    254 254 254 254 254 254 254 254 254 254 254 254 254)
                   response)
    (simple-close stream response (frame-opcode-type frame))))


(define-test case-1-2-3 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 11)
    (declare (ignore connection))
    (assert-equal 2 (frame-opcode frame))
    (assert-equal :binary (frame-opcode-type frame))
    (assert-equal 126 (frame-payload-length frame))
    (assert-equal 126 (length response))
    (simple-close stream response (frame-opcode-type frame))))


(define-test case-1-2-4 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 12)
    (declare (ignore connection))
    (assert-equal 2 (frame-opcode frame))
    (assert-equal :binary (frame-opcode-type frame))
    (assert-equal 127 (frame-payload-length frame))
    (assert-equal 127 (length response))
    (simple-close stream response (frame-opcode-type frame))))


(define-test case-1-2-5 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 13)
    (declare (ignore connection))
    (assert-equal 2 (frame-opcode frame))
    (assert-equal :binary (frame-opcode-type frame))
    (assert-equal 128 (frame-payload-length frame))
    (assert-equal 128 (length response))
    (simple-close stream response (frame-opcode-type frame))))


(define-test case-1-2-6 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 14)
    (declare (ignore connection))
    (assert-equal 2 (frame-opcode frame))
    (assert-equal :binary (frame-opcode-type frame))
    (assert-equal 65535 (frame-payload-length frame))
    (assert-equal 65535 (length response))
    (simple-close stream response (frame-opcode-type frame))))


(define-test case-1-2-7 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 15)
    (declare (ignore connection))
    (assert-equal 2 (frame-opcode frame))
    (assert-equal :binary (frame-opcode-type frame))
    (assert-equal 65536 (frame-payload-length frame))
    (assert-equal 65536 (length response))
    (simple-close stream response (frame-opcode-type frame))))


(define-test case-1-2-8 ()
  (multiple-value-bind (connection stream frame response)
      (simple-run-case 16)
    (declare (ignore connection))
    (assert-equal 2 (frame-opcode frame))
    (assert-equal :binary (frame-opcode-type frame))
    (assert-equal 65536 (frame-payload-length frame))
    (assert-equal 65536 (length response))
    (simple-close stream response (frame-opcode-type frame))))


;;; Run the tests.

(format t "Running Autobahn Testsuite...~%")
(run-tests)
(format t "~&")
(cl-user::quit)
