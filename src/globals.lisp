;;;; globals.lisp

(in-package :wet-sprockets)


;;; Globals

(defparameter *log-level* 1)

(defparameter *default-timeout* 32)

;; https://en.wikipedia.org/wiki/Unix_time
(defvar +unix-epoch-as-universal-time+ (encode-universal-time 0 0 0 1 1 1970 0))

(defparameter *connection* nil)

(defparameter *listener-thread-name* "websocket-listener")

(defparameter *frames-queue*       '())
(defparameter *frames-queue-mutex* (sb-thread:make-mutex))

(defparameter *poll-frequency* 1.0)
