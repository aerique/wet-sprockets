;;;; globals.lisp

(in-package :wet-sprockets)


;;; Globals

(defparameter *log-level* 1)

(defparameter *default-timeout* 32)

(defparameter *connection* nil)

(defparameter *listener-thread-name* "websocket-listener")

(defparameter *frames-queue*       '())
(defparameter *frames-queue-mutex* (sb-thread:make-mutex))

(defparameter *poll-frequency* 1.0)
