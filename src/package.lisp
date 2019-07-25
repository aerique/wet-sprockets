;;;; package.lisp

(in-package :cl-user)


;;; Package

(defpackage :wet-sprockets
  (:nicknames :wets)
  (:use :cl)
  (:export ;; globals
           :*listener-thread-name* :*frames-queue* :*poll-frequency*
           ;; functions
           :connect :connection-active-p :empty-frames-queue :keyword-to-opcode
           :listener :mask-payload-data :payload-data-to-string
           :payload-data-to-json :opcode-as-keyword :read-frame :start-listener
           :terminate-listener-thread :write-frame :write-binary-frame
           :write-close-frame :write-ping-frame :write-pong-frame
           :write-text-frame))
