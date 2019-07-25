;;;; wet-sprockets.asd

(in-package :asdf)

(defsystem :wet-sprockets
  :version "0.0.1-alpha"
  :description "A websockets library for Common Lisp"
  :author "Erik Winkels <aerique@xs4all.nl>"
  :components ((:module "src"
                        :components ((:file "package")
                                     (:file "globals")
                                     (:file "common")
                                     (:file "main"))))
  :depends-on (:babel
               :cl-base64
               :drakma
               :ironclad
               :jsown))
