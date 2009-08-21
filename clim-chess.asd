;;; -*- Mode: Lisp -*-

(defsystem #:clim-chess
  :name "clim-chess"
  :serial t
  :depends-on (#:mcclim)
  :components ((:file "packages")
               (:file "logic")
               (:file "chess")
               (:file "run-program")
               (:file "engines")
               (:file "xboard")))
