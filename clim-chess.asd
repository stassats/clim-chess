;;; -*- Mode: Lisp -*-

(defpackage #:clim-chess-asd
  (:use #:cl #:asdf))

(in-package #:clim-chess-asd)

(defsystem #:clim-chess
  :name "clim-chess"
  :serial t
  :depends-on (#:mcclim)
  :components ((:file "packages")
               (:file "logic")
               (:file "chess")))
