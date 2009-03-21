;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:clim-chess)

(defclass xboard-engine (chess-engine)
  ((process :initform nil
            :accessor engine-process)))

(defvar *init-commands* '("xboard" "protover 2"))

(defcommand init-engine (xboard-engine)
  (setf (values (engine-process engine) stream)
        (run-program "gnuchess" '("-x")))
  (format stream "~{~a~%~}" *init-commands*)
  engine)

(defcommand stop-engine (xboard-engine)
  (write-line "quit" stream))

(defcommand send-move (xboard-engine from to)
  (write-line (encode-move from to) stream))

(defcommand receive-answer (xboard-engine &optional hang)
  (when (or hang (listen stream))
    (loop for line = (read-line stream nil nil)
          until (and (> (length line) 12)
                     (string= "My move is: " line :end2 12))
          finally (return (decode-move (subseq line 12))))))
