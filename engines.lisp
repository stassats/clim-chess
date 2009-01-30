;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:clim-chess)

(defclass chess-engine ()
  ((stream :initform nil
           :accessor engine-stream)))

(defgeneric init-engine (engine))
(defgeneric stop-engine (engine))
(defgeneric send-move (engine from to))
(defgeneric receive-answer (engine))


(defun encode-move (from to)
  (concatenate 'string
               (square-keyword from)
               (square-keyword to)))

(defun decode-move (move)
  (cons
   (keyword-square (subseq move 0 2))
   (keyword-square (subseq move 2))))

;;; Macros

(defmacro defcommand (name (class &rest args) &body body)
  `(defmethod ,name ((engine ,class) ,@args)
     (with-accessors ((stream engine-stream)) engine
       (prog1 ,@body
        (force-output stream)))))
