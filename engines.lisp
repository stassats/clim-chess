;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:clim-chess)

(defclass chess-engine ()
  ((stream :initform nil
           :accessor engine-stream)))

(defgeneric init-engine (engine))
(defgeneric stop-engine (engine))
(defgeneric reset-engine (engine))
(defgeneric send-move (engine from to))
(defgeneric receive-answer (engine &optional hang))

(defmethod init-engine ((engine symbol))
  (init-engine (make-instance engine)))

(defun encode-move (from to)
  (concatenate 'string
               (square-keyword from)
               (square-keyword to)))

(defun decode-move (move)
  (values
   (keyword-square (subseq move 0 2))
   (keyword-square (subseq move 2))))

(defun poll-answer (engine function &optional (timeout 0.1))
  (loop when (receive-answer engine) return it
        do (funcall function)
        (sleep timeout)))

;;; Macros

(defmacro defcommand (name (class &rest args) &body body)
  `(defmethod ,name ((engine ,class) ,@args)
     (with-accessors ((stream engine-stream)) engine
       (multiple-value-prog1 (progn ,@body)
         (force-output stream)))))
