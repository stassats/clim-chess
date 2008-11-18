;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:clim-chess)

(defvar *images-path*
  (merge-pathnames "images/" #.(make-pathname :defaults *compile-file-truename*
                                              :name nil :type nil)))

(defvar *black* (make-rgb-color 209/255 139/255 71/255))
(defvar *white* (make-rgb-color 1 206/255 158/255))

(defvar *images* nil)
(defvar *square-size* 50)

(define-presentation-type square ())

(define-presentation-type square-with-piece ()
  :inherit-from 'square)

(define-presentation-type square-with-black-piece ()
  :inherit-from 'square-with-piece)

(define-presentation-type square-with-white-piece ()
  :inherit-from 'square-with-piece)

(define-presentation-type piece ())

(defclass board-pane (application-pane)
  ((board :initform (make-inital-position)
          :accessor board)
   (moves :initform nil
          :accessor moves))
  (:default-initargs
    :min-height (* *square-size* 8)
    :min-width (* *square-size* 8)
    :max-height (* *square-size* 8)
    :max-width (* *square-size* 8)))

(define-application-frame chess ()
  ()
  (:menu-bar t)
  (:panes
   (board (make-pane 'board-pane
                     :display-function '(draw-board)
                     :incremental-redisplay t
                     :scroll-bars nil))
   (black :application :scroll-bars nil)
   (white :application :scroll-bars nil)
   (interactor :interactor))
  (:layouts
   (default
       (vertically ()
         (horizontally () (1/2 white) (1/2 black))
         (2/3 board)
         (1/3 interactor)))))

;;;

(defun draw-board (frame pane)
  (declare (ignore frame))
  (loop for x to 7 do
        (loop for y to 7
              do (draw-square pane x y))))

(defun square-occupied-by (piece)
  (if (string= 'w piece :end2 1)
      'square-with-white-piece
      'square-with-black-piece))

(defun square-color (x y)
  (if (evenp (+ x y))
      *white* *black*))

(defun draw-square (pane x y)
  (let* ((square (square x (- 7 y)))
         (piece (board-square (board pane) square))
         (image (piece-image piece))
         (x* (* x *square-size*))
         (y* (* y *square-size*)))
    (with-output-as-presentation (pane square (square-occupied-by piece))
      (draw-rectangle* pane x* y*
                       (+ x* *square-size*)
                       (+ y* *square-size*)
                       :ink (square-color x y)))
    (when piece
      (draw-pattern* pane image
                     (+ x* (/ (- *square-size* (pattern-height image)) 2))
                     (+ y* (/ (- *square-size* (pattern-width image)) 2))))))

(define-presentation-method accept ((type piece) stream
                                    (view textual-view) &key)
  (values (completing-from-suggestions (stream)
            (dolist (piece *pieces*)
              (suggest (string-downcase piece) piece)))))

(define-presentation-method accept ((type square) stream
                                    (view textual-view) &key)
  (completing-from-suggestions (stream)
    (loop for x to 7 do
          (loop for y to 7
                for square = (square x y)
                do (suggest (string-downcase (square-keyword square))
                            square)))))

(define-presentation-method present
    (square (type square) stream (view textual-view) &key)
  (princ (square-keyword square) stream))

(defun image-path (piece-name)
  (merge-pathnames (make-pathname :name (string-downcase piece-name)
                                  :type "xpm")
                   *images-path*))

(defun load-piece (piece)
  (make-pattern-from-bitmap-file (image-path piece) :format :xpm))

(defun load-pieces ()
  (loop for piece in *pieces*
        collect (cons piece (load-piece piece))))

(defun piece-image (piece)
  (cdr (assoc piece *images*)))

(defmacro find-board ()
  '(board (find-pane-named *application-frame* 'board)))

;;;

(define-chess-command (com-quit :name t :menu t) ()
  (frame-exit *application-frame*))

(define-chess-command (com-reset-game :name t :menu t) ()
  (setf (find-board) (make-inital-position)))

(define-chess-command (com-clear-square :name t) ((square 'square))
  (setf (board-square (find-board) square) nil))

(define-chess-command (com-add :name t)
    ((square 'square) (piece 'piece))
  (setf (board-square (find-board) square )
        piece))

(define-chess-command (com-move :name t)
    ((from 'square-with-white-piece) (to 'square))
  (let ((board (find-board)))
    (if (check-move board from to)
        (psetf (board-square board from) nil
               (board-square board to) (board-square board from))
        (format (find-pane-named *application-frame* 'interactor)
                "Illegal move."))))

(defun chess ()
  (setf *images* (load-pieces))
  (run-frame-top-level (make-application-frame 'chess)))

;;; dragging-output currently does not work well in mcclim

;; (define-chess-command (com-drag)
;;     ((square 'square-with-piece)
;;      (x 'float)
;;      (y 'float))
;;   (dragging-output (t :finish-on-release t)
;;     (draw-pattern* *standard-output* (piece-image :bk) x y)))

;; (define-presentation-to-command-translator translator-drag
;;     (square-with-piece com-drag chess)
;;     (object x y)
;;   (list object x y))
