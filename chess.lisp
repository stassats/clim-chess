;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:clim-chess)

(defvar *images-path*
  (merge-pathnames "images/" #.(make-pathname :defaults *compile-file-truename*
                                              :name nil :type nil)))

(defvar *black* (make-rgb-color 209/255 139/255 71/255))
(defvar *white* (make-rgb-color 1 206/255 158/255))

(defvar *square-size* 50)
(defvar *board-size* (* *square-size* 8))

(define-presentation-type square ())

(define-presentation-type square-with-piece ()
  :inherit-from 'square)

(define-presentation-type square-with-black-piece ()
  :inherit-from 'square-with-piece)

(define-presentation-type square-with-white-piece ()
  :inherit-from 'square-with-piece)

(define-presentation-type piece ())

(defclass board-pane (application-pane)
  ((board :initform (make-instance 'board)
          :accessor board)
   (engine :accessor engine :initform nil))
  (:default-initargs
    :min-height *board-size*
    :min-width  *board-size*
    :max-height *board-size*
    :max-width  *board-size*))

(defclass draw-board-event (device-event)
  ()
  (:default-initargs :modifier-state 0))

(define-application-frame chess ()
  ((process :initform nil
            :accessor process))
  (:menu-bar t)
  (:panes
   (board (make-pane 'board-pane
                     :display-function 'draw-board
                     :incremental-redisplay t
                     :scroll-bars nil))
   (turn :application
         :scroll-bars nil
         :incremental-redisplay t
         :display-function 'display-turn)
   (interactor :interactor))
  (:layouts
   (default
       (vertically ()
         (2/3 board)
         (1/3 interactor)
         turn))))

(defmethod frame-standard-output ((chess chess))
  (get-frame-pane chess 'turn))

(defmethod handle-event ((client application-pane) (event draw-board-event))
  (with-application-frame (frame)
    (redisplay-frame-pane frame client)))

;;;

(defun display-turn (frame pane)
  (declare (ignore frame))
  (let ((board (find-board)))
    (draw-text* pane (format nil "~:[Black~;White~]'s turn." (next-to-move board))
                10 15)
    (when (checkmate board)
      (draw-text* pane "Checkmate." 10 30))))

(defun draw-board (frame pane)
  (declare (ignore frame))
  (updating-output (pane :unique-id 'board
                         :cache-value (move-number (board pane)))
    (do-matrix (x y)
      (let* ((square (square x (- 7 y)))
             (piece (board-square (board pane) square)))
        (updating-output (pane :unique-id (+ (* 8 x) y) ; 0 <= x,y <= 7
                               :cache-value piece
                               :cache-test #'equalp)
          (draw-square pane square piece x y))))))

(defun square-occupied-by (piece)
  (cond ((null piece) 'square)
        ((piece-color piece) 'square-with-white-piece)
        (t 'square-with-black-piece)))

(defun square-color (x y)
  (if (evenp (+ x y))
      *white* *black*))

(defun draw-square (pane square piece x y)
  (let ((x* (* x *square-size*))
        (y* (* y *square-size*)))
    (with-output-as-presentation (pane square (square-occupied-by piece))
      (draw-rectangle* pane x* y*
                       (+ x* *square-size*)
                       (+ y* *square-size*)
                       :ink (square-color x y)))
    (when piece
      (draw-piece pane piece x* y*))))

(defun draw-piece (pane piece x y)
  (let ((image (piece-image piece)))
    (draw-pattern* pane image
                   (+ x (/ (- *square-size* (pattern-height image)) 2))
                   (+ y (/ (- *square-size* (pattern-width image)) 2)))))

(define-presentation-method accept ((type piece) stream
                                    (view textual-view) &key)
  (completing-from-suggestions (stream)
    (dolist (piece *pieces*)
      (suggest piece (keyword-piece piece)))))

(define-presentation-method accept ((type square) stream
                                    (view textual-view) &key)
  (completing-from-suggestions (stream)
    (do-matrix (x y)
      (let ((square (square x y)))
        (suggest (square-keyword square) square)))))

(define-presentation-method present
    (square (type square) stream (view textual-view) &key)
  (write-string (square-keyword square) stream))

(define-presentation-method present
    (piece (type piece) stream (view textual-view) &key)
  (write-string (piece-keyword piece) stream))

(defun image-path (piece)
  (merge-pathnames (make-pathname :name piece :type "xpm")
                   *images-path*))

(defun load-piece (piece)
  (make-pattern-from-bitmap-file (image-path piece) :format :xpm))

(defun load-pieces ()
  (loop for piece in *pieces*
        collect (cons piece (load-piece piece))))

(defvar *images* (load-pieces))

(defun piece-image (piece)
  (cdr (assoc (piece-keyword piece) *images* :test #'string=)))

(defun find-board ()
  (board (find-pane-named *application-frame* 'board)))

(defun (setf find-board) (new)
  (setf (board (find-pane-named *application-frame* 'board))
        new))

;;;

(define-chess-command (com-quit :name t :menu t) ()
  (let ((board (find-pane-named *application-frame* 'board)))
    (when (engine board)
      (stop-engine (engine board)))
    (when (process *application-frame*)
      (clim-sys:destroy-process (process *application-frame*))))
  (frame-exit *application-frame*))

(define-chess-command (com-reset-game :name t :menu t) ()
  (setf (find-board) (make-instance 'board)))

(define-chess-command (com-clear-square :name t)
    ((square 'square-with-piece))
  (setf (board-square (find-board) square) nil))

(define-chess-command (com-add :name t)
    ((square 'square) (piece 'piece))
  (setf (board-square (find-board) square) piece))

(define-chess-command (com-move :name t)
    ((from (if (next-to-move (find-board))
               'square-with-white-piece
               'square-with-black-piece))
     (to 'square))
  (if (make-move (find-board) from to)
      (send-move (engine (find-pane-named *application-frame* 'board)) from to)
      (write-line "Illegal move.")))

(define-chess-command (com-retract :name t) ()
  (let ((board (find-board)))
    (retract-move board (pop (moves board)))))

(defun poll-engine (frame pane)
  (let ((engine (engine pane)))
    (loop
     (multiple-value-bind (from to) (receive-answer engine t)
       (make-move (board pane) from to))
     (queue-event (frame-top-level-sheet frame)
                  (make-instance 'draw-board-event :sheet pane)))))

(define-chess-command (com-start-engine :name t) ()
  (with-application-frame (frame)
    (let ((pane (find-pane-named frame 'board)))
      (setf (engine pane) (init-engine 'xboard-engine))
      (setf (process frame)
            (clim-sys:make-process (lambda () (poll-engine frame pane))
                                   :name "Engine poll.")))))

(defvar *promotion-alist*
  '(("Queen" . #\q)
    ("Rook" . #\r)
    ("Knight" . #\n)
    ("Bishop" . #\b)))

(defun select-promotion ()
  (menu-choose *promotion-alist*
               :label "Promote pawn to:"
               :scroll-bars nil))

(defun chess ()
  (run-frame-top-level (make-application-frame 'chess)))

;;; dragging-output currently does not work well in mcclim

(defun square-on-coordinates (x y)
  (let ((x (truncate x *square-size*))
        (y (truncate y *square-size*)))
    (square x (- 7 y))))

(define-chess-command (com-drag)
    ((square 'square-with-piece)
     (x 'float)
     (y 'float))
  (let ((pane (find-pane-named *application-frame* 'board)))
    (multiple-value-bind (final-x final-y)
        (dragging-output (pane :finish-on-release t)
                         (draw-circle* pane x y 10))
      (com-move square (square-on-coordinates final-x final-y)))))

(define-presentation-to-command-translator translator-drag
    (square-with-piece com-drag chess :echo nil)
    (object x y)
  (list object x y))
