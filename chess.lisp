;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:clim-chess)

(defvar *images-path*
  (merge-pathnames "images/"
                   #.(make-pathname :name nil :type nil
                                    :defaults (or *compile-file-truename*
                                                  *load-truename*))))

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
            :accessor process)
   (status-string :initform "" :accessor status-string))
  (:menu-bar t)
  (:panes
   (board (make-pane 'board-pane
                     :display-function 'draw-board
                     :incremental-redisplay t))
   (status :application
           :scroll-bars nil
           :incremental-redisplay t
           :display-function 'display-status
           :max-height 20
           :min-height 20)
   (color :application
          :scroll-bars nil
          :incremental-redisplay t
          :display-function 'display-color
          :max-height 35
          :min-height 35)
   (interactor :interactor))
  (:layouts
   (default
       (vertically ()
                   color
                   status
                   (3/5 board)
                   (1/5 interactor)))))

(defmethod frame-standard-output ((chess chess))
  (get-frame-pane chess 'turn))

(defmethod handle-event ((client application-pane) (event draw-board-event))
  (with-application-frame (frame)
    (redisplay-frame-pane frame client)
    (redisplay-frame-pane frame (find-pane-named frame 'status))
    (redisplay-frame-pane frame (find-pane-named frame 'color))))

;;;

(defun determine-status (pane)
  (let ((board (find-board)))
   (cond ((checkmate board) "Checkmate.")
         ((check board) "Check.")
         (t (status-string pane)))))

(defun display-status (frame pane)
  (let ((status (determine-status frame)))
   (updating-output (pane :unique-id 'status-bar
                          :cache-value status
                          :cache-test #'equal)
                    (princ status pane))))

(defun display-color (frame pane)
  (let ((color (next-to-move (find-board frame))))
    (updating-output (pane :unique-id 'color :cache-value color)
                     (with-text-size (pane :large)
                      (princ (if color "White." "Black.")
                             pane)))))

(defun draw-board (frame pane)
  (declare (ignore frame))
  (updating-output (pane :unique-id 'board
                         :cache-value (move-number (board pane)))
    (do-matrix (x y)
      (let* ((square (square x (- 7 y)))
             (piece (board-square (board pane) square)))
        (updating-output (pane :unique-id (+ (* 8 x) y) ; 0 <= x,y <= 7
                               :cache-value piece
                               :cache-test #'eql)
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

(defun find-board (&optional (frame *application-frame*))
  (board (find-pane-named frame 'board)))

(defun (setf find-board) (new &optional (frame *application-frame*))
  (setf (board (find-pane-named frame 'board))
        new))

;;;

(define-chess-command (com-quit :name t :menu t) ()
  (frame-exit *application-frame*))

(define-chess-command (com-reset-game :name t :menu t) ()
  (let ((board (find-pane-named *application-frame* 'board)))
    (when (engine board)
      (reset-engine (engine board))))
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
  (let* ((board-pane (find-pane-named *application-frame* 'board))
         (board (board board-pane))
         (move (make-move board from to)))
    (unless (engine board-pane)
      (start-engine *application-frame*))
    (if move
        (send-move (engine board-pane) from to)
        (setf (status-string *application-frame*) "Illegal move."))))

(define-chess-command (com-retract :name t) ()
  (let ((board (find-board)))
    (retract-last-move board)))


(defun poll-engine (frame pane)
  (let ((engine (engine pane)))
    (loop
     (multiple-value-bind (from to) (receive-answer engine t)
       (make-move (board pane) from to)
       (setf (status-string frame)
             (format nil "~a ~a" (square-keyword from)
                     (square-keyword to))))

     (queue-event (frame-top-level-sheet frame)
                  (make-instance 'draw-board-event :sheet pane))
     (sleep 0.001))))

(defun start-engine (frame)
  (let ((pane (find-pane-named frame 'board)))
    (setf (engine pane) (init-engine 'xboard-engine))
    (setf (process frame)
          (clim-sys:make-process (lambda () (poll-engine frame pane))
                                 :name "Engine poll."))))

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
  (let ((application (make-application-frame 'chess)))
    (unwind-protect (run-frame-top-level application)
      (let ((board (find-pane-named application 'board)))
        (when (engine board)
          (stop-engine (engine board)))
        (when (process application)
          (clim-sys:destroy-process (process application)))))))

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
