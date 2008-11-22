;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:clim-chess)

(defvar *initial-position*
  '(((t . :r) (t . :p) nil nil nil nil (nil . :p) (nil . :r))
    ((t . :n) (t . :p) nil nil nil nil (nil . :p) (nil . :n))
    ((t . :b) (t . :p) nil nil nil nil (nil . :p) (nil . :b))
    ((t . :q) (t . :p) nil nil nil nil (nil . :p) (nil . :q))
    ((t . :k) (t . :p) nil nil nil nil (nil . :p) (nil . :k))
    ((t . :b) (t . :p) nil nil nil nil (nil . :p) (nil . :b))
    ((t . :n) (t . :p) nil nil nil nil (nil . :p) (nil . :n))
    ((t . :r) (t . :p) nil nil nil nil (nil . :p) (nil . :r))))

(defvar *letters* "abcdefgh")
(defvar *pieces* '("wr" "wn" "wb" "wq" "wk" "wp"
                   "br" "bn" "bb" "bq" "bk" "bp"))

(defclass board ()
  ((contents :initform
             (make-array '(8 8) :initial-contents *initial-position*)
             :accessor contents)
   (moves :initform nil :accessor moves)
   (white-castling :initform (cons t t)  ; long - short
                   :accessor white-castling)
   (black-castling :initform (cons t t)
                   :accessor black-castling)))

;;; Square abstraction
(defun square (rank file) (cons rank file))
(defun rank (square) (car square))
(defun file (square) (cdr square))

(defun valid-square-p (square)
  (and (>= 7 (rank square) 0)
       (>= 7 (file square) 0)))

(defun board-square (board square)
  (when (valid-square-p square)
    (aref (contents board) (rank square) (file square))))

(defun (setf board-square) (value board square)
  (when (valid-square-p square)
    (setf (aref (contents board) (rank square) (file square))
          value)))

(defun square-keyword (square)
  "0 0 -> :a1"
  (coerce (vector (char *letters* (rank square))
                  (digit-char (1+ (file square))))
          'simple-string))

(defun keyword-square (keyword)
  "a1 -> 0 0"
  (square (position (char-downcase (char keyword 0)) *letters*)
          (1- (parse-integer keyword :start 1))))

(defun piece-color (piece) (car piece))
(defun piece-name  (piece) (cdr piece))
(defun piece  (color name) (cons color name))

(defun keyword-piece (piece)
  "wp -> (t . :p)"
  (piece (char-equal (char piece 0) #\w)
         (intern (string (char-upcase (char piece 1))) :keyword)))

(defun piece-keyword (piece)
  "(t . :p) -> wp"
  (concatenate 'string
               (if (piece-color piece) "w" "b")
               (string-downcase (piece-name piece))))

(defun equal-color (piece1 piece2)
  (eql (piece-color piece1) (piece-color piece2)))

(defun equal-square (square1 square2)
  (equal square1 square2))

;;;

(defun check-move (board from to color)
  (let ((piece-from (board-square board from))
        (piece-to (board-square board to)))
    (unless (or (null piece-from)
                (equal-square from to)
                (and piece-to (equal-color piece-from piece-to))
                (not (eql (piece-color piece-from) color)))
      (check-piece-move board from to color))))

(defun check-piece-move (board from to color)
  (funcall (intern (concatenate 'string "CHECK-"
                                (string (piece-name (board-square board from)))))
           board from to color))

(defun check-r (board from to color)
  (multiple-value-bind (rank+ file+ length) (directions from to)
    (and rank+
         (or (zerop rank+) (zerop file+))
         (free-path-p board from to))))

(defun check-n (board from to color)
  (let ((file-diff (abs (- (file to) (file from))))
        (rank-diff (abs (- (rank to) (rank from)))))
    (or (and (= file-diff 2) (= rank-diff 1))
        (and (= file-diff 1) (= rank-diff 2)))))

(defun check-p (board from to color)
  (multiple-value-bind (rank+ file+ length) (directions from to)
    (and rank+
         (if color
             (plusp file+)
             (minusp file+))
         (if (zerop rank+)
            (and
             (<= length (if (= (file from) (if color 1 6)) 2 1))
             (free-path-p board from to t))
            (and (<= length 1)
                 (board-square board to)))
         (if (= (file to) (if color 7 0))
             (cons :promotion
                   (select-promotion))
             t))))

(defun check-q (board from to color)
  (multiple-value-bind (rank+ file+ length) (directions from to)
    (and rank+
         (free-path-p board from to))))

(defun check-k (board from to color)
  (multiple-value-bind (rank+ file+ length) (directions from to)
    (and rank+
         (case length
           (2 (check-castling board from to color))
           (1 t)))))

(defun check-b (board from to color)
  (multiple-value-bind (rank+ file+ length) (directions from to)
    (and rank+
         (not (or (zerop rank+) (zerop file+)))
         (free-path-p board from to))))

;; Changes to coordinates:
;;  -+    0+   ++
;;     \  |  /
;; -0 --     -- +0
;;     /  |  \
;;  --    0-   +-
  
(defun directions (from to)
  "Return direction of the move and length of the path.
If move is illegal, return nil."
  (let ((file-diff (- (file to) (file from)))
        (rank-diff (- (rank to) (rank from))))
    (and (or (zerop file-diff) (zerop rank-diff)
             (= (abs file-diff) (abs rank-diff)))
         (values (signum rank-diff) (signum file-diff)
                 (max (abs file-diff) (abs rank-diff))))))

(defun free-path-p (board from to &optional inclusive)
  (multiple-value-bind (rank+ file+ length) (directions from to)
    (loop repeat (- length (if inclusive 0 1))
          for rank = (+ (rank from) rank+) then (+ rank rank+)
          for file = (+ (file from) file+)  then (+ file file+)
          never (board-square board (square rank file)))))

(defun make-move (board from to color)
  (let ((check (check-move board from to color)))
    (when check
      (push (record-move board from to) (moves board))
      
      (psetf (board-square board from) nil
             (board-square board to) (if (atom check)
                                         (board-square board from)
                                         (piece color (cdr check))))  ; Promotion
      (when (and (eql (piece-name (board-square board to)) :k)
                 (check-castling board from to color))
        (make-castling board to))
      (adjust-castling board from to)
      t)))

(defun record-move (board from to)
  (list from to (board-square board to)))

(defun retract-move (board move)
  (destructuring-bind (from to captured) move
      (setf (board-square board from) (board-square board to)
            (board-square board to) captured)))

;;; Castling

(defun castling-p (board color)
  (if color
      (white-castling board)
      (black-castling board)))

(defun (setf castling-p) (value board color)
  (if color
      (setf (white-castling board) value)
      (setf (black-castling board) value)))

(defun adjust-castling (board from to)
  (let ((piece (board-square board to)))
    (case (piece-name piece)
      (:k (setf (castling board (piece-color piece)) (cons nil nil)))
      (:r (case (rank from)
            (0 (setf (car (castling-p board (piece-color piece))) nil))
            (7 (setf (cdr (castling-p board (piece-color piece))) nil)))))))

(defun check-castling (board from to color)
  (multiple-value-bind (rank+ file+ length) (directions from to)
    (and rank+ (zerop file+) (= length 2)
         (if (plusp rank+)
             (cdr (castling-p board color))
             (car (castling-p board color))))))

(defun make-castling (board to)
  (let (from
        (file (file to)))
    (if (= (rank to) 6)                      ; short castling
        (setf from (square 7 file)
              to (square 5 file))
        (setf from (square 0 file)
              to (square 3 file)))
    (psetf (board-square board from) nil
           (board-square board to) (board-square board from))))
