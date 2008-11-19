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

(defun make-inital-position ()
  (make-array '(8 8) :initial-contents *initial-position*))

;;; Square abstraction
(defun square (rank file) (cons rank file))
(defun rank (square) (car square))
(defun file (square) (cdr square))

(defun valid-square-p (square)
  (and (>= 7 (rank square) 0)
       (>= 7 (file square) 0)))

(defun board-square (board square)
  (when (valid-square-p square)
    (aref board (rank square) (file square))))

(defun (setf board-square) (value board square)
  (when (valid-square-p square)
    (setf (aref board (rank square) (file square))
          value)))

(defun square-keyword (square)
  "square -> :a1"
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

(defun check-move (board from to color)
  (let ((piece-from (board-square board from))
        (piece-to (board-square board to)))
    (if (or (null piece-from)
            (equal-square from to)
            (and piece-to (equal-color piece-from piece-to))
            (not (eql (piece-color piece-from) color))
            (not (check-piece-move board from to color)))
        nil
        t)))

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
  t)

(defun check-p (board from to color)
  (multiple-value-bind (rank+ file+ length) (directions from to)
    (and rank+
         (if color
             (plusp file+)
             (minusp file+))
         (if (zerop rank+)
            (and
             (<= length (if (= (if color 1 6) (file from)) 2 1))
             (free-path-p board from to t))
            (and (<= length 1)
                 (board-square board to))))))

(defun check-q (board from to color)
  (multiple-value-bind (rank+ file+ length) (directions from to)
    (and rank+
         (free-path-p board from to))))

(defun check-k (board from to color)
  (multiple-value-bind (rank+ file+ length) (directions from to)
    (and rank+ (< length 2))))

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
  (let* ((file-diff (- (file to) (file from)))
         (rank-diff (- (rank to) (rank from)))
         (length (max (abs file-diff) (abs rank-diff))))
    (and (or (= (abs file-diff) (abs rank-diff)) (zerop file-diff) (zerop rank-diff))
         (values (/ rank-diff length)
                 (/ file-diff length)
                 length))))

(defun free-path-p (board from to &optional inclusive)
  (multiple-value-bind (rank+ file+ length) (directions from to)
    (loop repeat (- length (if inclusive 0 1))
          for rank = (+ (rank from) rank+) then (+ rank rank+)
          for file = (+ (file from) file+)  then (+ file file+)
          never (board-square board (square rank file)))))
