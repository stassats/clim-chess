;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:clim-chess)

(defvar *initial-position*
  #(((t . :r) (t . :p) nil nil nil nil (nil . :p) (nil . :r))
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
   (white-king :initform (keyword-square "e1") :accessor white-king)
   (black-king :initform (keyword-square "e8") :accessor black-king)
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

(defun add-square (square add)
  "(4 . 0) + (1 . 1) => (5 . 1)"
  (square (+ (rank square) (rank add))
          (+ (file square) (file add))))

(defun board-square (board square)
  (when (valid-square-p square)
    (aref (contents board) (rank square) (file square))))

(defun (setf board-square) (value board square)
  (when (valid-square-p square)
    (setf (aref (contents board) (rank square) (file square))
          value)))

(defun find-king (board color)
  (if color
      (white-king board)
      (black-king board)))

(defun square-keyword (square)
  "0 0 -> a1"
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

(defun same-color-p (piece1 piece2)
  (eql (piece-color piece1) (piece-color piece2)))

(defun same-square-p (square1 square2)
  (equal square1 square2))

(defun same-piece-p (piece1 piece2)
  (equal piece1 piece2))

(defmacro do-matrix ((x y) &body body)
  `(loop for ,x to 7
         do (loop for ,y to 7
                  do (progn ,@body))))

(defmacro do-board ((piece board &optional square) &body body)
  (let ((x (gensym "RANK"))
        (y (gensym "FILE"))
        (%board (gensym "BOARD"))
        (%square (or square (gensym "SQUARE"))))
    `(let ((,%board ,board))
       (do-matrix (,x ,y)
         (let* ((,%square (square ,x ,y))
                (,piece (board-square ,%board ,%square)))
           ,@body)))))

;;;

(defun check-move (board from to color)
  (let ((piece-from (board-square board from))
        (piece-to (board-square board to)))
    (unless (or (not (valid-square-p to))
                (null piece-from)
                (same-square-p from to)
                (when piece-to (same-color-p piece-from piece-to))
                (not (eql (piece-color piece-from) color)))
      (check-piece-move board from to color))))

(defun check-piece-move (board from to color)
  (funcall (intern (symbol-name (piece-name (board-square board from)))
                   :clim-chess)
           board from to color))


;; Changes to coordinates
;;  -+    0+   ++
;;     \  |  /
;; -0 -- 0 0 -- +0
;;     /  |  \
;;  --    0-   +-

(defun directions (from to)
  "Return direction of the move and length of the path.
If move is illegal, return nil."
  (let ((rank-diff (- (rank to) (rank from)))
        (file-diff (- (file to) (file from))))
    (and (or (zerop rank-diff) (zerop file-diff)  ; vertical/horizontal move
             (= (abs rank-diff) (abs file-diff))) ; diagonal move
         (values (signum rank-diff) (signum file-diff)
                 (max (abs rank-diff) (abs file-diff))))))

(defun %free-path-p (length diff board from &optional inclusive)
  (loop repeat (- length (if inclusive 0 1))
        for square = (add-square from diff) then (add-square square diff)
        never (board-square board square)))

(defmacro def-check (name &body body)
  `(defun ,name (board from to color)
     (declare (ignorable board from to color))
     (multiple-value-bind (rank+ file+ length) (directions from to)
       (declare (ignorable file+ length))
       (macrolet ((free-path-p (&optional inclusive)
                    `(%free-path-p length (cons rank+ file+)
                                   board from ,inclusive)))
        (if rank+ ,@body)))))

(def-check r
  (and (or (zerop rank+) (zerop file+))
       (free-path-p)))

(def-check p
  (and
   (if color
       (plusp file+)
       (minusp file+))
   (if (zerop rank+)
       (and
        (<= length
            (if (= (file from) (if color 1 6)) 2 1))
        (free-path-p t))
       (and (<= length 1)
            (board-square board to)))
   (if (= (file to) (if color 7 0))
       :promotion
       t)))

(def-check q
  (free-path-p))

(def-check k
  (case length
    (2 (check-castling board from to color))
    (1 t)))

(def-check b
  (and (not (zerop rank+))
       (not (zerop file+))
       (free-path-p)))

(defun n (board from to color)
  (declare (ignore board color))
  (let ((file-diff (abs (- (file to) (file from))))
        (rank-diff (abs (- (rank to) (rank from)))))
    (or (and (= file-diff 2) (= rank-diff 1))
        (and (= file-diff 1) (= rank-diff 2)))))

;;;

(defun make-move (board from to color)
  (let ((check (check-move board from to color)))
    (when check
      (push (record-move board from to) (moves board))

      (psetf (board-square board from) nil
             (board-square board to) (if (eq check t)
                                         (board-square board from)
                                         (piece color (select-promotion))))
      (when (eql (piece-name (board-square board to)) :k)
        (if color
            (setf (white-king board) to)
            (setf (black-king board) to))
        (when (check-castling board from to color)
            (make-castling board to)))
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
  (let* ((piece (board-square board to))
         (color (piece-color piece)))
    (case (piece-name piece)
      (:k (setf (castling-p board color) (cons nil nil)))
      (:r (case (rank from)
            (0 (setf (car (castling-p board color)) nil))
            (7 (setf (cdr (castling-p board color)) nil)))))))

(def-check check-castling
  (and (zerop file+) (= length 2)
       (if (plusp rank+)
           (cdr (castling-p board color))
           (car (castling-p board color)))))

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

;;;

(defvar *moves* '((-1  .  1) (0  .  1) (1  .  1)
                  (-1  .  0)           (1  .  0)
                  (-1  . -1) (0  . -1) (1  . -1)))

(defun check-p (board king-square color)
  (or (attacked-by-knight-p board king-square color)
      (loop for diff in *moves*
            thereis (attacked-from-p board king-square color diff))))

(defun attacked-from-p (board king-square color diff)
  (loop for square = (add-square king-square diff)
        then (add-square square diff)
        while (valid-square-p square)
        until (board-square board square)
        finally (return
                  (check-move board square king-square (not color)))))

(defvar *knight-moves* '((-1  .  2) (1  .  2)
                         (-2  .  1) (2  .  1)
                         (-2  . -1) (2  . -1)
                         (-1  . -2) (1  . -2)))

(defun attacked-by-knight-p (board king-square color)
  (loop for diff in *knight-moves*
        for square = (add-square king-square diff)
        thereis (check-move board square king-square (not color))))

