;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:clim-chess)

(defvar *initial-position*
  #((#\R #\P nil nil nil nil #\p #\r)
    (#\N #\P nil nil nil nil #\p #\n)
    (#\B #\P nil nil nil nil #\p #\b)
    (#\Q #\P nil nil nil nil #\p #\q)
    (#\K #\P nil nil nil nil #\p #\k)
    (#\B #\P nil nil nil nil #\p #\b)
    (#\N #\P nil nil nil nil #\p #\n)
    (#\R #\P nil nil nil nil #\p #\r)))

(defvar *letters* "abcdefgh")
(defvar *pieces* '("wr" "wn" "wb" "wq" "wk" "wp"
                   "br" "bn" "bb" "bq" "bk" "bp"))

(defclass board ()
  ((contents :initform
             (make-array '(8 8) :initial-contents *initial-position*)
             :reader contents)
   (moves :initform nil :accessor moves)
   (white-king :initform (keyword-square "e1") :accessor white-king)
   (black-king :initform (keyword-square "e8") :accessor black-king)
   (white-castling :initform (cons t t) ; long - short
                   :accessor white-castling)
   (black-castling :initform (cons t t)
                   :accessor black-castling)
   (en-passant :initform nil :accessor en-passant)
   (check :initform nil :accessor check)
   (checkmate :initform nil :accessor checkmate)
   (next-to-move :initform t :accessor next-to-move)
   (move-number :initform 0 :accessor move-number)))

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
  (aref (contents board) (rank square) (file square)))

(defun (setf board-square) (value board square)
  (setf (aref (contents board) (rank square) (file square))
        value))

(defun king-square (board color)
  (if color
      (white-king board)
      (black-king board)))

(defun (setf king-square) (square board color)
  (if color
      (setf (white-king board) square)
      (setf (black-king board) square)))

(defun square-keyword (square)
  "0 0 -> a1"
  (coerce (vector (char *letters* (rank square))
                  (digit-char (1+ (file square))))
          'string))

(defun keyword-square (keyword)
  "a1 -> 0 0"
  (square (position (char-downcase (char keyword 0)) *letters*)
          (1- (digit-char-p (char keyword 1)))))

(defun piece-color (piece) (upper-case-p piece))
(defun piece-name  (piece) (char-downcase piece))
(defun piece  (color name) (if color
                               (char-upcase name)
                               (char-downcase name)))

(defun keyword-piece (piece)
  "wp -> #\P"
  (piece (char-equal (char piece 0) #\w)
         (char piece 1)))

(defun piece-keyword (piece)
  "#\p -> bp"
  (concatenate 'string
               (if (piece-color piece) "w" "b")
               (string (piece-name piece))))

(defun same-color-p (piece1 piece2)
  (eql (piece-color piece1) (piece-color piece2)))

(defun same-square-p (square1 square2)
  (equal square1 square2))

(defun same-piece-p (piece1 piece2)
  (equal piece1 piece2))

(defmacro do-matrix ((x y) &body body)
  `(loop for ,x to 7
         do (loop named ,(gensym)
                  for ,y to 7
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

(defun copy-board (board &optional (into (make-instance 'board)))
  (macrolet ((copy (accessors)
               `(setf ,@(loop for i in accessors
                              for consp = (consp i)
                              if consp do (setf i (car i))
                              collect `(,i into)
                              collect (if consp
                                          `(copy-list (,i board))
                                          `(,i board))))))
    (do-board (piece board square)
      (setf (board-square into square) piece))
    (copy (en-passant check (moves)
           white-king black-king move-number
           (white-castling) (black-castling))))
  into)

(defun move (board from to &optional promote)
  (setf (board-square board to) (or promote (board-square board from))
        (board-square board from) nil))

;;;

(defun check-move (board from to color)
  (when (and (valid-square-p from) (valid-square-p to)
             (not (checkmate board)))
    (let ((piece-from (board-square board from))
          (piece-to (board-square board to)))
      (unless (or (null piece-from)
                  (same-square-p from to)
                  (when piece-to (same-color-p piece-from piece-to))
                  (not (eql (piece-color piece-from) color)))
        (check-piece-move board from to color)))))

(defun check-piece-move (board from to color)
  (funcall (find-symbol (string-upcase (piece-name (board-square board from)))
                        '#:clim-chess)
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
        do (setf from (add-square from diff))
        never (board-square board from)))

(defmacro def-check (name &body body)
  `(defun ,name (board from to color)
     (declare (ignorable board color))
     (multiple-value-bind (rank+ file+ length) (directions from to)
       (declare (ignorable file+ length))
       (macrolet ((free-path-p (&optional inclusive)
                    `(%free-path-p length (cons rank+ file+)
                                   board from ,inclusive)))
         (when rank+ ,@body)))))

(def-check r
  (and (or (zerop rank+) (zerop file+))
       (free-path-p)))

(def-check p
  (and
   (if color
       (plusp file+)   ; up
       (minusp file+)) ; down
   (if (zerop rank+)   ; vertical move
       (and
        (free-path-p t)
        (if (and (= (file from) (if color 1 6))
                 (= length 2))
            (return-from p 'en-passant)
            (= length 1)))
       (when (= length 1)
         (if (same-square-p to (en-passant board))
             (return-from p 'en-passant*)
             (board-square board to))))
   (if (= (file to) (if color 7 0))
       'promotion
       t)))

(def-check q
  (free-path-p))

(def-check k
  (case length
    (2 (when (check-castling board from to color)
         'castling))
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

(defun make-move (board from to)
  (let* ((color (next-to-move board))
         (move (test-move board from to color)))
    (when move
      (copy-board move board)
      (adjust-board board (not color))
      t)))

(defun test-move (board from to color &optional promotion)
  (let* ((board (copy-board board))
         (move (check-move board from to color)))
    (when move
      (move board from to
            (when (eq move 'promotion)
              (piece color (or promotion
                               (select-promotion)))))
      (adjust-castling board from to move)
      (adjust-en-passant board to color move)
      (unless (check-p board color)
        board))))

(defun adjust-board (board color)
  "Prepare board for the next move."
  (let ((check (check-p board color)))
    (setf (check board) (and check t))
    (when check
      (setf (checkmate board) (checkmate-p board color check))))
  (incf (move-number board))
  (setf (next-to-move board) color))

(defun retract-move (board move)
  (destructuring-bind (from to captured) move
    (setf (board-square board from) (board-square board to)
          (board-square board to) captured)))

;;; Castling

(defun castlings (board color)
  (if color
      (white-castling board)
      (black-castling board)))

(defun adjust-castling (board from to castling)
  (let* ((piece (board-square board to))
         (castlings (castlings board (piece-color piece))))
    (case (piece-name piece)
      (#\k (setf (king-square board (piece-color piece)) to)
           (when (eq castling 'castling)
             (make-castling board to))
           (setf (car castlings) nil (cdr castlings) nil))
      (#\r (case (rank from)
             (0 (setf (car castlings) nil))
             (7 (setf (cdr castlings) nil)))))))

(def-check check-castling
  (and (not (check board))
       (zerop file+) (= length 2) ; horizontal move
       (if (plusp rank+)          ; long / short castling
           (and
            (cdr (castlings board color))
            (%free-path-p 3 '(1 . 0) board from))
           (and (car (castlings board color))
                (%free-path-p 4 '(-1 . 0) board from)))))

(defun make-castling (board to)
  (let (from
        (file (file to)))
    (if (= (rank to) 6) ; short castling
        (setf from (square 7 file)
              to (square 5 file))
        (setf from (square 0 file)
              to (square 3 file)))
    (move board from to)))

;;; En-passant

(defun adjust-en-passant (board to color en-passant-p)
  (let ((square-above (add-square to (if color
                                         '(0 . -1)
                                         '(0 . 1)))))
    (when (eql en-passant-p 'en-passant*)
      (setf (board-square board square-above) nil))
    (setf (en-passant board)
          (when (eql en-passant-p 'en-passant)
            square-above))))

;;; Check

(defvar *moves* '((-1  .  1) (0  .  1) (1  .  1)
                  (-1  .  0)           (1  .  0)
                  (-1  . -1) (0  . -1) (1  . -1)))

(defun check-p (board color)
  (square-attacked-by board (king-square board color) color))

(defun square-attacked-by (board square color &optional test-move)
  (append (attacks-by-knight board square color test-move)
          (loop for diff in *moves*
                when (attacked-from-p board square color diff test-move)
                collect it)))

(defun attacked-from-p (board square color diff &optional test-move)
  (loop for from = (add-square square diff)
        then (add-square from diff)
        while (valid-square-p from)
        until (board-square board from)
        finally (return
                  (and (if test-move
                           (test-move board from square (not color) nil)
                           (check-move board from square (not color)))
                       from))))

(defvar *knight-moves* '((-1  .  2) (1  .  2)
                         (-2  .  1) (2  .  1)
                         (-2  . -1) (2  . -1)
                         (-1  . -2) (1  . -2)))

(defun attacks-by-knight (board square color &optional test-move)
  (loop for diff in *knight-moves*
        for knight-square = (add-square square diff)
        when (if test-move
                 (test-move board knight-square square (not color) nil)
                 (check-move board knight-square square (not color)))
        collect knight-square into result
        and count 1 into count
        when (= count 2) do (loop-finish)
        finally (return result)))

;;; Checkmate

(defun can-king-move-p (board color)
  (let ((king-square (king-square board color)))
    (loop for diff in *moves*
          for square = (add-square king-square diff)
          thereis (test-move board king-square square
                             color))))

(def-check %can-defend-from-p
  (loop repeat length
        for square = from then (add-square square (cons rank+ file+))
        thereis (square-attacked-by board square color t)))

(defun can-defend-from-p (board from color)
  ;; we can only capture a knight
  (if (eql #\n (piece-name (board-square board from)))
      (square-attacked-by board from (not color) t)
      (%can-defend-from-p board from (king-square board color) (not color))))

(defun checkmate-p (board color attacks)
  (not
   (or (can-king-move-p board color)
       ;; if king is attacked by more than one piece
       ;; and cannot move, he is dead
       (when (= (length attacks) 1)
         (can-defend-from-p board (car attacks) color)))))

;;; Stalemate

;; (defun stalemate-p (board color)
;;   (do-board (piece board square)
;;     (when (and piece (eql color (piece-color piece))
;;                (can-move-p board square piece)))))

;; (defun can-move-p ()
;;   )
