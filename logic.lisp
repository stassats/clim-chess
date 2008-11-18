;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:clim-chess)

(defvar *initial-position*
  '((:wr :wp nil nil nil nil :bp :br)
    (:wn :wp nil nil nil nil :bp :bn)
    (:wb :wp nil nil nil nil :bp :bb)
    (:wq :wp nil nil nil nil :bp :bq)
    (:wk :wp nil nil nil nil :bp :bk)
    (:wb :wp nil nil nil nil :bp :bb)
    (:wn :wp nil nil nil nil :bp :bn)
    (:wr :wp nil nil nil nil :bp :br)))

(defvar *letters* "abcdefgh")
(defvar *pieces* '(:wr :wn :wb :wq :wk :wp :br :bn :bb :bq :bk :bp))
(defvar *player-color* :w)

(defun make-inital-position ()
  (make-array '(8 8) :initial-contents *initial-position*))

;;; Square abstraction
(defun square (x y) (cons x y))
(defun rank (square) (car square))
(defun file (square) (cdr square))

(defun valid-square-p (square)
  (and (>= 7 (rank square) 0)
       (>= 7 (file square) 0)))

(defun square-keyword (square)
  "square -> :a1"
  (intern (string-upcase
           (coerce (vector (char *letters* (rank square))
                           (digit-char (1+ (file square))))
                   'simple-string))
          :keyword))

(defun keyword-square (keyword)
  "a1 -> 0 0"
  (let* ((string (string-downcase keyword))
         (first (char string 0))
         (second (parse-integer string :start 1)))
    (square (position first *letters*)
            (1- second))))

(defun board-square (board square)
  (when (valid-square-p square)
    (aref board (rank square) (file square))))

(defun (setf board-square) (value board square)
  (when (valid-square-p square)
    (setf (aref board (rank square) (file square))
          value)))

(defun piece-color (piece)
  (string (char (string (string piece)) 0)))

(defun piece-name (piece)
  (string (char (string (string piece)) 1)))

(defun equal-color (piece1 piece2)
  (string= piece2 piece1 :end1 1 :end2 1))

(defun equal-square (square1 square2)
  (equal square1 square2))

(defun check-move (board from to)
  (let ((piece-from (board-square board from))
        (piece-to (board-square board to)))
    (if (or (equal-square from to)
            (null piece-from)
            (equal-color piece-from piece-to)
            (not (equal-color piece-from *player-color*))
            (not (check-piece-move board from to)))
        nil
        t)))

(defun check-piece-move (board from to)
  (funcall (intern (concatenate 'string "CHECK-"
                                (piece-name (board-square board from))))
           board from to))

(defun check-r (board from to)
  (if (and (/= (file from) (file to))
           (/= (rank from) (rank to)))
      nil
      t))

(defun check-n (board from to)
  t)

(defun check-p (board from to)
  t)

(defun check-q (board from to)
  t)

(defun check-k (board from to)
  t)

(defun check-b (board from to)
  (if (or (= (file from) (file to))
          (= (rank from) (rank to))
          (and (/= (- (file to) (file from))
                   (- (rank from) (rank to)))
               (/= (- (file from) (file to))
                   (- (rank from) (rank to)))))
      nil
      t))
