(ql:quickload '(#:defpackage-plus #:alexandria #:serapeum #:array-operations))

(defpackage+-1:defpackage+ #:aoc2022-day12
  (:use #:cl)
  (:import-from #:serapeum #:-> #:~>> #:~>)
  (:local-nicknames (#:a #:alexandria) (#:s #:serapeum)))

(in-package #:aoc2022-day12)

(defstruct cell row-idx cost)

(defun slurp (f) (a:read-file-into-string f))
(defun xrid (cols pos) (list (floor (/ pos cols)) (- pos (* (floor (/ pos cols)) cols))))

(defun make-adj-list (mat &aux (rows (array-dimension mat 0)) (cols (array-dimension mat 1)))
  (flet ((f (x y cols) (+ y (* x cols))))
    (s:lret ((adj (make-hash-table :test #'equal)))
      (array-operations/utilities:nested-loop (x y) (array-dimensions mat)
        (when (> x 0)         (push (f (1- x) y cols) (s:href adj (f x y cols))))
        (when (> y 0)         (push (f x (1- y) cols) (s:href adj (f x y cols))))
        (when (< x (1- rows)) (push (f (1+ x) y cols) (s:href adj (f x y cols))))
        (when (< y (1- cols)) (push (f x (1+ y) cols) (s:href adj (f x y cols))))))))

(defun make-heightmap (asciimap)
  (flet ((f (c) 
           (- (char-code (case c (#\S #\a) (#\E #\z) (t c)))
              (char-code #\a))))
    (aops:each-index (x y)
      (f (aref asciimap x y)))))

(defun adj-lists (filename &aux (raw-arr (slurp filename)))
  (let* ((start (~>> raw-arr (remove #\NewLine) (position #\S)))
         (end   (~>> raw-arr (remove #\NewLine) (position #\E)))
         (cols  (~>> raw-arr (position #\NewLine)))
         (rows  (~>> raw-arr (count #\NewLine))) 
         (asciimap  (~>> raw-arr (remove #\NewLine) (aops:reshape _ `(,rows ,cols)))))
    `(:adj ,(make-adj-list asciimap)
      :start ,start :end ,end
      :heightmap ,(make-heightmap asciimap)
      :asciimap ,asciimap)))

;;------------------------------

(defun make-inf-array (n) (make-array n :initial-element MOST-POSITIVE-FIXNUM))

(defun bfs (start end asciimap heightmap adjlist &aux (distances (make-inf-array (array-total-size asciimap))) (queue (s:queue)))
  (s:enq start queue)
  (setf (aref distances start) 0)
  (dolist (i (s:href adjlist start)) (s:enq i queue))
  (loop :for idx := (s:deq queue)
        :while idx
        :do ))

(defun silver (filename)
  (destructuring-bind (&key adj start end mat &allow-other-keys) (adj-lists filename)
    (bfs start end (array-dimension mat 1) adj)))
