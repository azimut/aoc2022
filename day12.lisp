(ql:quickload '(#:defpackage-plus #:alexandria #:serapeum #:array-operations))

(defpackage+-1:defpackage+ #:aoc2022-day12
  (:use #:cl)
  (:import-from #:serapeum #:-> #:~>> #:~>)
  (:local-nicknames (#:a #:alexandria) (#:s #:serapeum)))

(in-package #:aoc2022-day12)

(defun slurp (f) (a:read-file-into-string f))

(defun make-adj-list (mat &aux (rows (array-dimension mat 0)) (cols (array-dimension mat 1)))
  (flet ((f (x y cols) (+ y (* x cols))))
    (s:lret ((adj (make-hash-table)))
      (array-operations/utilities:nested-loop (x y) (array-dimensions mat)
        (when (> x 0)         (push (f (1- x) y cols) (s:href adj (f x y cols))))
        (when (> y 0)         (push (f x (1- y) cols) (s:href adj (f x y cols))))
        (when (< x (1- rows)) (push (f (1+ x) y cols) (s:href adj (f x y cols))))
        (when (< y (1- cols)) (push (f x (1+ y) cols) (s:href adj (f x y cols))))))))

(defun make-heightmap (asciimap)
  (s:lret ((result (make-array (array-dimensions asciimap) :element-type 'fixnum)))
    (flet ((f (c) (- (char-code (case c (#\S #\a) (#\E #\z) (t c)))
                     (char-code #\a))))
      (array-operations/utilities:nested-loop (x y) (array-dimensions asciimap)
        (setf (aref result x y) (f (aref asciimap x y)))))))

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

(defun bfs (start end heightmap adjlist
            &aux (size (array-total-size heightmap))
              (visited (make-array size :initial-element NIL))
              (prev  (make-array size  :initial-element -1))
              (queue (s:queue)))
  (s:enq start queue)
  (loop :until (s:queue-empty-p queue)
        :for idx := (s:deq queue)
        :do (dolist (next (s:href adjlist idx))
              (when (and (not (aref visited next))
                         (<= (row-major-aref heightmap next)
                             (1+ (row-major-aref heightmap idx))))
                (setf (aref visited next) T)
                (s:enq next queue)
                (setf (aref prev next) idx))))
  (length 
   (bfs-reconstruct prev start end)))

(defun bfs-reconstruct (prev start end)
  (loop :with at := end
        :until (or (minusp at) (= at start))
        :collect (setf at (aref prev at))))

(defun silver (filename)
  (destructuring-bind (&key adj start end heightmap asciimap) (adj-lists filename)
    (bfs start end heightmap adj)))

(defun find-starts (heightmap)
  (loop :for idx :below (array-total-size heightmap)
        :when (zerop (row-major-aref heightmap idx))
          :collect idx))

(defun gold (filename)
  (destructuring-bind (&key adj end heightmap &allow-other-keys) (adj-lists filename)
    (loop :for some-a :in (find-starts heightmap)
          :for distance := (bfs some-a end heightmap adj)
          :when (> distance 1)
            :minimizing distance)))
