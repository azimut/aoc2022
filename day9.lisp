(ql:quickload '(#:defpackage-plus #:alexandria #:serapeum #:rtg-math))

(defpackage+-1:defpackage+ #:aoc2022-day9
  (:use #:cl #:rtg-math)
  (:export #:main)
  (:import-from #:serapeum #:->)
  (:local-nicknames (#:a #:alexandria)
                    (#:s #:serapeum)))

(in-package #:aoc2022-day9)

;; OUTPUT = count the N of positions the TAIL visited at least once, counting STARTING position
;; PART 1 = 13      = 5858
;; PART 2 =  1 = 36 = 2602

(-> motions (string) list)
(defun motions (filename)
  (flet ((f (s)
           (a:eswitch (s :test #'string=)
             ("U" (v!  0  1))
             ("D" (v!  0 -1))
             ("L" (v! -1  0))
             ("R" (v!  1  0)))))
    (with-open-file (fd filename)
      (loop :for line := (read-line fd nil nil)
            :while line
            :collect
            `(,(f (subseq line 0 1))
              ,(parse-integer (subseq line 2)))))))

(-> new-position (rtg-math.types:vec2 rtg-math.types:vec2)
    rtg-math.types:vec2)
(defun new-position (tail head &aux (diff (v2:- head tail)))
  (flet ((f (i) (cond ((> i 0)  1) ((< i 0) -1) ((= i 0)  0))))
    (if (>= (v2:distance head tail)
            2.0)
        (v2:+ tail (v! (f (x diff)) (f (y diff))))
        tail)))

(-> silver (list) fixnum)
(defun silver (motions &aux (visits ()) (head (v! 0 0)) (tail (v! 0 0)))
  (dolist (motion motions)
    (destructuring-bind (direction by) motion
      (dotimes (i by)
        (v2:incf head direction)
        (push (setf tail (new-position tail head))
              visits))))
  (length
   (remove-duplicates visits :test #'v2:=)))

(-> move-tails (list rtg-math.types:vec2)
    list)
(defun move-tails (tails head)
  (loop :for tail :in tails
        :with tmp-head := head
        :collect (setf tmp-head (new-position tail tmp-head))))

(-> gold (list) fixnum)
(defun gold (motions &aux (visits ()) (head (v! 0 0)) (tails (make-list 9 :initial-element (v! 0 0))))
  (dolist (motion motions)
    (destructuring-bind (direction by) motion
      (dotimes (i by)
        (v2:incf head direction)
        (push (a:lastcar (setf tails (move-tails tails head)))
              visits))))
  (length
   (remove-duplicates visits :test #'v2:=)))
