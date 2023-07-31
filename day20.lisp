;; (ql:quickload '(#:defpackage-plus #:alexandria #:serapeum #:cl-ppcre #:series))

(defpackage+-1:defpackage+ #:aoc2022-day20
  (:use #:cl #:series)
  (:import-from #:serapeum #:-> #:~>> #:~>)
  (:local-nicknames (#:a #:alexandria) (#:s #:serapeum) (#:re #:cl-ppcre)))

(in-package #:aoc2022-day20)

;; PART 1 = 3

(defstruct codex
  (new      0 :type fixnum)
  (original 0 :type fixnum)
  (value    0 :type fixnum))

(defun parse (filename &aux (id -1))
  (~>> (a:read-file-into-string filename)
       (re:all-matches-as-strings "[-]?\\d+")
       (mapcar #'parse-integer)
       (mapcar (s:op (make-codex :original (incf id) :new id :value _)))))

(defun mixing (numbers &aux (size (length numbers)))
  (print (mapcar #'codex-value numbers))
  (loop :for codex1 :in (loop :repeat 4 :for i :in numbers :collect i)
        :for old-pos := (codex-new codex1)
        :for new-pos := (mod (+ (codex-new codex1) (codex-value codex1)) size)
        :do (format t "~d = ~d -> ~d~%" (codex-value codex1) old-pos new-pos)
            (loop :for codex2 :in numbers
                  :for this-pos := (codex-new codex2)

                  :when (and (plusp (codex-value codex1)) (> new-pos old-pos)
                             (>  this-pos old-pos)
                             (<= this-pos new-pos))
                    :do (decf (codex-new codex2))
                  :when (and (plusp (codex-value codex1)) (< new-pos old-pos)
                             (>  this-pos old-pos)
                             (>= this-pos new-pos))
                    :do (setf (codex-new codex2) (mod (1- (codex-new codex2)) size))

                  :when (and (minusp (codex-value codex1)) (< new-pos old-pos)
                             (<  this-pos old-pos)
                             (>= this-pos new-pos))
                    :do (incf (codex-new codex2))
                  :when (and (minusp (codex-value codex1)) (> new-pos old-pos)
                             (<  this-pos old-pos)
                             (>= this-pos new-pos))
                    :do (incf (codex-new codex2)))
            (setf (codex-new codex1) new-pos))
  (print (mapcar #'codex-value (sort numbers #'< :key #'codex-new)))
  (sort numbers #'< :key #'codex-new))

(defun silver (numbers)
  (let ((circular (~>> numbers  (mixing) (apply #'a:circular-list))))
    (list (codex-value (nth 1000 circular))
          (codex-value (nth 2000 circular))
          (codex-value (nth 3000 circular)))))
