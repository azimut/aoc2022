(ql:quickload '(#:defpackage-plus #:alexandria #:serapeum #:cl-ppcre #:cl-slice))

(defpackage+-1:defpackage+ #:aoc2022-day10
  (:use #:cl)
  (:import-from #:serapeum #:-> #:~>>)
  (:import-from #:cl-slice #:slice)
  (:local-nicknames (#:a #:alexandria) (#:s #:serapeum) (#:re #:cl-ppcre)))

(in-package #:aoc2022-day10)

(defstruct cpu cycle x)

(-> cpu-states (String) List)
(defun cpu-states (filename)
  (with-open-file (fd filename)
    (loop
      :for line := (read-line fd nil nil)
      :while line
      :with cycle := 1
      :with x := 1
      :collect
      (let ((words (re:split " " line)))
        (if (a:length= 2 words)
            (make-cpu :cycle (incf cycle 2) :x (incf x (parse-integer (second words))))
            (make-cpu :cycle (incf cycle)   :x x))))))

(-> silver (List) Fixnum)
(defun silver (cpu-states &aux (rcs (reverse cpu-states)) (signal-cycles '(20 60 100 140 180 220)))
  (~>> (loop :for signal-cycle :in signal-cycles
             :collect (find signal-cycle rcs :key #'cpu-cycle :test #'>=))
       (remove-if #'null)
       (mapcar #'cpu-x)
       (mapcar #'* signal-cycles)
       (reduce #'+)))

(defun gold (cpu-states &aux (screen (make-array '(6 40) :initial-element #\?)))
  (loop :for cycle :from 1 :to (* 6 40)
        :for x := (cpu-x (find cycle cpu-states :key #'cpu-cycle :test #'<=))
        :for drawp := (>= (1+ x) (mod cycle 40) (1- x))
        :do (setf (row-major-aref screen (1- cycle))
                  (if drawp #\# #\.)))
  (loop :for i :from 0 :to (1- 6)
        :collect (coerce (slice screen i t) 'string)))
