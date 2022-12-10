(ql:quickload '(#:defpackage-plus #:alexandria #:str #:serapeum))

(defpackage+-1:defpackage+ #:aoc2022-day1
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria) (#:s #:serapeum) (#:re #:cl-ppcre)))

(in-package #:aoc2022-day1)

(defun parse (filename)
  (s:~>> (a:read-file-into-string filename)
         (re:split "\\n\\n")
         (mapcar #'str:lines)
         (mapcar (s:op (mapcar #'parse-integer _)))
         (mapcar (s:op (reduce #'+ _)))))

(defun silver (elfs)
  (reduce #'max elfs))

(defun gold (elfs)
  (s:~>> (sort elfs #'>)
         (nthcdr 3)
         (reduce #'+)))

(defun main ()
  (let ((elfs (parse "day1.test.txt")))
    (values (silver elfs)
            (gold elfs))))
