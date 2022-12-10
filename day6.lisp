(ql:quickload '(#:defpackage-plus #:alexandria #:str #:serapeum))

(defpackage+-1:defpackage+ #:aoc2022-day6
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:s #:serapeum)
                    (#:re #:cl-ppcre)))

(in-package #:aoc2022-day6)

(defun parse ()
  (str:trim
   (a:read-file-into-string "day6.test.txt")))

(defun silver ())
(defun gold ())
(defun main ())
