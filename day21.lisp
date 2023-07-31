;; (ql:quickload '(#:defpackage-plus #:alexandria #:serapeum #:cl-ppcre #:series))

(defpackage+-1:defpackage+ #:aoc2022-day21
  (:use #:cl #:series)
  (:import-from #:serapeum #:-> #:~>> #:~>)
  (:local-nicknames (#:a #:alexandria) (#:s #:serapeum) (#:re #:cl-ppcre)))

(in-package #:aoc2022-day21)

(s:eval-always (series::install))

;; OUTPUT what will monkey "root" will yell?
;; PART1 = 121

(defun parse (filename ))
