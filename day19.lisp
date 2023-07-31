;; (ql:quickload '(#:defpackage-plus #:alexandria #:serapeum #:cl-ppcre #:series))

(defpackage+-1:defpackage+ #:aoc2022-day19
  (:use #:series #:cl)
  (:import-from #:serapeum #:-> #:~>> #:~>)
  (:local-nicknames (#:a #:alexandria) (#:s #:serapeum) (#:re #:cl-ppcre)))

(in-package #:aoc2022-day19)

(s:eval-always (series::install))

;; you start wih 1 free ORE collecting robot
;; each robot collect 1 resource per minute
;; 1 minute to construct 1 robot
;; What is the BEST blueprint?
;; - maximize number of geodes in 24 minutes
;; "qualitity level" = (blueprint id * largest nr geodes)
;; OUTPUT = sum of all quality levels

(defun blueprints (filename)
  (~>> (a:read-file-into-string filename)
       (re:all-matches-as-strings "\\d+")
       (mapcar #'parse-integer)))

(defun blueprint-quality (lst)
  (collect-sum
   (mapping (((id ) (chunk 7 7 (scan lst)))))))

(defun silver (blueprints)
  (~>> (s:batches _ 7)
       (mapcar #'blueprint-quality blueprints)
       (reduce #'+)))
