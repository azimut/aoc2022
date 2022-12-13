(ql:quickload
 '(#:defpackage-plus
   #:alexandria
   #:serapeum
   #:trivia
   #:cl-ppcre))

(defpackage+-1:defpackage+ #:aoc2022-day13
  (:use #:cl #:trivia)
  (:import-from #:serapeum #:-> #:~>> #:~>)
  (:local-nicknames (#:a #:alexandria)
                    (#:s #:serapeum)
                    (#:re #:cl-ppcre)))

(in-package #:aoc2022-day13)

(defun lines (s) (re:split "\\n" s))
(defun slurp (f) (a:read-file-into-string f))

(defun packets (filename)
  (~>> (slurp filename)
       (substitute #\( #\[)
       (substitute #\) #\])
       (substitute #\Space #\,)
       (s:trim-whitespace)
       (re:split "\\n\\n")
       (mapcar #'lines)
       (mapcar (s:partial #'mapcar #'read-from-string))))

(defun-match ordered-p (packet-pair)
  ((list NIL NIL) T)
  ((list NIL _)   T)
  ((list _ NIL) NIL)
  ;; 3) One Integer - One List
  ((guard (list l n) (and (listp l) (numberp n))) (ordered-p (list l (list n))))
  ((guard (list n l) (and (listp l) (numberp n))) (ordered-p (list (list n) l)))
  ;; 2) Both Lists
  ((list (cons l ls) (cons r rs))
   (and (ordered-p (list l r))
        (ordered-p (list ls rs))))
  ;; 1) Both Integers
  ((list a b) (<= a b)))

(defun silver (filename)
  (loop :for packet :in (packets filename)
        :for i :from 1
        :when (ordered-p packet)
          :summing i))
