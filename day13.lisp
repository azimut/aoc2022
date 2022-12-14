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

(defun-ematch compare (packet-pair)
  ((guard (list (cons left left-tail) (cons right right-tail))
          (and (numberp left) (numberp right)))
   (if (and (numberp (compare (list left right)))
            (zerop (compare (list left right))))
       (compare (list left-tail right-tail))
       (compare (list left right))))
  ((guard (list (cons left left-tail) (cons right right-tail))
          (and (listp left) (listp right)))
   (if (and (numberp (compare (list left right)))
            (zerop   (compare (list left right))))
       (compare (list left-tail right-tail))
       (compare (list left right))))
  ((guard (list (cons left left-tail) right)
          (numberp left))
   (compare (list (cons `(,left) left-tail) right)))
  ((guard (list left (cons right right-tail))
          (numberp right))
   (compare (list left (cons `(,right) right-tail))))
  ((list NIL NIL)                  0)
  ((guard (list NIL r) (listp r)) -1)
  ((guard (list l NIL) (listp l))  1)
  ((guard (list l r) (and (numberp l) (numberp r)))
   (signum (- l r))))

(defun silver (filename)
  (loop :for packet-pair :in (packets filename)
        :for i :from 1
        :when (= -1 (compare packet-pair))
          :summing i))
