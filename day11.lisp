(ql:quickload
 '(#:defpackage-plus
   #:alexandria
   #:serapeum
   #:cl-ppcre
   #:arrows))

(defpackage+-1:defpackage+ #:aoc2022-day11
  (:use #:cl)
  (:import-from #:serapeum #:->)
  (:local-nicknames (#:a #:alexandria)
                    (#:s #:serapeum)
                    (#:re #:cl-ppcre)))

(in-package #:aoc2022-day11)

#|
monkeys operate based on how WORRIED you are about each item

ITEMS are listed by the WORRY LEVEL
OP shows how much the WORRY changes as the monkey inspect the item.
TEST shows how the WORRY is used to decide who to throw the item

AFTER a monkey inpects an item, but BEFORE the test. The WORRY is """divided by 3""" and round down to closes integer

monkeys take TURNS, where it thrws all his items in one turn, in order one at the time

in monkey ORDER

until a ROUND of monkeys is done

received at end of the monkey list

OUTPUT = "monkey business"
= multiplying the number of items inspected by the busiest monkeys

= 10605

|#

(defstruct monkey
  (id 0)
  (items ())
  (operation #'identity)
  (divisor 1)
  (iftrue 0)
  (iffalse 0)
  (counter 0))

(defun monkeys (filename &aux monkeys)
  (flet ((f1 (s) (s:~>> s (re:split ",") (mapcar #'parse-integer)))
         (f2 (op a b) (eval
                       (read-from-string
                        (format nil "(lambda (old) (~a ~a ~a))" op a b)))))
    (re:do-register-groups
        ((#'parse-integer id)
         (#'f1 items)
         a op b
         (#'parse-integer divisor iftrue iffalse))
        ((s:string-join
          '("Monkey (\\d+):\\s+"
            "Starting items: (\\d+(?:,\\s*\\d+)*)\\s+"
            "Operation: new = (.*) ([\\+\\*]) (.*)\\n\\s+"
            "Test: divisible by (\\d+)\\s+"
            "If true: throw to monkey (\\d+)\\s+"
            "If false: throw to monkey (\\d+)") "")
         (a:read-file-into-string filename)
         monkeys)
      (s:push-end (make-monkey :id id :items items
                               :operation (f2 op a b)
                               :divisor divisor
                               :iftrue iftrue :iffalse iffalse)
                  monkeys))))

(-> new-monkey (Integer Monkey) Integer)
(defun new-monkey (updated-item monkey)
  (if (zerop (mod updated-item (monkey-divisor monkey)))
      (monkey-iftrue monkey)
      (monkey-iffalse monkey)))

(-> update-worry (Integer Monkey) Integer)
(defun update-worry (item monkey)
  (floor (/ (funcall (monkey-operation monkey) item) 3)))

(defun silver (filename &key (rounds 20) &aux (monkeys (monkeys filename)))
  (dotimes (i rounds)
    (format t "Round: ~d~%" (1+ i))
    (dolist (monkey monkeys)
      (loop :for item := (pop (monkey-items monkey))
            :while item
            :for new-item := (update-worry item monkey)
            :for new-monkey := (new-monkey new-item monkey)
            :do (s:push-end new-item (monkey-items (nth new-monkey monkeys)))
                (incf (monkey-counter monkey)))))
  (arrows:-<>> (mapcar #'monkey-counter monkeys)
               (sort <> #'>)
               (s:take 2)
               (reduce #'*)))
