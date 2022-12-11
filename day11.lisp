(ql:quickload '(#:defpackage-plus #:alexandria #:serapeum #:cl-ppcre))

(defpackage+-1:defpackage+ #:aoc2022-day11
  (:use #:cl)
  (:import-from #:serapeum #:-> #:~>>)
  (:local-nicknames (#:a #:alexandria) (#:s #:serapeum) (#:re #:cl-ppcre)))

(in-package #:aoc2022-day11)

#|
PART 1 = 55458
PART 2= 14508081294
|#

(defstruct monkey id items operation divisor iftrue iffalse counter)

(defun monkeys (filename &aux monkeys)
  (flet ((f1 (s) (~>> s (re:split ",") (mapcar #'parse-integer)))
         (f2 (op a b) (eval
                       (read-from-string
                        (format nil "(lambda (old) (~a ~a ~a))" op a b)))))
    (re:do-register-groups
        ((#'parse-integer id) (#'f1 items) a op b (#'parse-integer divisor iftrue iffalse))
        ((s:string-join '("Monkey (\\d+):\\s+"
                          "Starting items: (\\d+(?:,\\s*\\d+)*)\\s+"
                          "Operation: new = (.*) ([\\+\\*]) (.*)\\n\\s+"
                          "Test: divisible by (\\d+)\\s+"
                          "If true: throw to monkey (\\d+)\\s+"
                          "If false: throw to monkey (\\d+)") "")
         (a:read-file-into-string filename) monkeys)
      (s:push-end (make-monkey :id id :items items
                               :operation (f2 op a b) :divisor divisor
                               :iftrue iftrue :iffalse iffalse)
                  monkeys))))

(-> new-monkey (Integer Monkey) Integer)
(defun new-monkey (updated-item monkey)
  (if (zerop (mod updated-item (monkey-divisor monkey)))
      (monkey-iftrue monkey)
      (monkey-iffalse monkey)))

(-> update-worry (Integer Monkey Integer) Integer)
(defun update-worry (item monkey relief)
  (if (/= 3 relief)
      (funcall (monkey-operation monkey) item)
      (floor (/ (funcall (monkey-operation monkey) item) relief))))

(defun resolve (filename &key (rounds 20) (relief 3)
                &aux (monkeys (monkeys filename)) (lcm (apply #'lcm (mapcar #'monkey-divisor monkeys))))
  (dotimes (i rounds)
    (dolist (monkey monkeys)
      (loop :for item := (pop (monkey-items monkey))
            :while item
            :for new-item := (mod (update-worry item monkey relief) lcm)
            :for new-monkey := (new-monkey new-item monkey)
            :do (s:push-end new-item (monkey-items (nth new-monkey monkeys)))
                (incf (monkey-counter monkey)))))
  (~>> (mapcar #'monkey-counter monkeys)
       (sort _ #'>)
       (s:take 2)
       (reduce #'*)))

(defun silver (filename) (resolve filename :rounds 20 :relief 3))
(defun gold (filename) (resolve filename :rounds 10000 :relief 1))
