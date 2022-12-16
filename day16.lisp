(ql:quickload
 '(#:defpackage-plus
   #:alexandria
   #:serapeum
   #:trivia
   #:cl-slice
   #:cl-ppcre
   #:rtg-math))

(defpackage+-1:defpackage+ #:aoc2022-day16
  (:use #:cl #:trivia #:rtg-math)
  (:import-from #:serapeum #:-> #:~>> #:~>)
  (:local-nicknames (#:a #:alexandria)
                    (#:s #:serapeum)
                    (#:re #:cl-ppcre)))

(in-package #:aoc2022-day16)

;; OUTPUT max possible total pressure released in 30 minutes
;; PART 1 = 1651

(defun lines (s)   (re:split "\\n" s))
(defun slurp (f)   (a:read-file-into-string f))

(defstruct node id rate neighbours)

(defun parse (filename &aux raw-nodes nodes (nodes-map (make-hash-table)))
  (flet ((f (s) (~>> s (re:split ", ") (mapcar #'a:make-keyword))))
    (re:do-register-groups
        ((#'a:make-keyword valve)
         (#'parse-integer rate)
         (#'f to-valves))
        ("Valve ([A-Z]+) has flow rate=(\\d+); tunnel[s]? lead[s]? to valve[s]? (.+)"
         (slurp filename))
      (push `(,valve ,rate ,to-valves) raw-nodes)
      (push (setf (s:href nodes-map valve) 
                  (make-node :id valve :rate rate :neighbours to-valves))
            nodes)))
  (assert (a:length= raw-nodes (length (lines (slurp filename)))))
  (list :first-node (caar (reverse raw-nodes))
        :nodes-map nodes-map
        :nodes (reverse nodes)))

(defun find-max-neighbour-greedy (node node-map)
  (~>> (node-neighbours node)
       (mapcar (lambda (id) (s:href node-map id)))
       (sort _ #'> :key #'node-rate)
       (first)))

(defun silver-greedy (filename &aux current-node)
  (destructuring-bind (&key first-node nodes-map nodes) (parse filename)
    (setf current-node (s:href nodes-map first-node))
    (loop :for i :from 0 :to 30
          :with pressure := 0
          :with acc := 0
          :with turned-p
          :if (and (plusp (node-rate current-node)) (not turned-p))
            :do (incf i)
                (setf turned-p T)
          :else
            :do 
               (when turned-p
                 (incf pressure (node-rate current-node)))
               (setf current-node (find-max-neighbour-greedy current-node nodes-map))
               (setf turned-p NIL)
          :do
             (incf acc pressure)
          :finally 
             (return acc))))
