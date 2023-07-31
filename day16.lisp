;; (ql:quickload '(#:defpackage-plus #:alexandria #:serapeum #:cl-ppcre))

(defpackage+-1:defpackage+ #:aoc2022-day16
  (:use #:cl)
  (:import-from #:serapeum #:-> #:~>> #:~>)
  (:local-nicknames (#:a #:alexandria) (#:s #:serapeum) (#:re #:cl-ppcre)))

(in-package #:aoc2022-day16)

(defstruct node
  (name :? :type keyword)
  (id -1 :type fixnum)
  (rate -1 :type fixnum)
  (cost 1 :type fixnum)
  (neighbours () :type list))

;; OUTPUT max possible total pressure released in 30 minutes
;; PART 1 = 1651 = ??

(defun lines (s)   (re:split "\\n" s))
(defun slurp (f)   (a:read-file-into-string f))

(defun parse (filename &aux raw-nodes nodes (nodes-map (make-hash-table)) (id 0))
  (flet ((f (s) (~>> s (re:split ", ") (mapcar #'a:make-keyword))))
    (re:do-register-groups
        ((#'a:make-keyword valve)
         (#'parse-integer rate)
         (#'f to-valves))
        ("Valve ([A-Z]+) has flow rate=(\\d+); tunnel[s]? lead[s]? to valve[s]? (.+)"
         (slurp filename))
      (push `(,valve ,rate ,to-valves) raw-nodes)
      (push (setf (s:href nodes-map valve) 
                  (make-node :name valve :id (1- (incf id))
                             :rate rate :neighbours to-valves))
            nodes)))
  (assert (a:length= raw-nodes (length (lines (slurp filename)))))
  (list :start (caar (reverse raw-nodes))
        :nodes-map nodes-map
        :results (make-array (* 30 (reduce #'+ (mapcar #'node-rate nodes))) :element-type 'bit)        
        :nodes (reverse nodes)))

(-> dfs (keyword hash-table (simple-bit-vector *) fixnum fixnum fixnum list) 
    (values))
(defun dfs (at nodes-hash results minutes sum-pressure current-pressure opened-valves)
  (declare (optimize (speed 3)))
  (declare (fixnum current-pressure sum-pressure))
  (when (minusp minutes) (setf (bit results sum-pressure) 1))
  (when (plusp minutes)
    (incf sum-pressure current-pressure)
    (dolist (next (node-neighbours (gethash at nodes-hash)))
      (when (and (plusp (node-rate (gethash at nodes-hash)))
                 (not (member at opened-valves)))
        (dfs next nodes-hash results 
             (+ -2 minutes)
             (+ sum-pressure current-pressure)
             (+ current-pressure (node-rate (gethash at nodes-hash))) 
             (cons at opened-valves)))
      (dfs next nodes-hash results
           (+ -1 minutes)
           sum-pressure
           current-pressure
           opened-valves))))

(-> silver (string) fixnum)
(defun silver (filename)
  (destructuring-bind (&key start nodes-map results &allow-other-keys) (parse filename)
    (declare (type hash-table nodes-map)
             (type (simple-bit-vector *) results)
             (type keyword start))
    (dfs start nodes-map results 30 0 0 '())
    (position 1 results :from-end t)))
