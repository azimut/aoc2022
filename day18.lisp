;; (ql:quickload '(#:defpackage-plus #:alexandria #:serapeum #:cl-ppcre #:rtg-math #:series))

(defpackage+-1:defpackage+ #:aoc2022-day18
  (:use #:cl #:rtg-math #:series)
  (:import-from #:serapeum #:-> #:~>> #:~>)
  (:local-nicknames (#:a #:alexandria) (#:s #:serapeum) (#:re #:cl-ppcre)))

(in-package #:aoc2022-day18)

;; OUTPUT count all sides that are NOT connected to another
;; PART1 = 64 = 4548
;; PART2 = 58 = 2588

(s:eval-always (series::install))

(-> voxels (string) list)
(defun voxels (filename)
  (with-open-file (f filename)
    (loop :for line := (read-line f nil nil)
          :while line
          :for (x y z) := (re:split "," line)
          :collect (v! (parse-integer x) (parse-integer y) (parse-integer z)))))

(-> neighbour-p (rtg-math.types:vec3 rtg-math.types:vec3) boolean)
(defun neighbour-p (a b)
  (or (v3:= b (v3:+ a (v!  0  0 +1)))
      (v3:= b (v3:+ a (v!  0  0 -1)))
      (v3:= b (v3:+ a (v! +1  0  0)))
      (v3:= b (v3:+ a (v! -1  0  0)))
      (v3:= b (v3:+ a (v!  0 -1  0)))
      (v3:= b (v3:+ a (v!  0 +1  0)))))

;;------------------------------

(s:defsubst neighbours-to-faces (n) (abs (- n 6)))
(defun silver (filename &aux (voxels (voxels filename)))
  (collect-sum
   (mapping ((voxel1 (scan voxels)))
     (neighbours-to-faces
      (collect-length
       (choose-if (s:op (neighbour-p voxel1 _))
                  (scan voxels)))))))

(defun silver (filename &aux (voxels (voxels filename)))
  (collect-sum
   (#M(s:op (abs (- _ 6)))
      (mapping ((voxel1 (scan voxels)))
        (collect-length
         (choose-if (s:op (neighbour-p voxel1 _))
                    (scan voxels)))))))

(defun silver (filename &aux (voxels (voxels filename)))
  (~>> (loop :for voxel1 :in voxels
             :collect (loop :for voxel2 :in voxels
                            :counting (neighbour-p voxel1 voxel2)))
       (mapcar (s:op (abs (- _ 6))))
       (reduce #'+)))

;;------------------------------

(defun neightbours (v3)
  (list (v3:+ v3 (v!  0  0 +1))
        (v3:+ v3 (v!  0  0 -1))
        (v3:+ v3 (v! +1  0  0))
        (v3:+ v3 (v! -1  0  0))
        (v3:+ v3 (v!  0 -1  0))
        (v3:+ v3 (v!  0 +1  0))))

(defun bounded-p (v3 min max)
  (and (<= (x min) (x v3) (x max))
       (<= (y min) (y v3) (y max))
       (<= (z min) (z v3) (z max))))

(defun floodfill (voxels queue min max &aux visited)
  (s:enq min queue)
  (loop :until (s:queue-empty-p queue)
        :for next := (s:deq queue)
        :summing (loop :for neighbour :in (neightbours next)
                       :counting (prog1 (when (bounded-p neighbour min max)
                                          (if (member neighbour voxels :test #'v3:=)
                                              (prog1 T)
                                              (prog1 NIL
                                                (when (not (member neighbour visited :test #'v3:=))
                                                  (s:enq neighbour queue)))))
                                   (push neighbour visited)))))

(defun gold (filename &aux (voxels (voxels filename)))
  (multiple-value-call #'floodfill voxels (s:queue)
    (let ((s (scan voxels)))
      (values
       (v3:- (v! (collect-min (#Mx s)) (collect-min (#My s)) (collect-min (#Mz s)))
             (v! 1 1 1))
       (v3:+ (v! (collect-max (#Mx s)) (collect-max (#My s)) (collect-max (#Mz s)))
             (v! 1 1 1))))))
