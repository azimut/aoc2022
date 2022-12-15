(ql:quickload '(#:defpackage-plus #:alexandria #:serapeum #:trivia #:cl-slice #:cl-ppcre #:rtg-math))

(defpackage+-1:defpackage+ #:aoc2022-day15
  (:use       #:cl #:trivia #:rtg-math)
  (:import-from #:serapeum #:-> #:~>> #:~>)
  (:local-nicknames (#:a #:alexandria) (#:s #:serapeum) (#:re #:cl-ppcre)))

(in-package #:aoc2022-day15)

;; SILVER = 4886370

(defun slurp (f)   (a:read-file-into-string f))
(defun spit  (s f) (a:write-string-into-file s f :if-exists :supersede))

(defstruct (cave (:constructor %make-cave)) mat x-offset y-offset)
(defstruct sensor (pos (v2!int 0 0) :type rtg-math.types:ivec2) (closest 0 :type (signed-byte 32)) beacon)
(defstruct beacon (pos (v2!int 0 0) :type rtg-math.types:ivec2))

(-> manhattan-distance (rtg-math.types:ivec2 rtg-math.types:ivec2) (signed-byte 32))
(defun manhattan-distance (from to)
  (+ (abs (- (x from) (x to)))
     (abs (- (y from) (y to)))))

(defun readings (filename &aux readings)
  (re:do-register-groups
      ((#'parse-integer s1 s2 b1 b2))
      ("Sensor at x=([-]?\\d+), y=([-]?\\d+): closest beacon is at x=([-]?\\d+), y=([-]?\\d+)"
       (slurp filename) readings)
    (push (make-sensor 
           :pos (v2!int s1 s2)
           :beacon (make-beacon :pos (v2!int b1 b2))
           :closest (manhattan-distance (v2!int s1 s2) (v2!int b1 b2)))
          readings)))

(-> v2= (rtg-math.types:ivec2 rtg-math.types:ivec2) Boolean)
(defun v2= (a b)
  (and (= (x a) (x b))
       (= (y a) (y b))))

(defun silver (sensors y-row &optional (magic-offset 2000000))
  (let* ((beacons-pos (~>> sensors (mapcar #'sensor-beacon) (mapcar #'beacon-pos)))
         (sensors-pos (~>> sensors (mapcar #'sensor-pos)))
         (all-pos     (append beacons-pos sensors-pos))
         (x-min       (~>> all-pos (mapcar #'x) (a:extremum _ #'<)))
         (y-min       (~>> all-pos (mapcar #'y) (a:extremum _ #'<)))
         (x-max       (~>> all-pos (mapcar #'x) (a:extremum _ #'>)))
         (y-max       (~>> all-pos (mapcar #'y) (a:extremum _ #'>)))
         (thing-in-row (first
                        (~>> (print all-pos)
                             (remove-if-not (lambda (v) (= y-row (y v))))))))
    (assert (<= y-min y-row y-max))
    (loop :for x :from (- x-min magic-offset) :to (+ x-max magic-offset)
          :for test-coord := (v2!int x y-row)
          :counting 
          (loop :for sensor :in sensors
                :thereis (and (not (v2= test-coord thing-in-row))
                              (<= (manhattan-distance (sensor-pos sensor) test-coord)
                                  (sensor-closest sensor)))))))
