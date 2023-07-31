(ql:quickload '(#:defpackage-plus #:alexandria #:serapeum #:trivia #:cl-slice #:cl-ppcre #:rtg-math))

(defpackage+-1:defpackage+ #:aoc2022-day15
  (:use       #:cl #:trivia #:rtg-math)
  (:import-from #:serapeum #:-> #:~>> #:~>)
  (:local-nicknames (#:a #:alexandria) (#:s #:serapeum) (#:re #:cl-ppcre)))

(in-package #:aoc2022-day15)

;; SILVER =        4886370
;; GOLD   = 11374534948438
;;         #(2843633 2948438)
(defstruct beacon (pos (v2!int 0 0) :type rtg-math.types:ivec2))
(defstruct sensor
  (idx 0 :type fixnum)
  (pos (v2!int 0 0) :type rtg-math.types:ivec2) 
  (closest 0 :type (signed-byte 32))
  (beacon #(beacon (pos (v2!int 0 0))) :type beacon))

(s:defsubst manhattan-distance (from to)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (declare (type rtg-math.types:ivec2 from to))
  (+ (abs (- (x from) (x to)))
     (abs (- (y from) (y to)))))

(defun readings (filename &aux readings (idx 0))
  (re:do-register-groups
      ((#'parse-integer s1 s2 b1 b2))
      ("Sensor at x=([-]?\\d+), y=([-]?\\d+): closest beacon is at x=([-]?\\d+), y=([-]?\\d+)"
       (a:read-file-into-string filename) readings)
    (push (make-sensor 
           :idx (incf idx)
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
         (thing-in-row (first (~>> all-pos (remove-if-not (lambda (v) (= y-row (y v))))))))
    (assert (<= y-min y-row y-max))
    (loop :for x :from (- x-min magic-offset) :to (+ x-max magic-offset)
          :for test-coord := (v2!int x y-row)
          :counting
          (loop :for sensor :in sensors
                :thereis (and (not (v2= test-coord thing-in-row))
                              (<= (manhattan-distance (sensor-pos sensor) test-coord)
                                  (sensor-closest sensor)))))))

;;------------------------------

(defun candidate-sensors (sensors &aux results)
  (dolist (sensor sensors results)
    (push (cons (sensor-idx sensor)
                (remove-if-not
                 (lambda (s) (and (= 2 (abs (- (manhattan-distance (sensor-pos s) (sensor-pos sensor))
                                          (+ (sensor-closest sensor) (sensor-closest s)))))
                             (/= (sensor-idx sensor) (sensor-idx s))))
                 sensors))
          results)))

(defun candidate-positions (sensors)
  (~>> sensors
       (candidate-sensors)
       (a:flatten)
       (remove-if-not #'sensor-p)
       (remove-duplicates)
       (mapcar #'sensor-pos)))

(-> search-beacon (list (signed-byte 32) (signed-byte 32) (signed-byte 32)  (signed-byte 32)) rtg-math.types:ivec2)
(defun search-beacon (sensors min-x max-x min-y max-y &aux (found (v2!int 0 0)))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (prog1 found
    (block outer
      (loop :for x :from max-x :downto min-x
            :do (loop :for y :from max-y :downto min-y
                      :do (setf (x (the rtg-math.types:ivec2 found)) (the fixnum x))
                          (setf (y (the rtg-math.types:ivec2 found)) (the fixnum y))
                          (unless (loop :for sensor :in sensors
                                        :thereis (<= (manhattan-distance found (sensor-pos sensor))
                                                     (sensor-closest sensor)))
                            (return-from outer)))))))

(defun sensors-in-range (sensors min-x max-x min-y max-y)
  (remove-if-not (lambda (sensor) (and (<= min-x (x (sensor-pos sensor)) max-x)
                                  (<= min-y (y (sensor-pos sensor)) max-y)))
                 sensors))
(-> gold (string) rtg-math.types:ivec2)
(defun gold (filename &aux (sensors (readings filename)) (positions (candidate-positions sensors)))
  (let ((min-x (~>> positions (mapcar #'x) (apply #'min)))
        (min-y (~>> positions (mapcar #'y) (apply #'min)))
        (max-x (~>> positions (mapcar #'x) (apply #'max)))
        (max-y (~>> positions (mapcar #'y) (apply #'max))))
    (format t "X = ~d-~d  Y = ~d-~d~%" min-x max-x min-y max-y)
    (search-beacon (sensors-in-range sensors min-x max-x min-y max-y)
                   min-x max-x min-y max-y)))
