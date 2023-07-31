;; (ql:quickload '(:defpackage-plus :vecto))

(defpackage+-1:defpackage+ #:aoc2022-day0
  (:use #:cl #:vecto)
  (:local-nicknames (#:re #:cl-ppcre)))

(in-package #:aoc2022-day0)

;; BAR
(let* ((y 13))
  (with-canvas (:width 600 :height (* 2 y))
    (let ((font (get-font "/usr/share/minetest/fonts/Cousine-Italic.ttf"))
          (step (/ pi 7)))
      (set-font font 18))
    (let ((x 500) (2y (* 2 y)))
      (set-rgba-fill  0.3 0.3 0.3 0.3)
      (rounded-rectangle 0 0 x 2y 10 10 )
      (set-gradient-fill  2 2y
                          0.4 0.4 0.4 0.8
                          2 y
                          0 0 0 0.2)
      (set-gradient-fill  2 y
                          0.4 0.4 0.4 0.9
                          2 0
                          0 0 0 0.2)
      ;; (clip-path)      
      ;; (rectangle 0 0 x y)
      (clip-path)
      (centered-circle-path 2 2 10)
      ;; (centered-circle-path x 2 10)
      (fill-path)

      (set-rgba-fill 255 255 255  1.0)
      (translate (/ x 2) 8)
      (draw-centered-string 0 0 "whadt")
      (fill-path)

      (save-png "/home/sendai/vecto1.png"))))

;; Star
(with-canvas (:width 200 :height 200)
  (let ((size 100)
        (angle 0)
        (step (* 2 (/ (* pi 2) 5))))
    (translate size size)
    (move-to 0 size)
    (dotimes (i 5)
      (setf angle (+ angle step))
      (line-to (* (sin angle) size)
               (* (cos angle) size)))
    (even-odd-clip-path)
    (end-path-no-op)
    (flet ((circle (distance)
             (set-rgba-fill distance 0 0 (- 1.0 distance))
             (centered-circle-path 0 0 (* size distance))
             (fill-path)))
      (loop for i downfrom 1.0 by 0.05
            repeat 20 do
              (circle i)))
    (save-png "/home/sendai/vecto1.png")))

;; LAMBDA
(with-canvas (:width 90 :height 90)
  (let (;(font (get-font "times.ttf"))
        (step (/ pi 7)))
    ;; (set-font font 40)
    (translate 45 45)
    ;; (draw-centered-string 0 -10 #(#x3BB))
    (set-rgb-stroke 1 0 0)
    (centered-circle-path 0 0 35)
    (stroke)
    (set-rgba-stroke 0 0 1.0 0.5)
    (set-line-width 4)
    (dotimes (i 14)
      (with-graphics-state
        (rotate (* i step))
        (move-to 30 0)
        (line-to 40 0)
        (stroke)))
    (save-png "/home/sendai/vecto1.png")))

;; RSS
(with-canvas (:width 100 :height 100)
  ;; []
  (set-rgb-fill 1.0 0.65 0.3)
  (rounded-rectangle 0 0 100 100 10 10)
  (fill-path)
  ;; o
  (set-rgb-fill 1.0 1.0 1.0)
  (centered-circle-path 20 20 10)
  (fill-path)
  ;; ))
  (flet ((quarter-circle (x y radius)
           (move-to (+ x radius) y)
           (arc x y radius 0 (/ pi 2))))

    (set-rgb-stroke 1.0 1.0 1.0)
    (set-line-width 15)

    (quarter-circle 20 20 30)
    (stroke)
    (quarter-circle 20 20 60)
    (stroke))
  ;; []
  (rounded-rectangle 5 5 90 90 7 7)
  (set-gradient-fill 50 90
                     1.0 1.0 1.0 0.7
                     50 20
                     1.0 1.0 1.0 0.0)
  (set-line-width 2)
  (set-rgba-stroke 1.0 1.0 1.0 0.1)
  (fill-and-stroke)
  
  (save-png "/home/sendai/vecto1.png"))
