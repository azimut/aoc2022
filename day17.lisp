;; (ql:quickload '(#:defpackage-plus #:alexandria #:serapeum #:series #:cl-ppcre #:cl-slice #:array-operations #:bit-smasher))

(defpackage+-1:defpackage+ #:aoc2022-day17
  (:use #:cl)
  (:import-from #:cl-slice #:slice)
  (:import-from #:serapeum #:-> #:~>> #:~>)
  (:local-nicknames (#:a #:alexandria) (#:s #:serapeum) (#:re #:cl-ppcre)))

(in-package #:aoc2022-day17)

(s:eval-always (series::install))

;; CHAMBER 7 units wide
;; PIECES order = - + BackL | [] = 4 5 4 4
;;        repeats (-) falls 1st 6th 11th 16th
;; new PIECES appear
;; - 2 units away from LEFT (his leftest piece)
;; - 3 units away from the highest piece in the room (his bottom)
;; INPUT = (<)left or (>)right displacement of the rocks, repeats
;;
;; MOVEMENT = alternates (even when nothing will happen)
;; - being pushed 1 (one) unit
;; - falling down 1 (one) unit
;; MOVEMENT ends when a push moves it against the floor or other rock
;;          NEW ROCK immediatly appears
;;
;; OUTPUT: how tall is the the tower of rocks will get after 2022 rocks stopped,
;;         before a new one
;; PART 1 = 3068

(defun make-chamber (max-n-rocks &aux (height (* max-n-rocks 5)))
  (assert (plusp max-n-rocks))
  (make-array `(,height 7) :element-type 'bit))

(defun jets (filename)
  (flet ((f (n) (ecase n
                  (#.(char-code #\<) (a:rcurry #'bitsmash:<< 1))
                  (#.(char-code #\>) (a:rcurry #'bitsmash:>> 1)))))
    (~>> (a:read-file-into-byte-vector filename)
         (s:halves _ -1)
         (map 'list #'f)
         (apply #'a:circular-list))))

(defun bit-truncate (bit-vector &aux (max 7))
  (subseq bit-vector (- (length bit-vector) max)))

(defun idx (mat height &aux (dim (length mat)))
  (mod (+ height dim) dim))

(defun push-it (chamber height jet)
  (a:when-let* ((idx        (idx chamber height))
                (unshifted  (slice chamber idx T))
                (shifted    (bit-truncate (funcall jet unshifted)))
                (can-move-p (= (count 1 shifted)
                               (count 1 unshifted))))
    (setf (aref chamber idx) shifted)))

(defun collide-on-fall-p (height shape chamber &aux (dim (array-dimension chamber 0)))
  (/= (count 1 (bit-and (last shape) (slice chamber (idx chamber height) t)))
      (count 1 (last shape))))

(defun fall-it (shape chamber height)
  (unless (collide-on-fall-p height shape chamber)
    (setf (slice chamber ))
    (setf (slice chamber) )))

(defun silver (jets n-rocks &aux (height 0) (chamber (make-chamber n-rocks)))
  (iterate ((round (scan-range :upto n-rocks))
            (shape (series '((#*0011110)
                             (#*0001000 #*0011100 #*0001000)
                             (#*0000100 #*0000100 #*0011100)
                             (#*0010000 #*0010000 #*0010000 #*0010000)
                             (#*0011000 #*0011000)))))
    (format t "Rock ~d of ~d~%" round n-rocks)
    (loop :until (collide-on-fall-p height shape chamber)
          :do (incf height)
              (push-it shape chamber height (pop jets))
              (fall-it shape chamber height)))
  height)

;;------------------------------
;; (auto-revert-mode)
