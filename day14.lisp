(ql:quickload '(#:defpackage-plus #:alexandria #:serapeum #:trivia #:cl-slice #:cl-ppcre))

(defpackage+-1:defpackage+ #:aoc2022-day14
  (:use #:cl #:trivia)
  (:import-from #:serapeum #:-> #:~>> #:~>)
  (:local-nicknames (#:a #:alexandria) (#:s #:serapeum) (#:re #:cl-ppcre)))

;; GOLD = 29805

(in-package #:aoc2022-day14)

(deftype coord () '(simple-vector 2))
(defstruct (cave (:constructor %make-cave)) mat offset (floor 0))

(-> x (coord) integer)
(-> y (coord) integer)
(defun x (c) (aref c 0))
(defun y (c) (aref c 1))

(defun lines (s)   (re:split "\\n" s))
(defun trim  (s)   (s:trim-whitespace s))
(defun slurp (f)   (a:read-file-into-string f))
(defun spit  (s f) (a:write-string-into-file s f :if-exists :supersede))

(defun paths (f)
  (flet ((f (s) (destructuring-bind (x y) (re:split "," s)
                  (vector (parse-integer x)
                          (parse-integer y)))))
    (~>> f slurp trim lines
         (mapcar (lambda (path) (re:split " -> " path)))
         (mapcar (lambda (coord) (mapcar #'f coord) )))))

(defun horizontal-draw (src dst cave &aux (x (x src)))
  (loop :for y
          :from (min (y dst) (y src))
            :to (max (y src) (y dst))
        :do (setf (aref (cave-mat cave) y (+ 244 (- x (cave-offset cave))))
                  #\#)))

(defun vertical-draw (src dst cave &aux (y (y src)))
  (loop :for x
          :from (+ 244 (min (x dst) (x src)))
            :to (+ 244 (max (x dst) (x src)))
        :do (setf (aref (cave-mat cave) y (- x (cave-offset cave)))
                  #\#)))

(defun draw-rock-path (src dst cave)
  (ematch (cons src dst)
    ((guard (cons (vector x1 _) (vector x2 _)) (= x1 x2))
     (horizontal-draw src dst cave))
    ((guard (cons (vector _ y1) (vector _ y2)) (= y1 y2))
     (vertical-draw src dst cave))))

(-> place-rocks (list cave) cave)
(defun place-rocks (paths cave)
  (dolist (path paths cave)
    (loop :for (src-coord dst-coord) :on path
          :while (and src-coord dst-coord)
          :do (draw-rock-path src-coord dst-coord cave))))

(-> make-cave (string) cave)
(defun make-cave (filename &aux (paths (paths filename)))
  (let* ((xs     (~> paths (a:flatten) (mapcar #'x _)))
         (ys     (~> paths (a:flatten) (mapcar #'y _)))
         (width  (+ 489 (- (a:extremum xs #'>) (a:extremum xs #'<))))
         (height (+ 3 (a:extremum ys #'>))))
    (~>> (%make-cave
          :floor (a:extremum ys #'>)
          :mat (make-array `(,height ,width) :initial-element #\.)
          :offset (a:extremum xs #'<))
         (place-rocks paths))))

(defun draw-cave (cave &aux (mat (cave-mat cave)))
  (~>> (loop :for i :from 0 :below (array-dimension mat 0)
             :collect (coerce (cl-slice:slice mat i t) 'string))
       (s:string-join _ #\Newline)
       (format nil "~%~a")))

;;----------------------------------------

(-> lookup (cave coord) standard-char)
(defun lookup (cave pos)
  (aref (cave-mat cave) (y pos) (- (x pos) (cave-offset cave))))

(-> down       (coord) coord)
(-> down-left  (coord) coord)
(-> down-right (coord) coord)
(defun down       (c) (vector (- (x c) 0) (+ (y c) 1)))
(defun down-left  (c) (vector (- (x c) 1) (+ (y c) 1)))
(defun down-right (c) (vector (+ (x c) 1) (+ (y c) 1)))

(-> can-fall-p (cave coord) (or (member :void :rest) coord))
(defun can-fall-p (cave current-pos)
  (cond ((>= (y current-pos) (cave-floor cave))
         :void)
        ((char= #\. (lookup cave (down current-pos)))
         (setf (aref (cave-mat cave) (y current-pos) (- (x current-pos) (cave-offset cave))) #\.)
         (setf (aref (cave-mat cave) (y (down current-pos)) (- (x (down current-pos)) (cave-offset cave))) #\o)
         (down current-pos))
        ((char= #\. (lookup cave (down-left current-pos)))
         (setf (aref (cave-mat cave) (y current-pos) (- (x current-pos) (cave-offset cave))) #\.)
         (setf (aref (cave-mat cave) (y (down-left current-pos)) (- (x (down-left current-pos)) (cave-offset cave))) #\o)
         (down-left current-pos))
        ((char= #\. (lookup cave (down-right current-pos)))
         (setf (aref (cave-mat cave) (y current-pos) (- (x current-pos) (cave-offset cave))) #\.)
         (setf (aref (cave-mat cave) (y (down-right current-pos)) (- (x (down-right current-pos)) (cave-offset cave))) #\o)
         (down-right current-pos))
        (t :rest)))

(-> drop-sand (cave coord) boolean)
(defun drop-sand (cave pos)
  (ematch (can-fall-p cave pos)
    ((vector x y) (drop-sand cave (vector x y)))
    (:rest T)
    (:void NIL)))

(-> silver (cave) integer)
(defun silver (cave &aux (sand-source #(764 0)))
  (loop :while (drop-sand cave sand-source)
        :counting T))

;;--------------------------------------------------

(defun can-fall-gold-p (cave current-pos)
  (cond ((char= #\. (lookup cave (down current-pos)))
         (setf (aref (cave-mat cave) (y current-pos) (- (x current-pos) (cave-offset cave))) #\.)
         (setf (aref (cave-mat cave) (y (down current-pos)) (- (x (down current-pos)) (cave-offset cave))) #\o)
         (down current-pos))
        ((char= #\. (lookup cave (down-left current-pos)))
         (setf (aref (cave-mat cave) (y current-pos) (- (x current-pos) (cave-offset cave))) #\.)
         (setf (aref (cave-mat cave) (y (down-left current-pos)) (- (x (down-left current-pos)) (cave-offset cave))) #\o)
         (down-left current-pos))
        ((char= #\. (lookup cave (down-right current-pos)))
         (setf (aref (cave-mat cave) (y current-pos) (- (x current-pos) (cave-offset cave))) #\.)
         (setf (aref (cave-mat cave) (y (down-right current-pos)) (- (x (down-right current-pos)) (cave-offset cave))) #\o)
         (down-right current-pos))
        (t :rest)))

(defun drop-gold-sand (cave pos)
  (ematch (can-fall-gold-p cave pos)
    ((vector x y) (drop-gold-sand cave (vector x y)))
    (:rest T)
    (:void NIL)))

(defun fill-cave-floor (cave)
  (dotimes (i (array-dimension (cave-mat cave) 1))
    (setf (aref (cave-mat cave)
                (1- (array-dimension (cave-mat cave) 0)) i)
          #\#)))

(defun gold (cave &optional (sand-source #(744 0)))
  (fill-cave-floor cave)
  (loop :until (and (char= #\o (aref (cave-mat cave) (1+ (y sand-source)) (- (x sand-source) (cave-offset cave))))
                    (char= #\o (aref (cave-mat cave) (1+ (y sand-source)) (+ 1 (- (x sand-source) (cave-offset cave)))))
                    (char= #\o (aref (cave-mat cave) (1+ (y sand-source)) (+ -1 (- (x sand-source) (cave-offset cave))))))
        :do (drop-gold-sand cave sand-source))
  (loop :for x :from 0 :below (array-dimension (cave-mat cave) 0)
        :summing (loop :for y :from 0 :below (array-dimension (cave-mat cave) 1)
                       :counting (char= #\o (aref (cave-mat cave) x y)))))
