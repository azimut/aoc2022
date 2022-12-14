(ql:quickload
 '(#:defpackage-plus
   #:alexandria
   #:serapeum
   #:trivia
   #:cl-slice
   #:cl-ppcre))

(defpackage+-1:defpackage+ #:aoc2022-day14
  (:use #:cl #:trivia)
  (:import-from #:serapeum #:-> #:~>> #:~>)
  (:local-nicknames (#:a #:alexandria) (#:s #:serapeum) (#:re #:cl-ppcre)))

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
  (loop :for y :from (y src) :to (y dst)
        :do (setf (aref (cave-mat cave)
                        y
                        (- x (cave-offset cave)))
                  #\#)))

(defun vertical-draw (src dst cave &aux (y (y src)))
  (loop :for x :from (x dst) :to (x src)
        :do (setf (aref (cave-mat cave) 
                        y
                        (- x (cave-offset cave)))
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
         (width  (+ 1 (- (a:extremum xs #'>) (a:extremum xs #'<))))
         (height (+ 1 (a:extremum ys #'>))))
    (~>> (%make-cave 
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
  (aref (cave-mat cave) (+ (x pos) (cave-offset cave)) (y pos)))

(-> down       (coord) coord)
(-> down-left  (coord) coord)
(-> down-right (coord) coord)
(defun down       (c) (vector (- (x c) 0) (- (y c) 1)))
(defun down-left  (c) (vector (- (x c) 1) (- (y c) 1)))
(defun down-right (c) (vector (+ (x c) 1) (- (y c) 1)))

(-> can-fall-p (cave coord) (or (member :void :rest) coord))
(defun can-fall-p (cave current-pos)
  (cond ((> (cave-floor cave) (y current-pos)) 
         :void)
        ((char= #\. (lookup cave (down current-pos)))
         (down current-pos))
        ((char= #\. (lookup cave (down-left current-pos)))
         (down-left current-pos))
        ((char= #\. (lookup cave (down-right current-pos)))
         (down-right current-pos))
        (t :rest)))

(-> drop-sand (cave coord) boolean)
(defun drop-sand (cave pos)
  (match (can-fall-p cave pos)
    ((cons x y) (drop-sand cave (vector x y)))
    (:rest T)
    (:void NIL)))

(-> simulation-loop (cave) integer)
(defun simulation-loop (cave)
  (loop :while (drop-sand cave #(500 0))
        :counting T))
