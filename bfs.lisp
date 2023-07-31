(s:defsubst hsize (hash) (hash-table-count hash))
(s:defsubst make-nil-array (n) (make-array n :initial-element NIL))

(defun solve (start adj cols &aux (queue (s:queue)))
  (s:lret ((visited (make-nil-array (hsize adj)))
           (prev    (make-nil-array (hsize adj))))
    (s:enq start queue)
    (loop :for e := (s:deq queue)
          :while e
          :for neightboors := (s:href adj e)
          :do (loop :for next :in neightboors
                    :for ridx := (ridx (first next) (second next) cols)
                    :when (not (aref visited ridx))
                      :do (s:enq next queue)
                          (setf (aref visited ridx) T)
                          (setf (aref prev    ridx) e)))))

(defun reconstruct-path (start end prev &aux (path ()))
  (prog1 (print path)
    (push end path)
    (loop :for p :across prev
          :while p
          :do (push p path))
    (a:nreversef path)
    (assert (equal start (first path)))))

(defun bfs (start end cols adj)
  (~>> (solve start adj cols)
       (reconstruct-path start end)))
