(defun list-from-to (from to)
  (loop for i from from to to collect i)
)

(defun vector-from-to (from to)
  (if (< from to)
    (loop
      with r = (make-array (- to from))
      for i from from below to
      do (setf (aref r (- i from)) i)
      finally (return r)
    )
    #()
  )
)

(defun list-range (n)
  (loop for i from 0 below n collect i)
)

(defun vector-range (n)
  (loop
    with r = (make-array n)
    for i from 0 below n
    do (setf (aref r i) i)
    finally (return r)
  )
)

(defmacro def-vec-op (type name op)
  `(defun ,name (&rest vs) (apply #'map ,type ,op vs))
)

(defmacro def-vec-type (type &rest ops)
  (cons 'progn (loop for (name op) in ops collect
    `(def-vec-op ,type ,name ,op)
  ))
)

(def-vec-type 'vector (v+ #'+) (v- #'-) (v* #'*) (v/ #'/))
(def-vec-type 'list (l+ #'+) (l- #'-) (l* #'*) (l/ #'/))
