(defgeneric with-vals (v &rest kvs))

(defmethod with-vals ((v vector) &rest kvs)
  (let ((c (copy-seq v)))
    (loop for (k v) on kvs by #'cddr do (setf (aref c k) v))
    c
  )
)

(defmethod with-vals ((h hash-table) &rest kvs)
  (let ((r (make-hash-table)))
    (maphash (lambda (k v) (setf (gethash k r) v)) h)
    (loop for (k v) on kvs by #'cddr do (setf (gethash k r) v))
    r
  )
)

(defun hash->str (h)
  (with-output-to-string (r)
    (format r "(")
    (maphash (lambda (k v) (format r " (~A ~A)" k v)) h)
    (format r " )")
  )
)

(defun make-hash (&rest kvs)
  (let ((r (make-hash-table)))
    (loop for (k v) on kvs by #'cddr do (setf (gethash k r) v))
    r
  )
)

(defun merge-hash (&rest hs)
  (let ((r (make-hash-table)))
    (dolist (h hs) (maphash (lambda (k v) (setf (gethash k r) v)) h))
    r
  )
)
