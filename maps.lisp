(defmacro forhash (params hash &body body)
  `(maphash (lambda ,params ,@body) ,hash)
)

(defgeneric with-vals (v &rest kvs))

(defmethod with-vals ((v vector) &rest kvs)
  (let ((c (copy-seq v)))
    (loop for (k v) on kvs by #'cddr do (setf (aref c k) v))
    c
  )
)

(defmethod with-vals ((h hash-table) &rest kvs)
  (let ((r (make-hash-table)))
    (forhash (k v) h (setf (gethash k r) v))
    (loop for (k v) on kvs by #'cddr do (setf (gethash k r) v))
    r
  )
)

(defun hash->str (h)
  (with-output-to-string (r)
    (format r "{")
    (forhash (k v) h (format r " ~A ~A" k v))
    (format r " }")
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
    (dolist (h hs) (forhash (k v) h (setf (gethash k r) v)))
    r
  )
)

(defun hash-keys (h)
  (let ((r nil))
    (forhash (k v) h (declare (ignore v)) (setf r (nconc r (list k))))
    r
  )
)

(defun hash-vals (h)
  (let ((r nil))
    (forhash (k v) h (declare (ignore k)) (setf r (nconc r (list v))))
    r
  )
)

(defun hash->assoc (h)
  (let ((r nil))
    (forhash (k v) h (setf r (nconc r (list (cons k v)))))
    r
  )
)

(defun assoc->hash (a)
  (loop with r = (make-hash-table) for (k . v) in a
    do (setf (gethash k r) v)
    finally (return r)
  )
)

(defgeneric update (map func &rest keys))

(defmethod update ((v vector) func &rest keys)
  (let ((c (copy-seq v)))
    (dolist (k keys) (setf (aref c k) (funcall func (aref v k))))
    c
  )
)
