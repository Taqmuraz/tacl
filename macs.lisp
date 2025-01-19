(defmacro -> (&body exprs)
  (reduce
    (lambda (acc e)
      (if (listp e)
        (apply #'list (first e) acc (rest e))
        (list e acc)
      )
    )
    exprs
  )
)

(defmacro ->> (&body exprs)
  (reduce
    (lambda (acc e)
      (if (listp e)
        (append e (list acc))
        (list e acc)
      )
    )
    exprs
  )
)
