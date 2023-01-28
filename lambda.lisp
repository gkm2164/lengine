(def f (lambda (x y) (+ x y)))

(def f-currying (lambda (x)
    (lambda (y)
        (+ x y))))

(println f)
(println (f 3 5))

(println 3)

(def curried (f-currying 3))

(println (curried 5))