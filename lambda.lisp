(def f (lambda (x y) (+ x y)))

(def f-currying (lambda (x)
    (lambda (y)
        (+ x y))))

(println f)
(println (f 3 5))

(println 3)

(def curried (f-currying 3))

(println (curried 5))

(fn map (f seq)
  (loop for x in seq
    (f x)))

(println (map (lambda (x) (+ x 1)) [1 2 3 4 5]))