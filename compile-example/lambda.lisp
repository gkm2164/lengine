(module Lambda)

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

(def m-f (lambda (x) (+ x 1)))

(println (map m-f [1 2 3 4 5]))

(def v 9)

(def f-3depth (lambda (x)
                (lambda (y)
                  (lambda (z)
                    (+ (+ (+ x y) z) v)))))

(println (((f-3depth 1) 2) 3))

(import Math.fact)

(println (fact 5))

(export map map)
(export fact fact)