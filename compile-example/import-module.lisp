(module ImportModule)

(import Module.map)
(import Math.fact)
(import Math.sum)
(import Math.product)

(println (map (lambda (x) (+ x 3)) [1 2 3 4 5]))

(println (fact 5))
(println (sum [1 2 3 4 5]))
(println (product [1 2 3 4 5]))
