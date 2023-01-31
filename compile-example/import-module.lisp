(module ImportModule)

(import Math.fact)
(import Math.fact2)
(import Math.sum)
(import Math.product)
(import Module.map)
(import Module.compose)
(import Module.filter2)

(println (map (lambda (x) (+ x 3)) [1 2 3 4 5]))

(println (fact 5))
(println (sum [1 2 3 4 5]))
(println (product [1 2 3 4 5]))

(fn f (x) (+ x "F"))
(fn g (x) (+ x "G"))
(fn h (x) (+ x "H"))
(fn . (f g) (compose f g))
(println ((. (. f g) h) "Text"))

(println (filter2 [1 2 3 4 5] (lambda (x) (> x 2))))

(println (fact2 1 5))